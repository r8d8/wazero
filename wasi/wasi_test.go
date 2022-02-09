package wasi

import (
	"context"
	_ "embed"
	"encoding/binary"
	"math"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/tetratelabs/wazero/wasm"
	"github.com/tetratelabs/wazero/wasm/interpreter"
	"github.com/tetratelabs/wazero/wasm/text"
)

func TestNewWasiStringArray(t *testing.T) {
	tests := []struct {
		name            string
		args            []string
		expectedBufSize uint32
	}{
		{
			name:            "nil args",
			args:            nil,
			expectedBufSize: 0,
		},
		{
			name:            "empty",
			args:            []string{},
			expectedBufSize: 0,
		},
		{
			name:            "simple",
			args:            []string{"foo", "bar", "foobar", "", "baz"},
			expectedBufSize: 20,
		},
		{
			name: "utf-8 string",
			// "😨", "🤣", and "️🏃‍♀️" have 4, 4, and 13 bytes respectively
			args:            []string{"😨🤣🏃\u200d♀️", "foo", "bar"},
			expectedBufSize: 30,
		},
		{
			name:            "invalid utf-8 string",
			args:            []string{"\xff\xfe\xfd", "foo", "bar"},
			expectedBufSize: 12,
		},
	}

	for _, tt := range tests {
		tc := tt

		t.Run(tc.name, func(t *testing.T) {
			wasiStringsArray, err := newWASIStringArray(tc.args)
			require.NoError(t, err)

			require.Equal(t, tc.expectedBufSize, wasiStringsArray.totalBufSize)
			require.Equal(t, len(wasiStringsArray.nullTerminatedValues), len(tc.args))
			for i, arg := range tc.args {
				wasiString := wasiStringsArray.nullTerminatedValues[i]
				require.Equal(t, wasiString[0:len(wasiString)-1], []byte(arg))
				require.Equal(t, wasiString[len(wasiString)-1], byte(0))
			}
		})
	}
}

// argsWat is a wasm module to call args_get and args_sizes_get.
var argsWat = []byte(`
(module
  (import "wasi_snapshot_preview1" "args_sizes_get" (func $wasi_args_sizes_get (param i32 i32) (result i32)))
  (import "wasi_snapshot_preview1" "args_get"       (func $wasi_args_get (param i32 i32) (result i32)))
  (memory 1)  ;; just an arbitrary size big enough for tests
  (export "memory" (memory 0))
  ;; Define proxy functions to let the host test code to call the API
  (func $args_sizes_get (param i32 i32) (result i32)
        local.get 0
        local.get 1
        call $wasi_args_sizes_get
        )
  (func $args_get (param i32 i32) (result i32)
        local.get 0
        local.get 1
        call $wasi_args_get
        )
  (export "args_sizes_get" (func $args_sizes_get))
  (export "args_get" (func $args_get))
  )
`)

// common test cases used by TestWASIEnvironment_args_sizes_get and TestWASIEnvironment_args_get
var argsTests = []struct {
	name            string
	args            []string
	expectedArgs    [][]byte
	expectedBufSize uint32
}{
	{
		name:            "no args",
		args:            nil,
		expectedArgs:    [][]byte{},
		expectedBufSize: 0,
	},
	{
		name:            "empty",
		args:            []string{},
		expectedArgs:    [][]byte{},
		expectedBufSize: 0,
	},
	{
		name: "simple",
		args: []string{"foo", "bar", "foobar", "", "baz"},
		expectedArgs: [][]byte{
			[]byte("foo\x00"),
			[]byte("bar\x00"),
			[]byte("foobar\x00"),
			[]byte("\x00"),
			[]byte("baz\x00"),
		},
		expectedBufSize: 20,
	},
	{
		name: "utf-8 string",
		// "😨", "🤣", and "️🏃‍♀️" have 4, 4, and 13 bytes respectively
		args: []string{"😨🤣🏃\u200d♀️", "foo", "bar"},
		expectedArgs: [][]byte{
			[]byte("😨🤣🏃\u200d♀️\x00"),
			[]byte("foo\x00"),
			[]byte("bar\x00"),
		},
		expectedBufSize: 30,
	},
	{
		name: "invalid utf-8 string",
		args: []string{"\xff\xfe\xfd", "foo", "bar"},
		expectedArgs: [][]byte{
			[]byte("\xff\xfe\xfd\x00"),
			[]byte("foo\x00"),
			[]byte("bar\x00"),
		},
		expectedBufSize: 12,
	},
}

func TestWASIEnvironment_args_sizes_get_Succeed(t *testing.T) {
	ctx := context.Background()

	for _, tt := range argsTests {
		tc := tt

		t.Run(tc.name, func(t *testing.T) {
			opts := []Option{}
			if tc.args != nil {
				argsOpt, err := Args(tc.args)
				require.NoError(t, err)
				opts = append(opts, argsOpt)
			}
			wasiEnv := NewEnvironment(opts...)
			store := instantiateWasmStore(t, argsWat, "test", wasiEnv)

			argCountPtr := uint32(0)    // arbitrary valid address
			bufSizePtr := uint32(0x100) // arbitrary valid address that doesn't overwrap with argCountPtr
			ret, _, err := store.CallFunction(ctx, "test", "args_sizes_get", uint64(argCountPtr), uint64(bufSizePtr))
			require.NoError(t, err)
			require.Equal(t, uint64(ESUCCESS), ret[0]) // ret[0] is errno
			require.Equal(t, uint32(len(tc.expectedArgs)), binary.LittleEndian.Uint32(store.Memories[0].Buffer[argCountPtr:]))
			require.Equal(t, tc.expectedBufSize, binary.LittleEndian.Uint32(store.Memories[0].Buffer[bufSizePtr:]))
		})
	}
}

func TestWASIEnvironment_args_get_Succeed(t *testing.T) {
	ctx := context.Background()

	for _, tt := range argsTests {
		tc := tt

		t.Run(tc.name, func(t *testing.T) {
			opts := []Option{}
			if tc.args != nil {
				argsOpt, err := Args(tc.args)
				require.NoError(t, err)
				opts = append(opts, argsOpt)
			}
			wasiEnv := NewEnvironment(opts...)
			store := instantiateWasmStore(t, argsWat, "test", wasiEnv)

			// Serialize the expected result of args_get
			expectedArgs := make([]byte, 4*len(tc.args)) // expected size of the pointers to the args. 4 is the size of uint32
			argsPtr := uint32(0)                         // arbitrary valid address
			expectedArgv := make([]byte, tc.expectedBufSize)
			argvPtr := uint32(0x100) // arbitrary valid address that doesn't overwrap with argsPtr
			argvWritten := uint32(0)
			for i, arg := range tc.expectedArgs {
				binary.LittleEndian.PutUint32(expectedArgs[argsPtr+uint32(i*4):], argvPtr+argvWritten) // 4 is the size of uint32
				copy(expectedArgv[argvWritten:], arg)
				argvWritten += uint32(len(arg))
			}

			// Compare them
			ret, _, err := store.CallFunction(ctx, "test", "args_get", uint64(argsPtr), uint64(argvPtr))
			require.NoError(t, err)
			require.Equal(t, uint64(ESUCCESS), ret[0]) // ret[0] is the returned errno
			require.Equal(t, expectedArgs, store.Memories[0].Buffer[argsPtr:argsPtr+uint32(len(expectedArgs))])
			require.Equal(t, expectedArgv, store.Memories[0].Buffer[argvPtr:argvPtr+uint32(len(expectedArgv))])
		})
	}
}

func TestWASIEnvironment_args_sizes_get_ReturnError(t *testing.T) {
	ctx := context.Background()
	validArgs := []string{"foo", "bar", "baz"} // arbitrary valid args
	argsOpt, err := Args(validArgs)
	require.NoError(t, err)
	wasiEnv := NewEnvironment(argsOpt)
	store := instantiateWasmStore(t, argsWat, "test", wasiEnv)

	memorySize := uint32(len(store.Memories[0].Buffer))
	validAddress := uint32(0) // arbitrary valid address as arguments to args_sizes_get. We chose 0 here.

	tests := []struct {
		name           string
		argsCountPtr   uint32
		argsBufSizePtr uint32
	}{
		{
			name:           "out-of-memory argsCountPtr",
			argsCountPtr:   memorySize,
			argsBufSizePtr: validAddress,
		},
		{
			name:           "out-of-memory argsBufSizePtr",
			argsCountPtr:   validAddress,
			argsBufSizePtr: memorySize,
		},
		{
			name:           "argsCountPtr exceeds the maximum valid address by 1",
			argsCountPtr:   memorySize - 4 + 1, // 4 is the size of uint32, the type of the count of args
			argsBufSizePtr: validAddress,
		},
		{
			name:           "argsBufSizePtr exceeds the maximum valid size by 1",
			argsCountPtr:   validAddress,
			argsBufSizePtr: memorySize - 4 + 1, // 4 is the size of uint32, the type of the buffer size
		},
	}

	for _, tt := range tests {
		tc := tt

		t.Run(tc.name, func(t *testing.T) {
			ret, _, err := store.CallFunction(ctx, "test", "args_sizes_get", uint64(tc.argsCountPtr), uint64(tc.argsBufSizePtr))
			require.NoError(t, err)
			require.Equal(t, uint64(EINVAL), ret[0]) // ret[0] is returned errno
		})
	}
}

func TestWASIEnvironment_args_get_ReturnError(t *testing.T) {
	ctx := context.Background()
	validArgs := []string{"foo", "bar", "baz"} // arbitrary valid args
	argsOpt, err := Args(validArgs)
	require.NoError(t, err)
	wasiEnv := NewEnvironment(argsOpt)
	store := instantiateWasmStore(t, argsWat, "test", wasiEnv)

	memorySize := uint32(len(store.Memories[0].Buffer))
	validAddress := uint32(0) // arbitrary valid address as arguments to args_get. We chose 0 here.

	tests := []struct {
		name       string
		argsPtr    uint32
		argsBufPtr uint32
	}{
		{
			name:       "out-of-memory argsPtr",
			argsPtr:    memorySize,
			argsBufPtr: validAddress,
		},
		{
			name:       "out-of-memory argsBufPtr",
			argsPtr:    validAddress,
			argsBufPtr: memorySize,
		},
		{
			name: "argsPtr exceeds the maximum valid address by 1",
			// 4*uint32(len(argsArray.nullTerminatedValues)) is the size of the result of the pointers to args, 4 is the size of uint32
			argsPtr:    memorySize - 4*uint32(len(wasiEnv.args.nullTerminatedValues)) + 1,
			argsBufPtr: validAddress,
		},
		{
			name:       "argsBufPtr exceeds the maximum valid address by 1",
			argsPtr:    validAddress,
			argsBufPtr: memorySize - wasiEnv.args.totalBufSize + 1,
		},
	}

	for _, tt := range tests {
		tc := tt

		t.Run(tc.name, func(t *testing.T) {
			ret, _, err := store.CallFunction(ctx, "test", "args_get", uint64(tc.argsPtr), uint64(tc.argsBufPtr))
			require.NoError(t, err)
			require.Equal(t, uint64(EINVAL), ret[0]) // ret[0] is returned errno
		})
	}
}

func instantiateWasmStore(t *testing.T, wat []byte, moduleName string, wasiEnv *WASIEnvironment) *wasm.Store {
	mod, err := text.DecodeModule(wat)
	require.NoError(t, err)

	store := wasm.NewStore(interpreter.NewEngine())
	err = wasiEnv.Register(store)
	require.NoError(t, err)

	err = store.Instantiate(mod, moduleName)
	require.NoError(t, err)

	return store
}

// clockWat is a wasm module to call clock_time_get.
var clockWat = []byte(`
(module
  (import "wasi_snapshot_preview1" "clock_time_get" (func $wasi_clock_time_get (param i32 i64 i32) (result i32)))
  (memory 1)  ;; just an arbitrary size big enough for tests
  (export "memory" (memory 0))
  ;; Define proxy functions to let the host test code to call the API
  (func $clock_time_get (param i32 i64 i32) (result i32)
        local.get 0
        local.get 1
        local.get 2
        call $wasi_clock_time_get
        )
  (export "clock_time_get" (func $clock_time_get))
  )
`)

func TestWASIEnvironment_clock_time_get(t *testing.T) {
	ctx := context.Background()
	wasiEnv := NewEnvironment()
	store := instantiateWasmStore(t, clockWat, "test", wasiEnv)
	memorySize := uint32(len(store.Memories[0].Buffer))
	validAddress := uint32(0) // arbitrary valid address as arguments to args_get. We chose 0 here.

	tests := []struct {
		name         string
		timestampVal uint64
		timestampPtr uint32
		result       Errno
	}{
		{
			name:         "zero uint64 value",
			timestampVal: 0,
			timestampPtr: validAddress,
			result:       ESUCCESS,
		},
		{
			name:         "low uint64 value",
			timestampVal: 12345,
			timestampPtr: validAddress,
			result:       ESUCCESS,
		},
		{
			name:         "high uint64 value - no truncation",
			timestampVal: math.MaxUint64,
			timestampPtr: validAddress,
			result:       ESUCCESS,
		},
		{
			name:         "with an endian-sensitive uint64 val - no truncation",
			timestampVal: math.MaxUint64 - 1,
			timestampPtr: validAddress,
			result:       ESUCCESS,
		},
		{
			name:         "timestampPtr exceeds the maximum valid address by 1",
			timestampVal: math.MaxUint64,
			timestampPtr: memorySize - 8 + 1,
			result:       EINVAL,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			wasiEnv.getTimeNanosFn = func() uint64 { return tt.timestampVal }
			ret, _, err := store.CallFunction(ctx, "test", "clock_time_get", uint64(0), uint64(0), uint64(tt.timestampPtr))
			require.NoError(t, err)
			errno := Errno(ret[0])
			require.Equal(t, tt.result, errno) // ret[0] is returned errno
			if errno == ESUCCESS {
				nanos := binary.LittleEndian.Uint64(store.Memories[0].Buffer)
				assert.Equal(t, tt.timestampVal, nanos)
			}
		})
	}
}

// environWat is a wasm module to call environ_get and environ_sizes_get.
var environWat = []byte(`(module
  (import "wasi_snapshot_preview1" "environ_sizes_get" (func $wasi_environ_sizes_get (param i32 i32) (result i32)))
  (import "wasi_snapshot_preview1" "environ_get" (func $wasi_environ_get (param i32 i32) (result i32)))
  (memory 1)  ;; just an arbitrary size big enough for tests
  (export "memory" (memory 0))
  ;; Define proxy functions to let the host test code to call the API
  (func $environ_sizes_get (param i32 i32) (result i32)
        local.get 0
        local.get 1
        call $wasi_environ_sizes_get
        )
  (func $environ_get (param i32 i32) (result i32)
        local.get 0
        local.get 1
        call $wasi_environ_get
        )
  (export "environ_sizes_get" (func $environ_sizes_get))
  (export "environ_get" (func $environ_get))
  )
`)

// common test cases used by TestWASIEnvironment_environ_sizes_get_Succeed and TestWASIEnvironment_environ_get_Succeed
var environTests = []struct {
	name                   string
	environ                []string
	expectedEnvironBufSize uint32
}{
	{
		name:                   "no environ",
		environ:                nil,
		expectedEnvironBufSize: 0,
	},
	{
		name:                   "empty",
		environ:                []string{},
		expectedEnvironBufSize: 0,
	},
	{
		name: "simple",
		environ: []string{
			"foo=FOO",
			"bar=BAR",
			"baz=BAZ",
		},
		expectedEnvironBufSize: 24,
	},
	{
		name: "empty value",
		environ: []string{
			"foo=",
		},
		expectedEnvironBufSize: 5,
	},
	{
		name: "utf-8 values",
		// "😨", "🤣", and "️🏃‍♀️" have 4, 4, and 13 bytes respectively
		environ: []string{
			"foo=😨🤣🏃\u200d♀️",
			"bar=BAR",
		},
		expectedEnvironBufSize: 34,
	},
	{
		name: "invalid utf-8 string",
		environ: []string{
			"foo=\xff\xfe\xfd",
			"bar=BAR",
		},
		expectedEnvironBufSize: 16,
	},
}

func TestWASIEnvironment_environ_sizes_get_Succeed(t *testing.T) {
	ctx := context.Background()
	for _, tt := range environTests {
		tc := tt
		opts := []Option{}
		if tc.environ != nil {
			environOpt, err := Environ(tc.environ)
			require.NoError(t, err)
			opts = append(opts, environOpt)
		}
		wasiEnv := NewEnvironment(opts...)
		store := instantiateWasmStore(t, environWat, "test", wasiEnv)

		t.Run(tc.name, func(t *testing.T) {
			environCountPtr := uint32(0)       // arbitrary valid address
			environBufSizePtr := uint32(0x100) // arbitrary valid address that doesn't overwrap with environCountPtr
			ret, _, err := store.CallFunction(ctx, "test", "environ_sizes_get", uint64(environCountPtr), uint64(environBufSizePtr))
			require.NoError(t, err)
			require.Equal(t, uint64(ESUCCESS), ret[0]) // ret[0] is errno
			require.Equal(t, uint32(len(tc.environ)), binary.LittleEndian.Uint32(store.Memories[0].Buffer[environCountPtr:]))
			require.Equal(t, tc.expectedEnvironBufSize, binary.LittleEndian.Uint32(store.Memories[0].Buffer[environBufSizePtr:]))
		})
	}
}

func TestWASIEnvironment_environ_get_Succeed(t *testing.T) {
	ctx := context.Background()
	for _, tt := range environTests {
		tc := tt
		opts := []Option{}
		if tc.environ != nil {
			environOpt, err := Environ(tc.environ)
			require.NoError(t, err)
			opts = append(opts, environOpt)
		}
		wasiEnv := NewEnvironment(opts...)
		store := instantiateWasmStore(t, environWat, "test", wasiEnv)

		t.Run(tc.name, func(t *testing.T) {
			// Serialize the expected result of environ_get
			expectedEnviron := make([]byte, 4*len(tc.environ)) // expected size of the pointers to the environ. 4 is the size of uint32
			environPtr := uint32(0)                            // arbitrary valid address
			expectedEnvironBuf := make([]byte, tc.expectedEnvironBufSize)
			environBufPtr := uint32(0x100) // arbitrary valid address that doesn't overwrap with environPtr
			environBufWritten := uint32(0)
			for i, env := range tc.environ {
				binary.LittleEndian.PutUint32(expectedEnviron[environPtr+uint32(i*4):], environBufPtr+environBufWritten) // 4 is the size of uint32
				copy(expectedEnvironBuf[environBufWritten:], env)
				environBufWritten += uint32(len(env)) + 1 // +1 for the trailing null character
			}

			// Compare them
			ret, _, err := store.CallFunction(ctx, "test", "environ_get", uint64(environPtr), uint64(environBufPtr))
			require.NoError(t, err)
			require.Equal(t, uint64(ESUCCESS), ret[0]) // ret[0] is the returned errno
			require.Equal(t, expectedEnviron, store.Memories[0].Buffer[environPtr:environPtr+uint32(len(expectedEnviron))])
			require.Equal(t, expectedEnvironBuf, store.Memories[0].Buffer[environBufPtr:environBufPtr+uint32(len(expectedEnvironBuf))])
		})
	}
}

func TestWASIEnvironment_environ_sizes_get_ReturnError(t *testing.T) {
	ctx := context.Background()
	validEnviron := []string{"foo=FOO", "bar=BAR", "baz=BAZ"} // arbitrary valid environ
	environOpt, err := Environ(validEnviron)
	require.NoError(t, err)
	wasiEnv := NewEnvironment(environOpt)
	store := instantiateWasmStore(t, environWat, "test", wasiEnv)

	memorySize := uint32(len(store.Memories[0].Buffer))
	validAddress := uint32(0) // arbitrary valid address as arguments to environ_sizes_get. We chose 0 here.

	tests := []struct {
		name              string
		environCountPtr   uint32
		environBufSizePtr uint32
	}{
		{
			name:              "out-of-memory environCountPtr",
			environCountPtr:   memorySize,
			environBufSizePtr: validAddress,
		},
		{
			name:              "out-of-memory environBufSizePtr",
			environCountPtr:   validAddress,
			environBufSizePtr: memorySize,
		},
		{
			name:              "environCountPtr exceeds the maximum valid address by 1",
			environCountPtr:   memorySize - 4 + 1, // 4 is the size of uint32, the type of the count of environ
			environBufSizePtr: validAddress,
		},
		{
			name:              "environBufSizePtr exceeds the maximum valid size by 1",
			environCountPtr:   validAddress,
			environBufSizePtr: memorySize - 4 + 1, // 4 is the size of uint32, the type of the buffer size
		},
	}

	for _, tt := range tests {
		tc := tt

		t.Run(tc.name, func(t *testing.T) {
			ret, _, err := store.CallFunction(ctx, "test", "environ_sizes_get", uint64(tc.environCountPtr), uint64(tc.environBufSizePtr))
			require.NoError(t, err)
			require.Equal(t, uint64(EINVAL), ret[0]) // ret[0] is returned errno
		})
	}
}

func TestWASIEnvironment_environ_get_ReturnError(t *testing.T) {
	ctx := context.Background()
	validEnviron := []string{"foo=FOO", "bar=BAR", "baz=BAZ"} // arbitrary valid environ
	environOpt, err := Environ(validEnviron)
	require.NoError(t, err)
	wasiEnv := NewEnvironment(environOpt)
	store := instantiateWasmStore(t, environWat, "test", wasiEnv)

	memorySize := uint32(len(store.Memories[0].Buffer))
	validAddress := uint32(0) // arbitrary valid address as arguments to environ_get. We chose 0 here.

	tests := []struct {
		name          string
		environPtr    uint32
		environBufPtr uint32
	}{
		{
			name:          "out-of-memory environPtr",
			environPtr:    memorySize,
			environBufPtr: validAddress,
		},
		{
			name:          "out-of-memory environBufPtr",
			environPtr:    validAddress,
			environBufPtr: memorySize,
		},
		{
			name: "environPtr exceeds the maximum valid address by 1",
			// 4*len(environ.nullTerminatedValues) is the expected buffer size for environPtr, 4 is the size of uint32
			environPtr:    memorySize - 4*uint32(len(wasiEnv.environ.nullTerminatedValues)) + 1,
			environBufPtr: validAddress,
		},
		{
			name:          "environBufPtr exceeds the maximum valid address by 1",
			environPtr:    validAddress,
			environBufPtr: memorySize - wasiEnv.environ.totalBufSize + 1,
		},
	}

	for _, tt := range tests {
		tc := tt

		t.Run(tc.name, func(t *testing.T) {
			ret, _, err := store.CallFunction(ctx, "test", "environ_get", uint64(tc.environPtr), uint64(tc.environBufPtr))
			require.NoError(t, err)
			require.Equal(t, uint64(EINVAL), ret[0]) // ret[0] is returned errno
		})
	}
}

func TestEnviron(t *testing.T) {
	tests := []struct {
		name         string
		environ      []string
		shouldSuceed bool
	}{
		{
			name:         "nil",
			environ:      nil,
			shouldSuceed: true,
		},
		{
			name:         "empty envs",
			environ:      []string{},
			shouldSuceed: true,
		},
		{
			name:         "simple envs",
			environ:      []string{"foo=FOO", "bar=BAR", "baz=FOOBAR"},
			shouldSuceed: true,
		},
		{
			name: "value with equal sign",
			environ: []string{
				"foo=FOO=BAZ",
				"bar=BAR",
			},
			shouldSuceed: true,
		},
		{
			name: "utf-8 values",
			environ: []string{
				"foo=😨🤣🏃\u200d♀️",
				"bar=BAR",
			},
			shouldSuceed: true,
		},
		{
			name: "invalid utf-8 values",
			environ: []string{
				"foo=\xff\xfe\xfd",
				"bar=BAR",
			},
			shouldSuceed: true,
		},
		{
			// this is valid but note that WASI doesn't explicitly specify the expected semantics of any environ
			name: "non-ascii keys",
			environ: []string{
				"😨🤣🏃\u200d♀️=utf-8",
				"foo=FOO",
			},
			shouldSuceed: true,
		},
		{
			// this is valid but note that WASI doesn't explicitly specify the expected semantics of any environ
			name: "duplicated keys",
			environ: []string{
				"foo=FOO1",
				"foo=FOO2",
				"baz=BAZ",
			},
			shouldSuceed: true,
		},
		{
			// this is valid but note that WASI doesn't explicitly specify the expected semantics of any environ
			name: "key with equal sign",
			environ: []string{
				"foo=FOO",
				"bar=baz=BAR",
			},
			shouldSuceed: true,
		},
		{
			name: "fails if environ doesn't contain '='",
			environ: []string{
				"foo=FOO",
				"bar",
			},
			shouldSuceed: false,
		},
	}

	for _, tt := range tests {
		tc := tt

		t.Run(tc.name, func(t *testing.T) {
			environOpt, err := Environ(tc.environ)
			if !tc.shouldSuceed {
				require.Error(t, err)
				return
			}
			require.NoError(t, err)

			// create a new WASIEnvironment to test the value of the WASIStringArray constructed by environOpt
			wasiEnv := NewEnvironment(environOpt)

			// construct the expected WASIStringArray value
			expectedWasiStringArray, err := newWASIStringArray(tc.environ)
			require.NoError(t, err)

			// Compare them. Note that `require.Equal` does 'reflect.DeepEqual' internally.
			require.Equal(t, expectedWasiStringArray, wasiEnv.environ)
		})
	}
}
