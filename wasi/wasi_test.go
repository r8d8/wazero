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
//go:embed testdata/args.wat
var argsWat []byte

func TestArgsAPISucceed(t *testing.T) {
	ctx := context.Background()
	tests := []struct {
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

	for _, tt := range tests {
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

			// Serialize the expected result of args_size_get
			argCountPtr := uint32(0)            // arbitrary valid address
			expectedArgCount := make([]byte, 4) // size of uint32
			binary.LittleEndian.PutUint32(expectedArgCount, uint32(len(tc.args)))
			bufSizePtr := uint32(0x100)        // arbitrary valid address that doesn't overwrap with argCountPtr
			expectedBufSize := make([]byte, 4) // size of uint32
			binary.LittleEndian.PutUint32(expectedBufSize, tc.expectedBufSize)

			// Compare them
			ret, _, err := store.CallFunction(ctx, "test", "args_sizes_get", uint64(argCountPtr), uint64(bufSizePtr))
			require.NoError(t, err)
			require.Equal(t, uint64(ESUCCESS), ret[0]) // ret[0] is errno
			require.Equal(t, expectedArgCount, store.Memories[0].Buffer[argCountPtr:argCountPtr+4])
			require.Equal(t, expectedBufSize, store.Memories[0].Buffer[bufSizePtr:bufSizePtr+4])

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
			ret, _, err = store.CallFunction(ctx, "test", "args_get", uint64(argsPtr), uint64(argvPtr))
			require.NoError(t, err)
			require.Equal(t, uint64(ESUCCESS), ret[0]) // ret[0] is the returned errno
			require.Equal(t, expectedArgs, store.Memories[0].Buffer[argsPtr:argsPtr+uint32(len(expectedArgs))])
			require.Equal(t, expectedArgv, store.Memories[0].Buffer[argvPtr:argvPtr+uint32(len(expectedArgv))])
		})
	}
}

func TestArgsSizesGetReturnError(t *testing.T) {
	ctx := context.Background()
	dummyArgs := []string{"foo", "bar", "baz"}
	argsOpt, err := Args(dummyArgs)
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

func TestArgsGetAPIReturnError(t *testing.T) {
	ctx := context.Background()
	dummyArgs := []string{"foo", "bar", "baz"}
	argsOpt, err := Args(dummyArgs)
	require.NoError(t, err)
	wasiEnv := NewEnvironment(argsOpt)
	store := instantiateWasmStore(t, argsWat, "test", wasiEnv)

	memorySize := uint32(len(store.Memories[0].Buffer))
	validAddress := uint32(0) // arbitrary valid address as arguments to args_get. We chose 0 here.
	argsArray, err := newWASIStringArray(dummyArgs)
	require.NoError(t, err)

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
			argsPtr:    memorySize - 4*uint32(len(argsArray.nullTerminatedValues)) + 1,
			argsBufPtr: validAddress,
		},
		{
			name:       "argsBufPtr exceeds the maximum valid address by 1",
			argsPtr:    validAddress,
			argsBufPtr: memorySize - argsArray.totalBufSize + 1,
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
//go:embed testdata/clock.wat
var clockWat []byte

func TestClockGetTime(t *testing.T) {
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
	keys                   []string
	values                 []string
	expectedEnviron        [][]byte
	expectedEnvironBufSize uint32
}{
	{
		name:                   "no environ",
		keys:                   nil,
		values:                 nil,
		expectedEnviron:        [][]byte{},
		expectedEnvironBufSize: 0,
	},
	{
		name:                   "empty",
		keys:                   []string{},
		values:                 []string{},
		expectedEnviron:        [][]byte{},
		expectedEnvironBufSize: 0,
	},
	{
		name:   "simple",
		keys:   []string{"foo", "bar", "baz"},
		values: []string{"FOO", "BAR", "BAZ"},
		expectedEnviron: [][]byte{
			[]byte("foo=FOO\x00"),
			[]byte("bar=BAR\x00"),
			[]byte("baz=BAZ\x00"),
		},
		expectedEnvironBufSize: 24,
	},
	{
		name:   "empty value",
		keys:   []string{"foo"},
		values: []string{""},
		expectedEnviron: [][]byte{
			[]byte("foo=\x00"),
		},
		expectedEnvironBufSize: 5,
	},
	{
		name: "utf-8 values",
		// "😨", "🤣", and "️🏃‍♀️" have 4, 4, and 13 bytes respectively
		keys:   []string{"foo", "bar"},
		values: []string{"😨🤣🏃\u200d♀️", "BAR"},
		expectedEnviron: [][]byte{
			[]byte("foo=😨🤣🏃\u200d♀️\x00"),
			[]byte("bar=BAR\x00"),
		},
		expectedEnvironBufSize: 34,
	},
	{
		name:   "invalid utf-8 string",
		keys:   []string{"foo", "bar"},
		values: []string{"\xff\xfe\xfd", "BAR"},
		expectedEnviron: [][]byte{
			[]byte("foo=\xff\xfe\xfd\x00"),
			[]byte("bar=BAR\x00"),
		},
		expectedEnvironBufSize: 16,
	},
}

func TestWASIEnvironment_environ_sizes_get_Succeed(t *testing.T) {
	ctx := context.Background()
	for _, tt := range environTests {
		tc := tt
		opts := []Option{}
		if tc.keys != nil {
			environOpt, err := Environ(tc.keys, tc.values)
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
			require.Equal(t, uint32(len(tc.expectedEnviron)), binary.LittleEndian.Uint32(store.Memories[0].Buffer[environCountPtr:]))
			require.Equal(t, tc.expectedEnvironBufSize, binary.LittleEndian.Uint32(store.Memories[0].Buffer[environBufSizePtr:]))
		})
	}
}

func TestWASIEnvironment_environ_get_Succeed(t *testing.T) {
	ctx := context.Background()
	for _, tt := range environTests {
		tc := tt
		opts := []Option{}
		if tc.keys != nil {
			environOpt, err := Environ(tc.keys, tc.values)
			require.NoError(t, err)
			opts = append(opts, environOpt)
		}
		wasiEnv := NewEnvironment(opts...)
		store := instantiateWasmStore(t, environWat, "test", wasiEnv)

		t.Run(tc.name, func(t *testing.T) {
			// Serialize the expected result of environ_get
			expectedEnviron := make([]byte, 4*len(tc.keys)) // expected size of the pointers to the environ. 4 is the size of uint32
			environPtr := uint32(0)                         // arbitrary valid address
			expectedEnvironBuf := make([]byte, tc.expectedEnvironBufSize)
			environBufPtr := uint32(0x100) // arbitrary valid address that doesn't overwrap with environPtr
			environBufWritten := uint32(0)
			for i, env := range tc.expectedEnviron {
				binary.LittleEndian.PutUint32(expectedEnviron[environPtr+uint32(i*4):], environBufPtr+environBufWritten) // 4 is the size of uint32
				copy(expectedEnvironBuf[environBufWritten:], env)
				environBufWritten += uint32(len(env))
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
	validEnvironKeys := []string{"foo", "bar", "baz"}   // arbitrary valid keys
	validEnvironValues := []string{"FOO", "BAR", "BAZ"} // arbitrary valid values
	environOpt, err := Environ(validEnvironKeys, validEnvironValues)
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
	validEnvironKeys := []string{"foo", "bar", "baz"}   // arbitrary valid keys
	validEnvironValues := []string{"FOO", "BAR", "BAZ"} // arbitrary valid values
	environOpt, err := Environ(validEnvironKeys, validEnvironValues)
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
		name            string
		keys            []string
		values          []string
		expectedEnviron []string
		shouldSuceed    bool
	}{
		{
			name:            "nil",
			keys:            nil,
			values:          nil,
			expectedEnviron: []string{},
			shouldSuceed:    true,
		},
		{
			name:            "empty envs",
			keys:            []string{},
			values:          []string{},
			expectedEnviron: []string{},
			shouldSuceed:    true,
		},
		{
			name:            "simple envs",
			keys:            []string{"foo", "bar", "baz"},
			values:          []string{"FOO", "BAR", "FOOBAR"},
			expectedEnviron: []string{"foo=FOO", "bar=BAR", "baz=FOOBAR"},
			shouldSuceed:    true,
		},
		{
			name:   "value with equal sign",
			keys:   []string{"foo", "bar"},
			values: []string{"FOO=BAZ", "BAR"},
			expectedEnviron: []string{
				"foo=FOO=BAZ",
				"bar=BAR",
			},
			shouldSuceed: true,
		},
		{
			name:   "utf-8 values",
			keys:   []string{"foo", "bar"},
			values: []string{"😨🤣🏃\u200d♀️", "BAR"},
			expectedEnviron: []string{
				"foo=😨🤣🏃\u200d♀️",
				"bar=BAR",
			},
			shouldSuceed: true,
		},
		{
			name:   "invalid utf-8 values",
			keys:   []string{"foo", "bar"},
			values: []string{"\xff\xfe\xfd", "BAR"},
			expectedEnviron: []string{
				"foo=\xff\xfe\xfd",
				"bar=BAR",
			},
			shouldSuceed: true,
		},
		{
			// this is valid but note that WASI doesn't explicitly specify the expected semantics of any environ
			name:   "non-ascii keys",
			keys:   []string{"😨🤣🏃\u200d♀️", "foo"},
			values: []string{"utf-8", "FOO"},
			expectedEnviron: []string{
				"😨🤣🏃\u200d♀️=utf-8",
				"foo=FOO",
			},
			shouldSuceed: true,
		},
		{
			// this is valid but note that WASI doesn't explicitly specify the expected semantics of any environ
			name:   "duplicated keys",
			keys:   []string{"foo", "foo", "baz"},
			values: []string{"FOO1", "FOO2", "BAZ"},
			expectedEnviron: []string{
				"foo=FOO1",
				"foo=FOO2",
				"baz=BAZ",
			},
			shouldSuceed: true,
		},
		{
			// this is valid but note that WASI doesn't explicitly specify the expected semantics of any environ
			name:   "key with equal sign",
			keys:   []string{"foo", "bar=baz"},
			values: []string{"FOO", "BAR"},
			expectedEnviron: []string{
				"foo=FOO",
				"bar=baz=BAR",
			},
			shouldSuceed: true,
		},
		{
			name:         "fails if keys/values of different lengths are passed",
			keys:         []string{"foo", "bar"},
			values:       []string{"foo"},
			shouldSuceed: false,
		},
	}

	for _, tt := range tests {
		tc := tt

		t.Run(tc.name, func(t *testing.T) {
			environOpt, err := Environ(tc.keys, tc.values)
			if !tc.shouldSuceed {
				require.Error(t, err)
				return
			}
			require.NoError(t, err)

			// create a new WASIEnvironment to test the value of the WASIStringArray constructed by environOpt
			wasiEnv := NewEnvironment(environOpt)

			// construct the expected WASIStringArray value
			expectedWasiStringArray, err := newWASIStringArray(tc.expectedEnviron)
			require.NoError(t, err)

			// Compare them. Note that `require.Equal` does 'reflect.DeepEqual' internally.
			require.Equal(t, expectedWasiStringArray, wasiEnv.environ)
		})
	}
}
