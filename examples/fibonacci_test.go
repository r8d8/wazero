package examples

import (
	"context"
	_ "embed"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/tetratelabs/wazero/wasi"
	"github.com/tetratelabs/wazero/wasm"
	"github.com/tetratelabs/wazero/wasm/binary"
	"github.com/tetratelabs/wazero/wasm/interpreter"
)

//go:embed testdata/fibonacci.wasm
var fibWasm []byte

func Test_fibonacci(t *testing.T) {
	ctx := context.Background()
	mod, err := binary.DecodeModule(fibWasm)
	require.NoError(t, err)

	store := wasm.NewStore(interpreter.NewEngine())
	require.NoError(t, err)

	err = wasi.RegisterAPI(store)
	require.NoError(t, err)

	err = store.Instantiate(mod, "test")
	require.NoError(t, err)

	for _, c := range []struct {
		in, exp int32
	}{
		{in: 20, exp: 6765},
		{in: 10, exp: 55},
		{in: 5, exp: 5},
	} {
		ret, retTypes, err := store.CallFunction(ctx, "test", "fibonacci", uint64(c.in))
		require.NoError(t, err)
		require.Len(t, ret, len(retTypes))
		require.Equal(t, wasm.ValueTypeI32, retTypes[0])
		require.Equal(t, c.exp, int32(ret[0]))
	}
}
