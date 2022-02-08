package text

import (
	"errors"
	"fmt"
	"strings"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/tetratelabs/wazero/wasm"
)

type abbreviationParserTest struct {
	name                            string
	source                          string
	expectedName                    string
	expectedExportNames             []string
	expectedImport                  *wasm.Import
	expectedOnAbbreviationsPosition callbackPosition
	expectedOnAbbreviationsToken    tokenType
	expectedTrailingTokens          []tokenType
}

var abbreviationParserTests = []*abbreviationParserTest{
	{
		name:   "empty",
		source: "()",
	},
	{
		name:                "export no import",
		source:              `((export "e"))`,
		expectedExportNames: []string{"e"},
	},
	{
		name:                "export no import - ID",
		source:              `($x (export "e"))`,
		expectedName:        "x",
		expectedExportNames: []string{"e"},
	},
	{
		name:           "import no export",
		source:         `((import "m" "n"))`,
		expectedImport: &wasm.Import{Module: "m", Name: "n"},
	},
	{
		name:                "mixed export no import",
		source:              `((export "e1") (export "e2"))`,
		expectedExportNames: []string{"e1", "e2"},
	},
	{
		name:                "mixed export no import - ID",
		source:              `($x (export "e1") (export "e2"))`,
		expectedName:        "x",
		expectedExportNames: []string{"e1", "e2"},
	},
	{
		name:                "mixed exportNames import",
		source:              `((export "e1") (export "e2") (import "m" "n"))`,
		expectedImport:      &wasm.Import{Module: "m", Name: "n"},
		expectedExportNames: []string{"e1", "e2"},
	},
	{
		name:                "mixed import exportNames",
		source:              `((import "m" "n") (export "e1") (export "e2"))`,
		expectedImport:      &wasm.Import{Module: "m", Name: "n"},
		expectedExportNames: []string{"e1", "e2"},
	},
	{
		name:                "mixed import exportNames - ID",
		source:              `($x (import "m" "n") (export "e1") (export "e2"))`,
		expectedName:        "x",
		expectedImport:      &wasm.Import{Module: "m", Name: "n"},
		expectedExportNames: []string{"e1", "e2"},
	},
}

func TestAbbreviationParser_InlinesTypesWhenNotYetAdded(t *testing.T) {
	runAbbreviationParserTests(t, abbreviationParserTests, func(tc *abbreviationParserTest) *abbreviationParser {
		return newAbbreviationParser(&wasm.Module{}, newIndexNamespace())
	})
}

func TestAbbreviationParser_BeginResets(t *testing.T) {
	runAbbreviationParserTests(t, abbreviationParserTests, func(tc *abbreviationParserTest) *abbreviationParser {
		tp := newAbbreviationParser(&wasm.Module{}, newIndexNamespace())
		// inline import/exportNames that uses all fields. Intentionally don't use the same ID or export name!
		require.NoError(t, parseAbbreviation(tp, `($z (import "m" "n") (export "e3") (export "e4"))`, ignoreAbbreviation))
		source := strings.ReplaceAll(strings.ReplaceAll(tc.source, "$x", "$y"), `"e`, `"d`)
		require.NoError(t, parseAbbreviation(tp, source, ignoreAbbreviation))
		return tp
	})
}

type abbreviationTestFunc func(tc *abbreviationParserTest) *abbreviationParser

// To prevent having to maintain a lot of tests to cover the necessary dimensions, this generates combinations that
// can happen in a type use.
// Ex. (func ) (func (local i32)) and (func nop) are all empty type uses, and we need to hit all three cases
func runAbbreviationParserTests(t *testing.T, tests []*abbreviationParserTest, tp abbreviationTestFunc) {
	moreTests := make([]*abbreviationParserTest, 0, len(tests)*2)
	for _, tt := range tests {
		tt.expectedOnAbbreviationsPosition = callbackPositionEndField
		tt.expectedOnAbbreviationsToken = tokenRParen // at the end of the field ')'
		tt.expectedTrailingTokens = nil

		kt := *tt // copy
		kt.name = fmt.Sprintf("%s - trailing keyword", tt.name)
		kt.source = fmt.Sprintf("%s nop)", tt.source[:len(tt.source)-1])
		kt.expectedOnAbbreviationsPosition = callbackPositionUnhandledToken
		kt.expectedOnAbbreviationsToken = tokenKeyword // at 'nop' and ')' remains
		kt.expectedTrailingTokens = []tokenType{tokenRParen}
		moreTests = append(moreTests, &kt)

		ft := *tt // copy
		ft.name = fmt.Sprintf("%s - trailing field", tt.name)
		ft.source = fmt.Sprintf("%s (nop))", tt.source[:len(tt.source)-1])
		// Two outcomes, we've reached a field not named "import" or "export"
		ft.expectedOnAbbreviationsPosition = callbackPositionUnhandledField
		ft.expectedOnAbbreviationsToken = tokenKeyword // at 'nop' and '))' remain
		ft.expectedTrailingTokens = []tokenType{tokenRParen, tokenRParen}
		moreTests = append(moreTests, &ft)
	}

	for _, tt := range append(tests, moreTests...) {
		tc := tt

		t.Run(tc.name, func(t *testing.T) {
			p := &collectTokenTypeParser{}
			var setAbbreviation onAbbreviations = func(name string, i *wasm.Import, exportNames []string, pos callbackPosition, tok tokenType, tokenBytes []byte, line, col uint32) (tokenParser, error) {
				require.Equal(t, tc.expectedName, name)
				require.Equal(t, tc.expectedExportNames, exportNames)
				require.Equal(t, tc.expectedImport, i)
				require.Equal(t, tc.expectedOnAbbreviationsPosition, pos)
				require.Equal(t, tc.expectedOnAbbreviationsToken, tok)
				return p.parse, nil
			}

			require.NoError(t, parseAbbreviation(tp(tc), tc.source, setAbbreviation))
			require.Equal(t, tc.expectedTrailingTokens, p.tokenTypes)
		})
	}
}

func TestAbbreviationParser_Errors(t *testing.T) {
	tests := []struct{ name, source, expectedErr string }{
		{
			name:        "not import",
			source:      `(($import "m" "n"))`,
			expectedErr: "1:3: unexpected ID: $import",
		},
		{
			name:        "second import",
			source:      `((import "m" "n") (import "m2" "n2"))`,
			expectedErr: "1:20: redundant import",
		},
		{
			name:        "import missing module and name",
			source:      `((import))`,
			expectedErr: "1:9: missing module and name",
		},
		{
			name:        "import wrong module",
			source:      "((import apple))",
			expectedErr: "1:10: unexpected keyword: apple",
		},
		{
			name:        "import missing name",
			source:      `((import "m"))`,
			expectedErr: "1:13: missing name",
		},
		{
			name:        "import wrong name",
			source:      `((import "m" apple))`,
			expectedErr: "1:14: unexpected keyword: apple",
		},
		{
			name:        "import ID",
			source:      `((import $i "m" "n"))`,
			expectedErr: "1:10: unexpected ID: $i",
		},
		{
			name:        "import second ID",
			source:      `($x $y (import "m" "n"))`,
			expectedErr: "1:5: redundant ID: $y",
		},
		{
			name:        "import redundant name",
			source:      `((import "m" "n" "o"))`,
			expectedErr: `1:18: redundant name "o"`,
		},
		{
			name:        "import wrong end",
			source:      `((import "m" "n" ()`,
			expectedErr: "1:18: unexpected '('",
		},
		{
			name:        "not export",
			source:      `((export "e1") ($export "e2"))`,
			expectedErr: "1:17: unexpected ID: $export",
		},
		{
			name:        "export missing name",
			source:      `((export))`,
			expectedErr: "1:9: missing name",
		},
		{
			name:        "export wrong name",
			source:      "((export apple))",
			expectedErr: "1:10: unexpected keyword: apple",
		},
		{
			name:        "export ID",
			source:      `((export "e1") (export $e2 "e2"))`,
			expectedErr: "1:24: unexpected ID: $e2",
		},
		{
			name:        "export second ID",
			source:      `($x $y (export "e1"))`,
			expectedErr: "1:5: redundant ID: $y",
		},
		{
			name:        "export duplicate name",
			source:      `((export "e") (export "e"))`,
			expectedErr: `1:23: duplicate name "e"`,
		},
		{
			name:        "export redundant name",
			source:      `((export "x" "y"))`,
			expectedErr: `1:14: redundant name "y"`,
		},
		{
			name:        "export wrong end",
			source:      `((export "x" ()`,
			expectedErr: "1:14: unexpected '('",
		},
	}

	for _, tt := range tests {
		tc := tt

		t.Run(tc.name, func(t *testing.T) {
			err := parseAbbreviation(newAbbreviationParser(&wasm.Module{}, newIndexNamespace()), tc.source, failOnAbbreviations)
			require.EqualError(t, err, tc.expectedErr)
		})
	}
}

var ignoreAbbreviation onAbbreviations = func(name string, i *wasm.Import, exportNames []string, pos callbackPosition, tok tokenType, tokenBytes []byte, line, col uint32) (tokenParser, error) {
	return parseNoop, nil
}

var failOnAbbreviations onAbbreviations = func(name string, i *wasm.Import, exportNames []string, pos callbackPosition, tok tokenType, tokenBytes []byte, line, col uint32) (tokenParser, error) {
	return nil, errors.New("unexpected to call onAbbreviations on error")
}

func parseAbbreviation(tp *abbreviationParser, source string, onAbbreviations onAbbreviations) error {
	var parser tokenParser = func(tok tokenType, tokenBytes []byte, line, col uint32) (tokenParser, error) {
		return tp.begin(onAbbreviations, tok, tokenBytes, line, col)
	}

	line, col, err := lex(skipTokens(1, parser), []byte(source))
	if err != nil {
		err = &FormatError{Line: line, Col: col, cause: err}
	}
	return err
}

func TestAbbreviationParser_ErrorContext(t *testing.T) {
	p := abbreviationParser{}
	tests := []struct {
		source   string
		pos      parserPosition
		expected string
	}{
		{source: "initial", pos: positionInitial, expected: ""},
		{source: "export", pos: positionExport, expected: ".export"},
		{source: "import", pos: positionImport, expected: ".import"},
	}

	for _, tt := range tests {
		tc := tt

		t.Run(tc.source, func(t *testing.T) {
			p.pos = tc.pos
			require.Equal(t, tc.expected, p.errorContext())
		})
	}
}
