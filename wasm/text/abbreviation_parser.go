package text

import (
	"errors"
	"fmt"

	"github.com/tetratelabs/wazero/wasm"
)

func newAbbreviationParser(module *wasm.Module, indexNamespace *indexNamespace) *abbreviationParser {
	return &abbreviationParser{module: module, indexNamespace: indexNamespace}
}

// onAbbreviations is invoked when the grammar "(export)* (import)?" completes.
//
// * name is the tokenID field stripped of '$' prefix
// * exports are nil unless there was at least one "export" field. When set, this includes possibly empty Name field.
// * i is nil unless there was only one "import" field. When set, this includes possibly empty Module and Name fields.
// * pos is the context used to determine which tokenParser to return
//
// Note: this is called when neither an "export" nor "import" field are parsed, or on any field following an "export"
// that is not an "import": pos clarifies this.
// Note: this signature returns references instead of strings because empty names are permitted. This allows us to
// differentiate no exports from one that has an empty name.
type onAbbreviations func(name string, i *wasm.Import, exports []string, pos callbackPosition, tok tokenType, tokenBytes []byte, line, col uint32) (tokenParser, error)

// abbreviationParser parses any abbreviated imports or exports from a field such "func" and calls onAbbreviations.
//
// Ex.     `(func $math.pi (import "Math" "PI"))`
//   begin here --^                            ^
//              onAbbreviations resumes here --+
//
// Note: Unlike normal parsers, this is not used for an entire field (enclosed by parens). Rather, this only handles
// "export" and "import" inner fields in the correct order.
// Note: abbreviationParser is reusable. The caller resets via begin.
type abbreviationParser struct {
	// module during parsing is a read-only pointer to the ExportSection used to enforce uniqueness on currentExportNames.
	module *wasm.Module

	indexNamespace *indexNamespace

	// onAbbreviations is invoked on end
	onAbbreviations onAbbreviations

	// pos is used to give an appropriate errorContext
	pos parserPosition

	// currentName is a tokenID field stripped of the leading '$'.
	//
	// Note: this for the wasm.NameSection, which has no reason to differentiate empty string from no name.
	currentName string

	// currentImport is the inlined import field
	//
	// Note: multiple inlined imports are not supported as their expansion would be ambiguous
	// See https://github.com/WebAssembly/spec/issues/1418
	currentImport *wasm.Import

	// currentExportNames are the names of any exports added to the wasm.Module ExportSection
	currentExportNames []string
}

// begin should be called after reading any ID in a field that contains a type use. Parsing starts with the returned
// beginImportOrExport and continues until onAbbreviations or error. This should be called regardless of the tokenType
// to ensure a valid empty type use is associated with the section index, if needed.
//
// Ex. Given the source `(module (func $main (export "a")))`
//          beginImportOrExport starts here --^          ^
//                        onAbbreviations resumes here --+
//
// Ex. Given the source `(module (func $main (import "" "") (param i32)`
//         beginImportOrExport starts here --^              ^
//                           onAbbreviations resumes here --+
//
func (p *abbreviationParser) begin(onAbbreviations onAbbreviations, tok tokenType, tokenBytes []byte, line, col uint32) (tokenParser, error) {
	pos := callbackPositionUnhandledToken
	p.pos = positionInitial // to ensure errorContext reports properly
	switch tok {
	case tokenID: // Ex. $main
		if err := p.setID(tokenBytes); err != nil {
			return nil, err
		}
		p.onAbbreviations = onAbbreviations
		return p.parseMoreImportOrExports, nil
	case tokenLParen:
		p.onAbbreviations = onAbbreviations
		return p.beginImportOrExport, nil
	case tokenRParen:
		pos = callbackPositionEndField
	}
	return onAbbreviations("", nil, nil, pos, tok, tokenBytes, line, col)
}

// setID adds the current ID into the indexNamespace. This errs when the ID was already in use.
//
// Note: Due to abbreviated syntax, `(func $main...` could later be found to be an import. Ex. `(func $main (import...`
// In other words, it isn't known if what's being parsed is module-defined vs import, and the latter could have an
// ordering error. The ordering constraint imposed on "module composition" is that abbreviations are well-formed if and
// only if their expansions are. This means `(module (func $one) (func $two (import "" "")))` is invalid as it expands
// to `(module (func $one) (import "" "" (func $two)))` which violates the ordering constraint of imports first.
//
// We may set an ID here and find that the function declaration is invalid later due to above. Should we save off the
// source position? No: this function only ensures there's no ID conflict: an error here is about reuse of an ID. It
// cannot and shouldn't check for other errors like ordering due to expansion. If later, there's a failure due to the
// "import" abbreviation field, the parser would be at a relevant source position to err.
//
// See https://github.com/WebAssembly/spec/issues/1417
// See https://www.w3.org/TR/wasm-core-1/#abbreviations%E2%91%A8
func (p *abbreviationParser) setID(tokenBytes []byte) error {
	name, err := p.indexNamespace.setID(tokenBytes)
	if err != nil {
		return err
	}
	p.currentName = name
	return nil
}

// beginImportOrExport decides which tokenParser to use based on its field name: "export" or "import".
func (p *abbreviationParser) beginImportOrExport(tok tokenType, tokenBytes []byte, line, col uint32) (tokenParser, error) {
	if tok != tokenKeyword {
		return nil, unexpectedToken(tok, tokenBytes)
	}

	switch string(tokenBytes) {
	case "export":
		p.pos = positionExport
		return p.parseExport, nil
	case "import":
		if p.currentImport != nil {
			return nil, errors.New("redundant import")
		}
		p.pos = positionImport
		return p.parseImportModule, nil
	default:
		return p.end(callbackPositionUnhandledField, tok, tokenBytes, line, col)
	}
}

// parseMoreImportOrExports looks for a '(', and if present returns beginImportOrExport to continue the type. Otherwise,
// it calls parseEnd.
func (p *abbreviationParser) parseMoreImportOrExports(tok tokenType, tokenBytes []byte, line, col uint32) (tokenParser, error) {
	switch tok {
	case tokenID:
		return nil, fmt.Errorf("redundant ID: %s", tokenBytes)
	case tokenLParen:
		return p.beginImportOrExport, nil
	}
	return p.parseEnd(tok, tokenBytes, line, col)
}

// parseExport returns parseAbbreviationEnd after recording the export name, or errs if it couldn't be read.
//
// Ex. Export name is present `(func (export "PI"))`
//                             starts here --^   ^
//                               records PI --^  |
//           parseAbbreviationEnd resumes here --+
//
// Ex. Export name is absent `(export (func 0))`
//                        errs here --^
func (p *abbreviationParser) parseExport(tok tokenType, tokenBytes []byte, _, _ uint32) (tokenParser, error) {
	switch tok {
	case tokenString: // Ex. "" or "PI"
		name := string(tokenBytes[1 : len(tokenBytes)-1]) // strip quotes
		if _, ok := p.module.ExportSection[name]; ok {
			return nil, fmt.Errorf("duplicate name %q", name)
		}
		if p.module.ExportSection == nil {
			p.module.ExportSection = map[string]*wasm.Export{name: {Name: name}}
		} else {
			p.module.ExportSection[name] = &wasm.Export{Name: name}
		}
		p.currentExportNames = append(p.currentExportNames, name)
		return p.parseAbbreviationEnd, nil
	case tokenRParen:
		return nil, errors.New("missing name")
	default:
		return nil, unexpectedToken(tok, tokenBytes)
	}
}

// parseImportModule returns parseImportName after recording the import module name, or errs if it couldn't be read.
//
// Ex. Imported module name is present `(func (import "Math" "PI") (result f32))`
//                                      records Math --^     ^
//                            parseImportName resumes here --+
//
// Ex. Imported module name is absent `(func (import) (result f32))`
//                                      errs here --^
func (p *abbreviationParser) parseImportModule(tok tokenType, tokenBytes []byte, _, _ uint32) (tokenParser, error) {
	switch tok {
	case tokenString: // Ex. "" or "Math"
		module := string(tokenBytes[1 : len(tokenBytes)-1]) // unquote
		p.currentImport = &wasm.Import{Module: module}
		return p.parseImportName, nil
	case tokenLParen, tokenRParen:
		return nil, errors.New("missing module and name")
	default:
		return nil, unexpectedToken(tok, tokenBytes)
	}
}

// parseImportName returns parseAbbreviationEnd after recording the import name, or errs if it couldn't be read.
//
// Ex. Import name is present `(func (import "Math" "PI") (result f32))`
//                                    starts here --^^  ^
//                                      records PI --+  |
//                              parseEnd resumes here --+
//
// Ex. Imported function name is absent `(func (import "Math") (result f32))`
//                                               errs here --^
func (p *abbreviationParser) parseImportName(tok tokenType, tokenBytes []byte, _, _ uint32) (tokenParser, error) {
	switch tok {
	case tokenString: // Ex. "" or "PI"
		name := string(tokenBytes[1 : len(tokenBytes)-1]) // unquote
		p.currentImport.Name = name
		return p.parseAbbreviationEnd, nil
	case tokenLParen, tokenRParen:
		return nil, errors.New("missing name")
	default:
		return nil, unexpectedToken(tok, tokenBytes)
	}
}

func (p *abbreviationParser) parseAbbreviationEnd(tok tokenType, tokenBytes []byte, _, _ uint32) (tokenParser, error) {
	switch tok {
	case tokenString: // Ex. "" or "PI"
		return nil, fmt.Errorf("redundant name %s", tokenBytes)
	case tokenRParen:
		p.pos = positionInitial
		return p.parseMoreImportOrExports, nil
	default:
		return nil, unexpectedToken(tok, tokenBytes)
	}
}

func (p *abbreviationParser) parseEnd(tok tokenType, tokenBytes []byte, line, col uint32) (tokenParser, error) {
	pos := callbackPositionUnhandledToken
	if tok == tokenRParen {
		pos = callbackPositionEndField
	}
	return p.end(pos, tok, tokenBytes, line, col)
}

func (p *abbreviationParser) errorContext() string {
	switch p.pos {
	case positionImport:
		return ".import"
	case positionExport:
		return ".export"
	}
	return ""
}

// end invokes onAbbreviations to continue parsing
func (p *abbreviationParser) end(pos callbackPosition, tok tokenType, tokenBytes []byte, line, col uint32) (parser tokenParser, err error) {
	// Invoke the onAbbreviations hook with the current token
	parser, err = p.onAbbreviations(p.currentName, p.currentImport, p.currentExportNames, pos, tok, tokenBytes, line, col)
	// reset
	p.currentName = ""
	p.currentImport = nil
	p.currentExportNames = nil
	return
}
