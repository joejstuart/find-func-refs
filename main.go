package main

import (
	"flag"
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"log"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"golang.org/x/tools/go/ast/inspector"
	"golang.org/x/tools/go/packages"
)

// symbol uniquely identifies a declared function/method
type symbol struct {
	// Fully qualified like: "package/path.Func" or "package/path.(T).Method"
	Qualified string
	// Types object for comparison (uses types.Origin to match generic instantiations)
	Obj *types.Func
}

type ref struct {
	PkgPath string
	File    string
	Line    int
	Col     int
	Snippet string
}

func main() {
	var filePath string
	var root string
	var withSnippet bool
	var allFiles bool
	var includeTests bool
	var excludeMain bool
	flag.StringVar(&filePath, "file", "", "Path to the Go source file whose declared functions/methods to search for (required unless -all is used)")
	flag.StringVar(&root, "root", ".", "Root directory to search (module/workspace root)")
	flag.BoolVar(&withSnippet, "snippet", false, "Include a trimmed code line as a snippet")
	flag.BoolVar(&allFiles, "all", false, "Process all Go source files in the codebase (excludes test files by default)")
	flag.BoolVar(&includeTests, "include-tests", false, "Include test files when using -all")
	flag.BoolVar(&excludeMain, "exclude-main", true, "Exclude main function from analysis (default: true)")
	flag.Parse()

	if filePath == "" && !allFiles {
		fmt.Fprintln(os.Stderr, "error: either -file or -all is required")
		os.Exit(2)
	}
	if filePath != "" && allFiles {
		fmt.Fprintln(os.Stderr, "error: cannot use both -file and -all")
		os.Exit(2)
	}

	absRoot, err := filepath.Abs(root)
	if err != nil {
		log.Fatalf("resolve root: %v", err)
	}

	cfg := &packages.Config{
		Mode: packages.NeedName |
			packages.NeedFiles |
			packages.NeedCompiledGoFiles |
			packages.NeedSyntax |
			packages.NeedTypes |
			packages.NeedTypesInfo |
			packages.NeedModule,
		Dir:  absRoot,
		Fset: token.NewFileSet(),
		Env:  os.Environ(),
	}

	// 1) Load all packages under root
	pkgs, err := packages.Load(cfg, "./...")
	if err != nil {
		log.Fatalf("packages.Load: %v", err)
	}
	if packages.PrintErrors(pkgs) > 0 {
		// Not fatal necessarily, but warn
		log.Println("warning: some packages had errors; results may be incomplete")
	}

	var targetSyms []symbol
	var results map[string][]ref

	if allFiles {
		// Process all Go files in the codebase
		allGoFiles := collectAllGoFiles(pkgs, includeTests)
		if len(allGoFiles) == 0 {
			log.Fatalf("no Go source files found in %s", absRoot)
		}

		// Collect symbols from all files
		for _, file := range allGoFiles {
			fileSyms := collectFileSymbols(pkgs, file)
			targetSyms = append(targetSyms, fileSyms...)
		}

		if len(targetSyms) == 0 {
			log.Fatalf("no function or method declarations found in any Go files")
		}

		// Remove duplicates and optionally exclude main
		targetSyms = uniqueSymbols(targetSyms)
		if excludeMain {
			targetSyms = filterMainFunction(targetSyms)
		}

		// Search all packages for references to those symbols
		results = findReferences(pkgs, targetSyms, withSnippet)
	} else {
		// Original single file mode
		absFile, err := filepath.Abs(filePath)
		if err != nil {
			log.Fatalf("resolve file: %v", err)
		}

		// Find the package that contains the target file; collect symbols declared in that file
		targetSyms = collectFileSymbols(pkgs, absFile)
		if len(targetSyms) == 0 {
			log.Fatalf("no function or method declarations found in %s (did you point to the right file?)", absFile)
		}

		// Optionally exclude main function
		if excludeMain {
			targetSyms = filterMainFunction(targetSyms)
		}

		// Search all packages for references to those symbols
		results = findReferences(pkgs, targetSyms, withSnippet)
	}

	// Print results grouped by symbol
	printResults(targetSyms, results)
}

func collectFileSymbols(pkgs []*packages.Package, absFile string) []symbol {
	var syms []symbol
	for _, pkg := range pkgs {
		// Walk files to find the exact syntax.File for absFile
		for i, f := range pkg.CompiledGoFiles {
			fp, _ := filepath.Abs(f)
			if fp != absFile {
				continue
			}
			// Ensure types info exists
			if pkg.Types == nil || pkg.TypesInfo == nil {
				continue
			}
			file := pkg.Syntax[i]
			// Inspect file for FuncDecl
			for _, decl := range file.Decls {
				fd, ok := decl.(*ast.FuncDecl)
				if !ok || fd.Name == nil {
					continue
				}
				// Resolve the object for the declared function/method
				obj := pkg.TypesInfo.Defs[fd.Name]
				fn, ok := obj.(*types.Func)
				if !ok || fn == nil {
					// Could be builtins or something unexpected
					continue
				}
				q := qualifiedFuncName(fn)
				syms = append(syms, symbol{
					Qualified: q,
					Obj:       fn,
				})
			}
			return uniqueSymbols(syms)
		}
	}
	return uniqueSymbols(syms)
}

func uniqueSymbols(in []symbol) []symbol {
	seen := map[string]bool{}
	var out []symbol
	for _, s := range in {
		if !seen[s.Qualified] {
			seen[s.Qualified] = true
			out = append(out, s)
		}
	}
	sort.Slice(out, func(i, j int) bool { return out[i].Qualified < out[j].Qualified })
	return out
}

// filterMainFunction removes the main function from the symbol list.
func filterMainFunction(syms []symbol) []symbol {
	var filtered []symbol
	for _, s := range syms {
		// Check if this is a main function (no receiver and name is "main")
		if s.Obj.Name() == "main" && s.Obj.Type().(*types.Signature).Recv() == nil {
			continue
		}
		filtered = append(filtered, s)
	}
	return filtered
}

// collectAllGoFiles returns all Go source files from the loaded packages,
// optionally including test files.
func collectAllGoFiles(pkgs []*packages.Package, includeTests bool) []string {
	var files []string
	seen := make(map[string]bool)

	for _, pkg := range pkgs {
		if pkg == nil {
			continue
		}

		// Collect from CompiledGoFiles (non-test files)
		for _, file := range pkg.CompiledGoFiles {
			if absFile, err := filepath.Abs(file); err == nil {
				if !seen[absFile] {
					seen[absFile] = true
					files = append(files, absFile)
				}
			}
		}

		// Optionally include test files
		if includeTests {
			for _, file := range pkg.OtherFiles {
				if absFile, err := filepath.Abs(file); err == nil {
					if !seen[absFile] {
						seen[absFile] = true
						files = append(files, absFile)
					}
				}
			}
		}
	}

	// Sort for deterministic output
	sort.Strings(files)
	return files
}

func qualifiedFuncName(fn *types.Func) string {
	pkgPath := ""
	if fn.Pkg() != nil {
		pkgPath = fn.Pkg().Path()
	}
	recv := fn.Type().(*types.Signature).Recv()
	if recv == nil {
		// top-level function
		return pkgPath + "." + fn.Name()
	}
	// method on receiver
	rcvType := recv.Type()
	// Strip pointer(s)
	for {
		if p, ok := rcvType.(*types.Pointer); ok {
			rcvType = p.Elem()
			continue
		}
		break
	}
	return fmt.Sprintf("%s.(%s).%s", pkgPath, types.TypeString(rcvType, nil), fn.Name())
}

// sameFunc compares two functions by fully-qualified identity without using types.Origin.
// This works for non-generic code and is robust across go versions.
func sameFunc(a, b *types.Func) bool {
	if a == nil || b == nil {
		return false
	}
	// pkg path + receiver (for methods) + name
	ap, bp := a.Pkg(), b.Pkg()
	if ap == nil || bp == nil {
		// Builtins or weird cases: fall back to pointer equality
		return a == b
	}

	aSig, _ := a.Type().(*types.Signature)
	bSig, _ := b.Type().(*types.Signature)

	var aRecv, bRecv string
	if aSig != nil && aSig.Recv() != nil {
		aRecv = types.TypeString(deref(aSig.Recv().Type()), nil)
	}
	if bSig != nil && bSig.Recv() != nil {
		bRecv = types.TypeString(deref(bSig.Recv().Type()), nil)
	}

	// Top-level function: recv is empty; method: recv is type name
	return ap.Path() == bp.Path() && aRecv == bRecv && a.Name() == b.Name()
}

func deref(t types.Type) types.Type {
	for {
		if p, ok := t.(*types.Pointer); ok {
			t = p.Elem()
			continue
		}
		return t
	}
}

// collectImplementers scans all packages for interface types that declare any of
// the given method names, and returns a map methodName -> []*types.Func for all
// concrete methods that implement those interface methods.
func collectImplementers(pkgs []*packages.Package, methodNames map[string]struct{}) map[string][]*types.Func {
	out := make(map[string][]*types.Func)

	// 1) Gather all named types in the workspace (interfaces and concrete types)
	var namedTypes []*types.Named
	for _, pkg := range pkgs {
		if pkg.Types == nil || pkg.Types.Scope() == nil {
			continue
		}
		scope := pkg.Types.Scope()
		for _, name := range scope.Names() {
			if tn, ok := scope.Lookup(name).(*types.TypeName); ok {
				if nt, ok := tn.Type().(*types.Named); ok {
					namedTypes = append(namedTypes, nt)
				}
			}
		}
	}

	// 2) For each interface that declares a method of interest,
	//    find every concrete type that implements it and collect the concrete method funcs.
	for _, nt := range namedTypes {
		if iface, ok := nt.Underlying().(*types.Interface); ok {
			iface = iface.Complete()
			// Filter interface methods by interest set
			var wanted []string
			for i := 0; i < iface.NumMethods(); i++ {
				m := iface.Method(i)
				if _, ok := methodNames[m.Name()]; ok {
					wanted = append(wanted, m.Name())
				}
			}
			if len(wanted) == 0 {
				continue
			}
			// For each named type T that is NOT an interface: if *T implements iface,
			// record T's concrete methods with those names.
			for _, cand := range namedTypes {
				if _, isIface := cand.Underlying().(*types.Interface); isIface {
					continue
				}
				ptr := types.NewPointer(cand)
				if !types.Implements(ptr, iface) {
					continue
				}
				for _, name := range wanted {
					if mf := methodByName(ptr, name); mf != nil {
						out[name] = append(out[name], mf)
					}
				}
			}
		}
	}
	return out
}

// methodByName finds the method object with the given name on type T (using method sets).
func methodByName(t types.Type, name string) *types.Func {
	// LookupFieldOrMethod returns (obj, index, indirect)
	obj, _, _ := types.LookupFieldOrMethod(t, true, nil, name)
	if fn, ok := obj.(*types.Func); ok {
		return fn
	}
	return nil
}

func findReferences(pkgs []*packages.Package, targets []symbol, withSnippet bool) map[string][]ref {
	index := map[*types.Func]string{} // func obj -> qualified name
	targetNames := make(map[string]struct{})
	for _, s := range targets {
		index[s.Obj] = s.Qualified
		targetNames[s.Obj.Name()] = struct{}{}
	}

	// Initialize result buckets for deterministic output
	results := map[string][]ref{}
	for _, s := range targets {
		results[s.Qualified] = []ref{}
	}

	// NEW: precompute implementers for any target method names
	// e.g., "PrintJSON" -> []*types.Func { (*VSAReport).PrintJSON, (*UnifiedResultAdapter).PrintJSON, ... }
	implementersByName := collectImplementers(pkgs, targetNames)

	for _, pkg := range pkgs {
		if pkg.TypesInfo == nil || len(pkg.Syntax) == 0 {
			continue
		}
		ins := inspector.New(pkg.Syntax)
		nodes := []ast.Node{
			(*ast.Ident)(nil),
			(*ast.SelectorExpr)(nil),
		}
		ins.Preorder(nodes, func(n ast.Node) {
			switch nd := n.(type) {

			// ----- DIRECT USES (unchanged): ident or selector bound to a concrete *types.Func -----
			case *ast.Ident:
				if nd.Name == "_" {
					return
				}
				if obj, ok := pkg.TypesInfo.Uses[nd].(*types.Func); ok {
					if matchKey := matchTarget(index, obj); matchKey != "" {
						results[matchKey] = append(results[matchKey], makeRef(pkg, pkg.Fset, nd.Pos(), withSnippet))
					}
				}

			case *ast.SelectorExpr:
				// 1) Direct concrete method or function use
				if obj, ok := pkg.TypesInfo.Uses[nd.Sel].(*types.Func); ok {
					if matchKey := matchTarget(index, obj); matchKey != "" {
						results[matchKey] = append(results[matchKey], makeRef(pkg, pkg.Fset, nd.Sel.Pos(), withSnippet))
						return
					}
				}

				// 2) INDIRECT via interface dispatch: x.Method where x's static type is an interface.
				// Use Selections to see what was selected.
				if sel, ok := pkg.TypesInfo.Selections[nd]; ok && sel.Kind() == types.MethodVal {
					recv := sel.Recv().Underlying()
					if _, ok := recv.(*types.Interface); ok {
						name := sel.Obj().Name() // method name on the interface
						if _, wanted := targetNames[name]; wanted {
							// Credit all known implementers' concrete methods with this callsite.
							if impls := implementersByName[name]; len(impls) > 0 {
								for _, fn := range impls {
									if q, ok := index[fn]; ok {
										results[q] = append(results[q], makeRef(pkg, pkg.Fset, nd.Sel.Pos(), withSnippet))
									}
								}
							}
						}
					}
				}
			}
		})
	}

	// Stable sort
	for k := range results {
		sort.Slice(results[k], func(i, j int) bool {
			a, b := results[k][i], results[k][j]
			if a.File != b.File {
				return a.File < b.File
			}
			if a.Line != b.Line {
				return a.Line < b.Line
			}
			return a.Col < b.Col
		})
	}
	return results
}

func matchTarget(index map[*types.Func]string, used *types.Func) string {
	// Compare against every target using sameFunc
	for target, q := range index {
		if sameFunc(target, used) {
			return q
		}
	}
	return ""
}

func makeRef(pkg *packages.Package, fset *token.FileSet, pos token.Pos, withSnippet bool) ref {
	p := fset.Position(pos)
	r := ref{
		PkgPath: safePkgPath(pkg),
		File:    p.Filename,
		Line:    p.Line,
		Col:     p.Column,
	}
	if withSnippet {
		r.Snippet = readLine(p.Filename, p.Line)
	}
	return r
}

func safePkgPath(pkg *packages.Package) string {
	if pkg == nil || pkg.PkgPath == "" {
		return "(unknown)"
	}
	return pkg.PkgPath
}

func readLine(filename string, line int) string {
	b, err := os.ReadFile(filename)
	if err != nil {
		return ""
	}
	lines := strings.Split(string(b), "\n")
	if line <= 0 || line > len(lines) {
		return ""
	}
	s := strings.TrimSpace(lines[line-1])
	if len(s) > 160 {
		s = s[:160] + "…"
	}
	return s
}

func printResults(targets []symbol, results map[string][]ref) {
	for _, s := range targets {
		fmt.Printf("\n=== %s ===\n", s.Qualified)
		refs := results[s.Qualified]
		if len(refs) == 0 {
			fmt.Println("(no references found)")
			continue
		}
		for _, r := range refs {
			if r.Snippet != "" {
				fmt.Printf("%s:%d:%d  [%s]\n    %s\n", r.File, r.Line, r.Col, r.PkgPath, r.Snippet)
			} else {
				fmt.Printf("%s:%d:%d  [%s]\n", r.File, r.Line, r.Col, r.PkgPath)
			}
		}
	}
	fmt.Println()
}
