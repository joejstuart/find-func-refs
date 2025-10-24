# find-func-refs

A Go command-line tool that finds all references to functions and methods declared in a specific Go source file across an entire codebase. This tool is particularly useful for understanding code usage patterns, refactoring, and impact analysis.

## Features

- **Function Reference Discovery**: Finds all direct calls to functions and methods declared in a target file
- **Interface Method Resolution**: Discovers indirect calls through interface dispatch (polymorphic calls)
- **Comprehensive Coverage**: Scans entire Go modules/workspaces for references
- **Detailed Output**: Provides file locations, line numbers, and optional code snippets
- **Robust Symbol Matching**: Handles generic types, method receivers, and complex type hierarchies

## Installation

```bash
go install github.com/joejstuart/find-func-refs@latest
```

## Usage

```bash
find-func-refs -file <path-to-go-file> [options]
```

### Required Arguments

- `-file`: Path to the Go source file whose declared functions/methods to search for

### Optional Arguments

- `-root`: Root directory to search (default: current directory)
- `-snippet`: Include a trimmed code line as a snippet in the output

### Examples

```bash
# Find all references to functions in a specific file
find-func-refs -file ./pkg/utils/helpers.go

# Search within a specific directory
find-func-refs -file ./internal/auth/validator.go -root ./myproject

# Include code snippets in output
find-func-refs -file ./pkg/models/user.go -snippet
```

## How It Works

The tool performs a comprehensive analysis in several stages:

1. **Package Loading**: Loads all Go packages in the specified root directory using the Go packages API
2. **Symbol Collection**: Extracts all function and method declarations from the target file
3. **Reference Discovery**: Searches all packages for:
   - Direct function/method calls
   - Interface method calls (polymorphic dispatch)
   - Method implementations that satisfy interface contracts
4. **Result Aggregation**: Groups and sorts results by symbol for clear output

## Output Format

The tool outputs results grouped by function/method with the following information:

```
=== package/path.FunctionName ===
/path/to/file.go:42:15  [package/path]
    result := someFunction(arg1, arg2)

=== package/path.(Type).MethodName ===
/path/to/other/file.go:88:5  [package/path]
    obj.MethodName()
```

Each reference includes:
- **File location**: Full path to the file containing the reference
- **Line and column**: Exact position of the reference
- **Package context**: The package where the reference was found
- **Code snippet**: (optional) The actual line of code containing the reference

## Use Cases

- **Refactoring Analysis**: Understand the impact of changing function signatures or removing functions
- **Code Exploration**: Discover how functions are used across a large codebase
- **Interface Analysis**: Find all implementations of interface methods
- **Dead Code Detection**: Identify potentially unused functions
- **Documentation**: Generate usage examples and documentation

## Technical Details

- **Go Version**: Requires Go 1.24.4 or later
- **Dependencies**: Uses `golang.org/x/tools` for advanced Go AST analysis
- **Performance**: Efficiently processes large codebases by leveraging Go's built-in package loading
- **Accuracy**: Uses Go's type system for precise symbol matching, including generic type handling

## Limitations

- Only works with Go source code
- Requires a valid Go module/workspace structure
- May not detect references in generated code or external dependencies
- Interface method resolution is limited to statically analyzable code

## Contributing

This tool is designed to be a utility for Go developers. Contributions are welcome for:
- Performance improvements
- Additional output formats
- Enhanced interface method detection
- Better error handling and diagnostics

## License

This project is open source. Please check the repository for license details.
