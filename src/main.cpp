#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <filesystem>
#include <cstdlib>
#include "lexer.h"
#include "parser.h"
#include "codegen.h"
#include "emitter.h"

// Using declarations for convenience
using std::cout;
using std::cerr;
using std::endl;
using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;
using std::stringstream;
using std::unique_ptr;

std::string read_file(const char* path) {
    ifstream file(path);
    if (!file.is_open()) { return ""; }
    stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

void print_usage() {
    cout << "Usage: mycc [stage] [-o <outfile>] <infile.c>\n";
    cout << "If no stage is specified, a full compilation is performed.\n\n";
    cout << "Stages:\n";
    cout << "  --lex, --parse, --codegen\n";
}

int main(int argc, char* argv[]) {
    string stop_stage = "";
    const char* input_file = nullptr;
    const char* output_file = nullptr;

    for (int i = 1; i < argc; i++) {
        string arg = argv[i];
        if (arg == "--lex" || arg == "--parse" || arg == "--codegen") {
            stop_stage = arg.substr(2);
        } else if (arg == "-o") {
            if (i + 1 < argc) {
                output_file = argv[++i];
            }
        } else if (arg[0] != '-') {
            input_file = argv[i];
        }
    }

    if (!input_file) {
        cerr << "Error: No input file provided.\n";
        print_usage();
        return 1;
    }
    
    string source_code = read_file(input_file);
    if (source_code.empty()) {
        cerr << "Error: Could not read or empty input file: " << input_file << "\n";
        return 1;
    }

    // --- Compiler Pipeline ---

    Lexer lexer(source_code);
    vector<Token> tokens = lexer.tokenize();
    if (lexer.hadError()) { return 1; }
    if (stop_stage == "lex") { return 0; }

    Parser parser(tokens);
    unique_ptr<FunctionAST> ast = parser.parse();
    if (!ast) { return 1; }
    if (stop_stage == "parse") { return 0; }

    CodeGenerator generator;
    unique_ptr<ir::Program> ir_program = generator.generate(ast.get());
    if (!ir_program) { return 1; }
    if (stop_stage == "codegen") { return 0; }
    
    // --- Final Build Stage (Default Action if no stage is specified) ---
    // The test script expects OUR compiler to do this entire process.
    
    namespace fs = std::filesystem;
    fs::path input_path(input_file);
    
    // 1. Define the path for our temporary assembly file.
    fs::path temp_asm_path = input_path;
    temp_asm_path.replace_extension(".s");

    // 2. Define the path for our final executable file.
    // The test script expects this to have no extension.
    fs::path final_exec_path = input_path;
    final_exec_path.replace_extension("");

    // 3. Emit the assembly code into the temporary .s file.
    {
        ofstream outfile(temp_asm_path);
        if (!outfile.is_open()) {
            cerr << "Error: Could not open temporary assembly file for writing.\n";
            return 1;
        }
        AssemblyEmitter emitter(outfile);
        emitter.emit(ir_program.get());
    }

    // 4. Call gcc to assemble the .s file into the final executable.
    string command = "gcc " + temp_asm_path.string() + " -o " + final_exec_path.string();
    int result = system(command.c_str());
    if (result != 0) {
        cerr << "Error: gcc failed to assemble the program.\n";
        // It's useful to keep the .s file for debugging if gcc fails.
        return 1;
    }

    // 5. Clean up the temporary assembly file.
    fs::remove(temp_asm_path);

    return 0;
}