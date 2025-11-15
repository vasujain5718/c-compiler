#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <filesystem>
#include <cstdlib>
#include "lexer.h"
#include "parser.h"
#include "semantic_analysis.h" // NEW
#include "tacky_generator.h"
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

string read_file(const char* path) {
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
    cout << "  --lex, --parse, --resolve, --tacky, --codegen\n"; // Added --resolve
}

int main(int argc, char* argv[]) {
    string stop_stage = "";
    const char* input_file = nullptr;
    const char* output_file = nullptr;

    for (int i = 1; i < argc; i++) {
        string arg = argv[i];
        if (arg == "--lex" || arg == "--parse" || arg == "--resolve" || 
            arg == "--tacky" || arg == "--codegen" || arg=="--validate") {
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

    // --- STAGE 1: LEXER ---
    Lexer lexer(source_code);
    vector<Token> tokens = lexer.tokenize();
    if (lexer.hadError()) { return 1; }
    if (stop_stage == "lex") { return 0; }

    // --- STAGE 2: PARSER ---
    Parser parser(tokens);
    unique_ptr<FunctionAST> ast = parser.parse();
    if (!ast) { return 1; }
    if (stop_stage == "parse") { return 0; }

    // --- STAGE 3: SEMANTIC ANALYSIS (VARIABLE RESOLUTION) ---
    SemanticAnalysis resolver;
    try {
        resolver.resolve(ast.get());
    } catch (const std::runtime_error& e) {
        cerr << e.what() << endl;
        return 1;
    }
    if (stop_stage == "validate") { return 0; }

    // --- STAGE 4: TACKY GENERATION ---
    TackyGenerator tacky_gen;
    unique_ptr<tacky::Program> tacky_program = tacky_gen.generate(ast.get());
    if (!tacky_program) { return 1; }
    if (stop_stage == "tacky") { return 0; }
    
    // --- STAGE 5: CODE GENERATION (ASSEMBLY IR) ---
    CodeGenerator code_gen;
    unique_ptr<ir::Program> ir_program = code_gen.generate(tacky_program.get());
    if (!ir_program) { return 1; }
    if (stop_stage == "codegen") { return 0; }

    // --- FINAL STAGES: EMIT & BUILD ---
    namespace fs = std::filesystem;
    fs::path input_path(input_file);
    fs::path asm_path = input_path;
    asm_path.replace_extension(".s");

    fs::path final_exec_path = input_path;
    if (output_file) {
        final_exec_path = output_file;
    } else {
        final_exec_path.replace_extension("");
    }

    {
        ofstream outfile_stream(asm_path);
        if (!outfile_stream.is_open()) {
            cerr << "Error: Could not open assembly file for writing.\n";
            return 1;
        }
        AssemblyEmitter emitter(outfile_stream);
        emitter.emit(ir_program.get());
    }

    string command = "gcc " + asm_path.string() + " -o " + final_exec_path.string();
    int result = system(command.c_str());
    if (result != 0) {
        cerr << "Error: gcc failed to assemble the program.\n";
        return 1;
    }

    // fs::remove(asm_path);
    
    return 0;
}