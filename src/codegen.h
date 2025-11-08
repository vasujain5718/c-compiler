#ifndef CODEGEN_H
#define CODEGEN_H

#include "tacky.h"
#include "ir.h"
#include <memory>
#include <map>
#include <string>

class CodeGenerator {
public:
    std::unique_ptr<ir::Program> generate(const tacky::Program* tacky_prog);

private:
    void generate_function(const tacky::Function* tacky_func, ir::Function* ir_func);
    void generate_instruction(const tacky::Instruction* tacky_inst, ir::Function* ir_func);
    std::unique_ptr<ir::Operand> translate_val(const tacky::Value* tacky_val);

    std::map<std::string, int> stack_map;
    int next_stack_offset = 0;
};

#endif