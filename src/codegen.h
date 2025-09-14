#ifndef CODEGEN_H
#define CODEGEN_H

#include "ast.h"
#include "ir.h"
#include <memory>

class CodeGenerator {
public:
    std::unique_ptr<ir::Program> generate(const FunctionAST* func);

private:
    void generate_statement(const StatementAST* stmt, ir::Function& current_function);
    std::unique_ptr<ir::Operand> generate_expression(const ExprAST* expr);
};

#endif