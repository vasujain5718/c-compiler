#include "codegen.h"
#include <memory>

using std::unique_ptr;
using std::make_unique;
using std::move;

unique_ptr<ir::Program> CodeGenerator::generate(const FunctionAST* func_ast) {
    auto program = make_unique<ir::Program>();
    program->function = make_unique<ir::Function>();
    program->function->name = func_ast->Name;

    generate_statement(func_ast->Body.get(), *program->function);
    
    return program;
}

void CodeGenerator::generate_statement(const StatementAST* stmt, ir::Function& current_function) {
    if (auto* ret_stmt = dynamic_cast<const ReturnStatementAST*>(stmt)) {
        auto operand = generate_expression(ret_stmt->Expression.get());

        auto dest_reg = make_unique<ir::Register>("%eax");
        auto mov_inst = make_unique<ir::MovInstruction>(move(operand), move(dest_reg));
        current_function.instructions.push_back(move(mov_inst));

        auto ret_inst = make_unique<ir::RetInstruction>();
        current_function.instructions.push_back(move(ret_inst));
    }
}

unique_ptr<ir::Operand> CodeGenerator::generate_expression(const ExprAST* expr) {
    if (auto* const_expr = dynamic_cast<const ConstantExprAST*>(expr)) {
        return make_unique<ir::Immediate>(const_expr->Val);
    }
    return nullptr;
}