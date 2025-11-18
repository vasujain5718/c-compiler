#ifndef TACKY_GENERATOR_H
#define TACKY_GENERATOR_H

#include "ast.h"
#include "tacky.h"
#include <memory>
#include <vector>
#include <string>
#include <utility>

class TackyGenerator
{
public:
    std::unique_ptr<tacky::Program> generate(const FunctionAST *ast);

private:
    
    std::unique_ptr<tacky::Value> generate_expression(
        const ExprAST *expr,
        std::vector<std::unique_ptr<tacky::Instruction>> &instructions);

    
    void generate_block_item(
        const BlockItemAST *item,
        std::vector<std::unique_ptr<tacky::Instruction>> &instructions);

    
    void generate_statement(
        const StatementAST *stmt,
        std::vector<std::unique_ptr<tacky::Instruction>> &instructions);

    void generate_declaration(
        const DeclarationAST *decl,
        std::vector<std::unique_ptr<tacky::Instruction>> &instructions);

    
    std::unique_ptr<tacky::Value> generate_assignment(
        const AssignmentExprAST *assign_expr,
        std::vector<std::unique_ptr<tacky::Instruction>> &instructions);

    
    void gen_for_init(const ForInitAST *init,
                      std::vector<std::unique_ptr<tacky::Instruction>> &instructions);

    
    std::unique_ptr<tacky::Var> make_temporary(tacky::TypeKind kind);
    std::string make_label();

    int next_var_id = 0;
    int next_label_id = 0;

    
    std::vector<std::pair<std::string, std::string>> loop_label_stack_;
};

#endif 