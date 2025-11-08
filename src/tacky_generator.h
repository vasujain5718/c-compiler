#ifndef TACKY_GENERATOR_H
#define TACKY_GENERATOR_H

#include "ast.h"
#include "tacky.h"
#include <memory>
#include <vector>
#include <string>

class TackyGenerator {
public:
    std::unique_ptr<tacky::Program> generate(const FunctionAST* ast);

private:
    // Generates TACKY for an expression and returns the Value where the result is stored.
    std::unique_ptr<tacky::Value> generate_expression(
        const ExprAST* expr, 
        std::vector<std::unique_ptr<tacky::Instruction>>& instructions
    );
    
    // New helper for the different types of block items
    void generate_block_item(
        const BlockItemAST* item,
        std::vector<std::unique_ptr<tacky::Instruction>>& instructions
    );

    void generate_statement(
        const StatementAST* stmt, 
        std::vector<std::unique_ptr<tacky::Instruction>>& instructions
    );

    void generate_declaration(
        const DeclarationAST* decl,
        std::vector<std::unique_ptr<tacky::Instruction>>& instructions
    );

    // New helper specifically for assignment expressions
    std::unique_ptr<tacky::Value> generate_assignment(
        const AssignmentExprAST* assign_expr,
        std::vector<std::unique_ptr<tacky::Instruction>>& instructions
    );
    
    std::unique_ptr<tacky::Var> make_temporary();
    std::string make_label();
    
    int next_var_id = 0;
    int next_label_id = 0;
};

#endif // TACKY_GENERATOR_H