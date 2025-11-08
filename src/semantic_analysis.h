#ifndef SEMANTIC_ANALYSIS_H
#define SEMANTIC_ANALYSIS_H

#include "ast.h"
#include <map>
#include <string>
#include <memory>

class SemanticAnalysis {
public:
    // The entry point for this pass. It can throw std::runtime_error on failure.
    void resolve(FunctionAST* program);

private:
    // The Symbol Table: Maps original source names ("x") to unique internal names ("x.0")
    std::map<std::string, std::string> current_scope;
    int unique_counter = 0;

    // Helper to generate unique names like "var_name.123"
    std::string make_unique_name(const std::string& original_name);

    // Recursive resolution functions matching the AST structure
    void resolve_block_item(BlockItemAST* item);
    void resolve_statement(StatementAST* stmt);
    void resolve_declaration(DeclarationAST* decl);
    void resolve_expression(ExprAST* expr);
};

#endif // SEMANTIC_ANALYSIS_H