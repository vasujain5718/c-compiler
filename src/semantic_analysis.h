#ifndef SEMANTIC_ANALYSIS_H
#define SEMANTIC_ANALYSIS_H

#include "ast.h"
#include <map>
#include <string>
#include <memory>

class SemanticAnalysis {
public:
    // Entry point: resolves names in-place (may throw std::runtime_error).
    void resolve(FunctionAST* function);

private:
    struct MapEntry {
        std::string unique_name;
        bool from_current_block;
    };

    using VarMap = std::map<std::string, MapEntry>;

    int unique_counter = 0;

    // Helpers
    std::string make_unique_name(const std::string& original_name);
    VarMap copy_variable_map(const VarMap& m);

    // Block processing
    void resolve_block(const std::vector<std::unique_ptr<BlockItemAST>>& items, VarMap& varmap);

    // Node-specific resolvers
    void resolve_block_item(BlockItemAST* item, VarMap& varmap);
    void resolve_statement(StatementAST* stmt, VarMap& varmap);
    void resolve_declaration(DeclarationAST* decl, VarMap& varmap);
    void resolve_expression(ExprAST* expr, VarMap& varmap);
};

#endif // SEMANTIC_ANALYSIS_H
