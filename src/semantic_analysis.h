#ifndef SEMANTIC_ANALYSIS_H
#define SEMANTIC_ANALYSIS_H

#include "ast.h"
#include <map>
#include <string>
#include <memory>
#include <unordered_map>

// SemanticAnalysis resolves variable names and annotates loops.
// Loop annotations are stored in side tables (no AST changes needed).
class SemanticAnalysis {
public:
    // Entry point: resolves names and then annotates loops (may throw std::runtime_error).
    void resolve(FunctionAST* function);

    // ----- Query APIs for later stages -----
    // Returns a unique positive ID for a loop statement (While/DoWhile/For), or -1 if not a loop.
    int get_loop_id(const StatementAST* stmt) const;

    // Returns the enclosing loop ID for a Break/Continue statement, or -1 if none (invalid usage).
    int get_enclosing_loop_id(const StatementAST* stmt) const;

private:
    struct MapEntry {
        std::string unique_name;
        bool from_current_block;
    };
    using VarMap = std::map<std::string, MapEntry>;

    // Counters
    int unique_counter = 0;
    int loop_label_counter = 0;

    // ----- Helpers -----
    std::string make_unique_name(const std::string& original_name);
    VarMap copy_variable_map(const VarMap& m);
    int make_loop_label() { return ++loop_label_counter; }

    // ----- Variable resolution -----
    void resolve_block(const std::vector<std::unique_ptr<BlockItemAST>>& items, VarMap& varmap);
    void resolve_block_item(BlockItemAST* item, VarMap& varmap);
    void resolve_statement(StatementAST* stmt, VarMap& varmap);
    void resolve_declaration(DeclarationAST* decl, VarMap& varmap);
    void resolve_expression(ExprAST* expr, VarMap& varmap);

    // For header helpers
    void resolve_for_init(ForInitAST* init, VarMap& varmap);
    void resolve_optional_expr(ExprAST* maybe, VarMap& varmap);

    // ----- Loop annotation pass -----
    void annotate_block(const std::vector<std::unique_ptr<BlockItemAST>>& items, int current_label);
    void annotate_block_item(BlockItemAST* item, int current_label);
    void annotate_loops(StatementAST* stmt, int current_label);

    // Side tables for loop IDs
    std::unordered_map<const StatementAST*, int> loop_id_of_stmt_;     // while/do/for -> id
    std::unordered_map<const StatementAST*, int> enclosing_id_of_bc_;  // break/continue -> id
};

#endif // SEMANTIC_ANALYSIS_H
