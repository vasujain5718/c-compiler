#ifndef SEMANTIC_ANALYSIS_H
#define SEMANTIC_ANALYSIS_H

#include "ast.h"
#include <map>
#include <string>
#include <memory>
#include <unordered_map>

class SemanticAnalysis
{
public:
    void resolve(FunctionAST *function);

   
    int get_loop_id(const StatementAST *stmt) const;

    int get_enclosing_loop_id(const StatementAST *stmt) const;

private:
    struct MapEntry
    {
        std::string unique_name;
        bool from_current_block;
        SimpleType decl_type;
        bool is_array;       
        int array_size;     
    };
    using VarMap = std::map<std::string, MapEntry>;

    int unique_counter = 0;
    int loop_label_counter = 0;

    std::string make_unique_name(const std::string &original_name);
    VarMap copy_variable_map(const VarMap &m);
    int make_loop_label() { return ++loop_label_counter; }

    void resolve_block(const std::vector<std::unique_ptr<BlockItemAST>> &items, VarMap &varmap);
    void resolve_block_item(BlockItemAST *item, VarMap &varmap);
    void resolve_statement(StatementAST *stmt, VarMap &varmap);
    void resolve_declaration(DeclarationAST *decl, VarMap &varmap);
    void resolve_expression(ExprAST *expr, VarMap &varmap);

    void resolve_for_init(ForInitAST *init, VarMap &varmap);
    void resolve_optional_expr(ExprAST *maybe, VarMap &varmap);

    void annotate_block(const std::vector<std::unique_ptr<BlockItemAST>> &items, int current_label);
    void annotate_block_item(BlockItemAST *item, int current_label);
    void annotate_loops(StatementAST *stmt, int current_label);

    std::unordered_map<const StatementAST *, int> loop_id_of_stmt_;    
    std::unordered_map<const StatementAST *, int> enclosing_id_of_bc_; 

   
    std::unordered_map<const ExprAST *, SimpleType> expr_type_;

    SimpleType infer_expr_type(ExprAST *expr, const VarMap &varmap);

    
    bool is_numeric_type(SimpleType t) const
    {
        return t == SimpleType::INT || t == SimpleType::FLOAT || t == SimpleType::DOUBLE|| t == SimpleType::CHAR;
    }

    bool convertible(SimpleType from, SimpleType to) const;
};

#endif 
