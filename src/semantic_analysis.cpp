
#include "semantic_analysis.h"
#include "type.h"          
#include "ast.h"            
#include <stdexcept>
#include <utility>
#include <algorithm>
#include <cctype>
#include <unordered_map>

using std::runtime_error;
using std::string;

static std::string simpletype_to_string(SimpleType t) {
    switch (t) {
        case SimpleType::INT:    return "int";
        case SimpleType::FLOAT:  return "float";
        case SimpleType::DOUBLE: return "double";
        case SimpleType::CHAR:   return "char"; 
        case SimpleType::VOID:   return "void";
        default:                 return "unknown";
    }
}

static bool is_numeric_type(SimpleType t) {
    return t == SimpleType::INT || t == SimpleType::FLOAT || t == SimpleType::DOUBLE || t == SimpleType::CHAR; 
}

static bool implicit_widening_allowed(SimpleType from, SimpleType to) {
    if (from == to) return true;
    if (from == SimpleType::CHAR && (to == SimpleType::INT || to == SimpleType::FLOAT || to == SimpleType::DOUBLE)) return true; 
    if (from == SimpleType::INT && (to == SimpleType::FLOAT || to == SimpleType::DOUBLE)) return true;
    if (from == SimpleType::FLOAT && to == SimpleType::DOUBLE) return true;
    if (from == SimpleType::INT && to == SimpleType::CHAR) return true;
    return false;
}


void SemanticAnalysis::resolve(FunctionAST* function) {
    unique_counter = 0;
    loop_label_counter = 0;
    loop_id_of_stmt_.clear();
    enclosing_id_of_bc_.clear();
    expr_type_.clear();

    VarMap varmap; 

    resolve_block(function->Body, varmap);

    annotate_block(function->Body, -1);
}

int SemanticAnalysis::get_loop_id(const StatementAST* stmt) const {
    auto it = loop_id_of_stmt_.find(stmt);
    return (it == loop_id_of_stmt_.end()) ? -1 : it->second;
}

int SemanticAnalysis::get_enclosing_loop_id(const StatementAST* stmt) const {
    auto it = enclosing_id_of_bc_.find(stmt);
    return (it == enclosing_id_of_bc_.end()) ? -1 : it->second;
}


std::string SemanticAnalysis::make_unique_name(const string& original_name) {
    return original_name + "." + std::to_string(unique_counter++);
}

SemanticAnalysis::VarMap SemanticAnalysis::copy_variable_map(const VarMap& m) {
    VarMap copy = m;
    for (auto& kv : copy) {
        kv.second.from_current_block = false; 
    }
    return copy;
}

bool SemanticAnalysis::convertible(SimpleType from, SimpleType to) const {
    return implicit_widening_allowed(from, to);
}


void SemanticAnalysis::resolve_block(
    const std::vector<std::unique_ptr<BlockItemAST>>& items,
    VarMap& varmap
) {
    for (auto& item : items) {
        resolve_block_item(item.get(), varmap);
    }
}

void SemanticAnalysis::resolve_block_item(BlockItemAST* item, VarMap& varmap) {
    if (auto* decl = dynamic_cast<DeclarationAST*>(item)) {
        resolve_declaration(decl, varmap);
        return;
    }
    if (auto* stmt = dynamic_cast<StatementAST*>(item)) {
        resolve_statement(stmt, varmap);
        return;
    }
    throw runtime_error("Unknown block item type during semantic analysis.");
}

void SemanticAnalysis::resolve_declaration(DeclarationAST* decl, VarMap& varmap) {
    auto it = varmap.find(decl->VarName);
    if (it != varmap.end() && it->second.from_current_block) {
        throw runtime_error("Semantic Error: Duplicate declaration of variable '" + decl->VarName + "' in the same block.");
    }

    SimpleType declared_type = decl->DeclType;
    if (!is_numeric_type(declared_type)) {
        throw runtime_error("Semantic Error: Unsupported declaration type '" + simpletype_to_string(declared_type) + "' for variable '" + decl->VarName + "'.");
    }

    std::string unique_name = make_unique_name(decl->VarName);

    bool is_array = (decl->ArraySize > 0);
    int array_size = decl->ArraySize;

    varmap[decl->VarName] = MapEntry{unique_name, true, declared_type, is_array, array_size};

    decl->VarName = unique_name;

    if (decl->InitExpr) {
        resolve_expression(decl->InitExpr.get(), varmap);
        SimpleType rhs_t = infer_expr_type(decl->InitExpr.get(), varmap);
        if (!convertible(rhs_t, declared_type)) { 
            throw runtime_error("Semantic Error: Cannot initialize variable '" + decl->VarName + "' of type '" + simpletype_to_string(declared_type) +
                                 "' with expression of type '" + simpletype_to_string(rhs_t) + "'.");
        }
    }

    if (is_array && decl->InitExpr) {
        throw runtime_error("Semantic Error: Array declarations with initializer not supported in this subset.");
    }
}

void SemanticAnalysis::resolve_statement(StatementAST* stmt, VarMap& varmap) {
    if (auto* ret_stmt = dynamic_cast<ReturnStatementAST*>(stmt)) {
        if (ret_stmt->Expression) resolve_expression(ret_stmt->Expression.get(), varmap);
        return;
    }

    if (auto* expr_stmt = dynamic_cast<ExpressionStatementAST*>(stmt)) {
        if (expr_stmt->Expression) resolve_expression(expr_stmt->Expression.get(), varmap);
        return;
    }

    if (dynamic_cast<NullStatementAST*>(stmt)) {
        return; 
    }

    if (auto* if_stmt = dynamic_cast<IfStatementAST*>(stmt)) {
        resolve_expression(if_stmt->Condition.get(), varmap);
        SimpleType cond_t = infer_expr_type(if_stmt->Condition.get(), varmap);
        if (!is_numeric_type(cond_t)) { 
            throw runtime_error("Semantic Error: If condition must be numeric (got '" + simpletype_to_string(cond_t) + "').");
        }
        resolve_statement(if_stmt->ThenBranch.get(), varmap);
        if (if_stmt->ElseBranch) resolve_statement(if_stmt->ElseBranch.get(), varmap);
        return;
    }

    if (auto* comp = dynamic_cast<CompoundStatementAST*>(stmt)) {
        VarMap inner = copy_variable_map(varmap);
        resolve_block(comp->body_->Items, inner);
        return;
    }

    if (auto* w = dynamic_cast<WhileStatementAST*>(stmt)) {
        resolve_expression(w->Condition.get(), varmap);
        SimpleType cond_t = infer_expr_type(w->Condition.get(), varmap);
        if (!is_numeric_type(cond_t)) { 
            throw runtime_error("Semantic Error: While condition must be numeric (got '" + simpletype_to_string(cond_t) + "').");
        }
        resolve_statement(w->Body.get(), varmap);
        return;
    }

    if (auto* dw = dynamic_cast<DoWhileStatementAST*>(stmt)) {
        resolve_statement(dw->Body.get(), varmap);
        resolve_expression(dw->Condition.get(), varmap);
        SimpleType cond_t = infer_expr_type(dw->Condition.get(), varmap);
        if (!is_numeric_type(cond_t)) { 
            throw runtime_error("Semantic Error: Do-while condition must be numeric (got '" + simpletype_to_string(cond_t) + "').");
        }
        return;
    }

    if (auto* f = dynamic_cast<ForStatementAST*>(stmt)) {
        VarMap inner = copy_variable_map(varmap);    
        resolve_for_init(f->Init.get(), inner);     
        resolve_optional_expr(f->Condition.get(), inner);
        if (f->Condition) {
            SimpleType cond_t = infer_expr_type(f->Condition.get(), inner);
            if (!is_numeric_type(cond_t)) { 
                throw runtime_error("Semantic Error: For condition must be numeric (got '" + simpletype_to_string(cond_t) + "').");
            }
        }
        resolve_optional_expr(f->Increment.get(), inner);
        resolve_statement(f->Body.get(), inner);
        return;
    }

    if (dynamic_cast<BreakStatementAST*>(stmt))   return;
    if (dynamic_cast<ContinueStatementAST*>(stmt)) return;

    throw runtime_error("Unknown statement type during resolution.");
}

void SemanticAnalysis::resolve_expression(ExprAST* expr, VarMap& varmap) {
    if (!expr) return;

    if (auto* var_expr = dynamic_cast<VarExprAST*>(expr)) {
        auto it = varmap.find(var_expr->Name);
        if (it == varmap.end()) {
            throw runtime_error("Semantic Error: Undeclared variable '" + var_expr->Name + "'.");
        }
        std::string resolved_name = it->second.unique_name;
        SimpleType var_type = it->second.decl_type;
        var_expr->Name = resolved_name;
        expr_type_[expr] = var_type; 
        return;
    }

    if (auto* index_expr = dynamic_cast<IndexExprAST*>(expr)) {
        resolve_expression(index_expr->Base.get(), varmap);

        VarExprAST* base_var = dynamic_cast<VarExprAST*>(index_expr->Base.get());
        if (!base_var) {
            throw runtime_error("Semantic Error: Array indexing requires a variable base (e.g., a[i]). Nested/indexing of non-variable bases not supported.");
        }
        bool found = false;
        MapEntry base_entry;
        for (const auto& kv : varmap) {
            if (kv.second.unique_name == base_var->Name) {
                base_entry = kv.second;
                found = true;
                break;
            }
        }
        if (!found) {
            throw runtime_error("Semantic Error: Unknown base variable for indexing: '" + base_var->Name + "'.");
        }
        if (!base_entry.is_array) {
            throw runtime_error("Semantic Error: Attempting to index non-array variable '" + base_var->Name + "'.");
        }

        resolve_expression(index_expr->Index.get(), varmap);
        SimpleType idx_t = infer_expr_type(index_expr->Index.get(), varmap);
        if (idx_t != SimpleType::INT) {
            throw runtime_error("Semantic Error: Array index must be of type int (got '" + simpletype_to_string(idx_t) + "').");
        }

        expr_type_[expr] = base_entry.decl_type;
        return;
    }

    if (auto* assign_expr = dynamic_cast<AssignmentExprAST*>(expr)) {
        auto* lhs_var = dynamic_cast<VarExprAST*>(assign_expr->LHS.get());
        auto* lhs_index = dynamic_cast<IndexExprAST*>(assign_expr->LHS.get());
        if (!lhs_var && !lhs_index) {
            throw runtime_error("Semantic Error: Invalid lvalue in assignment. Left side must be a variable or an array element.");
        }

        resolve_expression(assign_expr->LHS.get(), varmap);
        resolve_expression(assign_expr->RHS.get(), varmap);

        SimpleType lhs_t = infer_expr_type(assign_expr->LHS.get(), varmap);
        SimpleType rhs_t = infer_expr_type(assign_expr->RHS.get(), varmap);
        if (!convertible(rhs_t, lhs_t)) { 
            throw runtime_error("Semantic Error: Cannot assign expression of type '" + simpletype_to_string(rhs_t) +
                                 "' to variable/element of type '" + simpletype_to_string(lhs_t) + "'.");
        }
        expr_type_[expr] = lhs_t;
        return;
    }

    if (auto* unary_expr = dynamic_cast<UnaryExprAST*>(expr)) {
        resolve_expression(unary_expr->Operand.get(), varmap);
        expr_type_[expr] = infer_expr_type(expr, varmap);
        return;
    }

    if (auto* binary_expr = dynamic_cast<BinaryExprAST*>(expr)) {
        resolve_expression(binary_expr->LHS.get(), varmap);
        resolve_expression(binary_expr->RHS.get(), varmap);
        expr_type_[expr] = infer_expr_type(expr, varmap);
        return;
    }

    if (auto* cond = dynamic_cast<ConditionalExprAST*>(expr)) {
        resolve_expression(cond->Condition.get(), varmap);
        resolve_expression(cond->ThenExpr.get(), varmap);
        resolve_expression(cond->ElseExpr.get(), varmap);
        expr_type_[expr] = infer_expr_type(expr, varmap);
        return;
    }

    if (auto* c = dynamic_cast<ConstantExprAST*>(expr)) {
        expr_type_[expr] = infer_expr_type(expr, varmap);
        return;
    }

}

void SemanticAnalysis::resolve_for_init(ForInitAST* init, VarMap& varmap) {
    if (!init) return;

    if (auto* e = dynamic_cast<ForInitExprAST*>(init)) {
        if (e->InitExpr) resolve_expression(e->InitExpr.get(), varmap);
        return;
    }
    if (auto* d = dynamic_cast<ForInitDeclAST*>(init)) {
        resolve_declaration(d->InitDecl.get(), varmap); 
        return;
    }
    throw runtime_error("Unknown for-init variant during resolution.");
}

void SemanticAnalysis::resolve_optional_expr(ExprAST* maybe, VarMap& varmap) {
    if (maybe) resolve_expression(maybe, varmap);
}



static bool str_contains_any(const std::string& s, const std::string& chars) {
    return std::any_of(s.begin(), s.end(), [&](char c){ return chars.find(c) != std::string::npos; });
}

SimpleType SemanticAnalysis::infer_expr_type(ExprAST* expr, const VarMap& varmap) {
    if (!expr) return SimpleType::UNKNOWN;

    
    auto it_cache = expr_type_.find(expr);
    if (it_cache != expr_type_.end()) return it_cache->second;

    
    if (auto* c = dynamic_cast<ConstantExprAST*>(expr)) {
        const std::string& lit = c->Val;
        if (str_contains_any(lit, ".eE")) {
            expr_type_[expr] = SimpleType::DOUBLE;
            return SimpleType::DOUBLE;
        } else {
            
            expr_type_[expr] = SimpleType::INT;
            return SimpleType::INT;
        }
    }

    
    if (auto* v = dynamic_cast<VarExprAST*>(expr)) {
        auto it = expr_type_.find(v);
        if (it != expr_type_.end()) {
            return it->second; 
        }
        
        for (const auto& kv : varmap) {
            if (kv.second.unique_name == v->Name) {
                expr_type_[expr] = kv.second.decl_type;
                return kv.second.decl_type;
            }
        }
        throw runtime_error("Semantic Error: Unknown variable when inferring type: '" + v->Name + "'.");
    }

    
    if (auto* idx = dynamic_cast<IndexExprAST*>(expr)) {
        
        VarExprAST* base_var = dynamic_cast<VarExprAST*>(idx->Base.get());
        if (!base_var) {
            throw runtime_error("Semantic Error: Indexing base must be a variable (single-dimension arrays only).");
        }
        
        for (const auto& kv : varmap) {
            if (kv.second.unique_name == base_var->Name) {
                if (!kv.second.is_array) {
                    throw runtime_error("Semantic Error: Attempting to index non-array '" + base_var->Name + "'.");
                }
                
                expr_type_[expr] = kv.second.decl_type;
                return kv.second.decl_type;
            }
        }
        throw runtime_error("Semantic Error: Unknown base variable in index expression: '" + base_var->Name + "'.");
    }

    
    if (auto* u = dynamic_cast<UnaryExprAST*>(expr)) {
        std::string op = u->Op.value;
        SimpleType operand_t = infer_expr_type(u->Operand.get(), varmap);
        if (op == "-" ) {
            if (!is_numeric_type(operand_t)) throw runtime_error("Semantic Error: Unary '-' applied to non-numeric type '" + simpletype_to_string(operand_t) + "'.");
            
            SimpleType result_type = (operand_t == SimpleType::CHAR) ? SimpleType::INT : operand_t;
            expr_type_[expr] = result_type;
            return result_type;
        } else if (op == "!") {
            
            expr_type_[expr] = SimpleType::INT;
            return SimpleType::INT;
        } else if (op == "~") {
            
            if (operand_t != SimpleType::INT && operand_t != SimpleType::CHAR) {
                throw runtime_error("Semantic Error: Bitwise complement '~' requires integer or char operand.");
            }
            expr_type_[expr] = SimpleType::INT; 
            return SimpleType::INT;
        }
        
        expr_type_[expr] = operand_t;
        return operand_t;
    }

    
    if (auto* b = dynamic_cast<BinaryExprAST*>(expr)) {
        std::string op = b->Op.value;
        SimpleType lhs_t = infer_expr_type(b->LHS.get(), varmap);
        SimpleType rhs_t = infer_expr_type(b->RHS.get(), varmap);

        
        if (op == "+" || op == "-" || op == "*" || op == "/") {
            if (!is_numeric_type(lhs_t) || !is_numeric_type(rhs_t)) 
                throw runtime_error("Semantic Error: Arithmetic operator '" + op + "' applied to non-numeric operands.");
            
            if (lhs_t == SimpleType::DOUBLE || rhs_t == SimpleType::DOUBLE) {
                expr_type_[expr] = SimpleType::DOUBLE; 
                return SimpleType::DOUBLE;
            }
            if (lhs_t == SimpleType::FLOAT || rhs_t == SimpleType::FLOAT) {
                expr_type_[expr] = SimpleType::FLOAT; 
                return SimpleType::FLOAT;
            }
            
            expr_type_[expr] = SimpleType::INT;
            return SimpleType::INT;
        }

        if (op == "%") {
            
            if ((lhs_t != SimpleType::INT && lhs_t != SimpleType::CHAR) || 
                (rhs_t != SimpleType::INT && rhs_t != SimpleType::CHAR)) {
                throw runtime_error("Semantic Error: Modulo operator '%' requires integer or char operands.");
            }
            expr_type_[expr] = SimpleType::INT; 
            return SimpleType::INT;
        }

        
        if (op == "|" || op == "&") {
            
            if ((lhs_t != SimpleType::INT && lhs_t != SimpleType::CHAR) || 
                (rhs_t != SimpleType::INT && rhs_t != SimpleType::CHAR)) {
                throw runtime_error("Semantic Error: Bitwise operator '" + op + "' requires integer or char operands.");
            }
            expr_type_[expr] = SimpleType::INT; 
            return SimpleType::INT;
        }

        
        if (op == "==" || op == "!=" || op == "<" || op == "<=" || op == ">" || op == ">=") {
            if (!is_numeric_type(lhs_t) || !is_numeric_type(rhs_t)) 
                throw runtime_error("Semantic Error: Comparison operator '" + op + "' applied to non-numeric operands.");
            expr_type_[expr] = SimpleType::INT;
            return SimpleType::INT;
        }

        
        if (op == "&&" || op == "||") {
            if (!is_numeric_type(lhs_t) || !is_numeric_type(rhs_t)) 
                throw runtime_error("Semantic Error: Logical operator '" + op + "' applied to non-numeric operands.");
            expr_type_[expr] = SimpleType::INT;
           return SimpleType::INT;
        }

        
        throw runtime_error("Semantic Error: Unsupported binary operator '" + op + "' in type inference.");
    }

    
    if (auto* c = dynamic_cast<ConditionalExprAST*>(expr)) {
        SimpleType then_t = infer_expr_type(c->ThenExpr.get(), varmap);
        SimpleType else_t = infer_expr_type(c->ElseExpr.get(), varmap);
        if (then_t == else_t) {
            expr_type_[expr] = then_t;
            return then_t;
        }
        
        if (then_t == SimpleType::DOUBLE || else_t == SimpleType::DOUBLE) {
            expr_type_[expr] = SimpleType::DOUBLE;
            return SimpleType::DOUBLE;
        }
        if (then_t == SimpleType::FLOAT || else_t == SimpleType::FLOAT) {
           expr_type_[expr] = SimpleType::FLOAT;
            return SimpleType::FLOAT;
        }
        
        expr_type_[expr] = SimpleType::INT;
        return SimpleType::INT;
    }

    throw runtime_error("Semantic Error: Could not infer type for expression node.");
}




void SemanticAnalysis::annotate_block(
    const std::vector<std::unique_ptr<BlockItemAST>>& items,
    int current_label
) {
    for (auto& item : items) {
        annotate_block_item(item.get(), current_label);
    }
}

void SemanticAnalysis::annotate_block_item(BlockItemAST* item, int current_label) {
    if (dynamic_cast<DeclarationAST*>(item)) {
        
       return;
    }
    if (auto* stmt = dynamic_cast<StatementAST*>(item)) {
        annotate_loops(stmt, current_label);
        return;
    }
    throw runtime_error("Unknown block item during loop annotation.");
}

void SemanticAnalysis::annotate_loops(StatementAST* stmt, int current_label) {
    
    if (auto* b = dynamic_cast<BreakStatementAST*>(stmt)) {
        if (current_label < 0) {
            throw runtime_error("Semantic Error: 'break' statement outside of a loop.");
        }
        enclosing_id_of_bc_[b] = current_label;
        return;
    }

    
    if (auto* c = dynamic_cast<ContinueStatementAST*>(stmt)) {
   if (current_label < 0) {
            throw runtime_error("Semantic Error: 'continue' statement outside of a loop.");
        }
        enclosing_id_of_bc_[c] = current_label;
        return;
    }

    
    if (auto* w = dynamic_cast<WhileStatementAST*>(stmt)) {
       int new_label = make_loop_label();
        loop_id_of_stmt_[w] = new_label;
        annotate_loops(w->Body.get(), new_label);
        return;
    }

    
    if (auto* dw = dynamic_cast<DoWhileStatementAST*>(stmt)) {
        int new_label = make_loop_label();
        loop_id_of_stmt_[dw] = new_label;
        annotate_loops(dw->Body.get(), new_label);
        return;
    }

    
    if (auto* f = dynamic_cast<ForStatementAST*>(stmt)) {
        int new_label = make_loop_label();
        loop_id_of_stmt_[f] = new_label;
        annotate_loops(f->Body.get(), new_label);
        return;
   }

    
    if (auto* comp = dynamic_cast<CompoundStatementAST*>(stmt)) {
        annotate_block(comp->body_->Items, current_label);
        return;
    }

    
    if (auto* ifs = dynamic_cast<IfStatementAST*>(stmt)) {
        annotate_loops(ifs->ThenBranch.get(), current_label);
        if (ifs->ElseBranch) annotate_loops(ifs->ElseBranch.get(), current_label);
        return;
    }

    
    if (dynamic_cast<ReturnStatementAST*>(stmt)) return;
    if (dynamic_cast<ExpressionStatementAST*>(stmt)) return;
    if (dynamic_cast<NullStatementAST*>(stmt)) return;

    throw runtime_error("Unknown statement during loop annotation.");
}
