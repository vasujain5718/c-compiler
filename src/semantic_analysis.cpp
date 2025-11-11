#include "semantic_analysis.h"
#include <stdexcept>
#include <utility>

using std::runtime_error;
using std::string;

// ===== Public =====

void SemanticAnalysis::resolve(FunctionAST* function) {
    unique_counter = 0;
    loop_label_counter = 0;
    loop_id_of_stmt_.clear();
    enclosing_id_of_bc_.clear();

    VarMap varmap; // empty map at function scope

    // 1) Variable/name resolution
    resolve_block(function->Body, varmap);

    // 2) Loop annotation (separate pass)
    annotate_block(function->Body, /*current_label=*/-1);
}

int SemanticAnalysis::get_loop_id(const StatementAST* stmt) const {
    auto it = loop_id_of_stmt_.find(stmt);
    return (it == loop_id_of_stmt_.end()) ? -1 : it->second;
}

int SemanticAnalysis::get_enclosing_loop_id(const StatementAST* stmt) const {
    auto it = enclosing_id_of_bc_.find(stmt);
    return (it == enclosing_id_of_bc_.end()) ? -1 : it->second;
}

// ===== Helpers =====

std::string SemanticAnalysis::make_unique_name(const string& original_name) {
    return original_name + "." + std::to_string(unique_counter++);
}

SemanticAnalysis::VarMap SemanticAnalysis::copy_variable_map(const VarMap& m) {
    VarMap copy = m;
    for (auto& kv : copy) {
        kv.second.from_current_block = false; // entering a new block
    }
    return copy;
}

// ===== Variable resolution =====

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
    // duplicate in SAME block?
    auto it = varmap.find(decl->VarName);
    if (it != varmap.end() && it->second.from_current_block) {
        throw runtime_error("Semantic Error: Duplicate declaration of variable '" + decl->VarName + "' in the same block.");
    }

    // introduce name first so initializer can see it
    std::string unique_name = make_unique_name(decl->VarName);
    varmap[decl->VarName] = MapEntry{unique_name, /*from_current_block=*/true};

    // rewrite declaration name to the unique one
    decl->VarName = unique_name;

    if (decl->InitExpr) resolve_expression(decl->InitExpr.get(), varmap);
}

void SemanticAnalysis::resolve_statement(StatementAST* stmt, VarMap& varmap) {
    if (auto* ret_stmt = dynamic_cast<ReturnStatementAST*>(stmt)) {
        resolve_expression(ret_stmt->Expression.get(), varmap);
        return;
    }

    if (auto* expr_stmt = dynamic_cast<ExpressionStatementAST*>(stmt)) {
        resolve_expression(expr_stmt->Expression.get(), varmap);
        return;
    }

    if (dynamic_cast<NullStatementAST*>(stmt)) {
        return; // no-op
    }

    if (auto* if_stmt = dynamic_cast<IfStatementAST*>(stmt)) {
        resolve_expression(if_stmt->Condition.get(), varmap);
        resolve_statement(if_stmt->ThenBranch.get(), varmap);
        if (if_stmt->ElseBranch) resolve_statement(if_stmt->ElseBranch.get(), varmap);
        return;
    }

    // Compound block -> new scope
    if (auto* comp = dynamic_cast<CompoundStatementAST*>(stmt)) {
        VarMap inner = copy_variable_map(varmap);
        resolve_block(comp->body_->Items, inner);
        return;
    }

    // ----- NEW: while -----
    if (auto* w = dynamic_cast<WhileStatementAST*>(stmt)) {
        resolve_expression(w->Condition.get(), varmap);
        resolve_statement(w->Body.get(), varmap);
        return;
    }

    // ----- NEW: do-while -----
    if (auto* dw = dynamic_cast<DoWhileStatementAST*>(stmt)) {
        resolve_statement(dw->Body.get(), varmap);
        resolve_expression(dw->Condition.get(), varmap);
        return;
    }

    // ----- NEW: for (with header scope) -----
    if (auto* f = dynamic_cast<ForStatementAST*>(stmt)) {
        VarMap inner = copy_variable_map(varmap);   // header+body scope
        resolve_for_init(f->Init.get(), inner);     // may declare
        resolve_optional_expr(f->Condition.get(), inner);
        resolve_optional_expr(f->Increment.get(), inner);
        resolve_statement(f->Body.get(), inner);
        return;
    }

    // ----- NEW: break/continue (no children to resolve) -----
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
        var_expr->Name = it->second.unique_name;
        return;
    }

    if (auto* assign_expr = dynamic_cast<AssignmentExprAST*>(expr)) {
        auto* lhs_var = dynamic_cast<VarExprAST*>(assign_expr->LHS.get());
        if (!lhs_var) {
            throw runtime_error("Semantic Error: Invalid lvalue in assignment. Left side must be a variable.");
        }
        resolve_expression(assign_expr->LHS.get(), varmap);
        resolve_expression(assign_expr->RHS.get(), varmap);
        return;
    }

    if (auto* unary_expr = dynamic_cast<UnaryExprAST*>(expr)) {
        resolve_expression(unary_expr->Operand.get(), varmap);
        return;
    }

    if (auto* binary_expr = dynamic_cast<BinaryExprAST*>(expr)) {
        resolve_expression(binary_expr->LHS.get(), varmap);
        resolve_expression(binary_expr->RHS.get(), varmap);
        return;
    }

    if (auto* cond = dynamic_cast<ConditionalExprAST*>(expr)) {
        resolve_expression(cond->Condition.get(), varmap);
        resolve_expression(cond->ThenExpr.get(), varmap);
        resolve_expression(cond->ElseExpr.get(), varmap);
        return;
    }

    // ConstantExprAST or other leafs: no-op
}

void SemanticAnalysis::resolve_for_init(ForInitAST* init, VarMap& varmap) {
    if (!init) return;

    if (auto* e = dynamic_cast<ForInitExprAST*>(init)) {
        if (e->InitExpr) resolve_expression(e->InitExpr.get(), varmap);
        return;
    }
    if (auto* d = dynamic_cast<ForInitDeclAST*>(init)) {
        resolve_declaration(d->InitDecl.get(), varmap); // introduces header decls
        return;
    }
    throw runtime_error("Unknown for-init variant during resolution.");
}

void SemanticAnalysis::resolve_optional_expr(ExprAST* maybe, VarMap& varmap) {
    if (maybe) resolve_expression(maybe, varmap);
}

// ===== Loop annotation pass =====

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
        // Declarations carry no loop labels
        return;
    }
    if (auto* stmt = dynamic_cast<StatementAST*>(item)) {
        annotate_loops(stmt, current_label);
        return;
    }
    throw runtime_error("Unknown block item during loop annotation.");
}

void SemanticAnalysis::annotate_loops(StatementAST* stmt, int current_label) {
    // break
    if (auto* b = dynamic_cast<BreakStatementAST*>(stmt)) {
        if (current_label < 0) {
            throw runtime_error("Semantic Error: 'break' statement outside of a loop.");
        }
        enclosing_id_of_bc_[b] = current_label;
        return;
    }

    // continue
    if (auto* c = dynamic_cast<ContinueStatementAST*>(stmt)) {
        if (current_label < 0) {
            throw runtime_error("Semantic Error: 'continue' statement outside of a loop.");
        }
        enclosing_id_of_bc_[c] = current_label;
        return;
    }

    // while
    if (auto* w = dynamic_cast<WhileStatementAST*>(stmt)) {
        int new_label = make_loop_label();
        loop_id_of_stmt_[w] = new_label;
        annotate_loops(w->Body.get(), new_label);
        return;
    }

    // do-while
    if (auto* dw = dynamic_cast<DoWhileStatementAST*>(stmt)) {
        int new_label = make_loop_label();
        loop_id_of_stmt_[dw] = new_label;
        annotate_loops(dw->Body.get(), new_label);
        return;
    }

    // for
    if (auto* f = dynamic_cast<ForStatementAST*>(stmt)) {
        int new_label = make_loop_label();
        loop_id_of_stmt_[f] = new_label;
        annotate_loops(f->Body.get(), new_label);
        return;
    }

    // compound
    if (auto* comp = dynamic_cast<CompoundStatementAST*>(stmt)) {
        annotate_block(comp->body_->Items, current_label);
        return;
    }

    // if
    if (auto* ifs = dynamic_cast<IfStatementAST*>(stmt)) {
        annotate_loops(ifs->ThenBranch.get(), current_label);
        if (ifs->ElseBranch) annotate_loops(ifs->ElseBranch.get(), current_label);
        return;
    }

    // return / expr / null: nothing to annotate
    if (dynamic_cast<ReturnStatementAST*>(stmt)) return;
    if (dynamic_cast<ExpressionStatementAST*>(stmt)) return;
    if (dynamic_cast<NullStatementAST*>(stmt)) return;

    throw runtime_error("Unknown statement during loop annotation.");
}
