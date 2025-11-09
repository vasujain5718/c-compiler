#include "semantic_analysis.h"
#include <stdexcept>
#include <iostream>
#include <utility>

using std::runtime_error;
using std::string;

// ===== Public =====

void SemanticAnalysis::resolve(FunctionAST* function) {
    unique_counter = 0;

    VarMap varmap; // starts empty for the function body
    // The function's top-level "block" (its body vector) is processed as a block
    resolve_block(function->Body, varmap);
}

// ===== Helpers =====

std::string SemanticAnalysis::make_unique_name(const string& original_name) {
    return original_name + "." + std::to_string(unique_counter++);
}

SemanticAnalysis::VarMap SemanticAnalysis::copy_variable_map(const VarMap& m) {
    VarMap copy = m;
    // Entering a new block: every entry is now from an *outer* block
    for (auto& kv : copy) {
        kv.second.from_current_block = false;
    }
    return copy;
}

// Process a sequence of BlockItemAST in the *current* block scope
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
    } else if (auto* stmt = dynamic_cast<StatementAST*>(item)) {
        resolve_statement(stmt, varmap);
    } else {
        throw runtime_error("Unknown block item type during semantic analysis.");
    }
}

// ===== Declarations =====

void SemanticAnalysis::resolve_declaration(DeclarationAST* decl, VarMap& varmap) {
    // Duplicate only if already declared in *this* block
    auto it = varmap.find(decl->VarName);
    if (it != varmap.end() && it->second.from_current_block) {
        throw runtime_error("Semantic Error: Duplicate declaration of variable '" + decl->VarName + "' in the same block.");
    }

    // Introduce the variable first (visible to its own initializer)
    std::string unique_name = make_unique_name(decl->VarName);
    varmap[decl->VarName] = MapEntry{unique_name, /*from_current_block=*/true};

    // Update declaration's stored name to the unique one
    decl->VarName = unique_name;

    // Resolve initializer (may reference itself or other names)
    if (decl->InitExpr) {
        resolve_expression(decl->InitExpr.get(), varmap);
    }
}

// ===== Statements =====

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
        // no-op
        return;
    }

    if (auto* if_stmt = dynamic_cast<IfStatementAST*>(stmt)) {
        resolve_expression(if_stmt->Condition.get(), varmap);
        resolve_statement(if_stmt->ThenBranch.get(), varmap);
        if (if_stmt->ElseBranch) {
            resolve_statement(if_stmt->ElseBranch.get(), varmap);
        }
        return;
    }

    // Compound block: create a *copied* variable map where entries become outer
    if (auto* comp = dynamic_cast<CompoundStatementAST*>(stmt)) {
        VarMap inner = copy_variable_map(varmap);
        resolve_block(comp->body_->Items, inner);
        // Discard 'inner' so declarations are not visible outside
        return;
    }

    throw runtime_error("Unknown statement type during resolution.");
}

// ===== Expressions =====

void SemanticAnalysis::resolve_expression(ExprAST* expr, VarMap& varmap) {
    if (auto* var_expr = dynamic_cast<VarExprAST*>(expr)) {
        auto it = varmap.find(var_expr->Name);
        if (it == varmap.end()) {
            throw runtime_error("Semantic Error: Undeclared variable '" + var_expr->Name + "'.");
        }
        var_expr->Name = it->second.unique_name;
        return;
    }

    if (auto* assign_expr = dynamic_cast<AssignmentExprAST*>(expr)) {
        // LHS must be a var (lvalue). Check before rewriting the name.
        auto* lhs_var = dynamic_cast<VarExprAST*>(assign_expr->LHS.get());
        if (!lhs_var) {
            throw runtime_error("Semantic Error: Invalid lvalue in assignment. Left side must be a variable.");
        }

        // Resolve both sides (will rewrite LHS name to unique)
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

    // ConstantExprAST or unknown node types: constants are no-op
}
