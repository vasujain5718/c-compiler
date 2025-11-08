#include "semantic_analysis.h"
#include <stdexcept>
#include <iostream>

using std::string;
using std::map;
using std::to_string;
using std::runtime_error;

void SemanticAnalysis::resolve(FunctionAST* function) {
    // Reset for each function (though right now we only have one)
    current_scope.clear();
    unique_counter = 0;

    for (auto& block_item : function->Body) {
        resolve_block_item(block_item.get());
    }
}

void SemanticAnalysis::resolve_block_item(BlockItemAST* item) {
    if (auto* decl = dynamic_cast<DeclarationAST*>(item)) {
        resolve_declaration(decl);
    } else if (auto* stmt = dynamic_cast<StatementAST*>(item)) {
        resolve_statement(stmt);
    } else {
        throw runtime_error("Unknown block item type during semantic analysis.");
    }
}

void SemanticAnalysis::resolve_declaration(DeclarationAST* decl) {
    // 1) Duplicate in current scope?
    if (current_scope.count(decl->VarName)) {
        throw runtime_error("Semantic Error: Duplicate declaration of variable '" + decl->VarName + "'.");
    }

    // 2) Introduce the variable first (so it's visible in its own initializer)
    string unique_name = make_unique_name(decl->VarName);
    current_scope[decl->VarName] = unique_name;

    // 3) Update the decl's stored name to the unique one
    decl->VarName = unique_name;

    // 4) Now resolve the initializer (it can legally refer to the var)
    if (decl->InitExpr) {
        resolve_expression(decl->InitExpr.get());
    }
}


void SemanticAnalysis::resolve_statement(StatementAST* stmt) {
    if (auto* ret_stmt = dynamic_cast<ReturnStatementAST*>(stmt)) {
        resolve_expression(ret_stmt->Expression.get());
    } 
    else if (auto* expr_stmt = dynamic_cast<ExpressionStatementAST*>(stmt)) {
        resolve_expression(expr_stmt->Expression.get());
    }
    else if (dynamic_cast<NullStatementAST*>(stmt)) {
        // Do nothing for null statements
    }
    else {
        throw runtime_error("Unknown statement type during resolution.");
    }
}

void SemanticAnalysis::resolve_expression(ExprAST* expr) {
    if (auto* var_expr = dynamic_cast<VarExprAST*>(expr)) {
        // Variable Usage: Look it up in the symbol table
        if (current_scope.find(var_expr->Name) == current_scope.end()) {
            throw runtime_error("Semantic Error: Undeclared variable '" + var_expr->Name + "'.");
        }
        // Replace the name with the unique version from the table
        var_expr->Name = current_scope[var_expr->Name];
    }
    else if (auto* assign_expr = dynamic_cast<AssignmentExprAST*>(expr)) {
        // Assignment: The LHS *must* be a variable.
        // We check this BEFORE resolving the LHS, because we need its original name if it is a var.
        if (dynamic_cast<VarExprAST*>(assign_expr->LHS.get()) == nullptr) {
            throw runtime_error("Semantic Error: Invalid lvalue in assignment. Left side must be a variable.");
        }

        // Resolve both sides.
        resolve_expression(assign_expr->LHS.get());
        resolve_expression(assign_expr->RHS.get());
    }
    else if (auto* unary_expr = dynamic_cast<UnaryExprAST*>(expr)) {
        resolve_expression(unary_expr->Operand.get());
    }
    else if (auto* binary_expr = dynamic_cast<BinaryExprAST*>(expr)) {
        resolve_expression(binary_expr->LHS.get());
        resolve_expression(binary_expr->RHS.get());
    }
    // Constants require no resolution.
}

string SemanticAnalysis::make_unique_name(const string& original_name) {
    // Example: "x" might become "x.0" or "x.5"
    return original_name + "." + to_string(unique_counter++);
}