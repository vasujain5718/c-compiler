#ifndef AST_H
#define AST_H

#include "lexer.h"
#include <string>
#include <memory>
#include <vector>
#include <utility>

// --- Base class for all expressions ---
struct ExprAST {
    virtual ~ExprAST() = default;
};

// --- Expression Nodes (as per ASDL) ---
struct ConstantExprAST : ExprAST {
    std::string Val; // Keeping this as string for now, as in your parser
    explicit ConstantExprAST(std::string Val) : Val(std::move(Val)) {}
};

struct VarExprAST : ExprAST {
    std::string Name;
    explicit VarExprAST(std::string Name) : Name(std::move(Name)) {}
};

struct UnaryExprAST : ExprAST {
    Token Op;
    std::unique_ptr<ExprAST> Operand;
    UnaryExprAST(Token Op, std::unique_ptr<ExprAST> Operand) : Op(std::move(Op)), Operand(std::move(Operand)) {}
};

struct BinaryExprAST : ExprAST {
    Token Op;
    std::unique_ptr<ExprAST> LHS, RHS;
    BinaryExprAST(Token Op, std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RHS)
        : Op(std::move(Op)), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
};

struct AssignmentExprAST : ExprAST {
    std::unique_ptr<ExprAST> LHS;
    std::unique_ptr<ExprAST> RHS;
    AssignmentExprAST(std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RHS)
        : LHS(std::move(LHS)), RHS(std::move(RHS)) {}
};

// --- Base class for items in a function body ---
struct BlockItemAST {
    virtual ~BlockItemAST() = default;
};

// --- Statement Nodes (inherit from BlockItemAST) ---
struct StatementAST : BlockItemAST {};

struct ReturnStatementAST : StatementAST {
    std::unique_ptr<ExprAST> Expression;
    explicit ReturnStatementAST(std::unique_ptr<ExprAST> Expression) : Expression(std::move(Expression)) {}
};

struct ExpressionStatementAST : StatementAST {
    std::unique_ptr<ExprAST> Expression;
    explicit ExpressionStatementAST(std::unique_ptr<ExprAST> Expression) : Expression(std::move(Expression)) {}
};

struct NullStatementAST : StatementAST {
    NullStatementAST() = default;
};

// --- Declaration Node (also inherits from BlockItemAST) ---
struct DeclarationAST : BlockItemAST {
    std::string VarName;
    std::unique_ptr<ExprAST> InitExpr; // Can be nullptr
    DeclarationAST(std::string VarName, std::unique_ptr<ExprAST> InitExpr = nullptr)
        : VarName(std::move(VarName)), InitExpr(std::move(InitExpr)) {}
};

// --- Top Level ---
struct FunctionAST {
    std::string Name;
    std::vector<std::unique_ptr<BlockItemAST>> Body; // Body is now a list of BlockItemASTs
    FunctionAST(std::string Name, std::vector<std::unique_ptr<BlockItemAST>> Body)
        : Name(std::move(Name)), Body(std::move(Body)) {}
};

#endif