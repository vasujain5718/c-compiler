#ifndef AST_H
#define AST_H

#include "lexer.h"
#include <string>
#include <memory>
#include <vector>
#include <utility>

struct ExprAST {
    virtual ~ExprAST() = default;
};

struct ConstantExprAST : ExprAST {
    std::string Val;
    explicit ConstantExprAST(std::string Val) : Val(std::move(Val)) {}
};

struct StatementAST {
    virtual ~StatementAST() = default;
};

struct ReturnStatementAST : StatementAST {
    std::unique_ptr<ExprAST> Expression;
    explicit ReturnStatementAST(std::unique_ptr<ExprAST> Expression) : Expression(std::move(Expression)) {}
};

struct FunctionAST {
    std::string Name;
    std::unique_ptr<StatementAST> Body;
    FunctionAST(std::string Name, std::unique_ptr<StatementAST> Body) : Name(std::move(Name)), Body(std::move(Body)) {}
};

#endif 