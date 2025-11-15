#ifndef AST_H
#define AST_H

#include "lexer.h"
#include <memory>
#include <string>
#include <utility>
#include <vector>
#include "type.h"  // SimpleType enum
// ---------- Forward declarations (needed before unique_ptr<T> usage) ----------
struct ExprAST;
struct DeclarationAST;
struct StatementAST;
struct BlockItemAST;
struct BlockAST;

// ---------- For-init nodes ----------
struct ForInitAST {
    ForInitAST() = default;
    virtual ~ForInitAST() = default;  // <-- make base polymorphic for dynamic_cast
};
struct ForInitExprAST : ForInitAST {
    std::unique_ptr<ExprAST> InitExpr; // can be nullptr
    explicit ForInitExprAST(std::unique_ptr<ExprAST> init_expr)
        : InitExpr(std::move(init_expr)) {}
};

struct ForInitDeclAST : ForInitAST {
    std::unique_ptr<DeclarationAST> InitDecl; // cannot be nullptr
    explicit ForInitDeclAST(std::unique_ptr<DeclarationAST> init_decl)
        : InitDecl(std::move(init_decl)) {}
};

// ---------- Base class for all expressions ----------
struct ExprAST {
    virtual ~ExprAST() = default;
};

// ---------- Expression Nodes (as per ASDL) ----------
struct ConstantExprAST : ExprAST {
    std::string Val;          // the literal text as written (e.g., "1", "3.14", "1e-3")
    SimpleType literal_type;  // optional: explicit literal type if known (INT/FLOAT/DOUBLE)
    ConstantExprAST(std::string val, SimpleType t = SimpleType::UNKNOWN)
        : Val(std::move(val)), literal_type(t) {}
};


struct VarExprAST : ExprAST {
    std::string Name;
    SimpleType literal_type;
    explicit VarExprAST(std::string name, SimpleType t = SimpleType::UNKNOWN)
    : Name(std::move(name)), literal_type(std::move(t)) {}

};

struct UnaryExprAST : ExprAST {
    Token Op;
    std::unique_ptr<ExprAST> Operand;
    UnaryExprAST(Token op, std::unique_ptr<ExprAST> operand)
        : Op(std::move(op)), Operand(std::move(operand)) {}
};

struct BinaryExprAST : ExprAST {
    Token Op;
    std::unique_ptr<ExprAST> LHS, RHS;
    BinaryExprAST(Token op, std::unique_ptr<ExprAST> lhs, std::unique_ptr<ExprAST> rhs)
        : Op(std::move(op)), LHS(std::move(lhs)), RHS(std::move(rhs)) {}
};

struct AssignmentExprAST : ExprAST {
    std::unique_ptr<ExprAST> LHS;
    std::unique_ptr<ExprAST> RHS;
    AssignmentExprAST(std::unique_ptr<ExprAST> lhs, std::unique_ptr<ExprAST> rhs)
        : LHS(std::move(lhs)), RHS(std::move(rhs)) {}
};

struct ConditionalExprAST : ExprAST {
    std::unique_ptr<ExprAST> Condition;
    std::unique_ptr<ExprAST> ThenExpr;
    std::unique_ptr<ExprAST> ElseExpr;
    ConditionalExprAST(std::unique_ptr<ExprAST> condition,
                       std::unique_ptr<ExprAST> then_expr,
                       std::unique_ptr<ExprAST> else_expr)
        : Condition(std::move(condition)),
          ThenExpr(std::move(then_expr)),
          ElseExpr(std::move(else_expr)) {}
};

// ---------- Base class for items in a function body ----------
struct BlockItemAST {
    virtual ~BlockItemAST() = default;
};

// ---------- Statement Nodes (inherit from BlockItemAST) ----------
struct StatementAST : BlockItemAST {
    virtual ~StatementAST() = default;
};

struct ReturnStatementAST : StatementAST {
    std::unique_ptr<ExprAST> Expression;
    explicit ReturnStatementAST(std::unique_ptr<ExprAST> expression)
        : Expression(std::move(expression)) {}
};

struct ExpressionStatementAST : StatementAST {
    std::unique_ptr<ExprAST> Expression;
    explicit ExpressionStatementAST(std::unique_ptr<ExprAST> expression)
        : Expression(std::move(expression)) {}
};

struct NullStatementAST : StatementAST {
    NullStatementAST() = default;
};

struct IfStatementAST : StatementAST {
    std::unique_ptr<ExprAST> Condition;
    std::unique_ptr<StatementAST> ThenBranch;
    std::unique_ptr<StatementAST> ElseBranch; // Can be nullptr
    IfStatementAST(std::unique_ptr<ExprAST> condition,
                   std::unique_ptr<StatementAST> then_branch,
                   std::unique_ptr<StatementAST> else_branch = nullptr)
        : Condition(std::move(condition)),
          ThenBranch(std::move(then_branch)),
          ElseBranch(std::move(else_branch)) {}
};

struct BreakStatementAST : StatementAST {
    BreakStatementAST() = default;
};

struct ContinueStatementAST : StatementAST {
    ContinueStatementAST() = default;
};

struct WhileStatementAST : StatementAST {
    std::unique_ptr<ExprAST> Condition;
    std::unique_ptr<StatementAST> Body;
    WhileStatementAST(std::unique_ptr<ExprAST> condition,
                      std::unique_ptr<StatementAST> body)
        : Condition(std::move(condition)), Body(std::move(body)) {}
};

struct DoWhileStatementAST : StatementAST {
    std::unique_ptr<StatementAST> Body;
    std::unique_ptr<ExprAST> Condition;
    DoWhileStatementAST(std::unique_ptr<StatementAST> body,
                        std::unique_ptr<ExprAST> condition)
        : Body(std::move(body)), Condition(std::move(condition)) {}
};

struct ForStatementAST : StatementAST {
    std::unique_ptr<ForInitAST> Init;
    std::unique_ptr<ExprAST> Condition; // can be nullptr
    std::unique_ptr<ExprAST> Increment; // can be nullptr
    std::unique_ptr<StatementAST> Body;
    ForStatementAST(std::unique_ptr<ForInitAST> init,
                    std::unique_ptr<ExprAST> condition,
                    std::unique_ptr<ExprAST> increment,
                    std::unique_ptr<StatementAST> body)
        : Init(std::move(init)),
          Condition(std::move(condition)),
          Increment(std::move(increment)),
          Body(std::move(body)) {}
};

// ---------- Block container ----------
struct BlockAST {
    std::vector<std::unique_ptr<BlockItemAST>> Items;
    explicit BlockAST(std::vector<std::unique_ptr<BlockItemAST>> items)
        : Items(std::move(items)) {}
};

// ---------- Compound statement that owns a BlockAST ----------
struct CompoundStatementAST : StatementAST {
    std::unique_ptr<BlockAST> body_;
    explicit CompoundStatementAST(std::unique_ptr<BlockAST> body)
        : body_(std::move(body)) {}
};

// ---------- Declaration Node (also inherits from BlockItemAST) ----------
struct DeclarationAST : BlockItemAST {
    SimpleType DeclType; // e.g., "int", "float", "double"
    std::string VarName;
    std::unique_ptr<ExprAST> InitExpr; // Can be nullptr
    DeclarationAST(SimpleType decl_type, std::string var_name, std::unique_ptr<ExprAST> init_expr = nullptr)
        : DeclType(std::move(decl_type)), VarName(std::move(var_name)), InitExpr(std::move(init_expr)) {}
};

// ---------- Top Level ----------
struct FunctionAST {
    SimpleType ReturnType; // e.g., "int", "void", "float", "double"
    std::string Name;
    std::vector<std::unique_ptr<BlockItemAST>> Body; // list of BlockItemASTs
    FunctionAST(SimpleType return_type, std::string name, std::vector<std::unique_ptr<BlockItemAST>> body)
        : ReturnType(std::move(return_type)), Name(std::move(name)), Body(std::move(body)) {}
};

#endif // AST_H
