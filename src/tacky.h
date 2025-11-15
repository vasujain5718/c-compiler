#ifndef TACKY_H
#define TACKY_H

#include <string>
#include <vector>
#include <memory>
#include <utility>

namespace tacky {

enum class TypeKind {
    Int,
    Float,
    Double
};

enum class UnaryOperatorType {
    Complement,
    Negate,
    LogicalNot // NEW
};

enum class BinaryOperatorType {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
    LogicalAnd,
    LogicalOr
};

struct Value {
    TypeKind type;
    explicit Value(TypeKind t) : type(t) {}
    virtual ~Value() = default;
};

struct Constant : public Value {
    std::string value; // textual representation
    Constant(TypeKind t, std::string val) : Value(t), value(std::move(val)) {}
};

struct Var : public Value {
    std::string name; 
    Var(TypeKind t, std::string name) : Value(t), name(std::move(name)) {}
};

struct Instruction {
    virtual ~Instruction() = default;
};

struct ReturnInstruction : public Instruction {
    std::unique_ptr<Value> val; // may be nullptr
    explicit ReturnInstruction(std::unique_ptr<Value> val) : val(std::move(val)) {}
};

struct UnaryInstruction : public Instruction {
    UnaryOperatorType op;
    std::unique_ptr<Value> src;
    std::unique_ptr<Var> dst;
    UnaryInstruction(UnaryOperatorType op, std::unique_ptr<Value> src, std::unique_ptr<Var> dst)
        : op(op), src(std::move(src)), dst(std::move(dst)) {}
};

struct BinaryInstruction : public Instruction {
    BinaryOperatorType op;
    std::unique_ptr<Value> src1;
    std::unique_ptr<Value> src2;
    std::unique_ptr<Var> dst;
    BinaryInstruction(BinaryOperatorType op, std::unique_ptr<Value> src1, std::unique_ptr<Value> src2, std::unique_ptr<Var> dst)
        : op(op), src1(std::move(src1)), src2(std::move(src2)), dst(std::move(dst)) {}
};

struct CopyInstruction : public Instruction {
    std::unique_ptr<Value> src;
    std::unique_ptr<Var> dst;
    CopyInstruction(std::unique_ptr<Value> src, std::unique_ptr<Var> dst)
        : src(std::move(src)), dst(std::move(dst)) {}
};

struct JumpInstruction : public Instruction {
    std::string target;
    explicit JumpInstruction(std::string target) : target(std::move(target)) {}
};

struct JumpIfZeroInstruction : public Instruction {
    std::unique_ptr<Value> condition;
    std::string target;
    JumpIfZeroInstruction(std::unique_ptr<Value> condition, std::string target)
        : condition(std::move(condition)), target(std::move(target)) {}
};

struct JumpIfNotZeroInstruction : public Instruction {
    std::unique_ptr<Value> condition;
    std::string target;
    JumpIfNotZeroInstruction(std::unique_ptr<Value> condition, std::string target)
        : condition(std::move(condition)), target(std::move(target)) {}
};

struct LabelInstruction : public Instruction {
    std::string target;
    explicit LabelInstruction(std::string target) : target(std::move(target)) {}
};

struct Function {
    std::string name;
    std::vector<std::unique_ptr<Instruction>> body;
};

struct Program {
    std::unique_ptr<Function> function;
};

} // namespace tacky

#endif // TACKY_H

#ifndef TACKY_GENERATOR_H
#define TACKY_GENERATOR_H

#include "ast.h"
#include "tacky.h"
#include <memory>
#include <vector>
#include <string>
#include <utility>

class TackyGenerator {
public:
    std::unique_ptr<tacky::Program> generate(const FunctionAST* ast);

private:
    // Expr -> Value
    std::unique_ptr<tacky::Value> generate_expression(
        const ExprAST* expr,
        std::vector<std::unique_ptr<tacky::Instruction>>& instructions
    );

    // Block item dispatch
    void generate_block_item(
        const BlockItemAST* item,
        std::vector<std::unique_ptr<tacky::Instruction>>& instructions
    );

    // Statements / Decls
    void generate_statement(
        const StatementAST* stmt,
        std::vector<std::unique_ptr<tacky::Instruction>>& instructions
    );

    void generate_declaration(
        const DeclarationAST* decl,
        std::vector<std::unique_ptr<tacky::Instruction>>& instructions
    );

    // Assignments
    std::unique_ptr<tacky::Value> generate_assignment(
        const AssignmentExprAST* assign_expr,
        std::vector<std::unique_ptr<tacky::Instruction>>& instructions
    );

    // --- NEW: helpers for loops ---
    void gen_for_init(const ForInitAST* init,
                      std::vector<std::unique_ptr<tacky::Instruction>>& instructions);

    // label management
    std::unique_ptr<tacky::Var> make_temporary(tacky::TypeKind kind);
    std::string make_label();

    int next_var_id = 0;
    int next_label_id = 0;

    // Stack of (continue_label, break_label) for innermost loop
    std::vector<std::pair<std::string, std::string>> loop_label_stack_;
};

#endif // TACKY_GENERATOR_H
