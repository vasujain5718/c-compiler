#ifndef TACKY_H
#define TACKY_H

#include <string>
#include <vector>
#include <memory>
#include <utility>

namespace tacky {

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
    virtual ~Value() = default;
};

struct Constant : public Value {
    std::string value;
    explicit Constant(std::string val) : value(std::move(val)) {}
};

struct Var : public Value {
    std::string name; 
    explicit Var(std::string name) : name(std::move(name)) {}
};

struct Instruction {
    virtual ~Instruction() = default;
};

struct ReturnInstruction : public Instruction {
    std::unique_ptr<Value> val;
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