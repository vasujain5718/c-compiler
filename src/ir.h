#ifndef IR_H
#define IR_H

#include <string>
#include <vector>
#include <memory>
#include <utility>

// We'll use the 'ir' namespace
namespace ir {

// --- Enums ---

enum class UnaryType {
    NOT,
    NEG
};

enum class BinaryType {
    ADD,
    SUB,
    MUL,
    // integer-only or generic integer ops above; floating variants below:
    ADDSD,
    SUBSD,
    MULSD,
    DIVSD
};

// NEW: Condition codes for jumps and sets
enum class CondCode {
    E,  // Equal
    NE, // Not Equal
    L,  // Less Than
    LE, // Less Than or Equal
    G,  // Greater Than
    GE,
    A,
    AE,
    B,
    BE  // Greater Than or Equal

};

enum class RegType {
    AX,
    DX,
    R10,
    R11,
    // XMM registers for floating point
    XMM0,
    XMM1,
    XMM2,
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7
};

// --- Register Struct ---

struct Reg {
    RegType type;
    explicit Reg(RegType type) : type(type) {}
};

// --- Operands (matches ASDL 'operand = ...') ---
// Note: Stack carries optional size (bytes) for emitter to select movsd vs movl.
struct Operand {
    virtual ~Operand() = default;
};

struct Register : public Operand {
    Reg name;
    explicit Register(Reg name) : name(name) {}
};

struct Immediate : public Operand {
    std::string value;
    explicit Immediate(std::string value) : value(std::move(value)) {}
};

struct Pseudo : public Operand {
    std::string name;
    explicit Pseudo(std::string name) : name(std::move(name)) {}
};

struct Stack : public Operand {
    int val;
    int size; // bytes (4 for int, 8 for double)
    explicit Stack(int val, int size = 4) : val(val), size(size) {}
};

// --- Instructions (matches ASDL 'instruction = ...') ---

struct Instruction {
    virtual ~Instruction() = default;
};

struct MovInstruction : public Instruction {
    std::unique_ptr<Operand> src;
    std::unique_ptr<Operand> dest;
    // integer/int32 'movl' semantics
    MovInstruction(std::unique_ptr<Operand> src, std::unique_ptr<Operand> dest) 
        : src(std::move(src)), dest(std::move(dest)) {}
};

// Floating move (movsd) with 64-bit double semantics.
// src/dest may be Register(XMM)/Stack/Memory/Immediate (Immediate -> load constant required)
struct MovSDInstruction : public Instruction {
    std::unique_ptr<Operand> src;
    std::unique_ptr<Operand> dest;
    MovSDInstruction(std::unique_ptr<Operand> src, std::unique_ptr<Operand> dest)
        : src(std::move(src)), dest(std::move(dest)) {}
};

struct RetInstruction : public Instruction {};

// Integer unary
struct UnaryInstruction : public Instruction {
    UnaryType op; 
    std::unique_ptr<Operand> operand; 

    UnaryInstruction(UnaryType op, std::unique_ptr<Operand> operand)
        : op(op), operand(std::move(operand)) {}
};

// BinaryInstruction carries BinaryType; it can represent both integer and FP binary ops
struct BinaryInstruction : public Instruction {
    BinaryType op;
    std::unique_ptr<Operand> lhs;
    std::unique_ptr<Operand> rhs;

    BinaryInstruction(BinaryType op, std::unique_ptr<Operand> lhs, std::unique_ptr<Operand> rhs)
        : op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}
};

struct IdivInstruction : public Instruction {
    std::unique_ptr<Operand> divisor;
    explicit IdivInstruction(std::unique_ptr<Operand> divisor) : divisor(std::move(divisor)) {}
};

struct CDQInstruction : public Instruction {};

// Stack allocation
struct AllocateStackInstruction : public Instruction {
    int n;
    explicit AllocateStackInstruction(int n) : n(n) {}
}; 

// --- NEW INSTRUCTIONS for Chapter 4 (already present) ---

struct CmpInstruction : public Instruction {
    std::unique_ptr<Operand> lhs;
    std::unique_ptr<Operand> rhs;
    CmpInstruction(std::unique_ptr<Operand> lhs, std::unique_ptr<Operand> rhs)
        : lhs(std::move(lhs)), rhs(std::move(rhs)) {}
};

struct JmpInstruction : public Instruction {
    std::string target;
    explicit JmpInstruction(std::string target) : target(std::move(target)) {}
};

struct JmpCCInstruction : public Instruction {
    CondCode cond;
    std::string target;
    JmpCCInstruction(CondCode cond, std::string target)
        : cond(cond), target(std::move(target)) {}
};

struct SetCCInstruction : public Instruction {
    CondCode cond;
    std::unique_ptr<Operand> dest; // Destination must be a byte-capable register operand (emitter ensures)
    explicit SetCCInstruction(CondCode cond, std::unique_ptr<Operand> dest)
        : cond(cond), dest(std::move(dest)) {}
};

struct LabelInstruction : public Instruction {
    std::string name;
    explicit LabelInstruction(std::string name) : name(std::move(name)) {}
};

// --- NEW: FP compare instruction (ucomisd) ---
// We use a CmpSDInstruction which instructs emitter to do an unordered compare of two doubles (ucomisd)
struct CmpSDInstruction : public Instruction {
    std::unique_ptr<Operand> lhs;
    std::unique_ptr<Operand> rhs;
    CmpSDInstruction(std::unique_ptr<Operand> lhs, std::unique_ptr<Operand> rhs)
        : lhs(std::move(lhs)), rhs(std::move(rhs)) {}
};

// --- Containers ---

struct Function {
    std::string name;
    std::vector<std::unique_ptr<Instruction>> instructions;
};

struct Program {
    std::unique_ptr<Function> function;
};

} // namespace ir

#endif // IR_H
