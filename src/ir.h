#ifndef IR_H
#define IR_H

#include <string>
#include <vector>
#include <memory>
#include <utility>


namespace ir {



enum class UnaryType {
    NOT,
    NEG
};

enum class BinaryType {
    ADD,
    SUB,
    MUL,
    
    ADDSD,
    SUBSD,
    MULSD,
    DIVSD
};


enum class CondCode {
    E,  
    NE, 
    L,  
    LE, 
    G,  
    GE,
    A,
    AE,
    B,
    BE  

};

enum class RegType {
    AX,
    DX,
    R10,
    R11,
    
    XMM0,
    XMM1,
    XMM2,
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7
};



struct Reg {
    RegType type;
    explicit Reg(RegType type) : type(type) {}
};



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
    int size; 
    explicit Stack(int val, int size = 4) : val(val), size(size) {}
};



struct Instruction {
    virtual ~Instruction() = default;
};

struct MovInstruction : public Instruction {
    std::unique_ptr<Operand> src;
    std::unique_ptr<Operand> dest;
    
    MovInstruction(std::unique_ptr<Operand> src, std::unique_ptr<Operand> dest) 
        : src(std::move(src)), dest(std::move(dest)) {}
};



struct MovSDInstruction : public Instruction {
    std::unique_ptr<Operand> src;
    std::unique_ptr<Operand> dest;
    MovSDInstruction(std::unique_ptr<Operand> src, std::unique_ptr<Operand> dest)
        : src(std::move(src)), dest(std::move(dest)) {}
};

struct RetInstruction : public Instruction {};


struct UnaryInstruction : public Instruction {
    UnaryType op; 
    std::unique_ptr<Operand> operand; 

    UnaryInstruction(UnaryType op, std::unique_ptr<Operand> operand)
        : op(op), operand(std::move(operand)) {}
};


struct BinaryInstruction : public Instruction {
    BinaryType op;
    std::unique_ptr<Operand> lhs;
    std::unique_ptr<Operand> rhs;

    BinaryInstruction(BinaryType op, std::unique_ptr<Operand> lhs, std::unique_ptr<Operand> rhs)
        : op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}
};







struct CvtSI2SDInstruction : public Instruction {

    std::unique_ptr<Operand> src;  

    std::unique_ptr<Operand> dest; 

    CvtSI2SDInstruction(std::unique_ptr<Operand> src, std::unique_ptr<Operand> dest)

        : src(std::move(src)), dest(std::move(dest)) {}

};

struct IdivInstruction : public Instruction {
    std::unique_ptr<Operand> divisor;
    explicit IdivInstruction(std::unique_ptr<Operand> divisor) : divisor(std::move(divisor)) {}
};

struct CDQInstruction : public Instruction {};


struct AllocateStackInstruction : public Instruction {
    int n;
    explicit AllocateStackInstruction(int n) : n(n) {}
}; 



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
    std::unique_ptr<Operand> dest; 
    explicit SetCCInstruction(CondCode cond, std::unique_ptr<Operand> dest)
        : cond(cond), dest(std::move(dest)) {}
};

struct LabelInstruction : public Instruction {
    std::string name;
    explicit LabelInstruction(std::string name) : name(std::move(name)) {}
};



struct CmpSDInstruction : public Instruction {
    std::unique_ptr<Operand> lhs;
    std::unique_ptr<Operand> rhs;
    CmpSDInstruction(std::unique_ptr<Operand> lhs, std::unique_ptr<Operand> rhs)
        : lhs(std::move(lhs)), rhs(std::move(rhs)) {}
};



struct Function {
    std::string name;
    std::vector<std::unique_ptr<Instruction>> instructions;
};

struct Program {
    std::unique_ptr<Function> function;
};

} 

#endif 
