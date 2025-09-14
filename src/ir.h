#ifndef IR_H
#define IR_H

#include <string>
#include <vector>
#include <memory>
#include <utility>

namespace ir {

struct Operand {
    virtual ~Operand() = default;
};

struct Register : public Operand {
    std::string name;
    explicit Register(std::string name) : name(std::move(name)) {}
};

struct Immediate : public Operand {
    std::string value;
    explicit Immediate(std::string value) : value(std::move(value)) {}
};

struct Instruction {
    virtual ~Instruction() = default;
};

struct MovInstruction : public Instruction {
    std::unique_ptr<Operand> src;
    std::unique_ptr<Operand> dest;
    MovInstruction(std::unique_ptr<Operand> src, std::unique_ptr<Operand> dest) : src(std::move(src)), dest(std::move(dest)) {}
};

struct RetInstruction : public Instruction {};

struct Function {
    std::string name;
    std::vector<std::unique_ptr<Instruction>> instructions;
};

struct Program {
    std::unique_ptr<Function> function;
};

}

#endif