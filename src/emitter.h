#ifndef EMITTER_H
#define EMITTER_H

#include "ir.h"
#include <ostream> // Include for std::ostream

class AssemblyEmitter {
public:
    // The constructor now takes a reference to any output stream
    explicit AssemblyEmitter(std::ostream& out);

    void emit(const ir::Program* program);

private:
    // This will hold our output destination (e.g., std::cout or a file stream)
    std::ostream& out;

    void emit_function(const ir::Function* func);
    void emit_instruction(const ir::Instruction* inst);
    void emit_operand(const ir::Operand* op);
};

#endif