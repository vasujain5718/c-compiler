#ifndef EMITTER_H
#define EMITTER_H

#include "ir.h"
#include <ostream>
#include <string>

// Using declarations for convenience
using std::string;
using std::ostream;

class AssemblyEmitter {
public:
    explicit AssemblyEmitter(ostream& out);
    void emit(const ir::Program* program);

private:
    ostream& out;
    void emit_function(const ir::Function* func);
    void emit_instruction(const ir::Instruction* inst);
    void emit_operand(const ir::Operand* op);
};

#endif // EMITTER_H