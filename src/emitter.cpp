#include "emitter.h"
#include <iostream>

using std::cout;
using std::endl;
using std::ostream;

AssemblyEmitter::AssemblyEmitter(ostream& out) : out(out) {}

void AssemblyEmitter::emit(const ir::Program* program) {
    emit_function(program->function.get());
    out << "    .section .note.GNU-stack,\"\",@progbits" << endl;
}

void AssemblyEmitter::emit_function(const ir::Function* func) {
    out << "    .globl " << func->name << endl;
    out << func->name << ":" << endl;
    for (const auto& inst : func->instructions) {
        emit_instruction(inst.get());
    }
}

void AssemblyEmitter::emit_instruction(const ir::Instruction* inst) {
    if (auto* mov_inst = dynamic_cast<const ir::MovInstruction*>(inst)) {
        out << "    movl ";
        emit_operand(mov_inst->src.get());
        out << ", ";
        emit_operand(mov_inst->dest.get());
        out << endl;
    } else if (dynamic_cast<const ir::RetInstruction*>(inst)) {
        out << "    ret" << endl;
    }
}

void AssemblyEmitter::emit_operand(const ir::Operand* op) {
    if (auto* imm = dynamic_cast<const ir::Immediate*>(op)) {
        out << "$" << imm->value;
    } else if (auto* reg = dynamic_cast<const ir::Register*>(op)) {
        out << reg->name;
    }
}