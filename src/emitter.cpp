#include "emitter.h"
#include <iostream>
#include <stdexcept>
#include <map>
#include <sstream>

using std::ostream;
using std::endl;
using std::string;
using std::map;
using std::runtime_error;

// Helper to convert CondCode enum to assembly string (e.g., E -> e, NE -> ne)
string cond_code_to_string(ir::CondCode cond) {
    switch (cond) {
        case ir::CondCode::E:  return "e";
        case ir::CondCode::NE: return "ne";
        case ir::CondCode::L:  return "l";
        case ir::CondCode::LE: return "le";
        case ir::CondCode::G:  return "g";
        case ir::CondCode::GE: return "ge";
        default: throw runtime_error("Unknown condition code");
    }
}

AssemblyEmitter::AssemblyEmitter(ostream& out) : out(out) {}

void AssemblyEmitter::emit(const ir::Program* program) {
    emit_function(program->function.get());
    out << "    .section .note.GNU-stack,\"\",@progbits" << endl;
}

void AssemblyEmitter::emit_function(const ir::Function* func) {
    out << "    .globl " << func->name << endl;
    out << func->name << ":" << endl;
    
    out << "    push %rbp" << endl;
    out << "    movq %rsp, %rbp" << endl;

    int stack_size = 0;
    
    if (!func->instructions.empty()) {
        if (auto* alloc_inst = dynamic_cast<ir::AllocateStackInstruction*>(func->instructions[0].get())) {
            if(alloc_inst->n > 0) {
                out << "    subq $" << alloc_inst->n << ", %rsp" << endl;
                stack_size = alloc_inst->n;
            }
        }
    }

    for (size_t i = 1; i < func->instructions.size(); ++i) {
        emit_instruction(func->instructions[i].get());
    }
}

void AssemblyEmitter::emit_instruction(const ir::Instruction* inst) {
    if (auto* mov_inst = dynamic_cast<const ir::MovInstruction*>(inst)) {
        // --- FIXUP FOR movl <mem>, <mem> ---
        auto* src_is_stack = dynamic_cast<ir::Stack*>(mov_inst->src.get());
        auto* dst_is_stack = dynamic_cast<ir::Stack*>(mov_inst->dest.get());
        if (src_is_stack && dst_is_stack) {
            out << "    movl ";
            emit_operand(mov_inst->src.get());
            out << ", %r10d" << endl;
            out << "    movl %r10d, ";
            emit_operand(mov_inst->dest.get());
            out << endl;
        } else {
            out << "    movl ";
            emit_operand(mov_inst->src.get());
            out << ", ";
            emit_operand(mov_inst->dest.get());
            out << endl;
        }
    } 
    else if (dynamic_cast<const ir::RetInstruction*>(inst)) {
        // Function Epilogue is handled correctly in emit_function
        out << "    movq %rbp, %rsp" << endl;
        out << "    popq %rbp" << endl;
        out << "    ret" << endl;
    }
    else if (auto* unary_inst = dynamic_cast<const ir::UnaryInstruction*>(inst)) {
        string op_str = (unary_inst->op == ir::UnaryType::NEG) ? "neg" : "not";
        out << "    " << op_str << "l ";
        emit_operand(unary_inst->operand.get());
        out << endl;
    }
    else if (dynamic_cast<const ir::AllocateStackInstruction*>(inst)) {
        // Do nothing, handled in prologue in emit_function
    }
    else if (auto* bin_inst = dynamic_cast<const ir::BinaryInstruction*>(inst)) {
        string op_str;
        switch (bin_inst->op) {
            case ir::BinaryType::ADD: op_str = "addl"; break;
            case ir::BinaryType::SUB: op_str = "subl"; break;
            case ir::BinaryType::MUL: op_str = "imull"; break;
        }
        
        // --- FIXUP FOR <op> <mem>, <mem> ---
        auto* lhs_is_stack = dynamic_cast<ir::Stack*>(bin_inst->lhs.get());
        auto* rhs_is_stack = dynamic_cast<ir::Stack*>(bin_inst->rhs.get());
        auto* lhs_is_imm = dynamic_cast<ir::Immediate*>(bin_inst->lhs.get());
        
        if (lhs_is_stack && rhs_is_stack) {
            out << "    movl ";
            emit_operand(bin_inst->lhs.get());
            out << ", %r10d" << endl;
            out << "    " << op_str << " %r10d, ";
            emit_operand(bin_inst->rhs.get());
            out << endl;
        } else if (lhs_is_imm && rhs_is_stack && bin_inst->op == ir::BinaryType::MUL) {
            // Fixup for `imull <imm>, <mem>`
            out << "    movl ";
            emit_operand(bin_inst->rhs.get());
            out << ", %r11d" << endl;
            out << "    " << op_str << " ";
            emit_operand(bin_inst->lhs.get());
            out << ", %r11d" << endl;
            out << "    movl %r11d, ";
            emit_operand(bin_inst->rhs.get());
            out << endl;
        } else {
            out << "    " << op_str << " ";
            emit_operand(bin_inst->lhs.get());
            out << ", ";
            emit_operand(bin_inst->rhs.get());
            out << endl;
        }
    }
    else if (auto* idiv_inst = dynamic_cast<const ir::IdivInstruction*>(inst)) {
        // --- FIXUP for `idivl <imm>` ---
        if(dynamic_cast<ir::Immediate*>(idiv_inst->divisor.get())) {
            out << "    movl ";
            emit_operand(idiv_inst->divisor.get());
            out << ", %r10d" << endl;
            out << "    idivl %r10d" << endl;
        } else {
            out << "    idivl ";
            emit_operand(idiv_inst->divisor.get());
            out << endl;
        }
    }
    else if (dynamic_cast<const ir::CDQInstruction*>(inst)) {
        out << "    cdq" << endl;
    }
    else if (auto* cmp_inst = dynamic_cast<const ir::CmpInstruction*>(inst)) {
    auto* lhs_is_stack = dynamic_cast<ir::Stack*>(cmp_inst->lhs.get());
    auto* rhs_is_stack = dynamic_cast<ir::Stack*>(cmp_inst->rhs.get());
    auto* lhs_is_imm   = dynamic_cast<ir::Immediate*>(cmp_inst->lhs.get());
    auto* rhs_is_imm   = dynamic_cast<ir::Immediate*>(cmp_inst->rhs.get());

    // Ensure RHS is not immediate (cmpl SRC, DEST) => DEST cannot be immediate
    if (rhs_is_imm) {
        out << "    movl ";
        emit_operand(cmp_inst->rhs.get());
        out << ", %r10d" << std::endl;

        out << "    cmpl ";
        emit_operand(cmp_inst->lhs.get());
        out << ", %r10d" << std::endl;
        return;
    }

    // If LHS is mem or imm AND RHS is mem, move LHS to a reg first (mem/mem invalid)
    if ((lhs_is_stack || lhs_is_imm) && rhs_is_stack) {
        out << "    movl ";
        emit_operand(cmp_inst->lhs.get());
        out << ", %r10d" << std::endl;

        out << "    cmpl %r10d, ";
        emit_operand(cmp_inst->rhs.get());
        out << std::endl;
        return;
    }

    // Otherwise it's already valid (reg or imm vs reg or mem)
    out << "    cmpl ";
    emit_operand(cmp_inst->lhs.get());
    out << ", ";
    emit_operand(cmp_inst->rhs.get());
    out << std::endl;
}

    else if (auto* jmp_inst = dynamic_cast<const ir::JmpInstruction*>(inst)) {
        out << "    jmp " << jmp_inst->target << endl;
    }
    else if (auto* jmpcc_inst = dynamic_cast<const ir::JmpCCInstruction*>(inst)) {
        out << "    j" << cond_code_to_string(jmpcc_inst->cond) << " " << jmpcc_inst->target << endl;
    }
    else if (auto* setcc_inst = dynamic_cast<const ir::SetCCInstruction*>(inst)) {
        out << "    set" << cond_code_to_string(setcc_inst->cond) << " ";
        
        // Per book, SetCC operates on byte registers (%r10b).
        auto* reg = dynamic_cast<ir::Register*>(setcc_inst->dest.get());
        if(reg && reg->name.type == ir::RegType::R10) {
            out << "%r10b";
        } else {
             emit_operand(setcc_inst->dest.get());
        }
        out << endl;
    }
    else if (auto* label_inst = dynamic_cast<const ir::LabelInstruction*>(inst)) {
        out << label_inst->name << ":" << endl;
    }
    else {
        throw runtime_error("Unsupported Assembly IR instruction type");
    }
}

void AssemblyEmitter::emit_operand(const ir::Operand* op) {
    if (auto* imm = dynamic_cast<const ir::Immediate*>(op)) {
        out << "$" << imm->value;
    } 
    else if (auto* reg = dynamic_cast<const ir::Register*>(op)) {
        switch (reg->name.type) {
            case ir::RegType::AX:  out << "%eax";  break;
            case ir::RegType::DX:  out << "%edx";  break;
            case ir::RegType::R10: out << "%r10d"; break;
            case ir::RegType::R11: out << "%r11d"; break;
            default: throw runtime_error("Emitter error: Unknown RegType.");
        }
    }
    else if (auto* stack = dynamic_cast<const ir::Stack*>(op)) {
        out << stack->val << "(%rbp)";
    }
    else if (auto* pseudo = dynamic_cast<const ir::Pseudo*>(op)) {
         throw runtime_error("Emitter error: Found a Pseudo-register that was not replaced.");
    }
    else {
        throw runtime_error("Unsupported Assembly IR operand type");
    }
}