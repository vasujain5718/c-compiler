#include "emitter.h"
#include <iostream>
#include <stdexcept>
#include <map>
#include <sstream>
#include <set>

using std::endl;
using std::map;
using std::ostream;
using std::runtime_error;
using std::string;

string cond_code_to_string(ir::CondCode cond)
{
    switch (cond)
    {
    case ir::CondCode::E:
        return "e";
    case ir::CondCode::NE:
        return "ne";
    case ir::CondCode::L:
        return "l";
    case ir::CondCode::LE:
        return "le";
    case ir::CondCode::G:
        return "g";
    case ir::CondCode::GE:
        return "ge";
    case ir::CondCode::A:
        return "a";
    case ir::CondCode::AE:
        return "ae";
    case ir::CondCode::B:
        return "b";
    case ir::CondCode::BE:
        return "be";

    default:
        throw runtime_error("Unknown condition code");
    }
}

AssemblyEmitter::AssemblyEmitter(ostream &out) : out(out) {}

static std::map<std::string, std::string> float_constant_pool;
static int float_const_counter = 0;

static std::string intern_float_constant(const std::string &lit)
{
    auto it = float_constant_pool.find(lit);
    if (it != float_constant_pool.end())
        return it->second;
    std::string lbl = ".LCf" + std::to_string(float_const_counter++);
    float_constant_pool[lit] = lbl;
    return lbl;
}

void AssemblyEmitter::emit(const ir::Program *program)
{

    float_constant_pool.clear();
    float_const_counter = 0;

    emit_function(program->function.get());

    if (!float_constant_pool.empty())
    {
        out << "    .section .rodata" << endl;
        for (const auto &kv : float_constant_pool)
        {
            const string &lit = kv.first;
            const string &lbl = kv.second;

            out << lbl << ":" << endl;
            out << "    .double " << lit << endl;
        }
    }

    out << "    .section .note.GNU-stack,\"\",@progbits" << endl;
}

void AssemblyEmitter::emit_function(const ir::Function *func)
{
    out << "    .globl " << func->name << endl;
    out << func->name << ":" << endl;

    out << "    push %rbp" << endl;
    out << "    movq %rsp, %rbp" << endl;

    int stack_size = 0;

    if (!func->instructions.empty())
    {
        if (auto *alloc_inst = dynamic_cast<ir::AllocateStackInstruction *>(func->instructions[0].get()))
        {
            if (alloc_inst->n > 0)
            {
                out << "    subq $" << alloc_inst->n << ", %rsp" << endl;
                stack_size = alloc_inst->n;
            }
        }
    }

    for (size_t i = 1; i < func->instructions.size(); ++i)
    {
        emit_instruction(func->instructions[i].get());
    }

    out << "    movq %rbp, %rsp" << std::endl;
    out << "    popq %rbp" << std::endl;
    out << "    ret" << std::endl;
}

void AssemblyEmitter::emit_instruction(const ir::Instruction *inst)
{
    if (auto *movsd_inst = dynamic_cast<const ir::MovSDInstruction *>(inst))
    {

        auto *src_imm = dynamic_cast<const ir::Immediate *>(movsd_inst->src.get());
        auto *src_reg = dynamic_cast<const ir::Register *>(movsd_inst->src.get());
        auto *src_stack = dynamic_cast<const ir::Stack *>(movsd_inst->src.get());

        auto *dst_reg = dynamic_cast<const ir::Register *>(movsd_inst->dest.get());
        auto *dst_stack = dynamic_cast<const ir::Stack *>(movsd_inst->dest.get());

        if (src_imm)

        {

            std::string lbl = intern_float_constant(src_imm->value);

            if (dst_reg)

            {

                out << "    movsd " << lbl << "(%rip), ";

                emit_operand(dst_reg);

                out << endl;
            }

            else if (dst_stack)

            {

                out << "    movsd " << lbl << "(%rip), %xmm7" << endl;

                out << "    movsd %xmm7, " << dst_stack->val << "(%rbp)" << endl;
            }

            else

            {

                throw runtime_error("Emitter Error: movsd immediate destination unsupported.");
            }

            return;
        }

        if (src_reg && dst_reg)
        {

            auto srcname = [&]()
            {
                switch (src_reg->name.type)
                {
                case ir::RegType::XMM0:
                    return string("%xmm0");
                case ir::RegType::XMM1:
                    return string("%xmm1");
                case ir::RegType::XMM2:
                    return string("%xmm2");
                case ir::RegType::XMM3:
                    return string("%xmm3");
                case ir::RegType::XMM4:
                    return string("%xmm4");
                case ir::RegType::XMM5:
                    return string("%xmm5");
                case ir::RegType::XMM6:
                    return string("%xmm6");
                case ir::RegType::XMM7:
                    return string("%xmm7");
                default:
                    throw runtime_error("Emitter error: unknown XMM reg");
                }
            }();
            auto dstname = [&]()
            {
                switch (dst_reg->name.type)
                {
                case ir::RegType::XMM0:
                    return string("%xmm0");
                case ir::RegType::XMM1:
                    return string("%xmm1");
                case ir::RegType::XMM2:
                    return string("%xmm2");
                case ir::RegType::XMM3:
                    return string("%xmm3");
                case ir::RegType::XMM4:
                    return string("%xmm4");
                case ir::RegType::XMM5:
                    return string("%xmm5");
                case ir::RegType::XMM6:
                    return string("%xmm6");
                case ir::RegType::XMM7:
                    return string("%xmm7");
                default:
                    throw runtime_error("Emitter error: unknown XMM reg");
                }
            }();
            out << "    movsd " << srcname << ", " << dstname << endl;
            return;
        }

        if (src_stack && dst_reg)
        {
            out << "    movsd " << src_stack->val << "(%rbp), ";
            switch (dst_reg->name.type)
            {
            case ir::RegType::XMM0:
                out << "%xmm0";
                break;
            case ir::RegType::XMM1:
                out << "%xmm1";
                break;
            case ir::RegType::XMM2:
                out << "%xmm2";
                break;
            case ir::RegType::XMM3:
                out << "%xmm3";
                break;
            case ir::RegType::XMM4:
                out << "%xmm4";
                break;
            case ir::RegType::XMM5:
                out << "%xmm5";
                break;
            case ir::RegType::XMM6:
                out << "%xmm6";
                break;
            case ir::RegType::XMM7:
                out << "%xmm7";
                break;
            default:
                throw runtime_error("Emitter error: unknown XMM reg");
            }
            out << endl;
            return;
        }

        if (src_reg && dst_stack)
        {
            string srcname;
            switch (src_reg->name.type)
            {
            case ir::RegType::XMM0:
                srcname = "%xmm0";
                break;
            case ir::RegType::XMM1:
                srcname = "%xmm1";
                break;
            case ir::RegType::XMM2:
                srcname = "%xmm2";
                break;
            case ir::RegType::XMM3:
                srcname = "%xmm3";
                break;
            case ir::RegType::XMM4:
                srcname = "%xmm4";
                break;
            case ir::RegType::XMM5:
                srcname = "%xmm5";
                break;
            case ir::RegType::XMM6:
                srcname = "%xmm6";
                break;
            case ir::RegType::XMM7:
                srcname = "%xmm7";
                break;
            default:
                throw runtime_error("Emitter error: unknown XMM reg");
            }
            out << "    movsd " << srcname << ", " << dst_stack->val << "(%rbp)" << endl;
            return;
        }

        if (src_stack && dst_stack)
        {
            out << "    movsd " << src_stack->val << "(%rbp), %xmm7" << endl;
            out << "    movsd %xmm7, " << dst_stack->val << "(%rbp)" << endl;
            return;
        }

        throw runtime_error("Emitter Error: Unsupported movsd operand pattern");
    }

    else if (auto *cvtsi_inst = dynamic_cast<const ir::CvtSI2SDInstruction *>(inst))

    {

        out << "    cvtsi2sd ";

        emit_operand(cvtsi_inst->src.get());

        out << ", ";

        emit_operand(cvtsi_inst->dest.get());

        out << endl;
    }
    else if (auto *mov_inst = dynamic_cast<const ir::MovInstruction *>(inst))
    {

        auto *src_is_stack = dynamic_cast<ir::Stack *>(mov_inst->src.get());
        auto *dst_is_stack = dynamic_cast<ir::Stack *>(mov_inst->dest.get());

        if (src_is_stack && dst_is_stack)
        {
            out << "    movl ";
            emit_operand(mov_inst->src.get());
            out << ", %r10d" << endl;
            out << "    movl %r10d, ";
            emit_operand(mov_inst->dest.get());
            out << endl;
        }
        else
        {
            out << "    movl ";
            emit_operand(mov_inst->src.get());
            out << ", ";
            emit_operand(mov_inst->dest.get());
            out << endl;
        }
    }

    else if (dynamic_cast<const ir::RetInstruction *>(inst))
    {

        out << "    movq %rbp, %rsp" << endl;
        out << "    popq %rbp" << endl;
        out << "    ret" << endl;
    }
    else if (auto *unary_inst = dynamic_cast<const ir::UnaryInstruction *>(inst))
    {
        string op_str = (unary_inst->op == ir::UnaryType::NEG) ? "neg" : "not";
        out << "    " << op_str << "l ";
        emit_operand(unary_inst->operand.get());
        out << endl;
    }
    else if (dynamic_cast<const ir::AllocateStackInstruction *>(inst))
    {
    }
    else if (auto *bin_inst = dynamic_cast<const ir::BinaryInstruction *>(inst))
    {

        if (bin_inst->op == ir::BinaryType::ADDSD ||
            bin_inst->op == ir::BinaryType::SUBSD ||
            bin_inst->op == ir::BinaryType::MULSD ||
            bin_inst->op == ir::BinaryType::DIVSD)
        {

            string op_str;
            if (bin_inst->op == ir::BinaryType::ADDSD)
                op_str = "addsd";
            else if (bin_inst->op == ir::BinaryType::SUBSD)
                op_str = "subsd";
            else if (bin_inst->op == ir::BinaryType::MULSD)
                op_str = "mulsd";
            else if (bin_inst->op == ir::BinaryType::DIVSD)
                op_str = "divsd";

            out << "    " << op_str << " ";

            emit_operand(bin_inst->lhs.get());
            out << ", ";
            emit_operand(bin_inst->rhs.get());
            out << endl;
            return;
        }

        string op_str;
        switch (bin_inst->op)
        {
        case ir::BinaryType::ADD:
            op_str = "addl";
            break;
        case ir::BinaryType::SUB:
            op_str = "subl";
            break;
        case ir::BinaryType::MUL:
            op_str = "imull";
            break;
        default:
            throw runtime_error("Emitter: Unsupported binary op kind");
        }

        auto *lhs_is_stack = dynamic_cast<ir::Stack *>(bin_inst->lhs.get());
        auto *rhs_is_stack = dynamic_cast<ir::Stack *>(bin_inst->rhs.get());
        auto *lhs_is_imm = dynamic_cast<ir::Immediate *>(bin_inst->lhs.get());

        if (lhs_is_stack && rhs_is_stack)
        {
            out << "    movl ";
            emit_operand(bin_inst->lhs.get());
            out << ", %r10d" << endl;
            out << "    " << op_str << " %r10d, ";
            emit_operand(bin_inst->rhs.get());
            out << endl;
        }
        else if (lhs_is_imm && rhs_is_stack && bin_inst->op == ir::BinaryType::MUL)
        {

            out << "    movl ";
            emit_operand(bin_inst->rhs.get());
            out << ", %r11d" << endl;
            out << "    " << op_str << " ";
            emit_operand(bin_inst->lhs.get());
            out << ", %r11d" << endl;
            out << "    movl %r11d, ";
            emit_operand(bin_inst->rhs.get());
            out << endl;
        }
        else
        {
            out << "    " << op_str << " ";
            emit_operand(bin_inst->lhs.get());
            out << ", ";
            emit_operand(bin_inst->rhs.get());
            out << endl;
        }
    }
    else if (auto *idiv_inst = dynamic_cast<const ir::IdivInstruction *>(inst))
    {

        if (dynamic_cast<ir::Immediate *>(idiv_inst->divisor.get()))
        {
            out << "    movl ";
            emit_operand(idiv_inst->divisor.get());
            out << ", %r10d" << endl;
            out << "    idivl %r10d" << endl;
        }
        else
        {
            out << "    idivl ";
            emit_operand(idiv_inst->divisor.get());
            out << endl;
        }
    }
    else if (dynamic_cast<const ir::CDQInstruction *>(inst))
    {
        out << "    cdq" << endl;
    }
    else if (auto *cmp_inst = dynamic_cast<const ir::CmpInstruction *>(inst))
    {
        auto *lhs_is_stack = dynamic_cast<ir::Stack *>(cmp_inst->lhs.get());
        auto *rhs_is_stack = dynamic_cast<ir::Stack *>(cmp_inst->rhs.get());
        auto *lhs_is_imm = dynamic_cast<ir::Immediate *>(cmp_inst->lhs.get());
        auto *rhs_is_imm = dynamic_cast<ir::Immediate *>(cmp_inst->rhs.get());

        if (rhs_is_imm)
        {
            out << "    movl ";
            emit_operand(cmp_inst->rhs.get());
            out << ", %r10d" << std::endl;

            out << "    cmpl ";
            emit_operand(cmp_inst->lhs.get());
            out << ", %r10d" << std::endl;
            return;
        }

        if ((lhs_is_stack || lhs_is_imm) && rhs_is_stack)
        {
            out << "    movl ";
            emit_operand(cmp_inst->lhs.get());
            out << ", %r10d" << std::endl;

            out << "    cmpl %r10d, ";
            emit_operand(cmp_inst->rhs.get());
            out << std::endl;
            return;
        }

        out << "    cmpl ";
        emit_operand(cmp_inst->lhs.get());
        out << ", ";
        emit_operand(cmp_inst->rhs.get());
        out << std::endl;
    }

    else if (auto *cmpsd_inst = dynamic_cast<const ir::CmpSDInstruction *>(inst))
    {

        auto *lhs_stack = dynamic_cast<const ir::Stack *>(cmpsd_inst->lhs.get());
        auto *rhs_stack = dynamic_cast<const ir::Stack *>(cmpsd_inst->rhs.get());
        auto *lhs_reg = dynamic_cast<const ir::Register *>(cmpsd_inst->lhs.get());
        auto *rhs_reg = dynamic_cast<const ir::Register *>(cmpsd_inst->rhs.get());

        if (lhs_stack)
        {
            out << "    movsd " << lhs_stack->val << "(%rbp), %xmm1" << endl;
            if (rhs_stack)
            {
                out << "    movsd " << rhs_stack->val << "(%rbp), %xmm0" << endl;
                out << "    ucomisd %xmm0, %xmm1" << endl;
            }
            else if (rhs_reg)
            {

                string rname;
                switch (rhs_reg->name.type)
                {
                case ir::RegType::XMM0:
                    rname = "%xmm0";
                    break;
                case ir::RegType::XMM1:
                    rname = "%xmm1";
                    break;
                case ir::RegType::XMM2:
                    rname = "%xmm2";
                    break;
                case ir::RegType::XMM3:
                    rname = "%xmm3";
                    break;
                case ir::RegType::XMM4:
                    rname = "%xmm4";
                    break;
                case ir::RegType::XMM5:
                    rname = "%xmm5";
                    break;
                case ir::RegType::XMM6:
                    rname = "%xmm6";
                    break;
                case ir::RegType::XMM7:
                    rname = "%xmm7";
                    break;
                default:
                    throw runtime_error("Emitter: unknown XMM reg for cmpsd rhs");
                }
                out << "    ucomisd " << rname << ", %xmm1" << endl;
            }
            else
            {
                throw runtime_error("Emitter: Unsupported CmpSD RHS operand");
            }
        }
        else if (lhs_reg)
        {
            string lname;
            switch (lhs_reg->name.type)
            {
            case ir::RegType::XMM0:
                lname = "%xmm0";
                break;
            case ir::RegType::XMM1:
                lname = "%xmm1";
                break;
            case ir::RegType::XMM2:
                lname = "%xmm2";
                break;
            case ir::RegType::XMM3:
                lname = "%xmm3";
                break;
            case ir::RegType::XMM4:
                lname = "%xmm4";
                break;
            case ir::RegType::XMM5:
                lname = "%xmm5";
                break;
            case ir::RegType::XMM6:
                lname = "%xmm6";
                break;
            case ir::RegType::XMM7:
                lname = "%xmm7";
                break;
            default:
                throw runtime_error("Emitter: unknown XMM reg for cmpsd lhs");
            }
            if (rhs_stack)
            {
                out << "    movsd " << rhs_stack->val << "(%rbp), %xmm1" << endl;
                out << "    ucomisd %xmm1, " << lname << endl;
            }
            else if (rhs_reg)
            {
                string rname;
                switch (rhs_reg->name.type)
                {
                case ir::RegType::XMM0:
                    rname = "%xmm0";
                    break;
                case ir::RegType::XMM1:
                    rname = "%xmm1";
                    break;
                case ir::RegType::XMM2:
                    rname = "%xmm2";
                    break;
                case ir::RegType::XMM3:
                    rname = "%xmm3";
                    break;
                case ir::RegType::XMM4:
                    rname = "%xmm4";
                    break;
                case ir::RegType::XMM5:
                    rname = "%xmm5";
                    break;
                case ir::RegType::XMM6:
                    rname = "%xmm6";
                    break;
                case ir::RegType::XMM7:
                    rname = "%xmm7";
                    break;
                default:
                    throw runtime_error("Emitter: unknown XMM reg for cmpsd rhs");
                }
                out << "    ucomisd " << rname << ", " << lname << endl;
            }
            else
            {
                throw runtime_error("Emitter: Unsupported CmpSD RHS operand");
            }
        }
        else
        {
            throw runtime_error("Emitter: Unsupported CmpSD operand pattern");
        }
    }

    else if (auto *jmp_inst = dynamic_cast<const ir::JmpInstruction *>(inst))
    {
        out << "    jmp " << jmp_inst->target << endl;
    }
    else if (auto *jmpcc_inst = dynamic_cast<const ir::JmpCCInstruction *>(inst))
    {
        out << "    j" << cond_code_to_string(jmpcc_inst->cond) << " " << jmpcc_inst->target << endl;
    }
    else if (auto *setcc_inst = dynamic_cast<const ir::SetCCInstruction *>(inst))
    {
        out << "    set" << cond_code_to_string(setcc_inst->cond) << " ";

        auto *reg = dynamic_cast<ir::Register *>(setcc_inst->dest.get());
        if (reg && reg->name.type == ir::RegType::R10)
        {
            out << "%r10b";
        }
        else
        {
            emit_operand(setcc_inst->dest.get());
        }
        out << endl;
    }
    else if (auto *label_inst = dynamic_cast<const ir::LabelInstruction *>(inst))
    {
        out << label_inst->name << ":" << endl;
    }
    else
    {
        throw runtime_error("Unsupported Assembly IR instruction type");
    }
}

void AssemblyEmitter::emit_operand(const ir::Operand *op)
{
    if (auto *imm = dynamic_cast<const ir::Immediate *>(op))
    {

        if (imm->value.find('.') != std::string::npos || imm->value.find('e') != std::string::npos || imm->value.find('E') != std::string::npos)
        {
            std::string lbl = intern_float_constant(imm->value);
            out << lbl << "(%rip)";
            return;
        }

        out << "$" << imm->value;
    }

    else if (auto *reg = dynamic_cast<const ir::Register *>(op))
    {
        switch (reg->name.type)
        {
        case ir::RegType::AX:
            out << "%eax";
            break;
        case ir::RegType::DX:
            out << "%edx";
            break;
        case ir::RegType::R10:
            out << "%r10d";
            break;
        case ir::RegType::R11:
            out << "%r11d";
            break;
        case ir::RegType::XMM0:
            out << "%xmm0";
            break;
        case ir::RegType::XMM1:
            out << "%xmm1";
            break;
        case ir::RegType::XMM2:
            out << "%xmm2";
            break;
        case ir::RegType::XMM3:
            out << "%xmm3";
            break;
        case ir::RegType::XMM4:
            out << "%xmm4";
            break;
        case ir::RegType::XMM5:
            out << "%xmm5";
            break;
        case ir::RegType::XMM6:
            out << "%xmm6";
            break;
        case ir::RegType::XMM7:
            out << "%xmm7";
            break;
        default:
            throw runtime_error("Emitter error: Unknown RegType.");
        }
    }
    else if (auto *stack = dynamic_cast<const ir::Stack *>(op))
    {
        out << stack->val << "(%rbp)";
    }
    else if (auto *pseudo = dynamic_cast<const ir::Pseudo *>(op))
    {
        throw runtime_error("Emitter error: Found a Pseudo-register that was not replaced.");
    }
    else
    {
        throw runtime_error("Unsupported Assembly IR operand type");
    }
}
