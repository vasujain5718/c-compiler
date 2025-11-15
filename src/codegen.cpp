#include "codegen.h"
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <type_traits>

using std::make_unique;
using std::map;
using std::move;
using std::runtime_error;
using std::string;
using std::unique_ptr;

// Helper to safely clone a stack operand
unique_ptr<ir::Stack> clone_stack_op(const ir::Operand *op)
{
    auto *stack_op = dynamic_cast<const ir::Stack *>(op);
    if (!stack_op)
    {
        throw runtime_error("Expected a stack operand.");
    }
    return make_unique<ir::Stack>(stack_op->val, stack_op->size);
}

// Helper: get storage size (bytes) for a tacky value kind
static int size_of_tacky_kind(tacky::TypeKind kind)
{
    switch (kind)
    {
    case tacky::TypeKind::Double:
        return 8;
    case tacky::TypeKind::Float:
        return 8; // promote float to 8 bytes in stack to simplify lowering
    case tacky::TypeKind::Int:
    default:
        return 4;
    }
}

unique_ptr<ir::Program> CodeGenerator::generate(const tacky::Program *tacky_prog)
{
    auto ir_prog = make_unique<ir::Program>();
    ir_prog->function = make_unique<ir::Function>();

    stack_map.clear();
    next_stack_offset = 0;

    generate_function(tacky_prog->function.get(), ir_prog->function.get());

    return ir_prog;
}

void CodeGenerator::generate_function(const tacky::Function *tacky_func, ir::Function *ir_func)
{
    ir_func->name = tacky_func->name;

    auto alloc_inst = new ir::AllocateStackInstruction(0);
    ir_func->instructions.push_back(unique_ptr<ir::Instruction>(alloc_inst));

    for (const auto &inst : tacky_func->body)
    {
        generate_instruction(inst.get(), ir_func);
    }

    alloc_inst->n = -next_stack_offset;
}

void CodeGenerator::generate_instruction(const tacky::Instruction *tacky_inst, ir::Function *ir_func)
{

    // --- Return ---
    if (auto *ret_inst = dynamic_cast<const tacky::ReturnInstruction *>(tacky_inst))
    {
        auto val = translate_val(ret_inst->val.get());

        // If the value is a stack slot of size 8 (double/float), or an immediate that looks like a float,
        // emit a MovSD to %xmm0 (floating-point return). Otherwise use integer return in AX.
        if (auto *s = dynamic_cast<ir::Stack *>(val.get()))
        {
            if (s->size == 8)
            {
                auto reg_xmm0 = make_unique<ir::Register>(ir::Reg(ir::RegType::XMM0));
                ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(move(val), move(reg_xmm0)));
                ir_func->instructions.push_back(make_unique<ir::RetInstruction>());
                return;
            }
        }
        if (auto *imm = dynamic_cast<ir::Immediate *>(val.get()))
        {
            if (imm->value.find('.') != std::string::npos ||
                imm->value.find('e') != std::string::npos ||
                imm->value.find('E') != std::string::npos)
            {
                auto reg_xmm0 = make_unique<ir::Register>(ir::Reg(ir::RegType::XMM0));
                ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(move(val), move(reg_xmm0)));
                ir_func->instructions.push_back(make_unique<ir::RetInstruction>());
                return;
            }
        }

        // integer fallback: return in AX
        auto reg_ax = make_unique<ir::Register>(ir::Reg(ir::RegType::AX));
        ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(val), move(reg_ax)));
        ir_func->instructions.push_back(make_unique<ir::RetInstruction>());
        return;
    }

    // --- Unary ---
    if (auto *unary_inst = dynamic_cast<const tacky::UnaryInstruction *>(tacky_inst))
    {
        auto src = translate_val(unary_inst->src.get());
        auto dst = translate_val(unary_inst->dst.get());
        auto dst_clone = clone_stack_op(dst.get());

        if (unary_inst->op == tacky::UnaryOperatorType::LogicalNot)
        {
            // Implement !x as (x == 0) with setcc
            if (dynamic_cast<ir::Stack *>(src.get()))
            {
                ir_func->instructions.push_back(make_unique<ir::MovInstruction>(
                    make_unique<ir::Immediate>("0"),
                    make_unique<ir::Register>(ir::Reg(ir::RegType::R10))));
                ir_func->instructions.push_back(make_unique<ir::CmpInstruction>(
                    make_unique<ir::Register>(ir::Reg(ir::RegType::R10)),
                    move(src)));
            }
            else
            {
                ir_func->instructions.push_back(make_unique<ir::CmpInstruction>(
                    make_unique<ir::Immediate>("0"),
                    move(src)));
            }

            // zero dst then setcc into r10b and move into dst
            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(
                make_unique<ir::Immediate>("0"),
                move(dst)));
            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(
                make_unique<ir::Immediate>("0"),
                make_unique<ir::Register>(ir::Reg(ir::RegType::R10))));
            ir_func->instructions.push_back(make_unique<ir::SetCCInstruction>(
                ir::CondCode::E,
                make_unique<ir::Register>(ir::Reg(ir::RegType::R10))));
            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(
                make_unique<ir::Register>(ir::Reg(ir::RegType::R10)),
                move(dst_clone)));
            return;
        }

        if (unary_inst->op == tacky::UnaryOperatorType::Negate)

        {

            // Check if this is a floating-point operation

            bool is_fp = false;

            if (auto *s = dynamic_cast<ir::Stack *>(dst.get()))
            {

                if (s->size == 8)
                    is_fp = true;
            }

            // Also check if src is a float-like immediate (e.g., -10.0)

            if (auto *imm = dynamic_cast<ir::Immediate *>(src.get()))
            {

                if (imm->value.find('.') != std::string::npos ||

                    imm->value.find('e') != std::string::npos ||

                    imm->value.find('E') != std::string::npos)
                {

                    is_fp = true;
                }
            }

            if (is_fp)

            {

                // Implement FP negate as 0.0 - src

                // 1. Load 0.0 into %xmm1

                auto imm_zero = make_unique<ir::Immediate>("0.0");

                auto reg_xmm1 = make_unique<ir::Register>(ir::Reg(ir::RegType::XMM1));

                ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(move(imm_zero), move(reg_xmm1)));

                // 2. Load src (e.g., "10.0" or var b) into %xmm0

                auto reg_xmm0 = make_unique<ir::Register>(ir::Reg(ir::RegType::XMM0));

                ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(move(src), move(reg_xmm0)));

                // 3. SUBSD %xmm0, %xmm1 (calculates %xmm1 = %xmm1 - %xmm0, which is 0.0 - src)

                ir_func->instructions.push_back(make_unique<ir::BinaryInstruction>(

                    ir::BinaryType::SUBSD,

                    make_unique<ir::Register>(ir::Reg(ir::RegType::XMM0)), // src

                    make_unique<ir::Register>(ir::Reg(ir::RegType::XMM1)) // dest (%xmm1 holds 0.0)

                    ));

                // 4. Move result from %xmm1 into dst

                ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(

                    make_unique<ir::Register>(ir::Reg(ir::RegType::XMM1)),

                    move(dst)

                        ));

                return; // We are done with this instruction
            }
        }

        // --- END FIX ---

        // --- Fallback for Integer Negate or Bitwise NOT ---

        // (This is the original code, now it only handles int-negate or not)

        // Move src into dst (or via reg if mem->mem)

        if (dynamic_cast<ir::Stack *>(src.get()))

        {

            auto reg_r10_src = make_unique<ir::Register>(ir::Reg(ir::RegType::R10));

            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(src), move(reg_r10_src)));

            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(make_unique<ir::Register>(ir::Reg(ir::RegType::R10)), move(dst)));
        }

        else

        {

            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(src), move(dst)));
        }

        ir::UnaryType op_type = (unary_inst->op == tacky::UnaryOperatorType::Negate) ? ir::UnaryType::NEG : ir::UnaryType::NOT;

        ir_func->instructions.push_back(make_unique<ir::UnaryInstruction>(op_type, move(dst_clone)));

        return;
    }

    // --- Binary ---
    if (auto *binary_inst = dynamic_cast<const tacky::BinaryInstruction *>(tacky_inst))
    {

        // translate values once
        auto src1 = translate_val(binary_inst->src1.get());
        auto src2 = translate_val(binary_inst->src2.get());
        auto dst = translate_val(binary_inst->dst.get());

        // clones for memory destinations
        auto dst_clone1 = clone_stack_op(dst.get());
        auto dst_clone2 = clone_stack_op(dst.get());

        // simple helpers
        auto is_stack = [](const ir::Operand *o)
        { return dynamic_cast<const ir::Stack *>(o) != nullptr; };
        auto is_imm = [](const ir::Operand *o)
        { return dynamic_cast<const ir::Immediate *>(o) != nullptr; };
        auto is_xmm_reg = [](const ir::Operand *o)
        {
            if (auto *r = dynamic_cast<const ir::Register *>(o))
            {
                return (r->name.type == ir::RegType::XMM0 || r->name.type == ir::RegType::XMM1 ||
                        r->name.type == ir::RegType::XMM2 || r->name.type == ir::RegType::XMM3 ||
                        r->name.type == ir::RegType::XMM4 || r->name.type == ir::RegType::XMM5 ||
                        r->name.type == ir::RegType::XMM6 || r->name.type == ir::RegType::XMM7);
            }
            return false;
        };

        // detect FP-ish operand (stack size 8 or float-like immediate or XMM register)
        auto is_fp_operand = [&](const ir::Operand *o) -> bool
        {
            if (!o)
                return false;
            if (auto *s = dynamic_cast<const ir::Stack *>(o))
                return s->size == 8;
            if (auto *imm = dynamic_cast<const ir::Immediate *>(o))
            {
                return imm->value.find('.') != std::string::npos ||
                       imm->value.find('e') != std::string::npos ||
                       imm->value.find('E') != std::string::npos;
            }
            return is_xmm_reg(o);
        };

        bool src1_fp = is_fp_operand(src1.get());
        bool src2_fp = is_fp_operand(src2.get());
        bool dst_fp = (dynamic_cast<ir::Stack *>(dst.get()) && dynamic_cast<ir::Stack *>(dst.get())->size == 8);

        bool use_fp = src1_fp || src2_fp || dst_fp;

        // ---------- Floating-point lowering ----------
        if (use_fp)
        {
            // Arithmetic: promote and do FP binary (use ADDSD/SUBSD/MULSD/DIVSD)
            if (binary_inst->op == tacky::BinaryOperatorType::Add ||
                binary_inst->op == tacky::BinaryOperatorType::Subtract ||
                binary_inst->op == tacky::BinaryOperatorType::Multiply ||
                binary_inst->op == tacky::BinaryOperatorType::Divide)
            {

                // Normalize: left -> %xmm1, right -> %xmm0
                // src1 -> xmm1
                if (auto *s = dynamic_cast<ir::Stack *>(src1.get()))
                {
                    ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(
                        make_unique<ir::Stack>(s->val, s->size),
                        make_unique<ir::Register>(ir::Reg(ir::RegType::XMM1))));
                }
                else if (auto *imm = dynamic_cast<ir::Immediate *>(src1.get()))
                {
                    ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(
                        make_unique<ir::Immediate>(imm->value),
                        make_unique<ir::Register>(ir::Reg(ir::RegType::XMM1))));
                }
                else if (auto *r = dynamic_cast<ir::Register *>(src1.get()))
                {
                    ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(
                        make_unique<ir::Register>(r->name),
                        make_unique<ir::Register>(ir::Reg(ir::RegType::XMM1))));
                }
                else
                {
                    throw runtime_error("FP lowering: unsupported left operand");
                }

                // src2 -> xmm0
                if (auto *s2 = dynamic_cast<ir::Stack *>(src2.get()))
                {
                    ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(
                        make_unique<ir::Stack>(s2->val, s2->size),
                        make_unique<ir::Register>(ir::Reg(ir::RegType::XMM0))));
                }
                else if (auto *imm2 = dynamic_cast<ir::Immediate *>(src2.get()))
                {
                    ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(
                        make_unique<ir::Immediate>(imm2->value),
                        make_unique<ir::Register>(ir::Reg(ir::RegType::XMM0))));
                }
                else if (auto *r2 = dynamic_cast<ir::Register *>(src2.get()))
                {
                    ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(
                        make_unique<ir::Register>(r2->name),
                        make_unique<ir::Register>(ir::Reg(ir::RegType::XMM0))));
                }
                else
                {
                    throw runtime_error("FP lowering: unsupported right operand");
                }

                ir::BinaryType fp_op;
                if (binary_inst->op == tacky::BinaryOperatorType::Add)
                    fp_op = ir::BinaryType::ADDSD;
                else if (binary_inst->op == tacky::BinaryOperatorType::Subtract)
                    fp_op = ir::BinaryType::SUBSD;
                else if (binary_inst->op == tacky::BinaryOperatorType::Multiply)
                    fp_op = ir::BinaryType::MULSD;
                else
                    fp_op = ir::BinaryType::DIVSD;

                if (fp_op == ir::BinaryType::SUBSD || fp_op == ir::BinaryType::DIVSD)
                {

                    // Load src1 (LHS) into %xmm0

                    if (auto *s = dynamic_cast<ir::Stack *>(src1.get()))
                    {

                        ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(

                            make_unique<ir::Stack>(s->val, s->size),

                            make_unique<ir::Register>(ir::Reg(ir::RegType::XMM0))

                                ));
                    }
                    else if (auto *imm = dynamic_cast<ir::Immediate *>(src1.get()))
                    {

                        ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(

                            make_unique<ir::Immediate>(imm->value),

                            make_unique<ir::Register>(ir::Reg(ir::RegType::XMM0))

                                ));

                    } // ... add other cases like src1 is register if needed ...

                    // Load src2 (RHS) into %xmm1

                    if (auto *s2 = dynamic_cast<ir::Stack *>(src2.get()))
                    {

                        ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(

                            make_unique<ir::Stack>(s2->val, s2->size),

                            make_unique<ir::Register>(ir::Reg(ir::RegType::XMM1))

                                ));
                    }
                    else if (auto *imm2 = dynamic_cast<ir::Immediate *>(src2.get()))
                    {

                        ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(

                            make_unique<ir::Immediate>(imm2->value),

                            make_unique<ir::Register>(ir::Reg(ir::RegType::XMM1))

                                ));

                    } // ... add other cases like src2 is register if needed ...

                    // This will be op %xmm1, %xmm0 (e.g., divsd %xmm1, %xmm0)

                    // This correctly computes %xmm0 = %xmm0 / %xmm1 (src1 / src2)

                    ir_func->instructions.push_back(make_unique<ir::BinaryInstruction>(

                        fp_op,

                        make_unique<ir::Register>(ir::Reg(ir::RegType::XMM1)),

                        make_unique<ir::Register>(ir::Reg(ir::RegType::XMM0))

                            ));
                }
                else
                {

                    // This is your ORIGINAL code, which is correct for ADDSD and MULSD

                    // src1 -> xmm1

                    if (auto *s = dynamic_cast<ir::Stack *>(src1.get()))
                    {

                        ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(

                            make_unique<ir::Stack>(s->val, s->size),

                            make_unique<ir::Register>(ir::Reg(ir::RegType::XMM1))

                                ));
                    }
                    else if (auto *imm = dynamic_cast<ir::Immediate *>(src1.get()))
                    {

                        ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(

                            make_unique<ir::Immediate>(imm->value),

                            make_unique<ir::Register>(ir::Reg(ir::RegType::XMM1))

                                ));

                    } // ...

                    // src2 -> xmm0

                    if (auto *s2 = dynamic_cast<ir::Stack *>(src2.get()))
                    {

                        ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(

                            make_unique<ir::Stack>(s2->val, s2->size),

                            make_unique<ir::Register>(ir::Reg(ir::RegType::XMM0))

                                ));
                    }
                    else if (auto *imm2 = dynamic_cast<ir::Immediate *>(src2.get()))
                    {

                        ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(

                            make_unique<ir::Immediate>(imm2->value),

                            make_unique<ir::Register>(ir::Reg(ir::RegType::XMM0))

                                ));

                    } // ...

                    // op %xmm1, %xmm0 (e.g., addsd %xmm1, %xmm0)

                    // This computes %xmm0 = %xmm0 + %xmm1 (src2 + src1), which is fine.

                    ir_func->instructions.push_back(make_unique<ir::BinaryInstruction>(

                        fp_op,

                        make_unique<ir::Register>(ir::Reg(ir::RegType::XMM1)),

                        make_unique<ir::Register>(ir::Reg(ir::RegType::XMM0))

                            ));
                }
                ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(
                    make_unique<ir::Register>(ir::Reg(ir::RegType::XMM0)),
                    move(dst) // 'dst' is the original destination operand
                    ));
                return;
            }

            // Comparisons / relational ops: normalize and use CmpSDInstruction (ucomisd)
            if (binary_inst->op == tacky::BinaryOperatorType::Equal ||
                binary_inst->op == tacky::BinaryOperatorType::NotEqual ||
                binary_inst->op == tacky::BinaryOperatorType::LessThan ||
                binary_inst->op == tacky::BinaryOperatorType::LessOrEqual ||
                binary_inst->op == tacky::BinaryOperatorType::GreaterThan ||
                binary_inst->op == tacky::BinaryOperatorType::GreaterOrEqual)
            {

                // left -> xmm1
                if (auto *s = dynamic_cast<ir::Stack *>(src1.get()))
                {
                    ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(
                        make_unique<ir::Stack>(s->val, s->size),
                        make_unique<ir::Register>(ir::Reg(ir::RegType::XMM1))));
                }
                else if (auto *imm = dynamic_cast<ir::Immediate *>(src1.get()))
                {
                    ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(
                        make_unique<ir::Immediate>(imm->value),
                        make_unique<ir::Register>(ir::Reg(ir::RegType::XMM1))));
                }
                else if (auto *r = dynamic_cast<ir::Register *>(src1.get()))
                {
                    ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(
                        make_unique<ir::Register>(r->name),
                        make_unique<ir::Register>(ir::Reg(ir::RegType::XMM1))));
                }
                else
                {
                    throw runtime_error("FP compare: unsupported left operand");
                }

                // right -> xmm0  (important: not xmm1)
                if (auto *s2 = dynamic_cast<ir::Stack *>(src2.get()))
                {
                    ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(
                        make_unique<ir::Stack>(s2->val, s2->size),
                        make_unique<ir::Register>(ir::Reg(ir::RegType::XMM0))));
                }
                else if (auto *imm2 = dynamic_cast<ir::Immediate *>(src2.get()))
                {
                    ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(
                        make_unique<ir::Immediate>(imm2->value),
                        make_unique<ir::Register>(ir::Reg(ir::RegType::XMM0))));
                }
                else if (auto *r2 = dynamic_cast<ir::Register *>(src2.get()))
                {
                    ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(
                        make_unique<ir::Register>(r2->name),
                        make_unique<ir::Register>(ir::Reg(ir::RegType::XMM0))));
                }
                else
                {
                    throw runtime_error("FP compare: unsupported right operand");
                }

                // emit FP compare (ucomisd xmm1, xmm0)
                ir_func->instructions.push_back(make_unique<ir::CmpSDInstruction>(
                    make_unique<ir::Register>(ir::Reg(ir::RegType::XMM1)),
                    make_unique<ir::Register>(ir::Reg(ir::RegType::XMM0))));

                // dst = 0
                ir_func->instructions.push_back(make_unique<ir::MovInstruction>(make_unique<ir::Immediate>("0"), move(dst_clone1)));

                // ZERO r10d BEFORE setcc, setcc into r10b, move r10 -> dst
                ir_func->instructions.push_back(make_unique<ir::MovInstruction>(make_unique<ir::Immediate>("0"),
                                                                                make_unique<ir::Register>(ir::Reg(ir::RegType::R10))));
                ir::CondCode cond;
                switch (binary_inst->op)
                {
                case tacky::BinaryOperatorType::Equal:
                    cond = ir::CondCode::E;
                    break;
                case tacky::BinaryOperatorType::NotEqual:
                    cond = ir::CondCode::NE;
                    break;
                case tacky::BinaryOperatorType::LessThan:
                    cond = ir::CondCode::B;
                    break;
                case tacky::BinaryOperatorType::LessOrEqual:
                    cond = ir::CondCode::BE;
                    break;
                case tacky::BinaryOperatorType::GreaterThan:
                    cond = ir::CondCode::A;
                    break;
                case tacky::BinaryOperatorType::GreaterOrEqual:
                    cond = ir::CondCode::AE;
                    break;
                default:
                    throw runtime_error("Unknown relational op");
                }
                ir_func->instructions.push_back(make_unique<ir::SetCCInstruction>(cond, make_unique<ir::Register>(ir::Reg(ir::RegType::R10))));
                ir_func->instructions.push_back(make_unique<ir::MovInstruction>(make_unique<ir::Register>(ir::Reg(ir::RegType::R10)), move(dst_clone2)));
                return;
            }

            throw runtime_error("Unsupported FP binary operator in lowering");
        } // end FP path

        // ---------- Integer / generic path (unchanged) ----------
        // Move src1 into dst (handle mem->mem)
        bool src1_is_stack = dynamic_cast<ir::Stack *>(src1.get()) != nullptr;
        bool src2_is_stack = dynamic_cast<ir::Stack *>(src2.get()) != nullptr;
        bool dst_is_stack_bool = dynamic_cast<ir::Stack *>(dst.get()) != nullptr;

        // dst = src1 (mem->mem safe copy using r10)
        if (src1_is_stack && dst_is_stack_bool)
        {
            auto reg_r10_src = make_unique<ir::Register>(ir::Reg(ir::RegType::R10));
            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(src1), move(reg_r10_src)));
            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(make_unique<ir::Register>(ir::Reg(ir::RegType::R10)), move(dst)));
        }
        else
        {
            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(src1), move(dst)));
        }

        // handle binary ops
        switch (binary_inst->op)
        {
        case tacky::BinaryOperatorType::Add:
        case tacky::BinaryOperatorType::Subtract:
        {
            ir::BinaryType bin_op = (binary_inst->op == tacky::BinaryOperatorType::Add) ? ir::BinaryType::ADD : ir::BinaryType::SUB;

            if (src2_is_stack)
            {
                auto reg_r10_src = make_unique<ir::Register>(ir::Reg(ir::RegType::R10));
                ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(src2), move(reg_r10_src)));
                ir_func->instructions.push_back(make_unique<ir::BinaryInstruction>(bin_op, make_unique<ir::Register>(ir::Reg(ir::RegType::R10)), move(dst_clone1)));
            }
            else
            {
                ir_func->instructions.push_back(make_unique<ir::BinaryInstruction>(bin_op, move(src2), move(dst_clone1)));
            }
            break;
        }

        case tacky::BinaryOperatorType::Multiply:
        {
            // use r11 temporary
            auto reg_r11_dst = make_unique<ir::Register>(ir::Reg(ir::RegType::R11));
            auto reg_r11_src = make_unique<ir::Register>(ir::Reg(ir::RegType::R11));
            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(dst_clone1), move(reg_r11_dst)));
            ir_func->instructions.push_back(make_unique<ir::BinaryInstruction>(ir::BinaryType::MUL, move(src2), move(reg_r11_src)));
            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(make_unique<ir::Register>(ir::Reg(ir::RegType::R11)), move(dst_clone2)));
            break;
        }

        case tacky::BinaryOperatorType::Divide:
        case tacky::BinaryOperatorType::Modulo:
        {
            // integer division path
            auto reg_ax_src = make_unique<ir::Register>(ir::Reg(ir::RegType::AX));
            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(dst_clone1), move(reg_ax_src)));
            ir_func->instructions.push_back(make_unique<ir::CDQInstruction>());

            if (is_imm(src2.get()))
            {
                auto reg_r10_src = make_unique<ir::Register>(ir::Reg(ir::RegType::R10));
                ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(src2), move(reg_r10_src)));
                ir_func->instructions.push_back(make_unique<ir::IdivInstruction>(make_unique<ir::Register>(ir::Reg(ir::RegType::R10))));
            }
            else
            {
                ir_func->instructions.push_back(make_unique<ir::IdivInstruction>(move(src2)));
            }

            if (binary_inst->op == tacky::BinaryOperatorType::Divide)
            {
                auto reg_ax_dst = make_unique<ir::Register>(ir::Reg(ir::RegType::AX));
                ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(reg_ax_dst), move(dst_clone2)));
            }
            else
            {
                auto reg_dx_dst = make_unique<ir::Register>(ir::Reg(ir::RegType::DX));
                ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(reg_dx_dst), move(dst_clone2)));
            }
            break;
        }

        // relational / equality (integer)
        case tacky::BinaryOperatorType::Equal:
        case tacky::BinaryOperatorType::NotEqual:
        case tacky::BinaryOperatorType::LessThan:
        case tacky::BinaryOperatorType::LessOrEqual:
        case tacky::BinaryOperatorType::GreaterThan:
        case tacky::BinaryOperatorType::GreaterOrEqual:
        {
            // Use compare + setcc pattern (zero dst, setcc into r10b, mov r10 -> dst)
            auto src1_clone = translate_val(binary_inst->src1.get());
            auto src2_clone = translate_val(binary_inst->src2.get());

            // normalize cmp operands to valid cmpl operands
            auto *src1_clone_is_stack = dynamic_cast<ir::Stack *>(src1_clone.get());
            auto *src2_clone_is_imm = dynamic_cast<ir::Immediate *>(src2_clone.get());

            if (src1_clone_is_stack && src2_clone_is_imm)
            {
                ir_func->instructions.push_back(make_unique<ir::MovInstruction>(
                    move(src2_clone),
                    make_unique<ir::Register>(ir::Reg(ir::RegType::R10))));
                ir_func->instructions.push_back(make_unique<ir::CmpInstruction>(
                    make_unique<ir::Register>(ir::Reg(ir::RegType::R10)),
                    move(src1_clone)));
            }
            else
            {
                ir_func->instructions.push_back(make_unique<ir::CmpInstruction>(
                    move(src2_clone),
                    move(src1_clone)));
            }

            // dst = 0
            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(make_unique<ir::Immediate>("0"), move(dst_clone1)));
            // zero r10d
            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(make_unique<ir::Immediate>("0"),
                                                                            make_unique<ir::Register>(ir::Reg(ir::RegType::R10))));

            ir::CondCode cond;
            switch (binary_inst->op)
            {
            case tacky::BinaryOperatorType::Equal:
                cond = ir::CondCode::E;
                break;
            case tacky::BinaryOperatorType::NotEqual:
                cond = ir::CondCode::NE;
                break;
            case tacky::BinaryOperatorType::LessThan:
                cond = ir::CondCode::L;
                break;
            case tacky::BinaryOperatorType::LessOrEqual:
                cond = ir::CondCode::LE;
                break;
            case tacky::BinaryOperatorType::GreaterThan:
                cond = ir::CondCode::G;
                break;
            case tacky::BinaryOperatorType::GreaterOrEqual:
                cond = ir::CondCode::GE;
                break;
            default:
                throw runtime_error("Unknown relational op");
            }
            ir_func->instructions.push_back(make_unique<ir::SetCCInstruction>(cond, make_unique<ir::Register>(ir::Reg(ir::RegType::R10))));
            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(make_unique<ir::Register>(ir::Reg(ir::RegType::R10)), move(dst_clone2)));
            break;
        }

        default:
            throw runtime_error("Unknown binary operator type in TACKY generator.");
        }

        return;
    } // end binary_inst branch

    // --- Copy ---
    // In codegen.cpp, replace the tacky::CopyInstruction block (lines 376-393)

    if (auto *copy_inst = dynamic_cast<const tacky::CopyInstruction *>(tacky_inst))
    {

        auto src = translate_val(copy_inst->src.get());

        auto dst = translate_val(copy_inst->dst.get());

        auto *src_is_stack = dynamic_cast<ir::Stack *>(src.get());

        auto *dst_is_stack = dynamic_cast<ir::Stack *>(dst.get());

        // --- START FIX ---

        // Check if this is an 8-byte (double) copy

        bool is_double_copy = false;

        if (dst_is_stack && dst_is_stack->size == 8)
            is_double_copy = true;

        if (src_is_stack && src_is_stack->size == 8)
            is_double_copy = true;

        if (auto *imm = dynamic_cast<ir::Immediate *>(src.get()))
        {

            if (imm->value.find('.') != std::string::npos ||

                imm->value.find('e') != std::string::npos ||

                imm->value.find('E') != std::string::npos)

            {

                is_double_copy = true;
            }
        }

        if (is_double_copy)
        {

            // Emit MovSD (movsd) for doubles

            if (src_is_stack && dst_is_stack)
            {

                // Use a scratch XMM register (like %xmm7)

                auto reg_xmm_tmp = make_unique<ir::Register>(ir::Reg(ir::RegType::XMM7));

                ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(move(src), move(reg_xmm_tmp)));

                ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(make_unique<ir::Register>(ir::Reg(ir::RegType::XMM7)), move(dst)));
            }
            else
            {

                // imm->mem, reg->mem, mem->reg

                ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(move(src), move(dst)));
            }
        }
        else
        {

            // --- END FIX ---

            // This is your original code, which is correct for integers (movl)

            if (src_is_stack && dst_is_stack)
            {

                auto reg_r10_src = make_unique<ir::Register>(ir::Reg(ir::RegType::R10));

                ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(src), move(reg_r10_src)));

                ir_func->instructions.push_back(make_unique<ir::MovInstruction>(make_unique<ir::Register>(ir::Reg(ir::RegType::R10)), move(dst)));
            }
            else
            {

                ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(src), move(dst)));
            }

        } // end if (is_double_copy)

        return;
    }
    // --- Jumps / branches / labels ---
    if (auto *jmp_inst = dynamic_cast<const tacky::JumpInstruction *>(tacky_inst))
    {
        ir_func->instructions.push_back(make_unique<ir::JmpInstruction>(jmp_inst->target));
        return;
    }
    if (auto *jz_inst = dynamic_cast<const tacky::JumpIfZeroInstruction *>(tacky_inst))
    {
        auto cond = translate_val(jz_inst->condition.get());
        auto imm_zero = make_unique<ir::Immediate>("0");

        if (dynamic_cast<ir::Stack *>(cond.get()))
        {
            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(imm_zero), make_unique<ir::Register>(ir::Reg(ir::RegType::R10))));
            ir_func->instructions.push_back(make_unique<ir::CmpInstruction>(make_unique<ir::Register>(ir::Reg(ir::RegType::R10)), move(cond)));
        }
        else
        {
            ir_func->instructions.push_back(make_unique<ir::CmpInstruction>(move(imm_zero), move(cond)));
        }
        ir_func->instructions.push_back(make_unique<ir::JmpCCInstruction>(ir::CondCode::E, jz_inst->target));
        return;
    }
    if (auto *jnz_inst = dynamic_cast<const tacky::JumpIfNotZeroInstruction *>(tacky_inst))
    {
        auto cond = translate_val(jnz_inst->condition.get());
        auto imm_zero = make_unique<ir::Immediate>("0");

        if (dynamic_cast<ir::Stack *>(cond.get()))
        {
            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(imm_zero), make_unique<ir::Register>(ir::Reg(ir::RegType::R10))));
            ir_func->instructions.push_back(make_unique<ir::CmpInstruction>(make_unique<ir::Register>(ir::Reg(ir::RegType::R10)), move(cond)));
        }
        else
        {
            ir_func->instructions.push_back(make_unique<ir::CmpInstruction>(move(imm_zero), move(cond)));
        }
        ir_func->instructions.push_back(make_unique<ir::JmpCCInstruction>(ir::CondCode::NE, jnz_inst->target));
        return;
    }
    if (auto *label_inst = dynamic_cast<const tacky::LabelInstruction *>(tacky_inst))
    {
        ir_func->instructions.push_back(make_unique<ir::LabelInstruction>(label_inst->target));
        return;
    }

    throw runtime_error("Unknown TACKY instruction type");
}

unique_ptr<ir::Operand> CodeGenerator::translate_val(const tacky::Value *tacky_val)
{
    if (auto *c = dynamic_cast<const tacky::Constant *>(tacky_val))
    {
        return make_unique<ir::Immediate>(c->value);
    }

    if (auto *v = dynamic_cast<const tacky::Var *>(tacky_val))
    {
        // allocate stack slot sized by the variable's type
        if (stack_map.find(v->name) == stack_map.end())
        {
            int sz = 4;
            switch (v->type)
            {
            case tacky::TypeKind::Double:
                sz = 8;
                break;
            case tacky::TypeKind::Float:
                sz = 8;
                break; // we use 8 for simplicity
            case tacky::TypeKind::Int:
                sz = 4;
                break;
            }
            // align to 8 for doubles
            if (sz == 8 && ((-next_stack_offset) % 8 != 0))
            {
                int pad = (8 - ((-next_stack_offset) % 8)) % 8;
                next_stack_offset -= pad;
            }
            next_stack_offset -= sz;
            stack_map[v->name] = next_stack_offset;
        }
        int sz = 4;
        // retrieve the size we decided earlier from stack slot size:
        // For simplicity we recompute here: check whether slot offset aligns to 8
        // but better is to store type→size in a separate map; however since we used
        // v->type above, we can reuse it:
        switch (v->type)
        {
        case tacky::TypeKind::Double:
            sz = 8;
            break;
        case tacky::TypeKind::Float:
            sz = 8;
            break;
        case tacky::TypeKind::Int:
            sz = 4;
            break;
        }
        return make_unique<ir::Stack>(stack_map[v->name], sz);
    }
    throw runtime_error("Unknown TACKY value type");
}
