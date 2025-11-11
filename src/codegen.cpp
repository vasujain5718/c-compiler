#include "codegen.h"
#include <stdexcept>
#include <string>

using std::unique_ptr;
using std::make_unique;
using std::move;
using std::string;
using std::map;
using std::runtime_error;

// Helper to safely clone a stack operand
unique_ptr<ir::Stack> clone_stack_op(const ir::Operand* op) {
    auto* stack_op = dynamic_cast<const ir::Stack*>(op);
    if (!stack_op) {
        throw runtime_error("Expected a stack operand.");
    }
    return make_unique<ir::Stack>(stack_op->val);
}

unique_ptr<ir::Program> CodeGenerator::generate(const tacky::Program* tacky_prog) {
    auto ir_prog = make_unique<ir::Program>();
    ir_prog->function = make_unique<ir::Function>();
    
    stack_map.clear();
    next_stack_offset = 0;

    generate_function(tacky_prog->function.get(), ir_prog->function.get());

    return ir_prog;
}

void CodeGenerator::generate_function(const tacky::Function* tacky_func, ir::Function* ir_func) {
    ir_func->name = tacky_func->name;

    auto alloc_inst = new ir::AllocateStackInstruction(0);
    ir_func->instructions.push_back(unique_ptr<ir::Instruction>(alloc_inst));

    for (const auto& inst : tacky_func->body) {
        generate_instruction(inst.get(), ir_func);
    }

    alloc_inst->n = -next_stack_offset;
}

void CodeGenerator::generate_instruction(const tacky::Instruction* tacky_inst, ir::Function* ir_func) {
    
    if (auto* ret_inst = dynamic_cast<const tacky::ReturnInstruction*>(tacky_inst)) {
        auto val = translate_val(ret_inst->val.get());
        auto reg_ax = make_unique<ir::Register>(ir::Reg(ir::RegType::AX));
        
        ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(val), move(reg_ax)));
        ir_func->instructions.push_back(make_unique<ir::RetInstruction>());
    }
    else if (auto* unary_inst = dynamic_cast<const tacky::UnaryInstruction*>(tacky_inst)) {
        auto src = translate_val(unary_inst->src.get());
        auto dst = translate_val(unary_inst->dst.get());
        auto dst_clone = clone_stack_op(dst.get());

        // Logical NOT: (!x) == (x == 0)
        if (unary_inst->op == tacky::UnaryOperatorType::LogicalNot) {
            // 1) cmp 0, src  (taking care with mem operands)
            if (dynamic_cast<ir::Stack*>(src.get())) {
                ir_func->instructions.push_back(make_unique<ir::MovInstruction>(
                    make_unique<ir::Immediate>("0"),
                    make_unique<ir::Register>(ir::Reg(ir::RegType::R10))
                ));
                ir_func->instructions.push_back(make_unique<ir::CmpInstruction>(
                    make_unique<ir::Register>(ir::Reg(ir::RegType::R10)),
                    move(src)
                ));
            } else {
                ir_func->instructions.push_back(make_unique<ir::CmpInstruction>(
                    make_unique<ir::Immediate>("0"),
                    move(src)
                ));
            }
            // 2) dst = 0 (clear destination in memory)
            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(
                make_unique<ir::Immediate>("0"),
                move(dst)
            ));
            // 3) ZERO r10d BEFORE setcc to avoid garbage in upper bits
            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(
                make_unique<ir::Immediate>("0"),
                make_unique<ir::Register>(ir::Reg(ir::RegType::R10))
            ));
            // 4) setcc (E) -> %r10b
            ir_func->instructions.push_back(make_unique<ir::SetCCInstruction>(
                ir::CondCode::E,
                make_unique<ir::Register>(ir::Reg(ir::RegType::R10))
            ));
            // 5) mov %r10d -> dst
            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(
                make_unique<ir::Register>(ir::Reg(ir::RegType::R10)),
                move(dst_clone)
            ));
        } 
        else {
            // Negate / Bitwise NOT
            auto* src_is_stack = dynamic_cast<ir::Stack*>(src.get());
            if (src_is_stack) {
                auto reg_r10_src = make_unique<ir::Register>(ir::Reg(ir::RegType::R10));
                ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(src), move(reg_r10_src)));
                ir_func->instructions.push_back(make_unique<ir::MovInstruction>(make_unique<ir::Register>(ir::Reg(ir::RegType::R10)), move(dst)));
            } else {
                ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(src), move(dst)));
            }
            
            ir::UnaryType op_type = (unary_inst->op == tacky::UnaryOperatorType::Negate) ? ir::UnaryType::NEG : ir::UnaryType::NOT;
            ir_func->instructions.push_back(make_unique<ir::UnaryInstruction>(op_type, move(dst_clone)));
        }
    }
    else if(auto* binary_inst = dynamic_cast<const tacky::BinaryInstruction*>(tacky_inst)) {
        
        auto src1 = translate_val(binary_inst->src1.get());
        auto src2 = translate_val(binary_inst->src2.get());
        auto dst = translate_val(binary_inst->dst.get());
        
        auto dst_clone1 = clone_stack_op(dst.get());
        auto dst_clone2 = clone_stack_op(dst.get());

        auto* src1_is_stack = dynamic_cast<ir::Stack*>(src1.get());
        auto* dst_is_stack = dynamic_cast<ir::Stack*>(dst.get());
        
        // dst = src1
        if (src1_is_stack && dst_is_stack) {
            auto reg_r10_src = make_unique<ir::Register>(ir::Reg(ir::RegType::R10));
            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(src1), move(reg_r10_src)));
            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(make_unique<ir::Register>(ir::Reg(ir::RegType::R10)), move(dst)));
        } else {
            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(src1), move(dst)));
        }

        switch (binary_inst->op) {
            case tacky::BinaryOperatorType::Add:
            case tacky::BinaryOperatorType::Subtract: {
                ir::BinaryType bin_op = (binary_inst->op == tacky::BinaryOperatorType::Add) ? ir::BinaryType::ADD : ir::BinaryType::SUB;
                
                auto* src2_is_stack = dynamic_cast<ir::Stack*>(src2.get());
                if (src2_is_stack) {
                    auto reg_r10_src = make_unique<ir::Register>(ir::Reg(ir::RegType::R10));
                    ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(src2), move(reg_r10_src)));
                    ir_func->instructions.push_back(make_unique<ir::BinaryInstruction>(bin_op, make_unique<ir::Register>(ir::Reg(ir::RegType::R10)), move(dst_clone1)));
                } else {
                    ir_func->instructions.push_back(make_unique<ir::BinaryInstruction>(bin_op, move(src2), move(dst_clone1)));
                }
                break;
            }

            case tacky::BinaryOperatorType::Multiply: {
                auto reg_r11_dst = make_unique<ir::Register>(ir::Reg(ir::RegType::R11));
                auto reg_r11_src = make_unique<ir::Register>(ir::Reg(ir::RegType::R11));
                
                ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(dst_clone1), move(reg_r11_dst)));
                ir_func->instructions.push_back(make_unique<ir::BinaryInstruction>(ir::BinaryType::MUL, move(src2), move(reg_r11_src)));
                ir_func->instructions.push_back(make_unique<ir::MovInstruction>(make_unique<ir::Register>(ir::Reg(ir::RegType::R11)), move(dst_clone2)));
                break;
            }

            case tacky::BinaryOperatorType::Divide:
            case tacky::BinaryOperatorType::Modulo: {
                auto reg_ax_src = make_unique<ir::Register>(ir::Reg(ir::RegType::AX));
                ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(dst_clone1), move(reg_ax_src)));
                
                ir_func->instructions.push_back(make_unique<ir::CDQInstruction>());

                auto* src2_is_imm = dynamic_cast<ir::Immediate*>(src2.get());
                if (src2_is_imm) {
                    auto reg_r10_src = make_unique<ir::Register>(ir::Reg(ir::RegType::R10));
                    ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(src2), move(reg_r10_src)));
                    ir_func->instructions.push_back(make_unique<ir::IdivInstruction>(make_unique<ir::Register>(ir::Reg(ir::RegType::R10))));
                } else {
                    ir_func->instructions.push_back(make_unique<ir::IdivInstruction>(move(src2)));
                }

                if (binary_inst->op == tacky::BinaryOperatorType::Divide) {
                    auto reg_ax_dst = make_unique<ir::Register>(ir::Reg(ir::RegType::AX));
                    ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(reg_ax_dst), move(dst_clone2)));
                } else { // Modulo
                    auto reg_dx_dst = make_unique<ir::Register>(ir::Reg(ir::RegType::DX));
                    ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(reg_dx_dst), move(dst_clone2)));
                }
                break;
            }
            
            default: {
                // Relational operators: compare, zero dst, zero r10d, setcc -> r10b, mov r10d -> dst
                auto src1_clone = translate_val(binary_inst->src1.get());
                auto src2_clone = translate_val(binary_inst->src2.get());
                
                auto* src1_clone_is_stack = dynamic_cast<ir::Stack*>(src1_clone.get());
                auto* src2_clone_is_imm = dynamic_cast<ir::Immediate*>(src2_clone.get());
                
                if (src1_clone_is_stack && src2_clone_is_imm) {
                    ir_func->instructions.push_back(make_unique<ir::MovInstruction>(
                        move(src2_clone),
                        make_unique<ir::Register>(ir::Reg(ir::RegType::R10))
                    ));
                    ir_func->instructions.push_back(make_unique<ir::CmpInstruction>(
                        make_unique<ir::Register>(ir::Reg(ir::RegType::R10)),
                        move(src1_clone)
                    ));
                } else {
                    ir_func->instructions.push_back(make_unique<ir::CmpInstruction>(
                        move(src2_clone),
                        move(src1_clone)
                    ));
                }
                
                // dst = 0
                ir_func->instructions.push_back(make_unique<ir::MovInstruction>(
                    make_unique<ir::Immediate>("0"),
                    move(dst_clone1)
                ));

                // >>> ZERO r10d BEFORE setcc <<<
                ir_func->instructions.push_back(make_unique<ir::MovInstruction>(
                    make_unique<ir::Immediate>("0"),
                    make_unique<ir::Register>(ir::Reg(ir::RegType::R10))
                ));
                
                auto reg_r10_byte = make_unique<ir::Register>(ir::Reg(ir::RegType::R10));
                auto reg_r10_full = make_unique<ir::Register>(ir::Reg(ir::RegType::R10));
                
                ir::CondCode cond;
                switch(binary_inst->op) {
                    case tacky::BinaryOperatorType::Equal:          cond = ir::CondCode::E;  break;
                    case tacky::BinaryOperatorType::NotEqual:       cond = ir::CondCode::NE; break;
                    case tacky::BinaryOperatorType::LessThan:       cond = ir::CondCode::L;  break;
                    case tacky::BinaryOperatorType::LessOrEqual:    cond = ir::CondCode::LE; break;
                    case tacky::BinaryOperatorType::GreaterThan:    cond = ir::CondCode::G;  break;
                    case tacky::BinaryOperatorType::GreaterOrEqual: cond = ir::CondCode::GE; break;
                    default: throw runtime_error("Unknown relational op");
                }
                ir_func->instructions.push_back(make_unique<ir::SetCCInstruction>(
                    cond,
                    move(reg_r10_byte)
                ));
                
                ir_func->instructions.push_back(make_unique<ir::MovInstruction>(
                    move(reg_r10_full),
                    move(dst_clone2)
                ));
            }
        }
    }
    else if (auto* copy_inst = dynamic_cast<const tacky::CopyInstruction*>(tacky_inst)) {
        auto src = translate_val(copy_inst->src.get());
        auto dst = translate_val(copy_inst->dst.get());
        
        auto* src_is_stack = dynamic_cast<ir::Stack*>(src.get());
        auto* dst_is_stack = dynamic_cast<ir::Stack*>(dst.get());

        if (src_is_stack && dst_is_stack) {
            auto reg_r10_src = make_unique<ir::Register>(ir::Reg(ir::RegType::R10));
            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(src), move(reg_r10_src)));
            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(make_unique<ir::Register>(ir::Reg(ir::RegType::R10)), move(dst)));
        } else {
            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(src), move(dst)));
        }
    }
    else if (auto* jmp_inst = dynamic_cast<const tacky::JumpInstruction*>(tacky_inst)) {
        ir_func->instructions.push_back(make_unique<ir::JmpInstruction>(jmp_inst->target));
    }
    else if (auto* jz_inst = dynamic_cast<const tacky::JumpIfZeroInstruction*>(tacky_inst)) {
        auto cond = translate_val(jz_inst->condition.get());
        auto imm_zero = make_unique<ir::Immediate>("0");
        
        if(dynamic_cast<ir::Stack*>(cond.get())) {
            // cmp (reg, mem) — move imm 0 to reg first
            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(
                move(imm_zero),
                make_unique<ir::Register>(ir::Reg(ir::RegType::R10))
            ));
            ir_func->instructions.push_back(make_unique<ir::CmpInstruction>(
                make_unique<ir::Register>(ir::Reg(ir::RegType::R10)),
                move(cond)
            ));
        } else {
            ir_func->instructions.push_back(make_unique<ir::CmpInstruction>(
                move(imm_zero),
                move(cond)
            ));
        }
        ir_func->instructions.push_back(make_unique<ir::JmpCCInstruction>(ir::CondCode::E, jz_inst->target));
    }
    else if (auto* jnz_inst = dynamic_cast<const tacky::JumpIfNotZeroInstruction*>(tacky_inst)) {
        auto cond = translate_val(jnz_inst->condition.get());
        auto imm_zero = make_unique<ir::Immediate>("0");

        if(dynamic_cast<ir::Stack*>(cond.get())) {
            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(
                move(imm_zero),
                make_unique<ir::Register>(ir::Reg(ir::RegType::R10))
            ));
            ir_func->instructions.push_back(make_unique<ir::CmpInstruction>(
                make_unique<ir::Register>(ir::Reg(ir::RegType::R10)),
                move(cond)
            ));
        } else {
            ir_func->instructions.push_back(make_unique<ir::CmpInstruction>(
                move(imm_zero),
                move(cond)
            ));
        }
        ir_func->instructions.push_back(make_unique<ir::JmpCCInstruction>(ir::CondCode::NE, jnz_inst->target));
    }
    else if (auto* label_inst = dynamic_cast<const tacky::LabelInstruction*>(tacky_inst)) {
        ir_func->instructions.push_back(make_unique<ir::LabelInstruction>(label_inst->target));
    }
    else {
        throw runtime_error("Unknown TACKY instruction type");
    }
}

unique_ptr<ir::Operand> CodeGenerator::translate_val(const tacky::Value* tacky_val) {
    if (auto* c = dynamic_cast<const tacky::Constant*>(tacky_val)) {
        return make_unique<ir::Immediate>(c->value);
    }
    
    if (auto* v = dynamic_cast<const tacky::Var*>(tacky_val)) {
        if (stack_map.find(v->name) == stack_map.end()) {
            next_stack_offset -= 4;
            stack_map[v->name] = next_stack_offset;
        }
        return make_unique<ir::Stack>(stack_map[v->name]);
    }
    throw runtime_error("Unknown TACKY value type");
}
