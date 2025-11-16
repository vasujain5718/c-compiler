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
static std::unordered_map<std::string, int> array_allocation_map;
unique_ptr<ir::Program> CodeGenerator::generate(const tacky::Program *tacky_prog)
{
    auto ir_prog = make_unique<ir::Program>();
    ir_prog->function = make_unique<ir::Function>();

    stack_map.clear();
    next_stack_offset = 0;

    // clear array allocation tracking for this run
    array_allocation_map.clear();

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

// File-local helper map to track array allocation bytes we've reserved per base name
// key: base variable name, value: bytes reserved (positive)


// Ensure that base_name has at least 'needed_bytes' reserved starting at its stack_map entry.
// If base_name isn't yet allocated we allocate needed_bytes (aligned for doubles).
// If base_name is allocated with fewer bytes, we *do not* try to change existing layout
// because that would require shifting all previously allocated slots. Instead, we throw.
// Ensure that base_name has at least 'needed_bytes' reserved starting at its stack_map entry.
// If base_name isn't yet allocated we allocate needed_bytes (aligned for doubles).
// If base_name is allocated with fewer bytes, we allow expansion ONLY if the base was the last
// allocation (i.e., its offset equals the current next_stack_offset_local), otherwise throw.
static void ensure_array_allocation(std::map<std::string, int> &stack_map_local,
                                    int &next_stack_offset_local,
                                    std::unordered_map<std::string, int> &array_alloc_map,
                                    const std::string &base_name,
                                    int needed_bytes)
{
    // If already reserved, ensure it's at least needed_bytes
    auto it_alloc = array_alloc_map.find(base_name);
    if (it_alloc != array_alloc_map.end())
    {
        if (it_alloc->second >= needed_bytes)
            return; // already large enough
        // previously allocated smaller region -> try expansion below
    }

    // If stack_map already has an entry for base_name, attempt to expand if possible.
    auto it = stack_map_local.find(base_name);
    if (it != stack_map_local.end())
    {
        // existing base offset in stack_map
        int existing_base_off = it->second;

        // If we have an existing reservation entry, use that size; otherwise assume single-slot
        int existing_alloc = 0;
        auto it_res = array_alloc_map.find(base_name);
        if (it_res != array_alloc_map.end())
        {
            existing_alloc = it_res->second;
        }
        else
        {
            // We don't know full reserved size; assume a single-element slot of 4 or 8 depending on alignment.
            // For safety, compute distance between existing_base_off and current next_stack_offset_local.
            existing_alloc = (-next_stack_offset_local) - (-existing_base_off);
            if (existing_alloc < 0) existing_alloc = 0;
            // If this calculation fails (unexpected), fall back to 0 and require strict allocation.
        }

        if (existing_alloc >= needed_bytes)
        {
            // Already big enough (should have been caught earlier), return.
            array_alloc_map[base_name] = existing_alloc; // ensure entry exists
            return;
        }

        // Can we expand in-place? Only if nothing was allocated after this base,
        // meaning the base offset should equal current next_stack_offset_local.
        if (existing_base_off == next_stack_offset_local)
        {
            // expand by allocating the additional bytes we need (plus alignment)
            int extra_needed = needed_bytes - existing_alloc;
            int pad = 0;
            if (extra_needed % 8 != 0)
                pad = (8 - (extra_needed % 8));
            int alloc_more = extra_needed + pad;

            next_stack_offset_local -= alloc_more;
            // new base offset is the new next_stack_offset_local
            stack_map_local[base_name] = next_stack_offset_local;
            array_alloc_map[base_name] = existing_alloc + alloc_more;
            return;
        }

        // Otherwise, doing in-place expansion is unsafe; error out.
        throw runtime_error("Internal Error: previously allocated array region for '" + base_name +
                            "' is smaller than required index access and cannot be expanded safely.");
    }

    // Otherwise allocate needed_bytes now by decrementing next_stack_offset_local.
    // Align to 8 if needed
    int pad = 0;
    if (needed_bytes % 8 != 0)
    {
        pad = (8 - (needed_bytes % 8));
    }
    int alloc_bytes = needed_bytes + pad;

    next_stack_offset_local -= alloc_bytes;
    // store base offset
    stack_map_local[base_name] = next_stack_offset_local;
    array_alloc_map[base_name] = alloc_bytes;
}


void CodeGenerator::generate_instruction(const tacky::Instruction *tacky_inst, ir::Function *ir_func)
{
    if (auto *adecl = dynamic_cast<const tacky::ArrayDeclInstruction *>(tacky_inst))

    {

        // This is the array declaration. We must pre-allocate the

        // entire array's memory block *now*.



        // 1. Calculate element size

        int elem_size = size_of_tacky_kind(adecl->element_type);



        // 2. Calculate total bytes needed for all elements

        int total_bytes = adecl->num_elements * elem_size;



        // 3. Call ensure_array_allocation with the *total size*.

        // This will reserve the full block and set the base address

        // in stack_map correctly, once and for all.

        ensure_array_allocation(stack_map, 

                                next_stack_offset, 

                                array_allocation_map, 

                                adecl->base_name, 

                                total_bytes);

        

        // This instruction's job is done; it emits no assembly itself.

        return;

    }
    // --- NEW: handle array load/store instructions first (best-effort) ---
    if (auto *aload = dynamic_cast<const tacky::ArrayLoadInstruction *>(tacky_inst))
    {
        // We support immediate (constant) index only in this implementation.
        // Translate the index value; it must be an immediate for us to compute a stack offset now.
        auto idx_op = translate_val(aload->index_value.get());

        auto *imm_idx = dynamic_cast<ir::Immediate *>(idx_op.get());
        if (!imm_idx)
        {
            throw runtime_error("Array load with dynamic index not supported by this CodeGenerator. Add IR addressing modes or update emitter.");
        }

        // parse index integer (simple stoi; user must ensure it is integer constant)
        int idx = 0;
        try
        {
            idx = std::stoi(imm_idx->value);
        }
        catch (...)
        {
            throw runtime_error("Array load: non-integer index immediate.");
        }

        // element size from dst var type
        int elem_size = size_of_tacky_kind(aload->dst->type);

        // ensure we have reserved enough bytes for base_name up to this index (naive)
        // Note: we use file-local array_allocation_map to track reservations; translate_val uses member stack_map.
        ensure_array_allocation(stack_map, next_stack_offset, array_allocation_map, aload->base_name, (idx + 1) * elem_size);

        // compute element offset and create an ir::Stack operand for that element
        int base_off = stack_map[aload->base_name];
        int elem_off = base_off + idx * elem_size;
        auto element_stack = make_unique<ir::Stack>(elem_off, elem_size);

        // allocate destination (temp) on stack via translate_val of the dst Var (this will allocate a temp slot)
        auto dst_val = translate_val(aload->dst.get()); // will be a Stack temporary
        // Now emit mov from element_stack -> dst_val
        // For float/double use movsd, else movl
        if (elem_size == 8)
        {
            ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(move(element_stack), move(dst_val)));
        }
        else
        {
            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(element_stack), move(dst_val)));
        }
        return;
    }

    if (auto *astore = dynamic_cast<const tacky::ArrayStoreInstruction *>(tacky_inst))
    {
        // Similar constraints as above: only immediate index handled.
        auto idx_op = translate_val(astore->index_value.get());
        auto *imm_idx = dynamic_cast<ir::Immediate *>(idx_op.get());
        if (!imm_idx)
        {
            throw runtime_error("Array store with dynamic index not supported by this CodeGenerator. Add IR addressing modes or update emitter.");
        }

        int idx = 0;
        try
        {
            idx = std::stoi(imm_idx->value);
        }
        catch (...)
        {
            throw runtime_error("Array store: non-integer index immediate.");
        }

        // element size: we don't have direct element-type in ArrayStoreInstruction here, so attempt to deduce from src
        int elem_size = 4;
        if (auto *c = dynamic_cast<const tacky::Constant *>(astore->src.get()))
        {
            // infer using constant text: '.' or 'e' -> double
            if (c->value.find('.') != string::npos || c->value.find('e') != string::npos || c->value.find('E') != string::npos)
                elem_size = 8;
            else
                elem_size = 4;
        }
        else if (auto *v = dynamic_cast<const tacky::Var *>(astore->src.get()))
        {
            // translate_val would allocate stack; but here simply infer size by v->type if this is a Var produced earlier
            elem_size = size_of_tacky_kind(v->type);
        }
        else
        {
            // fallback to 4
            elem_size = 4;
        }

        ensure_array_allocation(stack_map, next_stack_offset, array_allocation_map, astore->base_name, (idx + 1) * elem_size);
        int base_off = stack_map[astore->base_name];
        int elem_off = base_off + idx * elem_size;
        auto element_stack = make_unique<ir::Stack>(elem_off, elem_size);

        // generate rhs value
        auto src_val = translate_val(astore->src.get());

        // emit store: mem(elem_off) = src_val
        if (elem_size == 8)
        {
            ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(move(src_val), move(element_stack)));
        }
        else
        {
            ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(src_val), move(element_stack)));
        }
        return;
    }

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
        // ---------- Floating-point lowering ----------

        if (use_fp)

        {

            // --- START HELPER: Load any operand into an XMM register ---

            auto load_operand_to_xmm = [&](unique_ptr<ir::Operand> op, ir::RegType xmm_reg) {

                auto xmm_dest = make_unique<ir::Register>(ir::Reg(xmm_reg));



                // Case 1: Operand is a Stack variable

                if (auto *s = dynamic_cast<ir::Stack *>(op.get()))

                {

                    if (s->size == 8) {

                        // It's a double -> use movsd

                        ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(move(op), move(xmm_dest)));

                    } else {

                        // It's an int/char -> use cvtsi2sd

                        ir_func->instructions.push_back(make_unique<ir::CvtSI2SDInstruction>(move(op), move(xmm_dest)));

                    }

                    return;

                }

                // Case 2: Operand is an immediate or other register (already float/double)

                // This assumes int immediates are not mixed in FP ops without a var

                // (which is OK, since a = b + 1; would be promoted by semantic pass)

                // We just use MovSD which will load from .rodata pool

                ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(move(op), move(xmm_dest)));

            };

            // --- END HELPER ---

            // Arithmetic: promote and do FP binary (use ADDSD/SUBSD/MULSD/DIVSD)

            if (binary_inst->op == tacky::BinaryOperatorType::Add ||

                binary_inst->op == tacky::BinaryOperatorType::Subtract ||

                binary_inst->op == tacky::BinaryOperatorType::Multiply ||

                binary_inst->op == tacky::BinaryOperatorType::Divide)

            {

                ir::BinaryType fp_op;

                if (binary_inst->op == tacky::BinaryOperatorType::Add)      fp_op = ir::BinaryType::ADDSD;

                else if (binary_inst->op == tacky::BinaryOperatorType::Subtract) fp_op = ir::BinaryType::SUBSD;

                else if (binary_inst->op == tacky::BinaryOperatorType::Multiply) fp_op = ir::BinaryType::MULSD;

                else                                                     fp_op = ir::BinaryType::DIVSD;



                if (fp_op == ir::BinaryType::SUBSD || fp_op == ir::BinaryType::DIVSD)

                {

                    // op is %xmm1, %xmm0  (calculates %xmm0 = %xmm0 - %xmm1)

                    // We want src1 - src2, so load src1 -> %xmm0, src2 -> %xmm1

                    load_operand_to_xmm(move(src1), ir::RegType::XMM0);

                    load_operand_to_xmm(move(src2), ir::RegType::XMM1);

                    ir_func->instructions.push_back(make_unique<ir::BinaryInstruction>(

                        fp_op,

                        make_unique<ir::Register>(ir::Reg(ir::RegType::XMM1)),

                        make_unique<ir::Register>(ir::Reg(ir::RegType::XMM0))

                    ));

                }

                else // ADDSD, MULSD

                {

                    // op is %xmm1, %xmm0  (calculates %xmm0 = %xmm0 + %xmm1)

                    // Order doesn't matter, so load src1 -> %xmm0, src2 -> %xmm1

                    load_operand_to_xmm(move(src1), ir::RegType::XMM0);

                    load_operand_to_xmm(move(src2), ir::RegType::XMM1);

                    ir_func->instructions.push_back(make_unique<ir::BinaryInstruction>(

                        fp_op,

                        make_unique<ir::Register>(ir::Reg(ir::RegType::XMM1)),

                        make_unique<ir::Register>(ir::Reg(ir::RegType::XMM0))

                    ));

                }

                // Store result from %xmm0 into destination

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

                // ucomisd %xmm0, %xmm1 (compares src2, src1)

                // Load src1 -> %xmm1

                load_operand_to_xmm(move(src1), ir::RegType::XMM0);

                // Load src2 -> %xmm0

                load_operand_to_xmm(move(src2), ir::RegType::XMM1);



                // emit FP compare (ucomisd xmm0, xmm1)

                ir_func->instructions.push_back(make_unique<ir::CmpSDInstruction>(

                    make_unique<ir::Register>(ir::Reg(ir::RegType::XMM0)), // src2

                    make_unique<ir::Register>(ir::Reg(ir::RegType::XMM1))  // src1

                ));



                // dst = 0

                ir_func->instructions.push_back(make_unique<ir::MovInstruction>(make_unique<ir::Immediate>("0"), move(dst_clone1)));



                // ZERO r10d BEFORE setcc, setcc into r10b, move r10 -> dst

                ir_func->instructions.push_back(make_unique<ir::MovInstruction>(make_unique<ir::Immediate>("0"),

                                                                                make_unique<ir::Register>(ir::Reg(ir::RegType::R10))));

                ir::CondCode cond;

                switch (binary_inst->op)

                {

                case tacky::BinaryOperatorType::Equal:        cond = ir::CondCode::E;  break;

                case tacky::BinaryOperatorType::NotEqual:      cond = ir::CondCode::NE; break;

                case tacky::BinaryOperatorType::LessThan:       cond = ir::CondCode::B;  break; // unsigned/below for floats

                case tacky::BinaryOperatorType::LessOrEqual:    cond = ir::CondCode::BE; break; // unsigned/below-equal

                case tacky::BinaryOperatorType::GreaterThan:     cond = ir::CondCode::A;  break; // unsigned/above

                case tacky::BinaryOperatorType::GreaterOrEqual: cond = ir::CondCode::AE; break; // unsigned/above-equal

                default: throw runtime_error("Unknown relational op");

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

    // --- Copy ---

    if (auto *copy_inst = dynamic_cast<const tacky::CopyInstruction *>(tacky_inst))

    {

        auto src = translate_val(copy_inst->src.get());

        auto dst = translate_val(copy_inst->dst.get());



        auto *src_is_stack = dynamic_cast<ir::Stack *>(src.get());

        auto *dst_is_stack = dynamic_cast<ir::Stack *>(dst.get());

        auto *src_is_imm = dynamic_cast<ir::Immediate *>(src.get());



        // --- START NEW LOGIC ---



        // Case 1: Destination is a double (size 8)

        if (dst_is_stack && dst_is_stack->size == 8)

        {

            // Case 1a: Source is also a double (size 8)

            if (src_is_stack && src_is_stack->size == 8)

            {

                // double-to-double copy (mem-to-mem)

                auto reg_xmm_tmp = make_unique<ir::Register>(ir::Reg(ir::RegType::XMM7));

                ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(move(src), move(reg_xmm_tmp)));

                ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(make_unique<ir::Register>(ir::Reg(ir::RegType::XMM7)), move(dst)));

            }

            // Case 1b: Source is an int/char (size 4)

            else if (src_is_stack && src_is_stack->size == 4)

            {

                // int-to-double copy (mem-to-mem)

                // We MUST use cvtsi2sd

                auto reg_xmm_tmp = make_unique<ir::Register>(ir::Reg(ir::RegType::XMM7));

 	            ir_func->instructions.push_back(make_unique<ir::CvtSI2SDInstruction>(move(src), move(reg_xmm_tmp)));

   	          ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(make_unique<ir::Register>(ir::Reg(ir::RegType::XMM7)), move(dst)));

            }

            // Case 1c: Source is a float-like immediate

            else if (src_is_imm && (src_is_imm->value.find('.') != std::string::npos ||

                                    src_is_imm->value.find('e') != std::string::npos ||

                                    src_is_imm->value.find('E') != std::string::npos))

            {

                // imm(float) -> mem(double)

                ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(move(src), move(dst)));

            }

            // Case 1d: Source is an int immediate (e.g. double d = 10;)

            else if (src_is_imm)

            {

                // imm(int) -> mem(double)

                // We must load int to a temp reg, then convert

                auto reg_r10_tmp = make_unique<ir::Register>(ir::Reg(ir::RegType::R10));

                auto reg_xmm_tmp = make_unique<ir::Register>(ir::Reg(ir::RegType::XMM7));

                ir_func->instructions.push_back(make_unique<ir::MovInstruction>(move(src), move(reg_r10_tmp)));

   	          ir_func->instructions.push_back(make_unique<ir::CvtSI2SDInstruction>(make_unique<ir::Register>(ir::Reg(ir::RegType::R10)), move(reg_xmm_tmp)));

   	          ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(make_unique<ir::Register>(ir::Reg(ir::RegType::XMM7)), move(dst)));

            }

            else 

            {

                // This handles reg->mem, which is fine

                ir_func->instructions.push_back(make_unique<ir::MovSDInstruction>(move(src), move(dst)));

            }

        }

        // Case 2: Destination is an int/char (size 4)

        else

        {

       	    // This is the original integer-copy logic, which is correct

       	    // (e.g., int-to-int, or char-to-int)

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

        }

        return;

    }

    // --- END NEW LOGIC ---
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
