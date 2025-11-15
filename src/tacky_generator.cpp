#include "tacky_generator.h"
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <map>
#include <algorithm>

using std::make_unique;
using std::move;
using std::runtime_error;
using std::string;
using std::to_string;
using std::unique_ptr;
using std::vector;

// File-local map used during a single generate() run to look up variable types
// keyed by the unique variable name (DeclarationAST::VarName after semantic pass).
static std::unordered_map<std::string, tacky::TypeKind> g_var_types;



// Helper: convert decl type string to tacky::TypeKind
// Helper: convert SimpleType (from AST) to tacky::TypeKind
static tacky::TypeKind type_from_simpletype(SimpleType t)
{
    switch (t) {
        case SimpleType::DOUBLE: return tacky::TypeKind::Double;
        case SimpleType::FLOAT:  return tacky::TypeKind::Float;
        case SimpleType::INT:    return tacky::TypeKind::Int;
        case SimpleType::CHAR:   return tacky::TypeKind::Int; // char treated as int
        default:                 return tacky::TypeKind::Int;
    }
}


// Helper: collect declarations (recursive) to populate g_var_types
static void collect_var_types_from_block(const std::vector<std::unique_ptr<BlockItemAST>> &items)
{
    for (const auto &item : items)
    {
        if (auto *decl = dynamic_cast<const DeclarationAST *>(item.get()))
        {
            // DeclarationAST::VarName was rewritten by semantic pass to the unique name.
            g_var_types[decl->VarName] = type_from_simpletype(decl->DeclType);
            continue;
        }
        if (auto *stmt = dynamic_cast<const StatementAST *>(item.get()))
        {
            // Recurse into compound statements and for-inits which may contain decls
            if (auto *comp = dynamic_cast<const CompoundStatementAST *>(stmt))
            {
                collect_var_types_from_block(comp->body_->Items);
            }
            else if (auto *f = dynamic_cast<const ForStatementAST *>(stmt))
            {
                // for-init might be a declaration
                if (f->Init)
                {
                    if (auto *dinit = dynamic_cast<const ForInitDeclAST *>(f->Init.get()))
                    {
                        if (dinit->InitDecl)
                        {
                            g_var_types[dinit->InitDecl->VarName] = type_from_simpletype(dinit->InitDecl->DeclType);
                        }
                    }
                }
                // Recurse into body as well
                if (auto *compbody = dynamic_cast<const CompoundStatementAST *>(f->Body.get()))
                {
                    collect_var_types_from_block(compbody->body_->Items);
                }
                else
                {
                    // body can be any statement type - if it's a compound, we already handled; otherwise,
                    // it may contain nested compounds; we conservatively do nothing here (top-level scan will find nested blocks).
                }
            }
            else if (auto *dw = dynamic_cast<const DoWhileStatementAST *>(stmt))
            {
                if (auto *compbody = dynamic_cast<const CompoundStatementAST *>(dw->Body.get()))
                {
                    collect_var_types_from_block(compbody->body_->Items);
                }
            }
            else if (auto *w = dynamic_cast<const WhileStatementAST *>(stmt))
            {
                if (auto *compbody = dynamic_cast<const CompoundStatementAST *>(w->Body.get()))
                {
                    collect_var_types_from_block(compbody->body_->Items);
                }
            }
            else if (auto *ifs = dynamic_cast<const IfStatementAST *>(stmt))
            {
                if (auto *compthen = dynamic_cast<const CompoundStatementAST *>(ifs->ThenBranch.get()))
                    collect_var_types_from_block(compthen->body_->Items);
                if (ifs->ElseBranch)
                {
                    if (auto *compelse = dynamic_cast<const CompoundStatementAST *>(ifs->ElseBranch.get()))
                        collect_var_types_from_block(compelse->body_->Items);
                }
            }
        }
    }
}

// Helper: decide promoted kind for arithmetic given two operands.
// Simple rule: if either is Double or Float -> Double, else Int.
static tacky::TypeKind promote_arith_kind(tacky::TypeKind a, tacky::TypeKind b)
{
    if (a == tacky::TypeKind::Double || b == tacky::TypeKind::Double)
        return tacky::TypeKind::Double;
    if (a == tacky::TypeKind::Float || b == tacky::TypeKind::Float)
        return tacky::TypeKind::Double; // promote float -> double for arithmetic
    return tacky::TypeKind::Int;
}
// put this near top of the file (file-local helper)
static tacky::TypeKind infer_kind_from_expr(const ExprAST* e) {
    if (!e) return tacky::TypeKind::Int;
    if (auto ce = dynamic_cast<const ConstantExprAST*>(e)) {
        const std::string& lit = ce->Val;
        if (lit.find('.') != std::string::npos || lit.find('e') != std::string::npos || lit.find('E') != std::string::npos)
            return tacky::TypeKind::Double;
        return tacky::TypeKind::Int;
    }
    if (auto ve = dynamic_cast<const VarExprAST*>(e)) {
        auto it = g_var_types.find(ve->Name);
        if (it != g_var_types.end()) return it->second;
        return tacky::TypeKind::Int;
    }
    if (auto ue = dynamic_cast<const UnaryExprAST*>(e)) {
        return infer_kind_from_expr(ue->Operand.get());
    }
    if (auto be = dynamic_cast<const BinaryExprAST*>(e)) {
        auto k1 = infer_kind_from_expr(be->LHS.get());
        auto k2 = infer_kind_from_expr(be->RHS.get());
        return promote_arith_kind(k1, k2);
    }
    if (auto ce2 = dynamic_cast<const ConditionalExprAST*>(e)) {
        auto k1 = infer_kind_from_expr(ce2->ThenExpr.get());
        auto k2 = infer_kind_from_expr(ce2->ElseExpr.get());
        return promote_arith_kind(k1, k2);
    }
    // fallback
    return tacky::TypeKind::Int;
}


unique_ptr<tacky::Program> TackyGenerator::generate(const FunctionAST *ast)
{
    // Prepare global var type map for this generate run
    g_var_types.clear();
    collect_var_types_from_block(ast->Body);

    auto tacky_program = make_unique<tacky::Program>();
    tacky_program->function = make_unique<tacky::Function>();
    tacky_program->function->name = ast->Name;

    next_var_id = 0;
    next_label_id = 0;
    loop_label_stack_.clear();

    for (const auto &item : ast->Body)
    {
        generate_block_item(item.get(), tacky_program->function->body);
    }

    // Implicit "return 0" for main if not already returned
    bool has_returned = false;
    if (!tacky_program->function->body.empty())
    {
        if (dynamic_cast<tacky::ReturnInstruction *>(tacky_program->function->body.back().get()))
            has_returned = true;
    }
    if (!has_returned && ast->Name == "main")
    {
        tacky_program->function->body.push_back(
            make_unique<tacky::ReturnInstruction>(make_unique<tacky::Constant>(tacky::TypeKind::Int, "0")));
    }

    return tacky_program;
}

void TackyGenerator::generate_block_item(
    const BlockItemAST *item,
    vector<unique_ptr<tacky::Instruction>> &instructions)
{
    if (auto *decl = dynamic_cast<const DeclarationAST *>(item))
    {
        generate_declaration(decl, instructions);
    }
    else if (auto *stmt = dynamic_cast<const StatementAST *>(item))
    {
        generate_statement(stmt, instructions);
    }
    else
    {
        throw runtime_error("Unknown block item type in TackyGenerator.");
    }
}

void TackyGenerator::generate_declaration(
    const DeclarationAST *decl,
    vector<unique_ptr<tacky::Instruction>> &instructions)
{
    if (decl->InitExpr)
    {
        auto init_val = generate_expression(decl->InitExpr.get(), instructions);
        // destination var should carry the declared type
        tacky::TypeKind dst_kind = type_from_simpletype(decl->DeclType);
        auto var_dst = make_unique<tacky::Var>(dst_kind, decl->VarName);
        instructions.push_back(make_unique<tacky::CopyInstruction>(move(init_val), move(var_dst)));
    }
    else
    {
        // no init -> nothing to emit (storage assumed)
    }
}

void TackyGenerator::generate_statement(
    const StatementAST *stmt,
    vector<unique_ptr<tacky::Instruction>> &instructions)
{
    // return
    if (auto *ret_stmt = dynamic_cast<const ReturnStatementAST *>(stmt))
    {
        auto return_val = generate_expression(ret_stmt->Expression.get(), instructions);
        instructions.push_back(make_unique<tacky::ReturnInstruction>(move(return_val)));
        return;
    }

    // if / else
    if (auto *if_stmt = dynamic_cast<const IfStatementAST *>(stmt))
    {
        auto c = generate_expression(if_stmt->Condition.get(), instructions);
        if (if_stmt->ElseBranch)
        {
            string else_label = make_label();
            string end_label = make_label();

            instructions.push_back(make_unique<tacky::JumpIfZeroInstruction>(move(c), else_label));
            generate_statement(if_stmt->ThenBranch.get(), instructions);
            instructions.push_back(make_unique<tacky::JumpInstruction>(end_label));
            instructions.push_back(make_unique<tacky::LabelInstruction>(else_label));
            generate_statement(if_stmt->ElseBranch.get(), instructions);
            instructions.push_back(make_unique<tacky::LabelInstruction>(end_label));
        }
        else
        {
            string end_label = make_label();
            instructions.push_back(make_unique<tacky::JumpIfZeroInstruction>(move(c), end_label));
            generate_statement(if_stmt->ThenBranch.get(), instructions);
            instructions.push_back(make_unique<tacky::LabelInstruction>(end_label));
        }
        return;
    }

    // compound block
    if (auto *comp = dynamic_cast<const CompoundStatementAST *>(stmt))
    {
        for (const auto &it : comp->body_->Items)
        {
            generate_block_item(it.get(), instructions);
        }
        return;
    }

    // simple expr / null
    if (auto *expr_stmt = dynamic_cast<const ExpressionStatementAST *>(stmt))
    {
        generate_expression(expr_stmt->Expression.get(), instructions);
        return;
    }
    if (dynamic_cast<const NullStatementAST *>(stmt))
        return;

    // ---------- Chapter 8: Loops & break/continue ----------

    // while (<cond>) <body>
    if (auto *w = dynamic_cast<const WhileStatementAST *>(stmt))
    {
        string continue_label = make_label(); // start of loop / also continue target
        string break_label = make_label();    // end of loop

        loop_label_stack_.emplace_back(continue_label, break_label);

        instructions.push_back(make_unique<tacky::LabelInstruction>(continue_label));
        auto cond_val = generate_expression(w->Condition.get(), instructions);
        instructions.push_back(make_unique<tacky::JumpIfZeroInstruction>(move(cond_val), break_label));
        generate_statement(w->Body.get(), instructions);
        instructions.push_back(make_unique<tacky::JumpInstruction>(continue_label));
        instructions.push_back(make_unique<tacky::LabelInstruction>(break_label));

        loop_label_stack_.pop_back();
        return;
    }

    // do <body> while (<cond>);
    if (auto *dw = dynamic_cast<const DoWhileStatementAST *>(stmt))
    {
        string start_label = make_label();    // start label
        string continue_label = make_label(); // between body and condition
        string break_label = make_label();    // end

        loop_label_stack_.emplace_back(continue_label, break_label);

        instructions.push_back(make_unique<tacky::LabelInstruction>(start_label));
        generate_statement(dw->Body.get(), instructions);
        instructions.push_back(make_unique<tacky::LabelInstruction>(continue_label));
        auto cond_val = generate_expression(dw->Condition.get(), instructions);
        instructions.push_back(make_unique<tacky::JumpIfNotZeroInstruction>(move(cond_val), start_label));
        instructions.push_back(make_unique<tacky::LabelInstruction>(break_label));

        loop_label_stack_.pop_back();
        return;
    }

    // for (<init>; <cond>; <post>) <body>
    if (auto *f = dynamic_cast<const ForStatementAST *>(stmt))
    {
        string start_label = make_label();
        string continue_label = make_label();
        string break_label = make_label();

        // init
        gen_for_init(f->Init.get(), instructions);

        loop_label_stack_.emplace_back(continue_label, break_label);

        // start / condition
        instructions.push_back(make_unique<tacky::LabelInstruction>(start_label));
        if (f->Condition)
        {
            auto c = generate_expression(f->Condition.get(), instructions);
            instructions.push_back(make_unique<tacky::JumpIfZeroInstruction>(move(c), break_label));
        }
        // body
        generate_statement(f->Body.get(), instructions);

        // continue: post
        instructions.push_back(make_unique<tacky::LabelInstruction>(continue_label));
        if (f->Increment)
        {
            // evaluate for side effects; ignore result
            (void)generate_expression(f->Increment.get(), instructions);
        }

        // back to start
        instructions.push_back(make_unique<tacky::JumpInstruction>(start_label));
        instructions.push_back(make_unique<tacky::LabelInstruction>(break_label));

        loop_label_stack_.pop_back();
        return;
    }

    // break / continue
    if (dynamic_cast<const BreakStatementAST *>(stmt))
    {
        if (loop_label_stack_.empty())
        {
            throw runtime_error("Internal Error: 'break' not inside a loop when generating TACKY.");
        }
        const auto &top = loop_label_stack_.back();
        instructions.push_back(make_unique<tacky::JumpInstruction>(top.second)); // break_label
        return;
    }
    if (dynamic_cast<const ContinueStatementAST *>(stmt))
    {
        if (loop_label_stack_.empty())
        {
            throw runtime_error("Internal Error: 'continue' not inside a loop when generating TACKY.");
        }
        const auto &top = loop_label_stack_.back();
        instructions.push_back(make_unique<tacky::JumpInstruction>(top.first)); // continue_label
        return;
    }

    // -------------------------------------------------------

    throw runtime_error("Unsupported statement type in TACKY generator");
}

unique_ptr<tacky::Value> TackyGenerator::generate_expression(
    const ExprAST *expr,
    vector<unique_ptr<tacky::Instruction>> &instructions)
{
    if (auto *const_expr = dynamic_cast<const ConstantExprAST *>(expr))
    {
        // type constants: if contains '.' or 'e'/'E' -> double, else int
        const string &lit = const_expr->Val;
        bool is_float = (lit.find('.') != string::npos) || (lit.find('e') != string::npos) || (lit.find('E') != string::npos);
        if (is_float)
        {
            return make_unique<tacky::Constant>(tacky::TypeKind::Double, lit);
        }
        else
        {
            return make_unique<tacky::Constant>(tacky::TypeKind::Int, lit);
        }
    }
    else if (auto* cexpr = dynamic_cast<const ConditionalExprAST*>(expr)) {
    // Evaluate condition first (must not evaluate branches unless needed)
    auto c = generate_expression(cexpr->Condition.get(), instructions);

    string else_label = make_label();
    string end_label = make_label();

    // infer kinds without evaluating branches (no side-effects)
    tacky::TypeKind then_kind = infer_kind_from_expr(cexpr->ThenExpr.get());
    tacky::TypeKind else_kind = infer_kind_from_expr(cexpr->ElseExpr.get());
    tacky::TypeKind result_kind = promote_arith_kind(then_kind, else_kind);

    auto result = make_temporary(result_kind);

    // Jump to else if condition is zero
    instructions.push_back(make_unique<tacky::JumpIfZeroInstruction>(move(c), else_label));

    // Then branch: generate and copy into result
    auto v_then = generate_expression(cexpr->ThenExpr.get(), instructions);
    instructions.push_back(make_unique<tacky::CopyInstruction>(move(v_then), make_unique<tacky::Var>(result->type, result->name)));
    instructions.push_back(make_unique<tacky::JumpInstruction>(end_label));

    // Else branch
    instructions.push_back(make_unique<tacky::LabelInstruction>(else_label));
    auto v_else = generate_expression(cexpr->ElseExpr.get(), instructions);
    instructions.push_back(make_unique<tacky::CopyInstruction>(move(v_else), make_unique<tacky::Var>(result->type, result->name)));

    // End
    instructions.push_back(make_unique<tacky::LabelInstruction>(end_label));
    return result;
}

    else if (auto *var_expr = dynamic_cast<const VarExprAST *>(expr))
    {
        // Look up variable type (unique name) from g_var_types
        auto it = g_var_types.find(var_expr->Name);
        tacky::TypeKind kind = (it != g_var_types.end()) ? it->second : tacky::TypeKind::Int;
        return make_unique<tacky::Var>(kind, var_expr->Name);
    }
    else if (auto *assign_expr = dynamic_cast<const AssignmentExprAST *>(expr))
    {
        return generate_assignment(assign_expr, instructions);
    }
    else if (auto *unary_expr = dynamic_cast<const UnaryExprAST *>(expr))
    {
        auto src_val = generate_expression(unary_expr->Operand.get(), instructions);
        // Determine destination kind
        tacky::TypeKind dst_kind = src_val->type;
        if (unary_expr->Op.type == TokenType::TOKEN_OPERATOR_LOGICAL_NEG)
        {
            // '!' yields int
            dst_kind = tacky::TypeKind::Int;
        }
        else if (unary_expr->Op.type == TokenType::TOKEN_OPERATOR_BITWISE_COMPLEMENT)
        {
            // '~' requires int operand
            if (src_val->type != tacky::TypeKind::Int)
                throw runtime_error("Internal Error: bitwise complement operand not integer in TACKY gen");
            dst_kind = tacky::TypeKind::Int;
        }
        auto dst_var = make_temporary(dst_kind);

        tacky::UnaryOperatorType op;
        if (unary_expr->Op.type == TokenType::TOKEN_OPERATOR_MINUS)
        {
            op = tacky::UnaryOperatorType::Negate;
        }
        else if (unary_expr->Op.type == TokenType::TOKEN_OPERATOR_BITWISE_COMPLEMENT)
        {
            op = tacky::UnaryOperatorType::Complement;
        }
        else if (unary_expr->Op.type == TokenType::TOKEN_OPERATOR_LOGICAL_NEG)
        {
            op = tacky::UnaryOperatorType::LogicalNot;
        }
        else
        {
            throw runtime_error("Unknown unary operator.");
        }

        instructions.push_back(make_unique<tacky::UnaryInstruction>(op, move(src_val), make_unique<tacky::Var>(dst_var->type, dst_var->name)));
        return dst_var;
    }
    else if (auto *binary_expr = dynamic_cast<const BinaryExprAST *>(expr))
    {
        // short-circuit logical ops
        if (binary_expr->Op.type == TokenType::TOKEN_OPERATOR_LOGICAL_AND)
        {
            string false_label = make_label();
            string end_label = make_label();
            auto v1 = generate_expression(binary_expr->LHS.get(), instructions);
            instructions.push_back(make_unique<tacky::JumpIfZeroInstruction>(move(v1), false_label));
            auto v2 = generate_expression(binary_expr->RHS.get(), instructions);
            instructions.push_back(make_unique<tacky::JumpIfZeroInstruction>(move(v2), false_label));
            auto result_var = make_temporary(tacky::TypeKind::Int);
            instructions.push_back(make_unique<tacky::CopyInstruction>(make_unique<tacky::Constant>(tacky::TypeKind::Int, "1"), make_unique<tacky::Var>(result_var->type, result_var->name)));
            instructions.push_back(make_unique<tacky::JumpInstruction>(end_label));
            instructions.push_back(make_unique<tacky::LabelInstruction>(false_label));
            instructions.push_back(make_unique<tacky::CopyInstruction>(make_unique<tacky::Constant>(tacky::TypeKind::Int, "0"), make_unique<tacky::Var>(result_var->type, result_var->name)));
            instructions.push_back(make_unique<tacky::LabelInstruction>(end_label));
            return result_var;
        }
        else if (binary_expr->Op.type == TokenType::TOKEN_OPERATOR_LOGICAL_OR)
        {
            string true_label = make_label();
            string end_label = make_label();
            auto v1 = generate_expression(binary_expr->LHS.get(), instructions);
            instructions.push_back(make_unique<tacky::JumpIfNotZeroInstruction>(move(v1), true_label));
            auto v2 = generate_expression(binary_expr->RHS.get(), instructions);
            instructions.push_back(make_unique<tacky::JumpIfNotZeroInstruction>(move(v2), true_label));
            auto result_var = make_temporary(tacky::TypeKind::Int);
            instructions.push_back(make_unique<tacky::CopyInstruction>(make_unique<tacky::Constant>(tacky::TypeKind::Int, "0"), make_unique<tacky::Var>(result_var->type, result_var->name)));
            instructions.push_back(make_unique<tacky::JumpInstruction>(end_label));
            instructions.push_back(make_unique<tacky::LabelInstruction>(true_label));
            instructions.push_back(make_unique<tacky::CopyInstruction>(make_unique<tacky::Constant>(tacky::TypeKind::Int, "1"), make_unique<tacky::Var>(result_var->type, result_var->name)));
            instructions.push_back(make_unique<tacky::LabelInstruction>(end_label));
            return result_var;
        }

        auto lhs = generate_expression(binary_expr->LHS.get(), instructions);
        auto rhs = generate_expression(binary_expr->RHS.get(), instructions);

        // Determine destination kind
        tacky::TypeKind dst_kind;
        // For arithmetic ops: promote
        if (binary_expr->Op.type == TokenType::TOKEN_OPERATOR_PLUS ||
            binary_expr->Op.type == TokenType::TOKEN_OPERATOR_MINUS ||
            binary_expr->Op.type == TokenType::TOKEN_OPERATOR_MULTIPLY ||
            binary_expr->Op.type == TokenType::TOKEN_OPERATOR_DIVIDE)
        {
            dst_kind = promote_arith_kind(lhs->type, rhs->type);
        }
        else
        {
            // comparisons, equality, etc. -> produce int
            dst_kind = tacky::TypeKind::Int;
        }

        auto dst = make_temporary(dst_kind);

        tacky::BinaryOperatorType op;
        switch (binary_expr->Op.type)
        {
        case TokenType::TOKEN_OPERATOR_PLUS:
            op = tacky::BinaryOperatorType::Add;
            break;
        case TokenType::TOKEN_OPERATOR_MINUS:
            op = tacky::BinaryOperatorType::Subtract;
            break;
        case TokenType::TOKEN_OPERATOR_MULTIPLY:
            op = tacky::BinaryOperatorType::Multiply;
            break;
        case TokenType::TOKEN_OPERATOR_DIVIDE:
            op = tacky::BinaryOperatorType::Divide;
            break;
        case TokenType::TOKEN_OPERATOR_MODULO:
            op = tacky::BinaryOperatorType::Modulo;
            break;
        case TokenType::TOKEN_OPERATOR_EQUAL:
            op = tacky::BinaryOperatorType::Equal;
            break;
        case TokenType::TOKEN_OPERATOR_NOT_EQUAL:
            op = tacky::BinaryOperatorType::NotEqual;
            break;
        case TokenType::TOKEN_OPERATOR_LESS:
            op = tacky::BinaryOperatorType::LessThan;
            break;
        case TokenType::TOKEN_OPERATOR_LESS_EQUAL:
            op = tacky::BinaryOperatorType::LessOrEqual;
            break;
        case TokenType::TOKEN_OPERATOR_GREATER:
            op = tacky::BinaryOperatorType::GreaterThan;
            break;
        case TokenType::TOKEN_OPERATOR_GREATER_EQUAL:
            op = tacky::BinaryOperatorType::GreaterOrEqual;
            break;
        default:
            throw runtime_error("Unknown binary operator type in TACKY generator.");
        }

        // Enforce integer-only ops where necessary
        if (binary_expr->Op.type == TokenType::TOKEN_OPERATOR_MODULO)
        {
            if (lhs->type != tacky::TypeKind::Int || rhs->type != tacky::TypeKind::Int)
                throw runtime_error("Internal Error: '%' used on non-int values in TACKY gen");
        }

        if ((binary_expr->Op.type == TokenType::TOKEN_OPERATOR_LESS ||
             binary_expr->Op.type == TokenType::TOKEN_OPERATOR_LESS_EQUAL ||
             binary_expr->Op.type == TokenType::TOKEN_OPERATOR_GREATER ||
             binary_expr->Op.type == TokenType::TOKEN_OPERATOR_GREATER_EQUAL ||
             binary_expr->Op.type == TokenType::TOKEN_OPERATOR_EQUAL ||
             binary_expr->Op.type == TokenType::TOKEN_OPERATOR_NOT_EQUAL) &&
            (!(lhs->type == tacky::TypeKind::Int || lhs->type == tacky::TypeKind::Double || lhs->type == tacky::TypeKind::Float)))
        {
            throw runtime_error("Internal Error: comparison on non-numeric types in TACKY gen");
        }

        // emit instruction: create a new Var for the dst inside the instruction (keep dst unique to return)
        instructions.push_back(make_unique<tacky::BinaryInstruction>(op, move(lhs), move(rhs), make_unique<tacky::Var>(dst->type, dst->name)));
        return dst;
    }

    throw runtime_error("Unsupported expression type in TACKY generator");
}

unique_ptr<tacky::Value> TackyGenerator::generate_assignment(
    const AssignmentExprAST *assign_expr,
    vector<unique_ptr<tacky::Instruction>> &instructions)
{
    auto rhs_result = generate_expression(assign_expr->RHS.get(), instructions);
    auto *lhs_var = dynamic_cast<const VarExprAST *>(assign_expr->LHS.get());
    if (!lhs_var)
        throw runtime_error("Internal Error: LHS of assignment is not a variable.");

    // Look up declared type for LHS
    tacky::TypeKind lhs_kind = tacky::TypeKind::Int;
    auto it = g_var_types.find(lhs_var->Name);
    if (it != g_var_types.end())
        lhs_kind = it->second;

    instructions.push_back(make_unique<tacky::CopyInstruction>(move(rhs_result), make_unique<tacky::Var>(lhs_kind, lhs_var->Name)));
    return make_unique<tacky::Var>(lhs_kind, lhs_var->Name);
}

void TackyGenerator::gen_for_init(const ForInitAST *init,
                                  vector<unique_ptr<tacky::Instruction>> &instructions)
{
    if (!init)
        return;
    if (auto *e = dynamic_cast<const ForInitExprAST *>(init))
    {
        if (e->InitExpr)
            (void)generate_expression(e->InitExpr.get(), instructions);
        return;
    }
    if (auto *d = dynamic_cast<const ForInitDeclAST *>(init))
    {
        generate_declaration(d->InitDecl.get(), instructions);
        return;
    }
    throw runtime_error("Unknown for-init variant in TACKY generator.");
}

unique_ptr<tacky::Var> TackyGenerator::make_temporary(tacky::TypeKind kind)
{
    return make_unique<tacky::Var>(kind, "t" + to_string(next_var_id++));
}

string TackyGenerator::make_label()
{
    return ".L" + to_string(next_label_id++);
}
