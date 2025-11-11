#include "tacky_generator.h"
#include <stdexcept>
#include <string>

using std::make_unique;
using std::move;
using std::runtime_error;
using std::string;
using std::to_string;
using std::unique_ptr;
using std::vector;

unique_ptr<tacky::Program> TackyGenerator::generate(const FunctionAST* ast) {
    auto tacky_program = make_unique<tacky::Program>();
    tacky_program->function = make_unique<tacky::Function>();
    tacky_program->function->name = ast->Name;

    next_var_id = 0;
    next_label_id = 0;
    loop_label_stack_.clear();

    for (const auto& item : ast->Body) {
        generate_block_item(item.get(), tacky_program->function->body);
    }

    // Implicit "return 0" for main if not already returned
    bool has_returned = false;
    if (!tacky_program->function->body.empty()) {
        if (dynamic_cast<tacky::ReturnInstruction*>(tacky_program->function->body.back().get()))
            has_returned = true;
    }
    if (!has_returned && ast->Name == "main") {
        tacky_program->function->body.push_back(
            make_unique<tacky::ReturnInstruction>(make_unique<tacky::Constant>("0"))
        );
    }

    return tacky_program;
}

void TackyGenerator::generate_block_item(
    const BlockItemAST* item,
    vector<unique_ptr<tacky::Instruction>>& instructions
) {
    if (auto* decl = dynamic_cast<const DeclarationAST*>(item)) {
        generate_declaration(decl, instructions);
    } else if (auto* stmt = dynamic_cast<const StatementAST*>(item)) {
        generate_statement(stmt, instructions);
    } else {
        throw runtime_error("Unknown block item type in TackyGenerator.");
    }
}

void TackyGenerator::generate_declaration(
    const DeclarationAST* decl,
    vector<unique_ptr<tacky::Instruction>>& instructions
) {
    if (decl->InitExpr) {
        auto init_val = generate_expression(decl->InitExpr.get(), instructions);
        auto var_dst = make_unique<tacky::Var>(decl->VarName);
        instructions.push_back(make_unique<tacky::CopyInstruction>(move(init_val), move(var_dst)));
    }
}

void TackyGenerator::generate_statement(
    const StatementAST* stmt,
    vector<unique_ptr<tacky::Instruction>>& instructions
) {
    // return
    if (auto* ret_stmt = dynamic_cast<const ReturnStatementAST*>(stmt)) {
        auto return_val = generate_expression(ret_stmt->Expression.get(), instructions);
        instructions.push_back(make_unique<tacky::ReturnInstruction>(move(return_val)));
        return;
    }

    // if / else
    if (auto* if_stmt = dynamic_cast<const IfStatementAST*>(stmt)) {
        auto c = generate_expression(if_stmt->Condition.get(), instructions);
        if (if_stmt->ElseBranch) {
            string else_label = make_label();
            string end_label  = make_label();

            instructions.push_back(make_unique<tacky::JumpIfZeroInstruction>(move(c), else_label));
            generate_statement(if_stmt->ThenBranch.get(), instructions);
            instructions.push_back(make_unique<tacky::JumpInstruction>(end_label));
            instructions.push_back(make_unique<tacky::LabelInstruction>(else_label));
            generate_statement(if_stmt->ElseBranch.get(), instructions);
            instructions.push_back(make_unique<tacky::LabelInstruction>(end_label));
        } else {
            string end_label = make_label();
            instructions.push_back(make_unique<tacky::JumpIfZeroInstruction>(move(c), end_label));
            generate_statement(if_stmt->ThenBranch.get(), instructions);
            instructions.push_back(make_unique<tacky::LabelInstruction>(end_label));
        }
        return;
    }

    // compound block
    if (auto* comp = dynamic_cast<const CompoundStatementAST*>(stmt)) {
        for (const auto& it : comp->body_->Items) {
            generate_block_item(it.get(), instructions);
        }
        return;
    }

    // simple expr / null
    if (auto* expr_stmt = dynamic_cast<const ExpressionStatementAST*>(stmt)) {
        generate_expression(expr_stmt->Expression.get(), instructions);
        return;
    }
    if (dynamic_cast<const NullStatementAST*>(stmt)) return;

    // ---------- Chapter 8: Loops & break/continue ----------

    // while (<cond>) <body>
    if (auto* w = dynamic_cast<const WhileStatementAST*>(stmt)) {
        string continue_label = make_label(); // start of loop / also continue target
        string break_label    = make_label(); // end of loop

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
    if (auto* dw = dynamic_cast<const DoWhileStatementAST*>(stmt)) {
        string start_label    = make_label(); // start label
        string continue_label = make_label(); // between body and condition
        string break_label    = make_label(); // end

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
    if (auto* f = dynamic_cast<const ForStatementAST*>(stmt)) {
        string start_label    = make_label();
        string continue_label = make_label();
        string break_label    = make_label();

        // init
        gen_for_init(f->Init.get(), instructions);

        loop_label_stack_.emplace_back(continue_label, break_label);

        // start / condition
        instructions.push_back(make_unique<tacky::LabelInstruction>(start_label));
        if (f->Condition) {
            auto c = generate_expression(f->Condition.get(), instructions);
            instructions.push_back(make_unique<tacky::JumpIfZeroInstruction>(move(c), break_label));
        }
        // body
        generate_statement(f->Body.get(), instructions);

        // continue: post
        instructions.push_back(make_unique<tacky::LabelInstruction>(continue_label));
        if (f->Increment) {
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
    if (dynamic_cast<const BreakStatementAST*>(stmt)) {
        if (loop_label_stack_.empty()) {
            throw runtime_error("Internal Error: 'break' not inside a loop when generating TACKY.");
        }
        const auto& top = loop_label_stack_.back();
        instructions.push_back(make_unique<tacky::JumpInstruction>(top.second)); // break_label
        return;
    }
    if (dynamic_cast<const ContinueStatementAST*>(stmt)) {
        if (loop_label_stack_.empty()) {
            throw runtime_error("Internal Error: 'continue' not inside a loop when generating TACKY.");
        }
        const auto& top = loop_label_stack_.back();
        instructions.push_back(make_unique<tacky::JumpInstruction>(top.first)); // continue_label
        return;
    }

    // -------------------------------------------------------

    throw runtime_error("Unsupported statement type in TACKY generator");
}

unique_ptr<tacky::Value> TackyGenerator::generate_expression(
    const ExprAST* expr,
    vector<unique_ptr<tacky::Instruction>>& instructions
) {
    if (auto* const_expr = dynamic_cast<const ConstantExprAST*>(expr)) {
        return make_unique<tacky::Constant>(const_expr->Val);
    }
    else if (auto* cexpr = dynamic_cast<const ConditionalExprAST*>(expr)) {
        auto c = generate_expression(cexpr->Condition.get(), instructions);
        string e2_label = make_label();
        string end_label = make_label();
        auto result = make_temporary();

        instructions.push_back(make_unique<tacky::JumpIfZeroInstruction>(move(c), e2_label));
        auto v1 = generate_expression(cexpr->ThenExpr.get(), instructions);
        instructions.push_back(make_unique<tacky::CopyInstruction>(move(v1), make_unique<tacky::Var>(result->name)));
        instructions.push_back(make_unique<tacky::JumpInstruction>(end_label));
        instructions.push_back(make_unique<tacky::LabelInstruction>(e2_label));
        auto v2 = generate_expression(cexpr->ElseExpr.get(), instructions);
        instructions.push_back(make_unique<tacky::CopyInstruction>(move(v2), make_unique<tacky::Var>(result->name)));
        instructions.push_back(make_unique<tacky::LabelInstruction>(end_label));
        return result;
    }
    else if (auto* var_expr = dynamic_cast<const VarExprAST*>(expr)) {
        return make_unique<tacky::Var>(var_expr->Name);
    }
    else if (auto* assign_expr = dynamic_cast<const AssignmentExprAST*>(expr)) {
        return generate_assignment(assign_expr, instructions);
    }
    else if (auto* unary_expr = dynamic_cast<const UnaryExprAST*>(expr)) {
        auto src_val = generate_expression(unary_expr->Operand.get(), instructions);
        auto dst_var = make_temporary();
        tacky::UnaryOperatorType op;
        if (unary_expr->Op.type == TokenType::TOKEN_OPERATOR_MINUS) {
            op = tacky::UnaryOperatorType::Negate;
        } else if (unary_expr->Op.type == TokenType::TOKEN_OPERATOR_BITWISE_COMPLEMENT) {
            op = tacky::UnaryOperatorType::Complement;
        } else if (unary_expr->Op.type == TokenType::TOKEN_OPERATOR_LOGICAL_NEG) {
            op = tacky::UnaryOperatorType::LogicalNot;
        } else {
            throw runtime_error("Unknown unary operator.");
        }
        instructions.push_back(make_unique<tacky::UnaryInstruction>(op, move(src_val), make_unique<tacky::Var>(dst_var->name)));
        return dst_var;
    }
    else if (auto* binary_expr = dynamic_cast<const BinaryExprAST*>(expr)) {
        // Keep your existing Chapter-4/5 logic for &&, ||, comparisons, arithmetic, etc.
        if (binary_expr->Op.type == TokenType::TOKEN_OPERATOR_LOGICAL_AND) {
            string false_label = make_label();
            string end_label   = make_label();
            auto v1 = generate_expression(binary_expr->LHS.get(), instructions);
            instructions.push_back(make_unique<tacky::JumpIfZeroInstruction>(move(v1), false_label));
            auto v2 = generate_expression(binary_expr->RHS.get(), instructions);
            instructions.push_back(make_unique<tacky::JumpIfZeroInstruction>(move(v2), false_label));
            auto result_var = make_temporary();
            instructions.push_back(make_unique<tacky::CopyInstruction>(make_unique<tacky::Constant>("1"), make_unique<tacky::Var>(result_var->name)));
            instructions.push_back(make_unique<tacky::JumpInstruction>(end_label));
            instructions.push_back(make_unique<tacky::LabelInstruction>(false_label));
            instructions.push_back(make_unique<tacky::CopyInstruction>(make_unique<tacky::Constant>("0"), make_unique<tacky::Var>(result_var->name)));
            instructions.push_back(make_unique<tacky::LabelInstruction>(end_label));
            return result_var;
        } else if (binary_expr->Op.type == TokenType::TOKEN_OPERATOR_LOGICAL_OR) {
            string true_label = make_label();
            string end_label  = make_label();
            auto v1 = generate_expression(binary_expr->LHS.get(), instructions);
            instructions.push_back(make_unique<tacky::JumpIfNotZeroInstruction>(move(v1), true_label));
            auto v2 = generate_expression(binary_expr->RHS.get(), instructions);
            instructions.push_back(make_unique<tacky::JumpIfNotZeroInstruction>(move(v2), true_label));
            auto result_var = make_temporary();
            instructions.push_back(make_unique<tacky::CopyInstruction>(make_unique<tacky::Constant>("0"), make_unique<tacky::Var>(result_var->name)));
            instructions.push_back(make_unique<tacky::JumpInstruction>(end_label));
            instructions.push_back(make_unique<tacky::LabelInstruction>(true_label));
            instructions.push_back(make_unique<tacky::CopyInstruction>(make_unique<tacky::Constant>("1"), make_unique<tacky::Var>(result_var->name)));
            instructions.push_back(make_unique<tacky::LabelInstruction>(end_label));
            return result_var;
        }

        auto lhs = generate_expression(binary_expr->LHS.get(), instructions);
        auto rhs = generate_expression(binary_expr->RHS.get(), instructions);
        auto dst = make_temporary();

        tacky::BinaryOperatorType op;
        switch (binary_expr->Op.type) {
            case TokenType::TOKEN_OPERATOR_PLUS:           op = tacky::BinaryOperatorType::Add; break;
            case TokenType::TOKEN_OPERATOR_MINUS:          op = tacky::BinaryOperatorType::Subtract; break;
            case TokenType::TOKEN_OPERATOR_MULTIPLY:       op = tacky::BinaryOperatorType::Multiply; break;
            case TokenType::TOKEN_OPERATOR_DIVIDE:         op = tacky::BinaryOperatorType::Divide; break;
            case TokenType::TOKEN_OPERATOR_MODULO:         op = tacky::BinaryOperatorType::Modulo; break;
            case TokenType::TOKEN_OPERATOR_EQUAL:          op = tacky::BinaryOperatorType::Equal; break;
            case TokenType::TOKEN_OPERATOR_NOT_EQUAL:      op = tacky::BinaryOperatorType::NotEqual; break;
            case TokenType::TOKEN_OPERATOR_LESS:           op = tacky::BinaryOperatorType::LessThan; break;
            case TokenType::TOKEN_OPERATOR_LESS_EQUAL:     op = tacky::BinaryOperatorType::LessOrEqual; break;
            case TokenType::TOKEN_OPERATOR_GREATER:        op = tacky::BinaryOperatorType::GreaterThan; break;
            case TokenType::TOKEN_OPERATOR_GREATER_EQUAL:  op = tacky::BinaryOperatorType::GreaterOrEqual; break;
            default: throw runtime_error("Unknown binary operator type in TACKY generator.");
        }
        instructions.push_back(make_unique<tacky::BinaryInstruction>(op, move(lhs), move(rhs), make_unique<tacky::Var>(dst->name)));
        return dst;
    }

    throw runtime_error("Unsupported expression type in TACKY generator");
}

unique_ptr<tacky::Value> TackyGenerator::generate_assignment(
    const AssignmentExprAST* assign_expr,
    vector<unique_ptr<tacky::Instruction>>& instructions
) {
    auto rhs_result = generate_expression(assign_expr->RHS.get(), instructions);
    auto* lhs_var = dynamic_cast<const VarExprAST*>(assign_expr->LHS.get());
    if (!lhs_var) throw runtime_error("Internal Error: LHS of assignment is not a variable.");
    instructions.push_back(make_unique<tacky::CopyInstruction>(move(rhs_result), make_unique<tacky::Var>(lhs_var->Name)));
    return make_unique<tacky::Var>(lhs_var->Name);
}

void TackyGenerator::gen_for_init(const ForInitAST* init,
                                  vector<unique_ptr<tacky::Instruction>>& instructions) {
    if (!init) return;
    if (auto* e = dynamic_cast<const ForInitExprAST*>(init)) {
        if (e->InitExpr) (void)generate_expression(e->InitExpr.get(), instructions);
        return;
    }
    if (auto* d = dynamic_cast<const ForInitDeclAST*>(init)) {
        generate_declaration(d->InitDecl.get(), instructions);
        return;
    }
    throw runtime_error("Unknown for-init variant in TACKY generator.");
}

unique_ptr<tacky::Var> TackyGenerator::make_temporary() {
    return make_unique<tacky::Var>("t" + to_string(next_var_id++));
}

string TackyGenerator::make_label() {
    return ".L" + to_string(next_label_id++);
}
