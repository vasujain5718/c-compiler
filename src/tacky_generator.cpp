#include "tacky_generator.h"
#include <stdexcept>
#include <string> 

using std::unique_ptr;
using std::make_unique;
using std::vector;
using std::move;
using std::string;
using std::runtime_error;
using std::to_string;

unique_ptr<tacky::Program> TackyGenerator::generate(const FunctionAST* ast) {
    auto tacky_program = make_unique<tacky::Program>();
    tacky_program->function = make_unique<tacky::Function>();
    tacky_program->function->name = ast->Name;

    next_var_id = 0; 
    next_label_id = 0;

    // --- NEW: Handle function body as a list of items ---
    for (const auto& item : ast->Body) {
        generate_block_item(item.get(), tacky_program->function->body);
    }

    // --- NEW: Implicit return zero for main ---
    // If the last instruction wasn't a return, and it's 'main', add 'return 0'
    bool has_returned = false;
    if (!tacky_program->function->body.empty()) {
        if (dynamic_cast<tacky::ReturnInstruction*>(tacky_program->function->body.back().get())) {
            has_returned = true;
        }
    }

    if (!has_returned && ast->Name == "main") {
        tacky_program->function->body.push_back(make_unique<tacky::ReturnInstruction>(make_unique<tacky::Constant>("0")));
    }
    // ---------------------------------------------------

    return tacky_program;
}

void TackyGenerator::generate_block_item(const BlockItemAST* item, vector<unique_ptr<tacky::Instruction>>& instructions) {
    if (auto* decl = dynamic_cast<const DeclarationAST*>(item)) {
        generate_declaration(decl, instructions);
    } else if (auto* stmt = dynamic_cast<const StatementAST*>(item)) {
        generate_statement(stmt, instructions);
    } else {
        throw runtime_error("Unknown block item type in TackyGenerator.");
    }
}

void TackyGenerator::generate_declaration(const DeclarationAST* decl, vector<unique_ptr<tacky::Instruction>>& instructions) {
    // If there is an initializer, treat it exactly like an assignment: x = init_expr
    if (decl->InitExpr) {
        auto init_val = generate_expression(decl->InitExpr.get(), instructions);
        auto var_dst = make_unique<tacky::Var>(decl->VarName);
        instructions.push_back(make_unique<tacky::CopyInstruction>(move(init_val), move(var_dst)));
    }
    // If no initializer, we do nothing in TACKY. Space is reserved later.
}

void TackyGenerator::generate_statement(const StatementAST* stmt, vector<unique_ptr<tacky::Instruction>>& instructions) {
    if (auto* ret_stmt = dynamic_cast<const ReturnStatementAST*>(stmt)) {
        auto return_val = generate_expression(ret_stmt->Expression.get(), instructions);
        instructions.push_back(make_unique<tacky::ReturnInstruction>(move(return_val)));
    } 
    else if (auto* if_stmt = dynamic_cast<const IfStatementAST*>(stmt)) {
    // Evaluate condition
    auto c = generate_expression(if_stmt->Condition.get(), instructions);

    if (if_stmt->ElseBranch) {
        // if (c) then S1 else S2
        string else_label = make_label();
        string end_label  = make_label();

        instructions.push_back(make_unique<tacky::JumpIfZeroInstruction>(std::move(c), else_label));
        generate_statement(if_stmt->ThenBranch.get(), instructions);
        instructions.push_back(make_unique<tacky::JumpInstruction>(end_label));
        instructions.push_back(make_unique<tacky::LabelInstruction>(else_label));
        generate_statement(if_stmt->ElseBranch.get(), instructions);
        instructions.push_back(make_unique<tacky::LabelInstruction>(end_label));
    } else {
        // if (c) then S
        string end_label = make_label();

        instructions.push_back(make_unique<tacky::JumpIfZeroInstruction>(std::move(c), end_label));
        generate_statement(if_stmt->ThenBranch.get(), instructions);
        instructions.push_back(make_unique<tacky::LabelInstruction>(end_label));
    }
}

    else if (auto* expr_stmt = dynamic_cast<const ExpressionStatementAST*>(stmt)) {
        // Generate the expression for its side effects (like assignment), but ignore the result.
        generate_expression(expr_stmt->Expression.get(), instructions);
    }
    else if (dynamic_cast<const NullStatementAST*>(stmt)) {
        // Do nothing for a null statement (';')
    }
    else {
        throw runtime_error("Unsupported statement type in TACKY generator");
    }
}

unique_ptr<tacky::Value> TackyGenerator::generate_expression(const ExprAST* expr, vector<unique_ptr<tacky::Instruction>>& instructions) {
    if (auto* const_expr = dynamic_cast<const ConstantExprAST*>(expr)) {
        // Note: using the string Val directly here.
        return make_unique<tacky::Constant>(const_expr->Val);
    }
    else if (auto* cexpr = dynamic_cast<const ConditionalExprAST*>(expr)) {
    // <instructions for condition>
    auto c = generate_expression(cexpr->Condition.get(), instructions);

    string e2_label = make_label();
    string end_label = make_label();
    auto result = make_temporary();  // destination to hold the overall expression result

    // JumpIfZero(c, e2_label)
    instructions.push_back(make_unique<tacky::JumpIfZeroInstruction>(std::move(c), e2_label));

    // <instructions to calculate e1>
    auto v1 = generate_expression(cexpr->ThenExpr.get(), instructions);
    // result = v1
    instructions.push_back(make_unique<tacky::CopyInstruction>(std::move(v1), make_unique<tacky::Var>(result->name)));
    // Jump(end)
    instructions.push_back(make_unique<tacky::JumpInstruction>(end_label));

    // Label(e2_label)
    instructions.push_back(make_unique<tacky::LabelInstruction>(e2_label));

    // <instructions to calculate e2>
    auto v2 = generate_expression(cexpr->ElseExpr.get(), instructions);
    // result = v2
    instructions.push_back(make_unique<tacky::CopyInstruction>(std::move(v2), make_unique<tacky::Var>(result->name)));

    // Label(end)
    instructions.push_back(make_unique<tacky::LabelInstruction>(end_label));

    return result; // the value of the conditional expression
}

    else if (auto* var_expr = dynamic_cast<const VarExprAST*>(expr)) {
        return make_unique<tacky::Var>(var_expr->Name);
    }
    else if (auto* assign_expr = dynamic_cast<const AssignmentExprAST*>(expr)) {
        return generate_assignment(assign_expr, instructions);
    }

    // ... (Rest of the function for Unary and Binary remains the same as before)
    // I will include it here for completeness so you can copy-paste the whole file.
    if (auto* unary_expr = dynamic_cast<const UnaryExprAST*>(expr)) {
        auto src_val = generate_expression(unary_expr->Operand.get(), instructions);
        auto dst_var = make_temporary();
        tacky::UnaryOperatorType tacky_op;
        if (unary_expr->Op.type == TokenType::TOKEN_OPERATOR_MINUS) { tacky_op = tacky::UnaryOperatorType::Negate; }
        else if (unary_expr->Op.type == TokenType::TOKEN_OPERATOR_BITWISE_COMPLEMENT) { tacky_op = tacky::UnaryOperatorType::Complement; }
        else if (unary_expr->Op.type == TokenType::TOKEN_OPERATOR_LOGICAL_NEG) { tacky_op = tacky::UnaryOperatorType::LogicalNot; }
        else { throw runtime_error("Unknown unary operator."); }
        instructions.push_back(make_unique<tacky::UnaryInstruction>(tacky_op, move(src_val), make_unique<tacky::Var>(dst_var->name)));
        return dst_var;
    }
    if(auto * binary_expr = dynamic_cast<const BinaryExprAST*>(expr)) {
        // ... (Include the full binary operator logic from Chapter 4 here) ...
        // For brevity in this response, I'm assuming you keep the Chapter 4 logic.
        // If you need me to repost the FULL Chapter 4 binary logic merged with this, let me know.
        // I will put a placeholder here for now to keep the response focused on Chapter 5 changes.
        // YOU MUST KEEP YOUR CHAPTER 4 BINARY LOGIC HERE.
         if (binary_expr->Op.type == TokenType::TOKEN_OPERATOR_LOGICAL_AND) {
             // ... keep your && logic ...
             string false_label = make_label();
             string end_label = make_label();
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
        }
        // ... (include || logic) ...
         else if (binary_expr->Op.type == TokenType::TOKEN_OPERATOR_LOGICAL_OR) {
             string true_label = make_label();
             string end_label = make_label();
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

        auto lhs_val = generate_expression(binary_expr->LHS.get(), instructions);
        auto rhs_val = generate_expression(binary_expr->RHS.get(), instructions);
        auto dst_var = make_temporary();
        tacky::BinaryOperatorType tacky_op;
        switch (binary_expr->Op.type) {
            case TokenType::TOKEN_OPERATOR_PLUS: tacky_op = tacky::BinaryOperatorType::Add; break;
            case TokenType::TOKEN_OPERATOR_MINUS: tacky_op = tacky::BinaryOperatorType::Subtract; break;
            case TokenType::TOKEN_OPERATOR_MULTIPLY: tacky_op = tacky::BinaryOperatorType::Multiply; break;
            case TokenType::TOKEN_OPERATOR_DIVIDE: tacky_op = tacky::BinaryOperatorType::Divide; break;
            case TokenType::TOKEN_OPERATOR_MODULO: tacky_op = tacky::BinaryOperatorType::Modulo; break;
            case TokenType::TOKEN_OPERATOR_EQUAL: tacky_op = tacky::BinaryOperatorType::Equal; break;
            case TokenType::TOKEN_OPERATOR_NOT_EQUAL: tacky_op = tacky::BinaryOperatorType::NotEqual; break;
            case TokenType::TOKEN_OPERATOR_LESS: tacky_op = tacky::BinaryOperatorType::LessThan; break;
            case TokenType::TOKEN_OPERATOR_LESS_EQUAL: tacky_op = tacky::BinaryOperatorType::LessOrEqual; break;
            case TokenType::TOKEN_OPERATOR_GREATER: tacky_op = tacky::BinaryOperatorType::GreaterThan; break;
            case TokenType::TOKEN_OPERATOR_GREATER_EQUAL: tacky_op = tacky::BinaryOperatorType::GreaterOrEqual; break;
            default: throw runtime_error("Unknown binary operator type in TACKY generator.");
        }
        instructions.push_back(make_unique<tacky::BinaryInstruction>(tacky_op, move(lhs_val), move(rhs_val), make_unique<tacky::Var>(dst_var->name)));
        return dst_var;
    }
    
    throw runtime_error("Unsupported expression type in TACKY generator");
}

// New helper for assignments
unique_ptr<tacky::Value> TackyGenerator::generate_assignment(const AssignmentExprAST* assign_expr, vector<unique_ptr<tacky::Instruction>>& instructions) {
    // 1. Generate TACKY for the RHS
    auto rhs_result = generate_expression(assign_expr->RHS.get(), instructions);
    
    // 2. Get the variable name from the LHS (Semantic Analysis guarantees this is a VarExprAST)
    auto* lhs_var = dynamic_cast<const VarExprAST*>(assign_expr->LHS.get());
    if (!lhs_var) throw runtime_error("Internal Error: LHS of assignment is not a variable.");

    // 3. Create a Copy instruction: Var(lhs_name) = rhs_result
    // We need two copies of the Var because we use one in the instruction and return the other.
    instructions.push_back(make_unique<tacky::CopyInstruction>(move(rhs_result), make_unique<tacky::Var>(lhs_var->Name)));
    
    // 4. Return the variable as the result of the expression
    return make_unique<tacky::Var>(lhs_var->Name);
}

unique_ptr<tacky::Var> TackyGenerator::make_temporary() {
    return make_unique<tacky::Var>("t" + to_string(next_var_id++));
}

string TackyGenerator::make_label() {
    return ".L" + to_string(next_label_id++);
}