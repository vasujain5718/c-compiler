#include "parser.h"
#include <iostream>
#include <memory>
#include <sstream>
#include <stdexcept>

using std::cerr;
using std::endl;
using std::make_unique;
using std::map;
using std::move;
using std::runtime_error;
using std::string;
using std::stringstream;
using std::unique_ptr;
using std::vector;

// Initialize the operator precedence table
const map<TokenType, int> Parser::OperatorPrecedence = {
    {TokenType::TOKEN_OPERATOR_ASSIGN, 1},
    {TokenType::TOKEN_OPERATOR_LOGICAL_OR, 5},
    {TokenType::TOKEN_OPERATOR_LOGICAL_AND, 10},
    {TokenType::TOKEN_OPERATOR_EQUAL, 30},
    {TokenType::TOKEN_OPERATOR_NOT_EQUAL, 30},
    {TokenType::TOKEN_OPERATOR_LESS, 35},
    {TokenType::TOKEN_OPERATOR_LESS_EQUAL, 35},
    {TokenType::TOKEN_OPERATOR_GREATER, 35},
    {TokenType::TOKEN_OPERATOR_GREATER_EQUAL, 35},
    {TokenType::TOKEN_OPERATOR_PLUS, 45},
    {TokenType::TOKEN_OPERATOR_MINUS, 45},
    {TokenType::TOKEN_OPERATOR_MULTIPLY, 50},
    {TokenType::TOKEN_OPERATOR_DIVIDE, 50},
    {TokenType::TOKEN_OPERATOR_MODULO, 50}
};

Parser::Parser(const vector<Token>& tokens) : tokens(tokens) {}

// --- Helper Functions ---
const Token& Parser::peek() { return tokens[current]; }

const Token& Parser::advance() {
    if (!is_at_end()) current++;
    return tokens[current - 1];
}

bool Parser::is_at_end() { return peek().type == TokenType::TOKEN_EOF; }

bool Parser::match(TokenType type) {
    if (is_at_end() || peek().type != type) {
        return false;
    }
    advance();
    return true;
}

runtime_error Parser::error(const Token& token, const string& message) {
    stringstream ss;
    ss << "[Line " << token.line << "] Error at '" << token.value << "': " << message;
    return runtime_error(ss.str());
}

int Parser::get_token_precedence() {
    auto it = OperatorPrecedence.find(peek().type);
    if (it != OperatorPrecedence.end()) return it->second;
    return -1;
}

// --- Main Parsing Logic ---
unique_ptr<FunctionAST> Parser::parse() {
    try {
        auto func_ast = parse_function();
        if (!is_at_end()) {
            throw error(peek(), "Extra tokens found after the end of the function.");
        }
        return func_ast;
    } catch (const runtime_error& e) {
        cerr << "Parse Error: " << e.what() << endl;
        return nullptr;
    }
}

unique_ptr<FunctionAST> Parser::parse_function() {
    if (!match(TokenType::TOKEN_KEYWORD_INT)) throw error(peek(), "Expected 'int' keyword.");
    if (peek().type != TokenType::TOKEN_IDENTIFIER) throw error(peek(), "Expected function name.");
    string name = advance().value;

    if (!match(TokenType::TOKEN_OPEN_PAREN)) throw error(peek(), "Expected '('.");
    if (peek().type == TokenType::TOKEN_KEYWORD_VOID) advance();
    if (!match(TokenType::TOKEN_CLOSE_PAREN)) throw error(peek(), "Expected ')'.");
    if (!match(TokenType::TOKEN_OPEN_BRACE)) throw error(peek(), "Expected '{'.");

    vector<unique_ptr<BlockItemAST>> body;
    while (peek().type != TokenType::TOKEN_CLOSE_BRACE) {
        body.push_back(parse_block_item());
    }
    if (!match(TokenType::TOKEN_CLOSE_BRACE)) throw error(peek(), "Expected '}'.");

    return make_unique<FunctionAST>(name, move(body));
}

unique_ptr<BlockItemAST> Parser::parse_block_item() {
    if (peek().type == TokenType::TOKEN_KEYWORD_INT) {
        return parse_declaration();
    }
    return parse_statement();
}

unique_ptr<DeclarationAST> Parser::parse_declaration() {
    match(TokenType::TOKEN_KEYWORD_INT); // consume 'int'
    if (peek().type != TokenType::TOKEN_IDENTIFIER) {
        throw error(peek(), "Expected identifier name in declaration.");
    }
    string name = advance().value;

    unique_ptr<ExprAST> init_expr = nullptr;
    if (match(TokenType::TOKEN_OPERATOR_ASSIGN)) {
        init_expr = parse_expression();
    }

    if (!match(TokenType::TOKEN_SEMICOLON)) {
        throw error(peek(), "Expected ';' after declaration.");
    }
    return make_unique<DeclarationAST>(name, move(init_expr));
}

unique_ptr<StatementAST> Parser::parse_statement() {
    // Compound block: { item* }
    if (match(TokenType::TOKEN_OPEN_BRACE)) {
        vector<unique_ptr<BlockItemAST>> items;
        while (peek().type != TokenType::TOKEN_CLOSE_BRACE) {
            items.push_back(parse_block_item());
        }
        if (!match(TokenType::TOKEN_CLOSE_BRACE)) {
            throw error(peek(), "Expected '}' to close block.");
        }
        auto block = make_unique<BlockAST>(move(items));
        return make_unique<CompoundStatementAST>(move(block));
    }

    // return <expr> ;
    if (match(TokenType::TOKEN_KEYWORD_RETURN)) {
        auto expr = parse_expression();
        if (!match(TokenType::TOKEN_SEMICOLON)) {
            throw error(peek(), "Expected ';' after return value.");
        }
        return make_unique<ReturnStatementAST>(move(expr));
    }

    // empty statement ;
    if (match(TokenType::TOKEN_SEMICOLON)) {
        return make_unique<NullStatementAST>();
    }

    // if (<cond>) <stmt> [else <stmt>]
    if (match(TokenType::TOKEN_KEYWORD_IF)) {
        if (!match(TokenType::TOKEN_OPEN_PAREN)) {
            throw error(peek(), "Expected '(' after 'if'.");
        }
        auto condition = parse_expression();
        if (!match(TokenType::TOKEN_CLOSE_PAREN)) {
            throw error(peek(), "Expected ')' after if condition.");
        }
        auto then_branch = parse_statement();
        unique_ptr<StatementAST> else_branch = nullptr;
        if (match(TokenType::TOKEN_KEYWORD_ELSE)) {
            else_branch = parse_statement();
        }
        return make_unique<IfStatementAST>(move(condition), move(then_branch), move(else_branch));
    }

    // expression statement: <expr> ;
    auto expr = parse_expression();
    if (!match(TokenType::TOKEN_SEMICOLON)) {
        throw error(peek(), "Expected ';' after expression statement.");
    }
    return make_unique<ExpressionStatementAST>(move(expr));
}

// Precedence-climbing expression parser (with ternary support)
unique_ptr<ExprAST> Parser::parse_expression(int min_precedence) {
    auto lhs = parse_factor();

    while (true) {
        // Conditional (ternary) ?: handled specially
        if (!is_at_end() && peek().type == TokenType::TOKEN_QUESTION) {
            // choose precedence lower than || (5) and higher than assignment (1)
            int condPrec = 3;
            if (condPrec < min_precedence) break;

            advance(); // consume '?'
            auto thenExpr = parse_expression(0);
            if (!match(TokenType::TOKEN_COLON)) {
                throw error(peek(), "Expected ':' in conditional expression.");
            }
            auto elseExpr = parse_expression(condPrec); // right-associative
            lhs = make_unique<ConditionalExprAST>(move(lhs), move(thenExpr), move(elseExpr));
            continue;
        }

        int prec = get_token_precedence();
        if (prec < min_precedence) break;

        Token op = advance();
        int next_precedence = (op.type == TokenType::TOKEN_OPERATOR_ASSIGN) ? prec : prec + 1;

        auto rhs = parse_expression(next_precedence);

        if (op.type == TokenType::TOKEN_OPERATOR_ASSIGN) {
            lhs = make_unique<AssignmentExprAST>(move(lhs), move(rhs));
        } else {
            lhs = make_unique<BinaryExprAST>(move(op), move(lhs), move(rhs));
        }
    }
    return lhs;
}

unique_ptr<ExprAST> Parser::parse_factor() {
    if (match(TokenType::TOKEN_CONSTANT)) {
        return make_unique<ConstantExprAST>(tokens[current - 1].value);
    }

    if (match(TokenType::TOKEN_IDENTIFIER)) {
        return make_unique<VarExprAST>(tokens[current - 1].value);
    }

    if (match(TokenType::TOKEN_OPERATOR_MINUS) ||
        match(TokenType::TOKEN_OPERATOR_BITWISE_COMPLEMENT) ||
        match(TokenType::TOKEN_OPERATOR_LOGICAL_NEG)) {
        Token op_token = tokens[current - 1];
        auto operand = parse_factor();
        return make_unique<UnaryExprAST>(op_token, move(operand));
    }

    if (match(TokenType::TOKEN_OPEN_PAREN)) {
        auto expr = parse_expression();
        if (!match(TokenType::TOKEN_CLOSE_PAREN)) {
            throw error(peek(), "Expected ')' after expression.");
        }
        return expr;
    }

    throw error(peek(), "Expected an expression (e.g., a number, variable, or parentheses).");
}
