#include "parser.h"
#include <iostream>
#include <stdexcept>
#include <memory>
#include <sstream>

using std::vector;
using std::string;
using std::unique_ptr;
using std::make_unique;
using std::move;
using std::runtime_error;
using std::cerr;
using std::endl;
using std::stringstream;
using std::map;

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

Parser::Parser(const vector<Token> &tokens) : tokens(tokens) {}

// --- Helper Functions (Unchanged) ---
const Token &Parser::peek() { return tokens[current]; }
const Token &Parser::advance() { if (!is_at_end()) current++; return tokens[current - 1]; }
bool Parser::is_at_end() { return peek().type == TokenType::TOKEN_EOF; }
bool Parser::match(TokenType type) { if (is_at_end() || peek().type != type) { return false; } advance(); return true; }
runtime_error Parser::error(const Token& token, const string& message) {
    stringstream ss; ss << "[Line " << token.line << "] Error at '" << token.value << "': " << message;
    return runtime_error(ss.str());
}

// --- New Helper Function ---
int Parser::get_token_precedence() {
    auto it = OperatorPrecedence.find(peek().type);
    if (it != OperatorPrecedence.end()) {
        return it->second;
    }
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
    }
    catch (const runtime_error &e) {
        cerr << "Parse Error: " << e.what() << endl;
        return nullptr;
    }
}

unique_ptr<FunctionAST> Parser::parse_function() {
    if (!match(TokenType::TOKEN_KEYWORD_INT)) { throw error(peek(), "Expected 'int' keyword."); }
    if (peek().type != TokenType::TOKEN_IDENTIFIER) { throw error(peek(), "Expected function name."); }
    string name = advance().value;

    if (!match(TokenType::TOKEN_OPEN_PAREN)) throw error(peek(), "Expected '('.");
    if (peek().type == TokenType::TOKEN_KEYWORD_VOID) { advance(); }
    if (!match(TokenType::TOKEN_CLOSE_PAREN)) throw error(peek(), "Expected ')'.");
    if (!match(TokenType::TOKEN_OPEN_BRACE)) throw error(peek(), "Expected '{'.");

    // New loop to parse a list of block items
    vector<unique_ptr<BlockItemAST>> body;
    while (peek().type != TokenType::TOKEN_CLOSE_BRACE) {
        body.push_back(parse_block_item());
    }

    if (!match(TokenType::TOKEN_CLOSE_BRACE)) throw error(peek(), "Expected '}'.");

    return make_unique<FunctionAST>(name, move(body));
}

// New function to decide between statement and declaration
unique_ptr<BlockItemAST> Parser::parse_block_item() {
    if (peek().type == TokenType::TOKEN_KEYWORD_INT) {
        // If it starts with 'int', it's a declaration
        return parse_declaration();
    }
    // Otherwise, it's a statement
    return parse_statement();
}

// New function to parse a declaration
unique_ptr<DeclarationAST> Parser::parse_declaration() {
    match(TokenType::TOKEN_KEYWORD_INT); // Consume 'int'
    if (peek().type != TokenType::TOKEN_IDENTIFIER) {
        throw error(peek(), "Expected identifier name in declaration.");
    }
    string name = advance().value;
    
    unique_ptr<ExprAST> init_expr = nullptr;
    if (match(TokenType::TOKEN_OPERATOR_ASSIGN)) {
        // Handle optional initializer
        init_expr = parse_expression();
    }

    if (!match(TokenType::TOKEN_SEMICOLON)) {
        throw error(peek(), "Expected ';' after declaration.");
    }
    return make_unique<DeclarationAST>(name, move(init_expr));
}

// Updated function to handle all statement types
unique_ptr<StatementAST> Parser::parse_statement() {
    if (match(TokenType::TOKEN_KEYWORD_RETURN)) {
        auto expr = parse_expression();
        if (!match(TokenType::TOKEN_SEMICOLON)) {
            throw error(peek(), "Expected ';' after return value.");
        }
        return make_unique<ReturnStatementAST>(move(expr));
    }
    if (match(TokenType::TOKEN_SEMICOLON)) {
        return make_unique<NullStatementAST>();
    }

    // It's an expression statement
    auto expr = parse_expression();
    if (!match(TokenType::TOKEN_SEMICOLON)) {
        throw error(peek(), "Expected ';' after expression statement.");
    }
    return make_unique<ExpressionStatementAST>(move(expr));
}

// The new precedence climbing parser
unique_ptr<ExprAST> Parser::parse_expression(int min_precedence) {
    auto lhs = parse_factor();

    while (true) {
        int prec = get_token_precedence();
        if (prec < min_precedence) {
            break;
        }

        Token op = advance();
        
        // Special case for right-associative assignment
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

// Updated to parse variables
unique_ptr<ExprAST> Parser::parse_factor() {
    if (match(TokenType::TOKEN_CONSTANT)) {
        return make_unique<ConstantExprAST>(tokens[current - 1].value);
    }
    
    if (match(TokenType::TOKEN_IDENTIFIER)) {
        // It's a variable
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