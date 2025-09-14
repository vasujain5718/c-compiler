#include "parser.h"
#include <iostream>
#include <stdexcept>
#include <memory>

using std::vector;
using std::string;
using std::unique_ptr;
using std::make_unique;
using std::move;
using std::runtime_error;
using std::cerr;
using std::endl;

Parser::Parser(const vector<Token>& tokens) : tokens(tokens) {}

const Token& Parser::peek() {
    return tokens[current];
}

const Token& Parser::advance() {
    if (!is_at_end()) current++;
    return tokens[current - 1];
}

bool Parser::is_at_end() {
    return peek().type == TokenType::TOKEN_EOF;
}

bool Parser::match(TokenType type) {
    if (is_at_end() || peek().type != type) {
        return false;
    }
    advance();
    return true;
}

unique_ptr<FunctionAST> Parser::parse() {
    try {
        auto func_ast = parse_function();
        if (!is_at_end()) {
            throw runtime_error("Extra tokens found after the end of the function.");
        }
        return func_ast;
    } catch (const runtime_error& e) {
        cerr << "Parse Error: " << e.what() << endl;
        return nullptr;
    }
}

unique_ptr<FunctionAST> Parser::parse_function() {
    if (!match(TokenType::TOKEN_KEYWORD_INT)) {
        throw runtime_error("Expected 'int' keyword at start of function.");
    }

    if (peek().type != TokenType::TOKEN_IDENTIFIER) {
         throw runtime_error("Expected function name after 'int'.");
    }
    string name = advance().value;

    if (!match(TokenType::TOKEN_OPEN_PAREN)) throw runtime_error("Expected '(' after function name.");

    if (peek().type == TokenType::TOKEN_KEYWORD_VOID) {
        advance();
    }

    if (!match(TokenType::TOKEN_CLOSE_PAREN)) throw runtime_error("Expected ')' after function arguments.");
    if (!match(TokenType::TOKEN_OPEN_BRACE)) throw runtime_error("Expected '{' before function body.");

    auto body = parse_statement();

    if (!match(TokenType::TOKEN_CLOSE_BRACE)) throw runtime_error("Expected '}' after function body.");
    
    return make_unique<FunctionAST>(name, move(body));
}

unique_ptr<StatementAST> Parser::parse_statement() {
    if (match(TokenType::TOKEN_KEYWORD_RETURN)) {
        auto expr = parse_expression();
        if (!match(TokenType::TOKEN_SEMICOLON)) {
            throw runtime_error("Expected ';' after return value.");
        }
        return make_unique<ReturnStatementAST>(move(expr));
    }
    throw runtime_error("Unknown statement type. Expected 'return'.");
}

unique_ptr<ExprAST> Parser::parse_expression() {
    if (peek().type == TokenType::TOKEN_CONSTANT) {
        return make_unique<ConstantExprAST>(advance().value);
    }
    throw runtime_error("Expected an expression (e.g., a number).");
}