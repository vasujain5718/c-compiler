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

// Initialize operator precedence
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

// --- Helper functions ---
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
    return (it != OperatorPrecedence.end()) ? it->second : -1;
}

// --- Main entry point ---
unique_ptr<FunctionAST> Parser::parse() {
    try {
        auto func_ast = parse_function();
        if (!is_at_end())
            throw error(peek(), "Extra tokens after function end.");
        return func_ast;
    } catch (const runtime_error& e) {
        cerr << "Parse Error: " << e.what() << endl;
        return nullptr;
    }
}

unique_ptr<FunctionAST> Parser::parse_function() {
    if (!match(TokenType::TOKEN_KEYWORD_INT) && !match(TokenType::TOKEN_KEYWORD_VOID))
        throw error(peek(), "Expected 'int' or 'void' at function start.");

    if (peek().type != TokenType::TOKEN_IDENTIFIER)
        throw error(peek(), "Expected function name.");
    string name = advance().value;

    if (!match(TokenType::TOKEN_OPEN_PAREN)) throw error(peek(), "Expected '(' after function name.");
    if (peek().type == TokenType::TOKEN_KEYWORD_VOID) advance(); // optional void param
    if (!match(TokenType::TOKEN_CLOSE_PAREN)) throw error(peek(), "Expected ')' after parameters.");
    if (!match(TokenType::TOKEN_OPEN_BRACE)) throw error(peek(), "Expected '{' before body.");

    vector<unique_ptr<BlockItemAST>> body;
    while (peek().type != TokenType::TOKEN_CLOSE_BRACE) {
        body.push_back(parse_block_item());
    }

    if (!match(TokenType::TOKEN_CLOSE_BRACE))
        throw error(peek(), "Expected '}' at function end.");

    return make_unique<FunctionAST>(name, move(body));
}

// --- Block and declarations ---
unique_ptr<BlockItemAST> Parser::parse_block_item() {
    if (peek().type == TokenType::TOKEN_KEYWORD_INT)
        return parse_declaration();
    return parse_statement();
}

unique_ptr<DeclarationAST> Parser::parse_declaration() {
    match(TokenType::TOKEN_KEYWORD_INT);
    if (peek().type != TokenType::TOKEN_IDENTIFIER)
        throw error(peek(), "Expected variable name.");

    string name = advance().value;
    unique_ptr<ExprAST> init_expr = nullptr;

    if (match(TokenType::TOKEN_OPERATOR_ASSIGN))
        init_expr = parse_expression();

    if (!match(TokenType::TOKEN_SEMICOLON))
        throw error(peek(), "Expected ';' after declaration.");

    return make_unique<DeclarationAST>(name, move(init_expr));
}

// --- New helper: parse optional expression ---

// --- Main statement parser ---
unique_ptr<StatementAST> Parser::parse_statement() {
    // Compound block
    if (match(TokenType::TOKEN_OPEN_BRACE)) {
        vector<unique_ptr<BlockItemAST>> items;
        while (peek().type != TokenType::TOKEN_CLOSE_BRACE)
            items.push_back(parse_block_item());
        if (!match(TokenType::TOKEN_CLOSE_BRACE))
            throw error(peek(), "Expected '}' at end of block.");
        return make_unique<CompoundStatementAST>(make_unique<BlockAST>(move(items)));
    }

    // return <expr> ;
    if (match(TokenType::TOKEN_KEYWORD_RETURN)) {
        auto expr = parse_expression();
        if (!match(TokenType::TOKEN_SEMICOLON))
            throw error(peek(), "Expected ';' after return.");
        return make_unique<ReturnStatementAST>(move(expr));
    }

    // break ;
    if (match(TokenType::TOKEN_KEYWORD_BREAK)) {
        if (!match(TokenType::TOKEN_SEMICOLON))
            throw error(peek(), "Expected ';' after 'break'.");
        return make_unique<BreakStatementAST>();
    }

    // continue ;
    if (match(TokenType::TOKEN_KEYWORD_CONTINUE)) {
        if (!match(TokenType::TOKEN_SEMICOLON))
            throw error(peek(), "Expected ';' after 'continue'.");
        return make_unique<ContinueStatementAST>();
    }

    // while ( <cond> ) <stmt>
    if (match(TokenType::TOKEN_KEYWORD_WHILE)) {
        if (!match(TokenType::TOKEN_OPEN_PAREN))
            throw error(peek(), "Expected '(' after 'while'.");
        auto cond = parse_expression();
        if (!match(TokenType::TOKEN_CLOSE_PAREN))
            throw error(peek(), "Expected ')' after condition.");
        auto body = parse_statement();
        return make_unique<WhileStatementAST>(move(cond), move(body));
    }

    // do <stmt> while ( <cond> ) ;
    if (match(TokenType::TOKEN_KEYWORD_DO)) {
        auto body = parse_statement();
        if (!match(TokenType::TOKEN_KEYWORD_WHILE))
            throw error(peek(), "Expected 'while' after do-body.");
        if (!match(TokenType::TOKEN_OPEN_PAREN))
            throw error(peek(), "Expected '(' after 'while'.");
        auto cond = parse_expression();
        if (!match(TokenType::TOKEN_CLOSE_PAREN))
            throw error(peek(), "Expected ')' after condition.");
        if (!match(TokenType::TOKEN_SEMICOLON))
            throw error(peek(), "Expected ';' after do-while.");
        return make_unique<DoWhileStatementAST>(move(body), move(cond));
    }

    // for ( <for-init> [<cond>] ; [<post>] ) <stmt>
    if (match(TokenType::TOKEN_KEYWORD_FOR)) {
        if (!match(TokenType::TOKEN_OPEN_PAREN))
            throw error(peek(), "Expected '(' after 'for'.");

        unique_ptr<ForInitAST> init;

        if (peek().type == TokenType::TOKEN_KEYWORD_INT) {
            auto decl = parse_declaration(); // consumes ';'
            init = make_unique<ForInitDeclAST>(move(decl));
        } else {
            unique_ptr<ExprAST> maybe_init = nullptr;
            if (peek().type != TokenType::TOKEN_SEMICOLON)
                maybe_init = parse_expression();
            if (!match(TokenType::TOKEN_SEMICOLON))
                throw error(peek(), "Expected ';' after init expression.");
            init = make_unique<ForInitExprAST>(move(maybe_init));
        }

        unique_ptr<ExprAST> cond = nullptr;
        if (peek().type != TokenType::TOKEN_SEMICOLON)
            cond = parse_expression();
        if (!match(TokenType::TOKEN_SEMICOLON))
            throw error(peek(), "Expected ';' after condition.");

        unique_ptr<ExprAST> post = nullptr;
        if (peek().type != TokenType::TOKEN_CLOSE_PAREN)
            post = parse_expression();
        if (!match(TokenType::TOKEN_CLOSE_PAREN))
            throw error(peek(), "Expected ')' after for-clause.");

        auto body = parse_statement();
        return make_unique<ForStatementAST>(move(init), move(cond), move(post), move(body));
    }

    // empty statement ;
    if (match(TokenType::TOKEN_SEMICOLON))
        return make_unique<NullStatementAST>();

    // if (<cond>) <stmt> [else <stmt>]
    if (match(TokenType::TOKEN_KEYWORD_IF)) {
        if (!match(TokenType::TOKEN_OPEN_PAREN))
            throw error(peek(), "Expected '(' after 'if'.");
        auto cond = parse_expression();
        if (!match(TokenType::TOKEN_CLOSE_PAREN))
            throw error(peek(), "Expected ')' after if condition.");
        auto then_branch = parse_statement();
        unique_ptr<StatementAST> else_branch = nullptr;
        if (match(TokenType::TOKEN_KEYWORD_ELSE))
            else_branch = parse_statement();
        return make_unique<IfStatementAST>(move(cond), move(then_branch), move(else_branch));
    }

    // Expression statement
    auto expr = parse_expression();
    if (!match(TokenType::TOKEN_SEMICOLON))
        throw error(peek(), "Expected ';' after expression.");
    return make_unique<ExpressionStatementAST>(move(expr));
}

// --- Expression parsing ---
unique_ptr<ExprAST> Parser::parse_expression(int min_precedence) {
    auto lhs = parse_factor();

    while (true) {
        // Ternary ?: operator
        if (!is_at_end() && peek().type == TokenType::TOKEN_QUESTION) {
            int condPrec = 3;
            if (condPrec < min_precedence) break;

            advance(); // consume '?'
            auto thenExpr = parse_expression(0);
            if (!match(TokenType::TOKEN_COLON))
                throw error(peek(), "Expected ':' in conditional expression.");
            auto elseExpr = parse_expression(condPrec);
            lhs = make_unique<ConditionalExprAST>(move(lhs), move(thenExpr), move(elseExpr));
            continue;
        }

        int prec = get_token_precedence();
        if (prec < min_precedence) break;

        Token op = advance();
        int next_prec = (op.type == TokenType::TOKEN_OPERATOR_ASSIGN) ? prec : prec + 1;

        auto rhs = parse_expression(next_prec);
        if (op.type == TokenType::TOKEN_OPERATOR_ASSIGN)
            lhs = make_unique<AssignmentExprAST>(move(lhs), move(rhs));
        else
            lhs = make_unique<BinaryExprAST>(move(op), move(lhs), move(rhs));
    }
    return lhs;
}

unique_ptr<ExprAST> Parser::parse_factor() {
    if (match(TokenType::TOKEN_CONSTANT))
        return make_unique<ConstantExprAST>(tokens[current - 1].value);

    if (match(TokenType::TOKEN_IDENTIFIER))
        return make_unique<VarExprAST>(tokens[current - 1].value);

    if (match(TokenType::TOKEN_OPERATOR_MINUS) ||
        match(TokenType::TOKEN_OPERATOR_BITWISE_COMPLEMENT) ||
        match(TokenType::TOKEN_OPERATOR_LOGICAL_NEG)) {
        Token op = tokens[current - 1];
        auto operand = parse_factor();
        return make_unique<UnaryExprAST>(op, move(operand));
    }

    if (match(TokenType::TOKEN_OPEN_PAREN)) {
        auto expr = parse_expression();
        if (!match(TokenType::TOKEN_CLOSE_PAREN))
            throw error(peek(), "Expected ')' after expression.");
        return expr;
    }

    throw error(peek(), "Expected expression (constant, variable, or parentheses).");
}
