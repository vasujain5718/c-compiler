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
    {TokenType::TOKEN_OPERATOR_MODULO, 50}};

Parser::Parser(const vector<Token> &tokens) : tokens(tokens) {}

// --- Helper functions ---
const Token &Parser::peek() { return tokens[current]; }

const Token &Parser::advance()
{
    if (!is_at_end())
        current++;
    return tokens[current - 1];
}

bool Parser::is_at_end() { return peek().type == TokenType::TOKEN_EOF; }

bool Parser::match(TokenType type)
{
    if (is_at_end() || peek().type != type)
    {
        return false;
    }
    advance();
    return true;
}

runtime_error Parser::error(const Token &token, const string &message)
{
    stringstream ss;
    ss << "[Line " << token.line << "] Error at '" << token.value << "': " << message;
    return runtime_error(ss.str());
}

int Parser::get_token_precedence()
{
    auto it = OperatorPrecedence.find(peek().type);
    return (it != OperatorPrecedence.end()) ? it->second : -1;
}

// Utility: return true if the next token is a type specifier we support
static bool is_type_specifier(const Token &tok)
{
    return tok.type == TokenType::TOKEN_KEYWORD_INT ||
           tok.type == TokenType::TOKEN_KEYWORD_VOID ||
           tok.type == TokenType::TOKEN_KEYWORD_FLOAT ||
           tok.type == TokenType::TOKEN_KEYWORD_DOUBLE;
}

static std::string token_type_to_simple_string(const Token &tok)
{
    switch (tok.type)
    {
    case TokenType::TOKEN_KEYWORD_INT:
        return "int";
    case TokenType::TOKEN_KEYWORD_VOID:
        return "void";
    case TokenType::TOKEN_KEYWORD_FLOAT:
        return "float";
    case TokenType::TOKEN_KEYWORD_DOUBLE:
        return "double";
    default:
        return "";
    }
}

// --- Main entry point ---
unique_ptr<FunctionAST> Parser::parse()
{
    try
    {
        auto func_ast = parse_function();
        if (!is_at_end())
            throw error(peek(), "Extra tokens after function end.");
        return func_ast;
    }
    catch (const runtime_error &e)
    {
        cerr << "Parse Error: " << e.what() << endl;
        return nullptr;
    }
}

unique_ptr<FunctionAST> Parser::parse_function()
{
    // Expect a type specifier (int/void/float/double)
    if (!is_type_specifier(peek()))
        throw error(peek(), "Expected type specifier (int, void, float, double) at function start.");

    SimpleType return_type = SimpleType::UNKNOWN;
    switch (peek().type)
    {
    case TokenType::TOKEN_KEYWORD_INT:
        return_type = SimpleType::INT;
        break;
    case TokenType::TOKEN_KEYWORD_FLOAT:
        return_type = SimpleType::FLOAT;
        break;
    case TokenType::TOKEN_KEYWORD_DOUBLE:
        return_type = SimpleType::DOUBLE;
        break;
    case TokenType::TOKEN_KEYWORD_VOID:
        return_type = SimpleType::VOID;
        break;
    default:
        return_type = SimpleType::UNKNOWN;
        break;
    }
    advance();
    if (peek().type != TokenType::TOKEN_IDENTIFIER)
        throw error(peek(), "Expected function name.");
    string name = advance().value;

    if (!match(TokenType::TOKEN_OPEN_PAREN))
        throw error(peek(), "Expected '(' after function name.");
    if (peek().type == TokenType::TOKEN_KEYWORD_VOID)
        advance(); // optional void param
    if (!match(TokenType::TOKEN_CLOSE_PAREN))
        throw error(peek(), "Expected ')' after parameters.");
    if (!match(TokenType::TOKEN_OPEN_BRACE))
        throw error(peek(), "Expected '{' before body.");

    vector<unique_ptr<BlockItemAST>> body;
    while ( !is_at_end() && peek().type != TokenType::TOKEN_CLOSE_BRACE)
    {
        body.push_back(parse_block_item());
    }

    if (!match(TokenType::TOKEN_CLOSE_BRACE))
        throw error(peek(), "Expected '}' at function end.");

    return make_unique<FunctionAST>(return_type, name, move(body));
}

// --- Block and declarations ---
unique_ptr<BlockItemAST> Parser::parse_block_item()
{
    if (is_type_specifier(peek()))
        return parse_declaration();
    return parse_statement();
}

unique_ptr<DeclarationAST> Parser::parse_declaration()
{
    // Read and record declaration type
    if (!is_type_specifier(peek()))
        throw error(peek(), "Expected type specifier in declaration.");
    SimpleType decl_type;
    switch (peek().type)
    {
    case TokenType::TOKEN_KEYWORD_INT:
        decl_type = SimpleType::INT;
        break;
    case TokenType::TOKEN_KEYWORD_FLOAT:
        decl_type = SimpleType::FLOAT;
        break;
    case TokenType::TOKEN_KEYWORD_DOUBLE:
        decl_type = SimpleType::DOUBLE;
        break;
    default:
        decl_type = SimpleType::UNKNOWN;
        break;
    }
    advance();

    if (peek().type != TokenType::TOKEN_IDENTIFIER)
        throw error(peek(), "Expected variable name.");

    string name = advance().value;
    unique_ptr<ExprAST> init_expr = nullptr;

    if (match(TokenType::TOKEN_OPERATOR_ASSIGN))
        init_expr = parse_expression();

    if (!match(TokenType::TOKEN_SEMICOLON))
        throw error(peek(), "Expected ';' after declaration.");

    return make_unique<DeclarationAST>(decl_type, name, move(init_expr));
}

// --- Main statement parser ---
unique_ptr<StatementAST> Parser::parse_statement()
{
    // Compound block
    if (match(TokenType::TOKEN_OPEN_BRACE))
    {
        vector<unique_ptr<BlockItemAST>> items;
        while ( !is_at_end() && peek().type != TokenType::TOKEN_CLOSE_BRACE)
            items.push_back(parse_block_item());
        if (!match(TokenType::TOKEN_CLOSE_BRACE))
            throw error(peek(), "Expected '}' at end of block.");
        return make_unique<CompoundStatementAST>(make_unique<BlockAST>(move(items)));
    }

    // return <expr> ;
    if (match(TokenType::TOKEN_KEYWORD_RETURN))
    {
        auto expr = parse_expression();
        if (!match(TokenType::TOKEN_SEMICOLON))
            throw error(peek(), "Expected ';' after return.");
        return make_unique<ReturnStatementAST>(move(expr));
    }

    // break ;
    if (match(TokenType::TOKEN_KEYWORD_BREAK))
    {
        if (!match(TokenType::TOKEN_SEMICOLON))
            throw error(peek(), "Expected ';' after 'break'.");
        return make_unique<BreakStatementAST>();
    }

    // continue ;
    if (match(TokenType::TOKEN_KEYWORD_CONTINUE))
    {
        if (!match(TokenType::TOKEN_SEMICOLON))
            throw error(peek(), "Expected ';' after 'continue'.");
        return make_unique<ContinueStatementAST>();
    }

    // while ( <cond> ) <stmt>
    if (match(TokenType::TOKEN_KEYWORD_WHILE))
    {
        if (!match(TokenType::TOKEN_OPEN_PAREN))
            throw error(peek(), "Expected '(' after 'while'.");
        auto cond = parse_expression();
        if (!match(TokenType::TOKEN_CLOSE_PAREN))
            throw error(peek(), "Expected ')' after condition.");
        auto body = parse_statement();
        return make_unique<WhileStatementAST>(move(cond), move(body));
    }

    // do <stmt> while ( <cond> ) ;
    if (match(TokenType::TOKEN_KEYWORD_DO))
    {
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
    if (match(TokenType::TOKEN_KEYWORD_FOR))
    {
        if (!match(TokenType::TOKEN_OPEN_PAREN))
            throw error(peek(), "Expected '(' after 'for'.");

        unique_ptr<ForInitAST> init;

        if (is_type_specifier(peek()))
        {
            // declaration for-init (parse_declaration consumes the ';')
            auto decl = parse_declaration();
            init = make_unique<ForInitDeclAST>(move(decl));
        }
        else
        {
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
    if (match(TokenType::TOKEN_KEYWORD_IF))
    {
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
unique_ptr<ExprAST> Parser::parse_expression(int min_precedence)
{
    auto lhs = parse_factor();

    while (true)
    {
        // Ternary ?: operator
        if (!is_at_end() && peek().type == TokenType::TOKEN_QUESTION)
        {
            int condPrec = 3;
            if (condPrec < min_precedence)
                break;

            advance(); // consume '?'
            auto thenExpr = parse_expression(0);
            if (!match(TokenType::TOKEN_COLON))
                throw error(peek(), "Expected ':' in conditional expression.");
            auto elseExpr = parse_expression(condPrec);
            lhs = make_unique<ConditionalExprAST>(move(lhs), move(thenExpr), move(elseExpr));
            continue;
        }

        int prec = get_token_precedence();
        if (prec < min_precedence)
            break;

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
static SimpleType token_to_simple_type(TokenType tt)
{
    switch (tt)
    {
    case TokenType::TOKEN_CONSTANT:
        return SimpleType::INT;
    case TokenType::TOKEN_CONSTANT_FLOAT:
        return SimpleType::DOUBLE;
    default:
        return SimpleType::UNKNOWN;
    }
}

unique_ptr<ExprAST> Parser::parse_factor()
{
    if (match(TokenType::TOKEN_CONSTANT))
    {
        Token t = tokens[current - 1];
        return make_unique<ConstantExprAST>(t.value, token_to_simple_type(t.type));
    }

    if (match(TokenType::TOKEN_CONSTANT_FLOAT))
    {
        Token t = tokens[current - 1];
        return make_unique<ConstantExprAST>(t.value, token_to_simple_type(t.type));
    }

    if (match(TokenType::TOKEN_IDENTIFIER))
        return make_unique<VarExprAST>(tokens[current - 1].value);

    // Note: match() calls consume the token; using sequence of match() calls is preserved
    // from your original code but it consumes - that's fine since we want to consume unary op.
    if (match(TokenType::TOKEN_OPERATOR_MINUS) ||
        match(TokenType::TOKEN_OPERATOR_BITWISE_COMPLEMENT) ||
        match(TokenType::TOKEN_OPERATOR_LOGICAL_NEG))
    {
        Token op = tokens[current - 1];
        auto operand = parse_factor();
        return make_unique<UnaryExprAST>(op, move(operand));
    }

    if (match(TokenType::TOKEN_OPEN_PAREN))
    {
        auto expr = parse_expression();
        if (!match(TokenType::TOKEN_CLOSE_PAREN))
            throw error(peek(), "Expected ')' after expression.");
        return expr;
    }

    throw error(peek(), "Expected expression (constant, variable, or parentheses).");
}
