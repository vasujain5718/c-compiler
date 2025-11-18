#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include "ast.h"
#include <vector>
#include <memory>
#include <stdexcept>
#include <map>

class Parser {
public:
    explicit Parser(const std::vector<Token>& tokens);
    std::unique_ptr<FunctionAST> parse();

private:
    const std::vector<Token>& tokens;
    size_t current = 0;
    
    static const std::map<TokenType, int> OperatorPrecedence;

    const Token& peek();
    const Token& advance();
    bool is_at_end();
    bool match(TokenType type);
    std::runtime_error error(const Token& token, const std::string& message);

    int get_token_precedence();

    std::unique_ptr<FunctionAST> parse_function();
    std::unique_ptr<BlockItemAST> parse_block_item();
    std::unique_ptr<StatementAST> parse_statement();
    std::unique_ptr<DeclarationAST> parse_declaration();

    std::unique_ptr<ExprAST> parse_expression(int min_precedence = 0);
    std::unique_ptr<ExprAST> parse_factor();
};

#endif