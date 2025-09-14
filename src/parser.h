#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include "ast.h"
#include <vector>
#include <memory>

class Parser {
public:
    explicit Parser(const std::vector<Token>& tokens);
    std::unique_ptr<FunctionAST> parse();

private:
    const std::vector<Token>& tokens;
    size_t current = 0;

    const Token& peek();
    const Token& advance();
    bool is_at_end();
    bool match(TokenType type);

    std::unique_ptr<FunctionAST> parse_function();
    std::unique_ptr<StatementAST> parse_statement();
    std::unique_ptr<ExprAST> parse_expression();
};

#endif 