#ifndef LEXER_H
#define LEXER_H

#include <string>
#include <vector>

enum class TokenType {
    TOKEN_EOF,
    TOKEN_KEYWORD_INT,
    TOKEN_KEYWORD_VOID,
    TOKEN_KEYWORD_RETURN,
    TOKEN_IDENTIFIER,
    TOKEN_CONSTANT,
    TOKEN_OPEN_PAREN,
    TOKEN_CLOSE_PAREN,
    TOKEN_OPEN_BRACE,
    TOKEN_CLOSE_BRACE,
    TOKEN_SEMICOLON,
    TOKEN_ILLEGAL
};

struct Token {
    TokenType type;
    std::string value;
    int line;
    int col;
};

class Lexer {
public:
    Lexer(const std::string& source);
    std::vector<Token> tokenize();
    bool hadError() const { return had_error; }

private:
    std::string source;
    size_t current_pos = 0;
    int line = 1;
    size_t line_start_pos = 0;
    bool had_error = false;

    char peek();
    char advance();
    Token create_token(TokenType type, const std::string& value);
    TokenType get_identifier_type(const std::string& identifier);
};

const char* token_type_to_string(TokenType type);

#endif // LEXER_H