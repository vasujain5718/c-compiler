#ifndef LEXER_H
#define LEXER_H

#include <string>
#include <vector>

enum class TokenType {
    TOKEN_EOF,

    // Keywords
    TOKEN_KEYWORD_INT,
    TOKEN_KEYWORD_VOID,
    TOKEN_KEYWORD_RETURN,
    TOKEN_KEYWORD_IF,
    TOKEN_KEYWORD_ELSE,
    TOKEN_QUESTION,
    TOKEN_COLON,

    // Identifiers and Constants
    TOKEN_IDENTIFIER,
    TOKEN_CONSTANT,

    // Punctuation
    TOKEN_OPEN_PAREN,
    TOKEN_CLOSE_PAREN,
    TOKEN_OPEN_BRACE,
    TOKEN_CLOSE_BRACE,
    TOKEN_SEMICOLON,

    // Chapter 2 Unary Operators
    TOKEN_OPERATOR_MINUS,
    TOKEN_OPERATOR_BITWISE_COMPLEMENT,

    // Chapter 3 Binary Operators
    TOKEN_OPERATOR_PLUS,
    TOKEN_OPERATOR_MULTIPLY,
    TOKEN_OPERATOR_DIVIDE,
    TOKEN_OPERATOR_MODULO,
    TOKEN_OPERATOR_DECREMENT, 

    // NEW: Chapter 4 Operators
    TOKEN_OPERATOR_LOGICAL_NEG,   // !
    TOKEN_OPERATOR_LOGICAL_AND,   // &&
    TOKEN_OPERATOR_LOGICAL_OR,    // ||
    TOKEN_OPERATOR_EQUAL,         // ==
    TOKEN_OPERATOR_NOT_EQUAL,     // !=
    TOKEN_OPERATOR_LESS,          // <
    TOKEN_OPERATOR_LESS_EQUAL,    // <=
    TOKEN_OPERATOR_GREATER,       // >
    TOKEN_OPERATOR_GREATER_EQUAL, // >=
    TOKEN_OPERATOR_ASSIGN,        // =
    TOKEN_OPERATOR_BITWISE_AND,   // &
    TOKEN_OPERATOR_BITWISE_OR,    // |

    // To handle unknown characters
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
    explicit Lexer(const std::string& source);
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

#endif