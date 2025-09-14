#include "lexer.h"
#include <cctype>
#include <iostream>
#include <map>

using std::string;
using std::vector;
using std::map;
using std::cout;
using std::cerr;
using std::endl;

Lexer::Lexer(const string& source) : source(source) {}

char Lexer::peek() {
    if (current_pos >= source.length()) {
        return '\0';
    }
    return source[current_pos];
}

char Lexer::advance() {
    if (current_pos >= source.length()) {
        return '\0';
    }
    return source[current_pos++];
}

Token Lexer::create_token(TokenType type, const string& value) {
    int col = static_cast<int>(current_pos - value.length() - line_start_pos + 1);
    return {type, value, line, col};
}

TokenType Lexer::get_identifier_type(const string& identifier) {
    static const map<string, TokenType> keywords = {
        {"int", TokenType::TOKEN_KEYWORD_INT},
        {"void", TokenType::TOKEN_KEYWORD_VOID},
        {"return", TokenType::TOKEN_KEYWORD_RETURN}
    };
    auto it = keywords.find(identifier);
    if (it != keywords.end()) {
        return it->second;
    }
    return TokenType::TOKEN_IDENTIFIER;
}

vector<Token> Lexer::tokenize() {
    vector<Token> tokens;

    while (current_pos < source.length()) {
        char current_char = peek();
        size_t start_pos = current_pos;

        if (isspace(current_char)) {
            if (current_char == '\n') {
                line++;
                line_start_pos = current_pos + 1;
            }
            advance();
            continue;
        }

        if (current_char == '/') {
            if (current_pos + 1 < source.length() && source[current_pos + 1] == '/') {
                while (peek() != '\n' && peek() != '\0') {
                    advance();
                }
            } else if (current_pos + 1 < source.length() && source[current_pos + 1] == '*') {
                advance();
                advance();
                
                while (true) {
                    if (peek() == '\0') {
                        had_error = true;
                        cerr << "Lexer Error: Unterminated block comment." << endl;
                        tokens.push_back(create_token(TokenType::TOKEN_ILLEGAL, "/*"));
                        break;
                    }
                    if (peek() == '*' && current_pos + 1 < source.length() && source[current_pos + 1] == '/') {
                        advance();
                        advance();
                        break;
                    }
                    if (peek() == '\n') {
                        line++;
                        line_start_pos = current_pos + 1;
                    }
                    advance();
                }
            } else {
                string single_char_val(1, advance());
                had_error = true;
                cerr << "Lexer Error: Illegal character '" << single_char_val[0] << "' at line " << line << endl;
                tokens.push_back(create_token(TokenType::TOKEN_ILLEGAL, single_char_val));
            }
        }
        else if (isalpha(current_char) || current_char == '_') {
            while (isalnum(peek()) || peek() == '_') {
                advance();
            }
            string value = source.substr(start_pos, current_pos - start_pos);
            tokens.push_back(create_token(get_identifier_type(value), value));
        }
        else if (isdigit(current_char)) {
            while (isdigit(peek())) {
                advance();
            }
            
            if (isalpha(peek()) || peek() == '_') {
                while (isalnum(peek()) || peek() == '_') {
                    advance();
                }
                string value = source.substr(start_pos, current_pos - start_pos);
                had_error = true;
                cerr << "Lexer Error: Invalid identifier format '" << value << "' at line " << line << endl;
                tokens.push_back(create_token(TokenType::TOKEN_ILLEGAL, value));
            } else {
                string value = source.substr(start_pos, current_pos - start_pos);
                tokens.push_back(create_token(TokenType::TOKEN_CONSTANT, value));
            }
        }
        else {
            string single_char_val(1, advance());
            switch (single_char_val[0]) {
                case '(': tokens.push_back(create_token(TokenType::TOKEN_OPEN_PAREN, single_char_val)); break;
                case ')': tokens.push_back(create_token(TokenType::TOKEN_CLOSE_PAREN, single_char_val)); break;
                case '{': tokens.push_back(create_token(TokenType::TOKEN_OPEN_BRACE, single_char_val)); break;
                case '}': tokens.push_back(create_token(TokenType::TOKEN_CLOSE_BRACE, single_char_val)); break;
                case ';': tokens.push_back(create_token(TokenType::TOKEN_SEMICOLON, single_char_val)); break;
                default:
                    had_error = true;
                    cerr << "Lexer Error: Illegal character '" << single_char_val[0] << "' at line " << line << endl;
                    tokens.push_back(create_token(TokenType::TOKEN_ILLEGAL, single_char_val));
                    break;
            }
        }
    }

    tokens.push_back({TokenType::TOKEN_EOF, "EOF", line, static_cast<int>(current_pos - line_start_pos + 1)});
    return tokens;
}