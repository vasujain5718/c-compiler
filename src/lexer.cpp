#include "lexer.h"

#include <cctype>

#include <iostream>

#include <map>

#include <string> // <-- Added for std::to_string

using std::cerr;

using std::cout;

using std::endl;

using std::map;

using std::string;

using std::vector;

Lexer::Lexer(const string &source) : source(source) {}

char Lexer::peek()

{

    if (current_pos >= source.length())

    {

        return '\0';
    }

    return source[current_pos];
}

char Lexer::advance()

{

    if (current_pos >= source.length())

    {

        return '\0';
    }

    return source[current_pos++];
}

Token Lexer::create_token(TokenType type, const string &value)

{

    int col = static_cast<int>(current_pos - value.length() - line_start_pos + 1);

    return {type, value, line, col};
}

TokenType Lexer::get_identifier_type(const string &identifier)

{

    static const map<string, TokenType> keywords = {

        {"int", TokenType::TOKEN_KEYWORD_INT},

        {"void", TokenType::TOKEN_KEYWORD_VOID},

        {"return", TokenType::TOKEN_KEYWORD_RETURN},

        {"if", TokenType::TOKEN_KEYWORD_IF},

        {"else", TokenType::TOKEN_KEYWORD_ELSE},

        {"do", TokenType::TOKEN_KEYWORD_DO},

        {"while", TokenType::TOKEN_KEYWORD_WHILE},

        {"for", TokenType::TOKEN_KEYWORD_FOR},

        {"break", TokenType::TOKEN_KEYWORD_BREAK},

        {"continue", TokenType::TOKEN_KEYWORD_CONTINUE},

        {"float", TokenType::TOKEN_KEYWORD_FLOAT},

        {"double", TokenType::TOKEN_KEYWORD_DOUBLE},

        {"char", TokenType::TOKEN_KEYWORD_CHAR}, // <-- ADDED

    };

    auto it = keywords.find(identifier);

    if (it != keywords.end())

    {

        return it->second;
    }

    return TokenType::TOKEN_IDENTIFIER;
}

vector<Token> Lexer::tokenize()

{

    vector<Token> tokens;

    auto is_digit = [&](char c)

    { return c >= '0' && c <= '9'; };

    while (current_pos < source.length())

    {

        char current_char = peek();

        size_t start_pos = current_pos;

        if (isspace(current_char))

        {

            if (current_char == '\n')

            {

                line++;

                line_start_pos = current_pos + 1;
            }

            advance();

            continue;
        }

        if (isalpha(current_char) || current_char == '_')

        {

            while (isalnum(peek()) || peek() == '_')

            {

                advance();
            }

            string value = source.substr(start_pos, current_pos - start_pos);

            tokens.push_back(create_token(get_identifier_type(value), value));

            continue;
        }

        // Numbers: integer and floating-point

        if (is_digit(current_char))

        {

            bool is_float = false;

            // integer part

            while (is_digit(peek()))

                advance();

            // fractional part

            // fractional part

            if (peek() == '.')

            {

                // Accept a '.' after digits even if no fractional digits follow (e.g., "1.").

                // That makes "1." a valid floating literal. If there are digits after '.', consume them.

                is_float = true;

                advance(); // consume '.'

                while (is_digit(peek()))

                    advance();
            }

            // exponent part

            if (peek() == 'e' || peek() == 'E')

            {

                is_float = true;

                advance();

                if (peek() == '+' || peek() == '-')

                    advance();

                if (!is_digit(peek()))

                {

                    // malformed exponent

                    string value = source.substr(start_pos, current_pos - start_pos);

                    had_error = true;

                    cerr << "Lexer Error: Malformed floating constant '" << value << "' at line " << line << endl;

                    tokens.push_back(create_token(TokenType::TOKEN_ILLEGAL, value));

                    continue;
                }

                while (is_digit(peek()))

                    advance();
            }

            // After numeric literal, ensure it's not followed by identifier chars (e.g., 123abc)

            if (isalpha(peek()) || peek() == '_')

            {

                while (isalnum(peek()) || peek() == '_')

                {

                    advance();
                }

                string value = source.substr(start_pos, current_pos - start_pos);

                had_error = true;

                cerr << "Lexer Error: Invalid identifier format '" << value << "' at line " << line << endl;

                tokens.push_back(create_token(TokenType::TOKEN_ILLEGAL, value));
            }

            else

            {

                string value = source.substr(start_pos, current_pos - start_pos);

                if (is_float)

                {

                    tokens.push_back(create_token(TokenType::TOKEN_CONSTANT_FLOAT, value));
                }

                else

                {

                    tokens.push_back(create_token(TokenType::TOKEN_CONSTANT, value));
                }
            }

            continue;
        }

        // Handle a float that starts with '.' like .5

        if (current_char == '.')

        {

            if (current_pos + 1 < source.length() && is_digit(source[current_pos + 1]))

            {

                size_t start = current_pos;

                bool is_float = true;

                advance(); // consume '.'

                while (is_digit(peek()))

                    advance();

                if (peek() == 'e' || peek() == 'E')

                {

                    advance();

                    if (peek() == '+' || peek() == '-')

                        advance();

                    if (!is_digit(peek()))

                    {

                        string value = source.substr(start, current_pos - start);

                        had_error = true;

                        cerr << "Lexer Error: Malformed floating constant '" << value << "' at line " << line << endl;

                        tokens.push_back(create_token(TokenType::TOKEN_ILLEGAL, value));

                        continue;
                    }

                    while (is_digit(peek()))

                        advance();
                }

                string value = source.substr(start, current_pos - start);

                tokens.push_back(create_token(TokenType::TOKEN_CONSTANT_FLOAT, value));

                continue;
            }

            else

            {

                // a standalone '.' is not currently part of the supported grammar in this lexer

                // mark as illegal to avoid silently dropping it

                string val(1, advance());

                had_error = true;

                cerr << "Lexer Error: Illegal character '.' at line " << line << endl;

                tokens.push_back(create_token(TokenType::TOKEN_ILLEGAL, val));

                continue;
            }
        }

        string single_char_val(1, advance());

        switch (single_char_val[0])

        {

            // Punctuation

        case '(':

            tokens.push_back(create_token(TokenType::TOKEN_OPEN_PAREN, single_char_val));

            break;

        case ')':

            tokens.push_back(create_token(TokenType::TOKEN_CLOSE_PAREN, single_char_val));

            break;

        case '{':

            tokens.push_back(create_token(TokenType::TOKEN_OPEN_BRACE, single_char_val));

            break;

        case '}':

            tokens.push_back(create_token(TokenType::TOKEN_CLOSE_BRACE, single_char_val));

            break;

        case ';':

            tokens.push_back(create_token(TokenType::TOKEN_SEMICOLON, single_char_val));

            break;

        case '?':

            tokens.push_back(create_token(TokenType::TOKEN_QUESTION, single_char_val));

            break;

        case ':':

            tokens.push_back(create_token(TokenType::TOKEN_COLON, single_char_val));

            break;

            // Simple Operators

        case '+':

            tokens.push_back(create_token(TokenType::TOKEN_OPERATOR_PLUS, single_char_val));

            break;

        case '*':

            tokens.push_back(create_token(TokenType::TOKEN_OPERATOR_MULTIPLY, single_char_val));

            break;

        case '%':

            tokens.push_back(create_token(TokenType::TOKEN_OPERATOR_MODULO, single_char_val));

            break;

        case '~':

            tokens.push_back(create_token(TokenType::TOKEN_OPERATOR_BITWISE_COMPLEMENT, single_char_val));

            break;

            // Operators with lookahead

        case '-':

            if (peek() == '-')

            {

                advance();

                tokens.push_back(create_token(TokenType::TOKEN_OPERATOR_DECREMENT, "--"));
            }

            else

            {

                tokens.push_back(create_token(TokenType::TOKEN_OPERATOR_MINUS, "-"));
            }

            break;

        case '/':

            if (peek() == '/')

            {

                while (peek() != '\n' && peek() != '\0')

                {

                    advance();
                }
            }

            else if (peek() == '*')

            {

                advance();

                while (true)

                {

                    if (peek() == '\0')

                    {

                        had_error = true;

                        cerr << "Lexer Error: Unterminated block comment." << endl;

                        tokens.push_back(create_token(TokenType::TOKEN_ILLEGAL, "/*"));

                        break;
                    }

                    char last_char = advance();

                    if (last_char == '*' && peek() == '/')

                    {

                        advance();

                        break;
                    }

                    if (last_char == '\n')

                    {

                        line++;

                        line_start_pos = current_pos;
                    }
                }
            }

            else

            {

                tokens.push_back(create_token(TokenType::TOKEN_OPERATOR_DIVIDE, "/"));
            }

            break;

            // NEW: Chapter 4 operators with lookahead

        case '!':

            if (peek() == '=')

            {

                advance();

                tokens.push_back(create_token(TokenType::TOKEN_OPERATOR_NOT_EQUAL, "!="));
            }

            else

            {

                tokens.push_back(create_token(TokenType::TOKEN_OPERATOR_LOGICAL_NEG, "!"));
            }

            break;

        case '&':

            if (peek() == '&')

            {

                advance();

                tokens.push_back(create_token(TokenType::TOKEN_OPERATOR_LOGICAL_AND, "&&"));
            }

            else

            {

                tokens.push_back(create_token(TokenType::TOKEN_OPERATOR_BITWISE_AND, "&"));
            }

            break;

        case '|':

            if (peek() == '|')

            {

                advance();

                tokens.push_back(create_token(TokenType::TOKEN_OPERATOR_LOGICAL_OR, "||"));
            }

            else

            {

                tokens.push_back(create_token(TokenType::TOKEN_OPERATOR_BITWISE_OR, "|"));
            }

            break;

        case '=':

            if (peek() == '=')

            {

                advance();

                tokens.push_back(create_token(TokenType::TOKEN_OPERATOR_EQUAL, "=="));
            }

            else

            {

                tokens.push_back(create_token(TokenType::TOKEN_OPERATOR_ASSIGN, "="));
            }

            break;

        case '<':

            if (peek() == '=')

            {

                advance();

                tokens.push_back(create_token(TokenType::TOKEN_OPERATOR_LESS_EQUAL, "<="));
            }

            else

            {

                tokens.push_back(create_token(TokenType::TOKEN_OPERATOR_LESS, "<"));
            }

            break;

        case '>':

            if (peek() == '=')

            {

                advance();

                tokens.push_back(create_token(TokenType::TOKEN_OPERATOR_GREATER_EQUAL, ">="));
            }

            else

            {

                tokens.push_back(create_token(TokenType::TOKEN_OPERATOR_GREATER, ">"));
            }

            break;

            // NEW: Chapter 4 "skip" rules

        case '#':

            while (peek() != '\n' && peek() != '\0')

            {

                advance();
            }

            break;

        case '"':

            while (true)

            {

                if (peek() == '\0')

                {

                    had_error = true;

                    cerr << "Lexer Error: Unterminated string literal." << endl;

                    tokens.push_back(create_token(TokenType::TOKEN_ILLEGAL, "\""));

                    break;
                }

                if (peek() == '\n')

                {

                    had_error = true;

                    cerr << "Lexer Error: Newline in string literal." << endl;

                    tokens.push_back(create_token(TokenType::TOKEN_ILLEGAL, "\""));

                    break;
                }

                char last_char = advance();

                if (last_char == '"')

                {

                    break;
                }

                if (last_char == '\\' && peek() == '\"')

                {

                    advance();
                }
            }

            break;

            // --- ADDED BLOCK for char literals ---

        case '\'':

        {

            size_t literal_start_pos = current_pos - 1; // Start of the literal (at the opening ' )

            char char_value = 0;

            if (peek() == '\'') // Check for empty char literal ''

            {

                had_error = true;

                cerr << "Lexer Error: Empty character literal at line " << line << endl;

                advance(); // consume closing '

                string literal_string = source.substr(literal_start_pos, current_pos - literal_start_pos);

                tokens.push_back(create_token(TokenType::TOKEN_ILLEGAL, literal_string));

                break;
            }

            // Handle escape sequence

            if (peek() == '\\')

            {

                advance(); // consume '\'

                if (peek() == '\0' || peek() == '\n')
                { // Error: unterminated escape

                    had_error = true;

                    cerr << "Lexer Error: Unterminated escape sequence in character literal at line " << line << endl;

                    string literal_string = source.substr(literal_start_pos, current_pos - literal_start_pos);

                    tokens.push_back(create_token(TokenType::TOKEN_ILLEGAL, literal_string));

                    break;
                }

                char escaped_char = advance();

                switch (escaped_char)

                {

                case 'n':
                    char_value = '\n';
                    break;

                case 't':
                    char_value = '\t';
                    break;

                case '0':
                    char_value = '\0';
                    break;

                case '\\':
                    char_value = '\\';
                    break;

                case '\'':
                    char_value = '\'';
                    break;

                default:
                    char_value = escaped_char; // e.g., '\c' is just 'c'
                }
            }

            else // Handle normal character

            {

                char_value = advance();
            }

            // Check for closing quote

            if (peek() != '\'')

            {

                had_error = true;

                cerr << "Lexer Error: Unterminated or multi-character literal at line " << line << endl;

                // consume until a likely end

                while (peek() != '\'' && peek() != '\n' && peek() != '\0')
                    advance();

                if (peek() == '\'')
                    advance(); // consume the ' if it's there

                string literal_string = source.substr(literal_start_pos, current_pos - literal_start_pos);

                tokens.push_back(create_token(TokenType::TOKEN_ILLEGAL, literal_string));

                break;
            }

            advance(); // consume closing '

            string literal_string = source.substr(literal_start_pos, current_pos - literal_start_pos);

            string value_string = std::to_string(static_cast<int>(char_value));

            // Create token with the literal string for correct column calculation

            Token t = create_token(TokenType::TOKEN_CONSTANT, literal_string);

            // Overwrite the token's value with the integer ASCII value

            t.value = value_string;

            tokens.push_back(t);

            break;
        }

            // --- END ADDED BLOCK ---

        default:

            had_error = true;

            cerr << "Lexer Error: Illegal character '" << single_char_val[0] << "' at line " << line << endl;

            tokens.push_back(create_token(TokenType::TOKEN_ILLEGAL, single_char_val));

            break;
        }
    }

    tokens.push_back({TokenType::TOKEN_EOF, "EOF", line, static_cast<int>(current_pos - line_start_pos + 1)});

    return tokens;
}

const char *token_type_to_string(TokenType type)

{

    switch (type)

    {

    case TokenType::TOKEN_EOF:

        return "EOF";

    case TokenType::TOKEN_KEYWORD_INT:

        return "KEYWORD_INT";

    case TokenType::TOKEN_KEYWORD_VOID:

        return "KEYWORD_VOID";

    case TokenType::TOKEN_KEYWORD_RETURN:

        return "KEYWORD_RETURN";

    case TokenType::TOKEN_KEYWORD_CHAR: // <-- ADDED

        return "KEYWORD_CHAR";

    case TokenType::TOKEN_IDENTIFIER:

        return "IDENTIFIER";

    case TokenType::TOKEN_CONSTANT:

        return "CONSTANT";

    case TokenType::TOKEN_CONSTANT_FLOAT:

        return "CONSTANT_FLOAT";

    case TokenType::TOKEN_OPEN_PAREN:

        return "OPEN_PAREN";

    case TokenType::TOKEN_CLOSE_PAREN:

        return "CLOSE_PAREN";

    case TokenType::TOKEN_OPEN_BRACE:

        return "OPEN_BRACE";

    case TokenType::TOKEN_CLOSE_BRACE:

        return "CLOSE_BRACE";

    case TokenType::TOKEN_SEMICOLON:

        return "SEMICOLON";

    case TokenType::TOKEN_OPERATOR_MINUS:

        return "OPERATOR_MINUS";

    case TokenType::TOKEN_OPERATOR_BITWISE_COMPLEMENT:

        return "OPERATOR_BITWISE_COMPLEMENT";

    case TokenType::TOKEN_OPERATOR_PLUS:

        return "OPERATOR_PLUS";

    case TokenType::TOKEN_OPERATOR_MULTIPLY:

        return "OPERATOR_MULTIPLY";

    case TokenType::TOKEN_OPERATOR_DIVIDE:

        return "OPERATOR_DIVIDE";

    case TokenType::TOKEN_OPERATOR_MODULO:

        return "OPERATOR_MODULO";

    case TokenType::TOKEN_OPERATOR_DECREMENT:

        return "OPERATOR_DECREMENT";

        // NEW: Add strings for new operators

    case TokenType::TOKEN_OPERATOR_LOGICAL_NEG:

        return "OPERATOR_LOGICAL_NEG";

    case TokenType::TOKEN_OPERATOR_LOGICAL_AND:

        return "OPERATOR_LOGICAL_AND";

    case TokenType::TOKEN_OPERATOR_LOGICAL_OR:

        return "OPERATOR_LOGICAL_OR";

    case TokenType::TOKEN_OPERATOR_EQUAL:

        return "OPERATOR_EQUAL";

    case TokenType::TOKEN_OPERATOR_NOT_EQUAL:

        return "OPERATOR_NOT_EQUAL";

    case TokenType::TOKEN_OPERATOR_LESS:

        return "OPERATOR_LESS";

    case TokenType::TOKEN_OPERATOR_LESS_EQUAL:

        return "OPERATOR_LESS_EQUAL";

    case TokenType::TOKEN_OPERATOR_GREATER:

        return "OPERATOR_GREATER";

    case TokenType::TOKEN_OPERATOR_GREATER_EQUAL:

        return "OPERATOR_GREATER_EQUAL";

    case TokenType::TOKEN_OPERATOR_ASSIGN:

        return "OPERATOR_ASSIGN";

    case TokenType::TOKEN_OPERATOR_BITWISE_AND:

        return "OPERATOR_BITWISE_AND";

    case TokenType::TOKEN_OPERATOR_BITWISE_OR:

        return "OPERATOR_BITWISE_OR";

    case TokenType::TOKEN_ILLEGAL:

        return "ILLEGAL";

    default:

        return "UNKNOWN";
    }
}