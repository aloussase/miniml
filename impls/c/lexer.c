#include "lexer.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

int advance(Lexer* lexer)
{
    if (lexer->current < lexer->input_length) {
        return lexer->input[lexer->current++];
    }
    return -1;
}

int peek(Lexer* lexer)
{
    if (lexer->current < lexer->input_length) {
        return lexer->input[lexer->current];
    }
    return -1;
}

int is_at_end(Lexer* lexer) { return lexer->current >= lexer->input_length; }

int is_valid_identifier_char(char c) { return c != ')' && c != '(' && c != ':' && c != ';' && c != ' '; }

Token* make_token(Lexer* lexer, TokenType type)
{
    Token* token         = malloc(sizeof(Token));
    token->type          = type;
    token->line          = lexer->line;
    size_t lexeme_length = lexer->current - lexer->start;
    token->lexeme        = strndup(lexer->input + lexer->start, lexeme_length);
    token->length        = lexeme_length;
    return token;
}

void destroy_token(Token* token)
{
    if (token == NULL) return;

    free(token->lexeme);
    free(token);
}

void lexer_init(Lexer* lexer, const char* input)
{
    lexer->input        = input;
    lexer->input_length = strlen(input);
    lexer->current      = 0;
    lexer->start        = 0;
    lexer->line         = 1;
}

Token* next_token(Lexer* lexer)
{
    if (is_at_end(lexer)) {
        return NULL;
    }

    lexer->start = lexer->current;

    int c = advance(lexer);
    if (c == -1) {
        return NULL;
    }

    switch (c) {
    case '*':
        return make_token(lexer, TT_times);
    case '+':
        return make_token(lexer, TT_plus);
    case '-':
        return make_token(lexer, TT_minus);
    case '<':
        return make_token(lexer, TT_less);
    case '=':
        return make_token(lexer, TT_equal);
    case ':':
        return make_token(lexer, TT_colon);
    case '(':
        return make_token(lexer, TT_lparen);
    case ')':
        return make_token(lexer, TT_rparen);
    case ';':
        if (peek(lexer) == ';') {
            advance(lexer);
            return make_token(lexer, TT_semisemi);
        }
        return NULL;
    case '\n':
        lexer->line++;
        return next_token(lexer);
    case ' ':
    case '\t':
        return next_token(lexer);
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
        while (peek(lexer) != -1 && isdigit(peek(lexer))) {
            advance(lexer);
        }
        return make_token(lexer, TT_int);
    default: {
        while (peek(lexer) != -1 && is_valid_identifier_char(peek(lexer))) {
            advance(lexer);
        }

        uint32_t length   = lexer->current - lexer->start;
        const char* start = lexer->input + lexer->start;

        if (strncmp(start, "int", length) == 0) {
            return make_token(lexer, TT_tint);
        } else if (strncmp(start, "bool", length) == 0) {
            return make_token(lexer, TT_tbool);
        } else if (strncmp(start, "true", length) == 0) {
            return make_token(lexer, TT_true);
        } else if (strncmp(start, "false", length) == 0) {
            return make_token(lexer, TT_false);
        } else if (strncmp(start, "fun", length) == 0) {
            return make_token(lexer, TT_fun);
        } else if (strncmp(start, "is", length) == 0) {
            return make_token(lexer, TT_is);
        } else if (strncmp(start, "if", length) == 0) {
            return make_token(lexer, TT_if);
        } else if (strncmp(start, "then", length) == 0) {
            return make_token(lexer, TT_then);
        } else if (strncmp(start, "else", length) == 0) {
            return make_token(lexer, TT_else);
        } else if (strncmp(start, "let", length) == 0) {
            return make_token(lexer, TT_let);
        } else {
            return make_token(lexer, TT_ident);
        }
    }
    }
}
