#ifndef lexer_h__
#define lexer_h__

#include <stdint.h>

typedef enum {
    TT_tint,
    TT_tbool,
    TT_colon,
    TT_int,
    TT_true,
    TT_false,
    TT_fun,
    TT_is,
    TT_if,
    TT_then,
    TT_else,
    TT_lparen,
    TT_rparen,
    TT_less,
    TT_equal,
    TT_plus,
    TT_minus,
    TT_times,
    TT_ident,
    TT_let,
    TT_semisemi,
    TT_eof,
} TokenType;

typedef struct {
    TokenType type;
    char* lexeme;
    uint32_t length;
    uint32_t line;
} Token;

typedef struct {
    const char* input;
    uint32_t input_length;
    uint32_t current;
    uint32_t start;
    uint32_t line;
} Lexer;

/**
 * Initialize the lexer state.
 */
void lexer_init(Lexer* lexer, const char* input);

/**
 * Return the next token in the input stream or NULL
 * if there are none.
 */
Token* next_token(Lexer* lexer);

/**
 * Deallocate memory allocated for the given token.
 */
void destroy_token(Token* token);

#endif
