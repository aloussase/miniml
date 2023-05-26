#include <stdio.h>

#include "lexer.h"

int main()
{
    const char* program = "let x = 1 * 2;;";

    Token* token;
    Lexer lexer;
    lexer_init(&lexer, program);

    while ((token = next_token(&lexer)) != NULL) {
        printf("%s\n", token->lexeme);
        destroy_token(token);
    }
}
