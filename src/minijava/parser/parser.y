%{

#include <cstdio>

#include <parser.hpp>
#include <lexer.hpp>

int yyerror(yyscan_t scanner, const char* msg)
{
    std::fprintf(stderr, "%s\n", msg);
    return 0;
}

%}

%code requires
{

using yyscan_t = void*;

}

%param { yyscan_t scanner }

%union {
    int number;
    void* expr;
}

%token TOK_IDENTIFIER
%token TOK_BOOL
%token TOK_CLASS
%token TOK_ELSE
%token TOK_EXTENDS
%token TOK_FALSE
%token TOK_IF
%token TOK_INT
%token TOK_NEW
%token TOK_PRIVATE
%token TOK_PUBLIC
%token TOK_RETURN
%token TOK_STATIC
%token TOK_STRING
%token TOK_THIS
%token TOK_TRUE
%token TOK_VOID
%token TOK_WHILE
%token TOK_LBRACE
%token TOK_RBRACE
%token TOK_LBRACKET
%token TOK_RBRACKET
%token TOK_LPAREN
%token TOK_RPAREN
%token TOK_EXCLAIM
%token TOK_EXCLAIMEQUAL
%token TOK_EQUAL
%token TOK_EQUALEQUAL
%token TOK_LESS
%token TOK_LESSEQUAL
%token TOK_GREATER
%token TOK_GREATEREQUAL
%token TOK_AMPAMP
%token TOK_PIPEPIPE
%token TOK_PLUS
%token TOK_MINUS
%token TOK_STAR
%token TOK_SLASH
%token TOK_PERCENT
%token TOK_COMMA
%token TOK_DOT
%token TOK_SEMICOLON
%token TOK_ERROR
%token <number> TOK_NUMBER
%type  <expr> Expression

%left TOK_PLUS TOK_MINUS
%left TOK_STAR TOK_SLASH TOK_PERCENT

%%

Start: Expression[E] {}
;

Expression: TOK_NUMBER[N] { std::puts("Parsed number"); }
          | TOK_LPAREN Expression[E] TOK_RPAREN {}
          | Expression[L] TOK_PLUS    Expression[R] { std::puts("Parsed 'add'"); }
          | Expression[L] TOK_MINUS   Expression[R] { std::puts("Parsed 'sub'"); }
          | Expression[L] TOK_STAR    Expression[R] { std::puts("Parsed 'mul'"); }
          | Expression[L] TOK_SLASH   Expression[R] { std::puts("Parsed 'div'"); }
          | Expression[L] TOK_PERCENT Expression[R] { std::puts("Parsed 'rem'"); }
;
