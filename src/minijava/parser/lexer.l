%{
#define YY_EXTRA_TYPE minijava::BumpAllocator*
#include <parser_impl.hpp>

#define YY_USER_INIT yylineno = yycolumn = 1;

#define YY_USER_ACTION                                                         \
    {                                                                          \
        yylloc->first_line = yylloc->last_line = yylineno;                     \
        yylloc->first_column = yycolumn;                                       \
        yylloc->last_column = yycolumn + yyleng - 1;                           \
        yycolumn += yyleng;                                                    \
    }

using namespace minijava;
%}

%option 8bit reentrant never-interactive bison-bridge bison-locations
%option warn noinput nounput noyywrap nodefault yylineno

NUMBER [0-9]+
IDENT [A-Za-z_][0-9A-Za-z_]*
PRINT System\.out\.println
COMMENT \/\/[^\n]*

%%

{COMMENT}       {}
[\r ]+          {}
"\t"            { yycolumn += 7; }
"\n"            { yycolumn = 1; }

"boolean"       { return TOK_BOOL; }
"class"         { return TOK_CLASS; }
"else"          { return TOK_ELSE; }
"extends"       { return TOK_EXTENDS; }
"false"         { return TOK_FALSE; }
"if"            { return TOK_IF; }
"int"           { return TOK_INT; }
"new"           { return TOK_NEW; }
"private"       { return TOK_PRIVATE; }
"public"        { return TOK_PUBLIC; }
"return"        { return TOK_RETURN; }
"static"        { return TOK_STATIC; }
"String"        { return TOK_STRING; }
"this"          { return TOK_THIS; }
"true"          { return TOK_TRUE; }
"void"          { return TOK_VOID; }
"while"         { return TOK_WHILE; }
{PRINT}         { return TOK_PRINT; }

"\{"            { return TOK_LBRACE; }
"\}"            { return TOK_RBRACE; }
"\["            { return TOK_LBRACKET; }
"\]"            { return TOK_RBRACKET; }
"\("            { return TOK_LPAREN; }
"\)"            { return TOK_RPAREN; }

"!"             { return TOK_EXCLAIM; }
"!="            { return TOK_EXCLAIM_EQUAL; }
"="             { return TOK_EQUAL; }
"=="            { return TOK_EQUAL_EQUAL; }
"<"             { return TOK_LESS; }
"<="            { return TOK_LESS_EQUAL; }
">"             { return TOK_GREATER; }
">="            { return TOK_GREATER_EQUAL; }
"&&"            { return TOK_AMP_AMP; }
"||"            { return TOK_PIPE_PIPE; }
"+"             { return TOK_PLUS; }
"-"             { return TOK_MINUS; }
"*"             { return TOK_STAR; }
"\/"            { return TOK_SLASH; }
"%"             { return TOK_PERCENT; }

","             { return TOK_COMMA; }
"\."            { return TOK_DOT; }
";"             { return TOK_SEMICOLON; }

{NUMBER}        {
                    yylval->string = dup_string(*yyextra, yytext, yyleng);
                    return TOK_NUMBER;
                }

{IDENT}         {
                    yylval->symbol = Symbol(*yyextra, yytext, yyleng);
                    return TOK_IDENT;
                }

.               { return TOK_INVALID; }
