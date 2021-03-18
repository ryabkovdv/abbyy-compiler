%{

#include <parser.hpp>

%}

%option reentrant noyywrap never-interactive

NUMBER [0-9]+
SPACE [\t\n\v\f\r ]
IDENTIFIER [A-Za-z_][0-9A-Za-z_]*
COMMENT \/\/[^\n]*

%%

{SPACE}+        {}
{COMMENT}       {}

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

"\{"            { return TOK_LBRACE; }
"\}"            { return TOK_RBRACE; }
"\["            { return TOK_LBRACKET; }
"\]"            { return TOK_RBRACKET; }
"\("            { return TOK_LPAREN; }
"\)"            { return TOK_RPAREN; }

"!"             { return TOK_EXCLAIM; }
"!="            { return TOK_EXCLAIMEQUAL; }
"="             { return TOK_EQUAL; }
"=="            { return TOK_EQUALEQUAL; }
"<"             { return TOK_LESS; }
"<="            { return TOK_LESSEQUAL; }
">"             { return TOK_GREATER; }
">="            { return TOK_GREATEREQUAL; }
"&&"            { return TOK_AMPAMP; }
"||"            { return TOK_PIPEPIPE; }
"+"             { return TOK_PLUS; }
"-"             { return TOK_MINUS; }
"*"             { return TOK_STAR; }
"\/"            { return TOK_SLASH; }
"%"             { return TOK_PERCENT; }

","             { return TOK_COMMA; }
"\."            { return TOK_DOT; }
";"             { return TOK_SEMICOLON; }

.               { return TOK_ERROR; }
{NUMBER}        { return TOK_NUMBER; }
{IDENTIFIER}    { return TOK_IDENTIFIER; }

%%
