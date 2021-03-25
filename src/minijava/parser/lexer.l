%{
#define YY_EXTRA_TYPE minijava::BumpAllocator*
#include <parser_impl.hpp>

#define YY_USER_ACTION                                                         \
    {                                                                          \
        yylloc->first_line = yylloc->last_line;                                \
        yylloc->first_column = yylloc->last_column;                            \
        if (yylloc->last_line == yylineno) {                                   \
            yylloc->last_column += yyleng;                                     \
        } else {                                                               \
            yylloc->last_line = yylineno;                                      \
            yylloc->last_column = yytext + yyleng - strrchr(yytext, '\n');     \
        }                                                                      \
    }

using namespace minijava;

static std::string_view
make_gcstring(const char* str, int len, BumpAllocator* pool);
%}

%option 8bit reentrant never-interactive bison-bridge bison-locations
%option warn noinput nounput noyywrap nodefault yylineno

SPACE [\t\n\v\f\r ]+
NUMBER [0-9]+
IDENT [A-Za-z_][0-9A-Za-z_]*
PRINT System\.out\.println
COMMENT \/\/[^\n]*

%%

{SPACE}         {}
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
                    yylval->string = make_gcstring(yytext, yyleng, yyextra);
                    return TOK_NUMBER;
                }

{IDENT}    {
                    yylval->string = make_gcstring(yytext, yyleng, yyextra);
                    return TOK_IDENT;
                }

.               { return TOK_INVALID; }

%%

static std::string_view
make_gcstring(const char* str, int len, BumpAllocator* pool)
{
    size_t size = (unsigned)len;
    auto* data = static_cast<char*>(pool->allocate(size, 1));
    std::memcpy(data, str, size);
    return std::string_view(data, size);
}
