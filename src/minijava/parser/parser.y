%{
#define POOL (tree->pool)
#define YY_EXTRA_TYPE minijava::detail::yy_extra_type*

#include <minijava/parser/yyextra.hpp>
#include <parser_impl.hpp>
#include <lexer_impl.hpp>

#pragma GCC diagnostic ignored "-Wclass-memaccess"
#pragma GCC diagnostic ignored "-Wfree-nonheap-object"

using namespace minijava;

template <typename T>
static void init_array(GCArray<T>& array, BumpAllocator& pool);

template <typename T>
static void move_array(GCArray<T>& to, GCArray<T>& from);

static void yyerror(YYLTYPE* yyllocp, yyscan_t, AstTree*,
                    DiagnosticEngine& diag, const char* msg);
%}

%define api.pure full
%define api.value.type {minijava::YYSTYPE}
%define api.token.prefix {TOK_}
%define parse.error verbose
%locations

%param {yyscan_t scanner}
%parse-param {minijava::AstTree* tree}
%parse-param {minijava::DiagnosticEngine& diag}

%initial-action { @$ = {1, 1, 1, 1}; }

%code requires {
#include <minijava/diagnostic.hpp>

#include <minijava/ast/ast.hpp>

#define YYLTYPE minijava::Location

using yyscan_t = void*;

namespace minijava {
union YYSTYPE {
    YYSTYPE(YYSTYPE&&) noexcept = default;
    YYSTYPE& operator=(YYSTYPE&&) noexcept = default;

    constexpr YYSTYPE() noexcept : access()
    {}

    constexpr YYSTYPE(YYSTYPE& other) noexcept : YYSTYPE(std::move(other))
    {}

    constexpr YYSTYPE& operator=(YYSTYPE& other) noexcept
    {
        *this = std::move(other);
        return *this;
    }

    MethodDecl::Access access;
    std::string_view string;
    Symbol symbol;
    const Expr* expr;
    const Stmt* stmt;
    const ClassDecl* class_decl;
    const ClassRecord* class_record;
    GCArray<const ClassDecl*> classes;
    GCArray<const ClassRecord*> class_records;
    GCArray<const Stmt*> statements;
    GCArray<Parameter> params;
    GCArray<const Expr*> args;
};
} // namespace minijava
}

%token END 0            "end of file"
%token INVALID          "invalid token"
%token IDENT            "identifier"
%token NUMBER           "number"
%token BOOL             "boolean"
%token CLASS            "class"
%token ELSE             "else"
%token EXTENDS          "extends"
%token FALSE            "false"
%token IF               "if"
%token INT              "int"
%token NEW              "new"
%token PRIVATE          "private"
%token PUBLIC           "public"
%token RETURN           "return"
%token STATIC           "static"
%token STRING           "String"
%token THIS             "this"
%token TRUE             "true"
%token VOID             "void"
%token WHILE            "while"
%token PRINT            "System.out.println"
%token LBRACE           "{"
%token RBRACE           "}"
%token LBRACKET         "["
%token RBRACKET         "]"
%token LPAREN           "("
%token RPAREN           ")"
%token EXCLAIM          "!"
%token EXCLAIM_EQUAL    "!="
%token EQUAL            "="
%token EQUAL_EQUAL      "=="
%token LESS             "<"
%token LESS_EQUAL       "<="
%token GREATER          ">"
%token GREATER_EQUAL    ">="
%token AMP_AMP          "&&"
%token PIPE_PIPE        "||"
%token PLUS             "+"
%token MINUS            "-"
%token STAR             "*"
%token SLASH            "/"
%token PERCENT          "%"
%token COMMA            ","
%token DOT              "."
%token SEMICOLON        ";"

%type<access> Access
%type<string> NUMBER
%type<symbol> IDENT BaseClass Type
%type<expr> Expression PrimaryExpression
%type<stmt> Statement
%type<class_decl> ClassDeclaration
%type<class_record> ClassRecord
%type<classes> Classes
%type<class_records> ClassRecords
%type<statements> Statements Main
%type<params> Parameters ParameterList
%type<args> Arguments ArgumentList

%left "||"
%left "&&"
%left "!=" "==" "<" "<=" ">" ">="
%left "+" "-"
%left "*" "/" "%"
%left "!"

%%

Module
    : Main Classes[classes]
        { tree->classes = $classes; }

Main
    : "class" IDENT "{" "public" "static" "void" IDENT[name]
      "(" "String" "[" "]" IDENT ")" "{" Statement[body] "}" "}"
        {
            if ($name.to_view() != "main") {
                yyerror(&@name, scanner, tree, diag,
                        "entrypoint must be called `main'");
                YYERROR;
            }
            tree->main = $body;
        }

ClassDeclaration
    : "class" IDENT[name] BaseClass[base] "{" ClassRecords[records] "}"
        { $$ = new(POOL) ClassDecl($name, $base, $records); }

BaseClass
    : %empty { $$ = {}; }
    | "extends" IDENT[name] { $$ = $name; }

ClassRecord
    : Type[type] IDENT[name] ";"
        { $$ = new(POOL) ClassVarDecl($name, $type); }
    | Access[access] Type[type] IDENT[name]
      "(" Parameters[params] ")" "{" Statements[body] "}"
        { $$ = new(POOL) MethodDecl($access, $name, $type, $params, $body); }

Access
    : "public" { $$ = MethodDecl::Public; }
    | "private" { $$ = MethodDecl::Private; }

Type
    : "boolean" { $$ = Symbol(POOL, "bool"); }
    | "int" { $$ = Symbol(POOL, "int"); }
    | "int" "[" "]" { $$ = Symbol(POOL, "int[]"); }
    | IDENT[name] { $$ = $name; }

Statement
    : Type[type] IDENT[name] ";"
        { $$ = new(POOL) VarDecl($name, $type); }
    | "{" Statements[body] "}"
        { $$ = new(POOL) CompoundStmt($body); }
    | "if" "(" Expression[cond] ")" Statement[then_branch]
      "else" Statement[else_branch]
        { $$ = new(POOL) IfStmt($cond, $then_branch, $else_branch); }
    | "while" "(" Expression[cond] ")" Statement[body]
        { $$ = new(POOL) WhileStmt($cond, $body); }
    | "return" Expression[expr] ";"
        { $$ = new(POOL) ReturnStmt($expr); }
    | PRINT "(" Arguments[args] ")" ";"
        { $$ = new(POOL) PrintStmt($args); }
    | Expression[expr] ";"
        { $$ = $expr; }
    | Expression[lhs] "=" Expression[rhs] ";"
        { $$ = new(POOL) AssignStmt($lhs, $rhs); }

Expression
    : PrimaryExpression
    | "!" Expression[expr]
        { $$ = new(POOL) NotExpr($expr); }
    | Expression[lhs] "*" Expression[rhs]
        { $$ = new(POOL) MulExpr($lhs, $rhs); }
    | Expression[lhs] "/" Expression[rhs]
        { $$ = new(POOL) DivExpr($lhs, $rhs); }
    | Expression[lhs] "%" Expression[rhs]
        { $$ = new(POOL) RemExpr($lhs, $rhs); }
    | Expression[lhs] "+" Expression[rhs]
        { $$ = new(POOL) AddExpr($lhs, $rhs); }
    | Expression[lhs] "-" Expression[rhs]
        { $$ = new(POOL) SubExpr($lhs, $rhs); }
    | Expression[lhs] "!=" Expression[rhs]
        { $$ = new(POOL) NeExpr($lhs, $rhs); }
    | Expression[lhs] "==" Expression[rhs]
        { $$ = new(POOL) EqExpr($lhs, $rhs); }
    | Expression[lhs] "<" Expression[rhs]
        { $$ = new(POOL) LtExpr($lhs, $rhs); }
    | Expression[lhs] "<=" Expression[rhs]
        { $$ = new(POOL) LeExpr($lhs, $rhs); }
    | Expression[lhs] ">" Expression[rhs]
        { $$ = new(POOL) GtExpr($lhs, $rhs); }
    | Expression[lhs] ">=" Expression[rhs]
        { $$ = new(POOL) GeExpr($lhs, $rhs); }
    | Expression[lhs] "&&" Expression[rhs]
        { $$ = new(POOL) AndExpr($lhs, $rhs); }
    | Expression[lhs] "||" Expression[rhs]
        { $$ = new(POOL) OrExpr($lhs, $rhs); }

PrimaryExpression
    : IDENT
        { $$ = new(POOL) IdentExpr($1); }
    | NUMBER
        { $$ = new(POOL) IntLiteral($1); }
    | "this"
        { $$ = &ThisExpr::Instance; }
    | "true"
        { $$ = &BoolLiteral::True; }
    | "false"
        { $$ = &BoolLiteral::False; }
    | "(" Expression ")"
        { $$ = $2; }
    | "new" "int" "[" Expression[count] "]"
        { $$ = new(POOL) NewIntArrayExpr($count); }
    | "new" IDENT[name] "(" ")"
        { $$ = new(POOL) NewObjectExpr($name); }
    | PrimaryExpression[array] "[" Expression[index] "]"
        { $$ = new(POOL) SubscriptExpr($array, $index); }
    | PrimaryExpression[object] "." IDENT[name] "(" Arguments[args] ")"
        { $$ = new(POOL) CallExpr($object, $name, $args); }
    | PrimaryExpression[object] "." IDENT[name]
        {
            if ($name.to_view() != "length") {
                yyerror(&@name, scanner, tree, diag,
                        "only `length' property is supported");
                YYERROR;
            }
            $$ = new(POOL) LengthExpr($object);
        }

Classes
    : %empty { init_array($$, POOL); }
    | Classes[list] ClassDeclaration[decl]
        { move_array($$, $list); $$.push_back($decl); }

ClassRecords
    : %empty { init_array($$, POOL); }
    | ClassRecords[list] ClassRecord[decl]
        { move_array($$, $list); $$.push_back($decl); }

Statements
    : %empty { init_array($$, POOL); }
    | Statements[list] Statement[stmt]
        { move_array($$, $list); $$.push_back($stmt); }

Parameters
    : %empty { init_array($$, POOL); }
    | ParameterList

ParameterList
    : Type[type] IDENT[name]
        { init_array($$, POOL); $$.emplace_back($name, $type); }
    | ParameterList[list] "," Type[type] IDENT[name]
        { move_array($$, $list); $$.emplace_back($name, $type); }

Arguments
    : %empty { init_array($$, POOL); }
    | ArgumentList

ArgumentList
    : Expression[expr]
        { init_array($$, POOL); $$.push_back($expr); }
    | ArgumentList[list] "," Expression[expr]
        { move_array($$, $list); $$.push_back($expr); }

%%

template <typename T>
static void init_array(GCArray<T>& array, BumpAllocator& pool)
{
    ::new(&array) GCArray<T>(pool);
}

template <typename T>
static void move_array(GCArray<T>& to, GCArray<T>& from)
{
    ::new(&to) GCArray<T>(std::move(from));
}

static void yyerror(YYLTYPE* yyllocp, yyscan_t, AstTree*,
                    DiagnosticEngine& diag, const char* msg)
{
    diag.error(*yyllocp, "{}", msg);
}
