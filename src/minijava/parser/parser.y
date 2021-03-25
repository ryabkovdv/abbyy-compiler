%{
#define POOL (tree->pool)
#define YY_EXTRA_TYPE minijava::BumpAllocator*
#define YYCOPY(Dst, Src, Count) \
        std::memcpy((void*)(Dst), (const void*)(Src), (Count) * sizeof(*(Src)))

#include <parser_impl.hpp>
#include <lexer_impl.hpp>

using namespace minijava;

template <typename T>
static void init_array(GCArray<T>& array, BumpAllocator& pool);

template <typename T>
static void move_array(GCArray<T>& to, GCArray<T>& from);

static const Stmt*
make_assign_stmt(const Expr* target, const Expr* value, AstTree* tree);

static void yyerror(YYLTYPE* yyllocp, yyscan_t, AstTree* tree, const char* msg);
%}

%define api.pure full
%define api.value.type { minijava::YYSTYPE }
%define parse.error verbose
%locations
%token-table

%param { yyscan_t scanner }
%parse-param { minijava::AstTree* tree }

%code requires {
#include <minijava/ast/ast.hpp>

using yyscan_t = void*;

namespace minijava {
union YYSTYPE {
    YYSTYPE(YYSTYPE&&) noexcept = default;
    YYSTYPE& operator=(YYSTYPE&&) noexcept = default;

    YYSTYPE() noexcept : access()
    {}

    YYSTYPE(YYSTYPE& other) noexcept : YYSTYPE(std::move(other))
    {}

    YYSTYPE& operator=(YYSTYPE& other) noexcept
    {
        *this = std::move(other);
        return *this;
    }

    MethodDecl::Access access;
    std::string_view string;
    const Expr* expr;
    const Stmt* stmt;
    const ClassDecl* class_decl;
    const ClassRecord* class_record;
    GCArray<const ClassDecl*> classes;
    GCArray<const ClassRecord*> class_records;
    GCArray<const Stmt*> statements;
    GCArray<Parameter> parameters;
    GCArray<const Expr*> arguments;
};
} // namespace minijava
}

%token TOK_INVALID
%token TOK_AMP_AMP
%token TOK_BOOL
%token TOK_CLASS
%token TOK_COMMA
%token TOK_DOT
%token TOK_ELSE
%token TOK_EQUAL
%token TOK_EQUAL_EQUAL
%token TOK_EXCLAIM
%token TOK_EXCLAIM_EQUAL
%token TOK_EXTENDS
%token TOK_FALSE
%token TOK_GREATER
%token TOK_GREATER_EQUAL
%token TOK_IDENT
%token TOK_IF
%token TOK_INT
%token TOK_LBRACE
%token TOK_LBRACKET
%token TOK_LESS
%token TOK_LESS_EQUAL
%token TOK_LPAREN
%token TOK_MINUS
%token TOK_NEW
%token TOK_NUMBER
%token TOK_PERCENT
%token TOK_PIPE_PIPE
%token TOK_PLUS
%token TOK_PRINT
%token TOK_PRIVATE
%token TOK_PUBLIC
%token TOK_RBRACE
%token TOK_RBRACKET
%token TOK_RETURN
%token TOK_RPAREN
%token TOK_SEMICOLON
%token TOK_SLASH
%token TOK_STAR
%token TOK_STATIC
%token TOK_STRING
%token TOK_THIS
%token TOK_TRUE
%token TOK_VOID
%token TOK_WHILE

%type<access> Access
%type<string> TOK_IDENT TOK_NUMBER BaseClass Type
%type<expr> Expression UnaryExpression PrimaryExpression
%type<stmt> Statement
%type<class_decl> ClassDeclaration
%type<class_record> ClassRecord
%type<classes> ClassList
%type<class_records> ClassRecords
%type<statements> Statements Main
%type<parameters> Parameters ParameterList
%type<arguments> Arguments ArgumentList

%left TOK_PIPE_PIPE
%left TOK_AMP_AMP
%left TOK_EQUAL_EQUAL TOK_EXCLAIM_EQUAL TOK_LESS TOK_LESS_EQUAL TOK_GREATER TOK_GREATER_EQUAL
%left TOK_PLUS TOK_MINUS
%left TOK_STAR TOK_SLASH TOK_PERCENT

%%

Module
    : Main ClassList[list]
        { tree->classes = $list; }

Main
    : TOK_CLASS TOK_IDENT TOK_LBRACE
      TOK_PUBLIC TOK_STATIC TOK_VOID TOK_IDENT[name]
      TOK_LPAREN TOK_STRING TOK_LBRACKET TOK_RBRACKET TOK_IDENT TOK_RPAREN
      TOK_LBRACE Statements[body] TOK_RBRACE TOK_RBRACE
        {
            if ($name != "main") {
                yyerror(&@name, scanner, tree,
                        "entrypoint must be called `main'");
            }
            tree->main = $body;
        }

ClassList
    : %empty { init_array($$, POOL); }
    | ClassList[list] ClassDeclaration[decl]
        { move_array($$, $list); $$.push_back($decl); }

ClassDeclaration
    : TOK_CLASS TOK_IDENT[name] BaseClass[base]
      TOK_LBRACE ClassRecords[records] TOK_RBRACE
        { $$ = new(POOL) ClassDecl($name, $base, $records); }

BaseClass
    : %empty { $$ = {}; }
    | TOK_EXTENDS TOK_IDENT[name] { $$ = $name; }

ClassRecord
    : Type[type] TOK_IDENT[name] TOK_SEMICOLON
        { $$ = new(POOL) ClassVarDecl($name, $type); }
    | Access[access] Type[type] TOK_IDENT[name]
      TOK_LPAREN Parameters[params] TOK_RPAREN
      TOK_LBRACE Statements[body] TOK_RBRACE
        { $$ = new(POOL) MethodDecl($access, $name, $type, $params, $body); }

Access
    : TOK_PUBLIC { $$ = MethodDecl::Public; }
    | TOK_PRIVATE { $$ = MethodDecl::Private; }

Type
    : TOK_BOOL { $$ = "bool"; }
    | TOK_INT { $$ = "int"; }
    | TOK_INT TOK_LBRACKET TOK_RBRACKET { $$ = "int[]"; }
    | TOK_IDENT[name] { $$ = $name; }

Statement
    : Type[type] TOK_IDENT[name] TOK_SEMICOLON
        { $$ = new(POOL) VarDecl($name, $type); }
    | TOK_LBRACE Statements[list] TOK_RBRACE
        { $$ = new(POOL) CompoundStmt($list); }
    | TOK_IF TOK_LPAREN Expression[cond] TOK_RPAREN
      Statement[if_branch] TOK_ELSE Statement[else_branch]
        { $$ = new(POOL) IfStmt($cond, $if_branch, $else_branch); }
    | TOK_WHILE TOK_LPAREN Expression[cond] TOK_RPAREN Statement[body]
        { $$ = new(POOL) WhileStmt($cond, $body); }
    | TOK_RETURN Expression[expr] TOK_SEMICOLON
        { $$ = new(POOL) ReturnStmt($expr); }
    | TOK_PRINT TOK_LPAREN Arguments[args] TOK_RPAREN TOK_SEMICOLON
        { $$ = new(POOL) PrintStmt($args); }
    | Expression[expr] TOK_SEMICOLON
        { $$ = $expr; }
    | PrimaryExpression[target] TOK_EQUAL Expression[value] TOK_SEMICOLON
        {
            $$ = make_assign_stmt($target, $value, tree);
            if ($$ == nullptr) {
                yyerror(&@target, scanner, tree,
                        "left hand side is not an lvalue");
            }
        }

Expression
    : UnaryExpression
    | Expression[left] TOK_STAR Expression[right]
        { $$ = new(POOL) MulExpr($left, $right); }
    | Expression[left] TOK_SLASH Expression[right]
        { $$ = new(POOL) DivExpr($left, $right); }
    | Expression[left] TOK_PERCENT Expression[right]
        { $$ = new(POOL) RemExpr($left, $right); }
    | Expression[left] TOK_PLUS Expression[right]
        { $$ = new(POOL) AddExpr($left, $right); }
    | Expression[left] TOK_MINUS Expression[right]
        { $$ = new(POOL) SubExpr($left, $right); }
    | Expression[left] TOK_EQUAL_EQUAL Expression[right]
        { $$ = new(POOL) EqExpr($left, $right); }
    | Expression[left] TOK_EXCLAIM_EQUAL Expression[right]
        { $$ = new(POOL) NeExpr($left, $right); }
    | Expression[left] TOK_LESS Expression[right]
        { $$ = new(POOL) LtExpr($left, $right); }
    | Expression[left] TOK_LESS_EQUAL Expression[right]
        { $$ = new(POOL) LeExpr($left, $right); }
    | Expression[left] TOK_GREATER Expression[right]
        { $$ = new(POOL) GtExpr($left, $right); }
    | Expression[left] TOK_GREATER_EQUAL Expression[right]
        { $$ = new(POOL) GeExpr($left, $right); }
    | Expression[left] TOK_AMP_AMP Expression[right]
        { $$ = new(POOL) AndExpr($left, $right); }
    | Expression[left] TOK_PIPE_PIPE Expression[right]
        { $$ = new(POOL) OrExpr($left, $right); }

UnaryExpression
    : PrimaryExpression
    | TOK_EXCLAIM UnaryExpression[expr]
        { $$ = new(POOL) NotExpr($expr); }

PrimaryExpression
    : TOK_IDENT
        { $$ = new(POOL) IdentExpr($1); }
    | TOK_NUMBER
        { $$ = new(POOL) IntExpr($1); }
    | TOK_TRUE
        { $$ = &BoolExpr::TrueValue; }
    | TOK_FALSE
        { $$ = &BoolExpr::FalseValue; }
    | TOK_THIS
        { $$ = &ThisExpr::Value; }
    | TOK_LPAREN Expression TOK_RPAREN { $$ = $2; }
    | TOK_NEW TOK_INT TOK_LBRACKET Expression[count] TOK_RBRACKET
        { $$ = new(POOL) NewIntArrayExpr($count); }
    | TOK_NEW TOK_IDENT[name] TOK_LPAREN TOK_RPAREN
        { $$ = new(POOL) NewObjectExpr($name); }
    | PrimaryExpression[array] TOK_LBRACKET Expression[index] TOK_RBRACKET
        { $$ = new(POOL) SubscriptExpr($array, $index); }
    | PrimaryExpression[object] TOK_DOT TOK_IDENT[name]
      TOK_LPAREN Arguments[args] TOK_RPAREN
        { $$ = new(POOL) CallExpr($object, $name, $args); }
    | PrimaryExpression[object] TOK_DOT TOK_IDENT[name]
        {
            if ($name == "length") {
                $$ = new(POOL) LengthExpr($object);
            } else {
                yyerror(&@name, scanner, tree,
                        "only `length' property is supported");
                $$ = nullptr;
            }
        }

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
    : Type[type] TOK_IDENT[name]
        { init_array($$, POOL); $$.emplace_back($name, $type); }
    | ParameterList[list] TOK_COMMA Type[type] TOK_IDENT[name]
        { move_array($$, $list); $$.emplace_back($name, $type); }

Arguments
    : %empty { init_array($$, POOL); }
    | ArgumentList

ArgumentList
    : Expression[expr]
        { init_array($$, POOL); $$.push_back($expr); }
    | ArgumentList[list] TOK_COMMA Expression[expr]
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

static const Stmt*
make_assign_stmt(const Expr* target, const Expr* value, AstTree* tree)
{
    if (auto* ident = dyn_cast<IdentExpr>(target))
        return new(POOL) VarAssignStmt(ident->name, value);

    auto* subscript = dyn_cast<SubscriptExpr>(target);
    if (subscript == nullptr)
        return nullptr;

    auto* ident = dyn_cast<IdentExpr>(subscript->array);
    if (ident == nullptr)
        return nullptr;

    return new(POOL) ArrayAssignStmt(ident->name, subscript->index, value);
}

static void yyerror(YYLTYPE* yyllocp, yyscan_t, AstTree* tree, const char* msg)
{
    tree->valid = false;
    std::fprintf(stderr, "%d:%d: %s\n",
                 yyllocp->first_line, yyllocp->first_column, msg);
}

void print_token(int token)
{
    std::puts(yytname[YYTRANSLATE(token)]);
}
