#include <minijava/ast/graphviz.hpp>
#include <minijava/ir/graphviz.hpp>
#include <minijava/ir/ir.hpp>
#include <minijava/parser/parser.hpp>
#include <minijava/typecheck/typecheck.hpp>

#include <fmt/core.h>

using namespace minijava;

enum struct Command {
    Parse,
    IR,
};

static constexpr std::pair<std::string_view, Command> CommandMap[] = {
    {"parse", Command::Parse},
    {"ir", Command::IR},
};

int main(int argc, const char** argv)
{
    if (argc != 3) {
        fmt::print(stderr, "Usage: {} COMMAND FILE\n", argv[0]);
        return EXIT_FAILURE;
    }

    Command command;
    {
        std::string_view cmd_str = argv[1];
        auto* it = std::find_if(std::begin(CommandMap), std::end(CommandMap),
                                [=](auto& p) { return p.first == cmd_str; });
        if (it == std::end(CommandMap)) {
            fmt::print(stderr,
                       "Invalid command: {}.\n"
                       "Valid commands: scan parse typecheck.\n",
                       cmd_str);
            return EXIT_FAILURE;
        }
        command = it->second;
    }

    const char* filename = argv[2];
    FILE* file = std::fopen(filename, "r");
    if (file == nullptr) {
        fmt::print(stderr, "Unable to open file {}\n", filename);
        return EXIT_FAILURE;
    }

    std::fseek(file, 0, SEEK_END);
    size_t file_size = std::ftell(file);
    std::string content(file_size, '\0');
    std::rewind(file);
    std::fread(content.data(), 1, file_size, file);

    std::error_code error;
    DiagnosticEngine diag(content);
    AstTree ast_tree;
    switch (command) {
    case Command::Parse:
        error = parse(content, diag, &ast_tree);
        if (error)
            break;
        print_ast(ast_tree);
        break;
    case Command::IR:
        error = parse(content, diag, &ast_tree);
        if (error)
            break;
        IRTree ir_tree;
        IRBuilder ir_builder(ir_tree);
        typecheck(ast_tree, diag, &ir_builder);
        print_ir(ir_tree);
        break;
    }

    if (error)
        return EXIT_FAILURE;
    return EXIT_SUCCESS;
}
