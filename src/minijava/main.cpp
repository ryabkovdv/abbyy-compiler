#include <minijava/ast/graphviz.hpp>
#include <minijava/parser/parser.hpp>

#include <fmt/format.h>

using namespace minijava;

int main(int argc, const char** argv)
{
    if (argc != 2) {
        fmt::print(stderr, "Usage: {} FILE\n", argv[0]);
        return EXIT_FAILURE;
    }

    const char* filename = argv[1];
    FILE* file = std::fopen(filename, "r");
    if (file == nullptr) {
        fmt::print(stderr, "Unable to open file {}\n", filename);
        return EXIT_FAILURE;
    }

    AstTree tree;
    auto error = parse(file, &tree);
    if (error) {
        fmt::print(stderr, "Error: {}\n", error.message());
        return EXIT_FAILURE;
    }

    print_ast(tree);
    return EXIT_SUCCESS;
}
