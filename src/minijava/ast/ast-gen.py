import json
import sys
from io import StringIO
from string import Template
from time import time_ns


class Node:
    __slots__ = ("name", "base", "fields", "subclasses")

    def __init__(self, name, base, fields):
        self.name = name
        self.base = base
        self.fields = fields
        self.subclasses = None

    def get_base_fields(self):
        base = self.base
        if base is None:
            return ()
        return base.get_base_fields() + base.fields

    def find_min_leaf(self):
        if not self.subclasses:
            return self
        return self.subclasses[0].find_min_leaf()

    def find_max_leaf(self):
        if not self.subclasses:
            return self
        return self.subclasses[-1].find_max_leaf()


class Hierarchy:
    __slots__ = ("root", "nodes", "leaves", "buf")

    def __init__(self, root):
        self.root = root
        self.nodes = []
        if root.subclasses:
            self.leaves = []
            for subclass in root.subclasses:
                self.dfs(subclass)
        else:
            self.leaves = [root]

    def dfs(self, node):
        # TODO: check for loops
        if node.fields is not None:
            self.nodes.append(node)
        if node.subclasses:
            for subclass in node.subclasses:
                self.dfs(subclass)
        else:
            self.leaves.append(node)

    def write(self, s):
        self.buf.write(s)

    def generate(self):
        self.buf = StringIO()

        self.write(f"struct {self.root.name} {{\n")
        if len(self.leaves) > 1:
            self.generate_tags()
        self.generate_fields(self.root)
        self.generate_constructor(self.root)
        self.write("};\n")

        for node in self.nodes:
            self.generate_node(node)

        return self.buf.getvalue()

    def generate_tags(self):
        self.write("    enum Kind : int {\n")
        for leaf in self.leaves:
            self.write(f"        {leaf.name},\n")
        self.write("    } kind;\n")

    def generate_node(self, node):
        if node.base:
            base = f": {node.base.name} "
        else:
            base = ""
        self.write(f"struct {node.name} {base}{{\n")

        self.generate_fields(node)
        self.generate_constructor(node)
        self.generate_classof(node)
        self.write("};\n")

    def generate_fields(self, node):
        for name, field_type in node.fields:
            self.write(f"    {field_type} {name};\n")

    def generate_constructor(self, node):
        if node.subclasses:
            return

        base_fields = node.get_base_fields()
        all_fields = base_fields + node.fields

        arguments = [f"{type} {name}" for name, type in all_fields]

        if len(self.leaves) > 1:
            base_init_list = [f"{self.root.name}::{node.name}"]
        else:
            base_init_list = []
        base_init_list.extend(name for name, _ in base_fields)

        if base_init_list:
            base_init_list = ", ".join(base_init_list)
            init_list = [f"{node.base.name}{{{base_init_list}}}"]
        else:
            init_list = []
        init_list.extend(f"{name}({name})" for name, _ in node.fields)

        if not init_list:
            return

        constructor = CONSTRUCTOR.substitute(name=node.name,
                                             arguments=", ".join(arguments),
                                             init_list=", ".join(init_list))
        self.write(constructor)

    def generate_classof(self, node):
        if len(self.leaves) == 1:
            classof = CLASSOF_TRUE.substitute(root=self.root.name)
        elif node.subclasses:
            classof = CLASSOF_BASE.substitute(root=self.root.name,
                                              first=node.find_min_leaf().name,
                                              last=node.find_max_leaf().name)
        else:
            classof = CLASSOF_LEAF.substitute(root=self.root.name,
                                              name=node.name)
        self.write(classof)


CONSTRUCTOR = Template(R"""
    explicit $name($arguments)
        : $init_list
    {}
""")

CLASSOF_TRUE = Template(R"""
    static bool classof(const $root* base)
    {
        return true;
    }
""")

CLASSOF_BASE = Template(R"""
    static bool classof(const $root* base)
    {
        auto kind = base->kind;
        return $root::$first <= kind && kind <= $root::$last;
    }
""")

CLASSOF_LEAF = Template(R"""
    static bool classof(const $root* base)
    {
        return base->kind == $root::$name;
    }
""")

HEADER = Template(R"""
#ifndef MINIJAVA_AST_${time}_HPP
#define MINIJAVA_AST_${time}_HPP

#include <minijava/utility.hpp>

namespace minijava {

$declarations

$definitions
}

#endif
""")


def parse_type(field_type):
    if field_type.startswith("["):
        if not field_type.startswith("[]"):
            raise ValueError(f"invalid field type: {field_type}")
        subtype = parse_type(field_type[2:])
        return f"Span<{subtype}>"
    if field_type == "string":
        return "std::string_view"
    # TODO: check if field_type is valid
    return f"const {field_type}*"


def make_node(name, decl, subclass_map):
    extern_base = decl.pop("<extern base>", None)
    if extern_base is not None:
        if decl:
            raise ValueError(f"external node cannot have fields: {name}")
        node = Node(name=name, base=extern_base, fields=None)
        subclass_map.setdefault(extern_base, []).append(node)
        return node

    base = decl.pop("<base>", None)
    fields = tuple((name, parse_type(type)) for name, type in decl.items())
    node = Node(name=name, base=base, fields=fields)
    subclass_map.setdefault(base, []).append(node)
    return node


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} INPUT OUTPUT")
        exit(1)

    with open(sys.argv[1]) as f:
        data = json.load(f)

    nodes = {}
    subclass_map = {}
    for name, decl in data.items():
        nodes[name] = make_node(name, decl, subclass_map)

    roots = []
    declarations = []
    for node in nodes.values():
        if node.base is None:
            roots.append(node)
        else:
            node.base = nodes[node.base]
        node.subclasses = subclass_map.get(node.name, [])
        declarations.append(f"struct {node.name};")

    definitions = []
    for root in roots:
        definitions.append(Hierarchy(root).generate())

    with open(sys.argv[2], "w") as f:
        f.write(
            HEADER.substitute(time=time_ns(),
                              declarations="\n".join(declarations),
                              definitions="".join(definitions)))
