import json
import sys
from io import StringIO
from itertools import chain
from string import Template

CONSTRUCTOR = Template("""\
    explicit constexpr $name($arguments)
        : $init_list
    {}
""")

ISA_TRUE = Template("""\
    friend constexpr bool do_isa(const $root& base, struct $name*)
    {
        return true;
    }
""")

ISA_BASE = Template("""\
    friend constexpr bool do_isa(const $root& base, struct $name*)
    {
        auto kind = base.kind;
        return $root::$first <= kind && kind <= $root::$last;
    }
""")

ISA_LEAF = Template("""\
    friend constexpr bool do_isa(const $root& base, struct $name*)
    {
        return base.kind == $root::$name;
    }
""")

VISITOR_DEGENERATE = Template("""\
    constexpr RetT visit(const $root* node)
    {
        return visit(*node);
    }
    constexpr RetT visit(const $root& node)
    {
        return static_cast<V*>(this)->visit$name(static_cast<const $name&>(node));
    }
""")

VISITOR = Template("""\
    constexpr RetT visit(const $root* node)
    {
        return visit(*node);
    }
    constexpr RetT visit(const $root& node)
    {
        switch (node.kind) {
$cases
        }
        unreachable("invalid node kind");
    }
""")

VISITOR_CASE = Template("""\
        case $root::$name:
            return static_cast<V*>(this)->visit$name(static_cast<const $name&>(node));
""")

VISIT_ROOT = Template("""\
    constexpr void visit$name(const $name& node)
    {}
""")

VISIT_NODE = Template("""\
    constexpr RetT visit$name(const $name& node)
    {
        return static_cast<V*>(this)->visit$base(node);
    }
""")


class Node:
    __slots__ = ("name", "base", "fields", "subclasses")

    def __init__(self, name, base, fields):
        self.name = name
        self.base = base
        self.fields = fields
        self.subclasses = None


class Hierarchy:
    __slots__ = ("root", "nodes", "leaves", "extern", "buf")

    def __init__(self, root):
        self.root = root
        self.nodes = []
        self.extern = []
        if root.subclasses:
            self.leaves = []
            for subclass in root.subclasses:
                self.dfs(subclass)
        else:
            self.leaves = [root]

    def dfs(self, node):
        # TODO: check for loops

        if node.fields is None:
            self.extern.append(node)
        else:
            self.nodes.append(node)

        if node.subclasses:
            for subclass in node.subclasses:
                self.dfs(subclass)
        else:
            self.leaves.append(node)

    def write(self, s):
        self.buf.write(s)

    def generate_decls(self):
        self.buf = StringIO()

        self.write(f"struct {self.root.name} {{\n")
        if len(self.leaves) > 1:
            self.generate_tags()
        for node in chain(self.nodes, self.extern):
            self.generate_isa(node)
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

    def generate_isa(self, node):
        if len(self.leaves) == 1:
            isa = ISA_TRUE.substitute(
                root=self.root.name,
                name=node.name,
            )
        elif node.subclasses:
            isa = ISA_BASE.substitute(
                root=self.root.name,
                name=node.name,
                first=find_min_leaf(node).name,
                last=find_max_leaf(node).name,
            )
        else:
            isa = ISA_LEAF.substitute(
                root=self.root.name,
                name=node.name,
            )
        self.write(isa)

    def generate_node(self, node):
        if node.base:
            base = f": {node.base.name} "
        else:
            base = ""
        self.write(f"struct {node.name} {base}{{\n")

        self.generate_fields(node)
        self.generate_constructor(node)
        self.write("};\n")

    def generate_fields(self, node):
        for name, field_type in node.fields:
            self.write(f"    {field_type} {name};\n")

    def generate_constructor(self, node):
        if node.subclasses:
            return

        base_fields = get_base_fields(node)
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

        constructor = CONSTRUCTOR.substitute(
            name=node.name,
            arguments=", ".join(arguments),
            init_list=", ".join(init_list),
        )
        self.write(constructor)

    def generate_visitor(self):
        buf = StringIO()

        if len(self.leaves) > 1:
            cases = [
                VISITOR_CASE.substitute(
                    root=self.root.name,
                    name=node.name,
                ) for node in self.leaves
            ]
            visitor = VISITOR.substitute(
                root=self.root.name,
                cases="".join(cases),
            )
        else:
            node = self.leaves[0]
            visitor = VISITOR_DEGENERATE.substitute(
                root=self.root.name,
                name=node.name,
            )

        buf.write(visitor)
        buf.write(VISIT_ROOT.substitute(name=self.root.name))
        for node in chain(self.nodes, self.extern):
            buf.write(
                VISIT_NODE.substitute(
                    name=node.name,
                    base=node.base.name,
                ))
        return buf.getvalue()


def get_base_fields(node):
    base = node.base
    if base is None:
        return ()
    return get_base_fields(base) + base.fields


def find_min_leaf(node):
    if not node.subclasses:
        return node
    return find_min_leaf(node.subclasses[0])


def find_max_leaf(node):
    if not node.subclasses:
        return node
    return find_max_leaf(node.subclasses[-1])


def parse_type(field_type, type_map):
    if field_type.startswith("["):
        if not field_type.startswith("[]"):
            raise ValueError(f"invalid field type: {field_type}")
        subtype = parse_type(field_type[2:], type_map)
        return f"Span<{subtype} const>"

    custom_type = type_map.get(field_type)
    if custom_type is not None:
        return custom_type
    return f"const {field_type}*"


def make_node(name, decl, type_map, subclass_map):
    extern_base = decl.pop("<extern base>", None)
    if extern_base is not None:
        if decl:
            raise ValueError(f"external node cannot have fields: {name}")
        node = Node(name=name, base=extern_base, fields=None)
        subclass_map.setdefault(extern_base, []).append(node)
        return node

    base = decl.pop("<base>", None)
    fields = tuple(
        (name, parse_type(type, type_map)) for name, type in decl.items())
    node = Node(name=name, base=base, fields=fields)
    subclass_map.setdefault(base, []).append(node)
    return node


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} INPUT OUTPUT")
        exit(1)

    with open(sys.argv[1]) as f:
        data = json.load(f)
    template = data.pop("<template>")
    type_map = data.pop("<types>", {})

    nodes = {}
    subclass_map = {}
    for name, decl in data.items():
        nodes[name] = make_node(name, decl, type_map, subclass_map)

    roots = []
    declarations = []
    for node in nodes.values():
        if node.base is None:
            roots.append(node)
        else:
            node.base = nodes[node.base]
        node.subclasses = subclass_map.get(node.name, [])
        declarations.append(f"struct {node.name};\n")

    definitions = []
    visitors = []
    for root in roots:
        hierarchy = Hierarchy(root)
        definitions.append(hierarchy.generate_decls())
        visitors.append(hierarchy.generate_visitor())

    with open(template) as template:
        template = "".join(template)

    with open(sys.argv[2], "w") as f:
        f.write(
            Template(template).substitute(
                declarations="".join(declarations),
                definitions="".join(definitions),
                visitors="".join(visitors),
            ))
