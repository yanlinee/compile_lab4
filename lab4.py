import ply.lex as lex
import ply.yacc as yacc

# Lexical analysis
tokens = (
    'ID', 'NUM',
    'INT', 'VOID',
    'LPAREN', 'RPAREN', 'LBRACK', 'RBRACK', 'LBRACE', 'RBRACE',
    'SEMI', 'COMMA'
)

t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACK = r'\['
t_RBRACK = r'\]'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_SEMI = r';'
t_COMMA = r','

reserved = {
    'int': 'INT',
    'void': 'VOID',
}


def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'ID')
    return t


def t_NUM(t):
    r'\d+'
    t.value = int(t.value)
    return t


t_ignore = ' \t\r'


def t_error(t):
    print(f"Illegal character: {t.value[0]}")
    t.lexer.skip(1)


# Symbol table classes
class SymbolTableHeader:
    def __init__(self, outer=None, name="global"):
        self.outer = outer
        self.width = 0
        self.rtype = None
        self.argc = 0
        self.level = 0
        self.name = name

    def __repr__(self):
        return (f"Scope [{self.name}]:\n"
                f"Size={self.width}, Return Type={self.rtype}, "
                f"Param Count={self.argc}, Level={self.level}")


class SymbolTableEntry:
    def __init__(self, name, kind):
        self.name = name
        self.kind = kind
        self.type = None
        self.offset = 0
        self.size = 4
        self.dims = []
        self.params = []
        self.symtab = None

    def __repr__(self):
        # kind 为 FUNC 时，type 显示为 FUNC
        type_str = self.kind if self.kind == 'FUNC' else self.type
        info = [f"{self.name}", f"{type_str}", f"{self.offset}"]
        if self.kind == 'ARRAY':
            info.append(f"dims={self.dims}")
        return " ".join(info)


class SymbolTable:
    def __init__(self, outer=None, name="global"):
        self.header = SymbolTableHeader(outer, name)
        if outer:
            self.header.level = outer.header.level + 1
        self.entries = {}

    def add(self, name, kind, type=None, dims=None):
        if name in self.entries:
            print(f"Warning: Redefinition of {name}")
            return None

        entry = SymbolTableEntry(name, kind)
        entry.type = type
        entry.offset = self.header.width

        if kind == 'ARRAY':
            if dims:
                entry.dims = dims
                entry.size = 4 * dims[0]
            else:
                entry.size = 4

        self.entries[name] = entry
        if kind != 'FUNC':
            self.header.width += entry.size

        return entry

    def lookup(self, name):
        if name in self.entries:
            return self.entries[name]
        if self.header.outer:
            return self.header.outer.lookup(name)
        return None

    def __repr__(self):
        result = [str(self.header)]
        if self.entries:
            result.append("Symbols:")
            for entry in self.entries.values():
                result.append(f"  {entry}")
        # 函数符号表补充参数和 code 信息
        if self.header.name != "global":
            # 参数个数
            result.append(f"Param Count: {self.header.argc}")
            # 只输出参数名，无参数为NIL
            param_names = [entry.name for entry in self.entries.values() if entry.offset < self.header.argc * 4]
            result.append("Param List: " + (", ".join(param_names) if param_names else "NIL"))
            result.append("Code: ...")
        return "\n".join(result)


# Parser
start = 'program'
global_table = SymbolTable()


def p_program(p):
    '''program : decl_list'''
    print("\nGlobal Symbol Table:")
    print(global_table)
    for entry in global_table.entries.values():
        if entry.kind == 'FUNC' and entry.symtab:
            print(f"\nFunction {entry.name} Symbol Table:")
            print(entry.symtab)
    p[0] = p[1]


def p_compound_stmt(p):
    '''compound_stmt : LBRACE var_decls stmt_list RBRACE'''
    p[0] = (p[2], p[3])


def p_stmt_list(p):
    '''stmt_list : stmt_list stmt
                 | '''
    if len(p) == 1:
        p[0] = []
    else:
        p[0] = p[1] + [p[2]]


def p_stmt(p):
    '''stmt : SEMI'''
    p[0] = None


def p_decl_list(p):
    '''decl_list : decl_list decl
                 | decl'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]


def p_decl(p):
    '''decl : var_decl
            | func_decl'''
    p[0] = p[1]


def p_var_decl(p):
    '''var_decl : type_spec ID SEMI
                | type_spec ID LBRACK NUM RBRACK SEMI'''
    name = p[2]
    type = p[1]
    current_table = p.parser.current_table  # 只用当前表

    if len(p) == 4:
        entry = current_table.add(name, 'VAR', type)
    else:
        dims = [p[4]]
        entry = current_table.add(name, 'ARRAY', type, dims=dims)
    p[0] = entry


def p_type_spec(p):
    '''type_spec : INT
                | VOID'''
    p[0] = p[1]


def p_params(p):
    '''params : param_list
              | VOID
              | '''
    if len(p) == 1:  # ()
        p[0] = []
    elif p[1] == 'void':
        p[0] = []
    else:
        p[0] = p[1]


def p_param_list(p):
    '''param_list : param_list COMMA param
                  | param'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[3]]


def p_param(p):
    '''param : type_spec ID
            | type_spec ID LBRACK RBRACK'''
    name = p[2]
    type = p[1]
    if len(p) == 3:
        p[0] = {'name': name, 'kind': 'VAR', 'type': type}
    else:
        p[0] = {'name': name, 'kind': 'ARRPTR', 'type': type, 'dims': [0]}


def p_var_decls(p):
    '''var_decls : var_decls var_decl
                | var_decl
                | '''
    if len(p) == 1:  # empty production
        p[0] = []
    elif len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]


def p_func_decl(p):
    '''func_decl : type_spec ID LPAREN params RPAREN compound_stmt'''
    name = p[2]
    rtype = p[1]
    params = p[4]

    func_table = SymbolTable(outer=global_table, name=name)
    func_table.header.rtype = rtype
    func_table.header.argc = len(params)

    # 切换当前符号表为函数表
    old_table = p.parser.current_table
    p.parser.current_table = func_table

    # 先处理参数
    param_offset = 0
    for param in params:
        entry = func_table.add(param['name'], param['kind'], param['type'])
        if entry:
            entry.offset = param_offset
            param_offset += 4

    # 重置偏移量，处理局部变量
    func_table.header.width = 0  # 重要：重置宽度
    var_decls = p[6][0] if p[6][0] is not None else []
    for decl in var_decls:
        if decl:
            entry = func_table.add(decl.name, decl.kind, decl.type, getattr(decl, 'dims', None))
            if entry:
                entry.offset = func_table.header.width
                func_table.header.width += entry.size

    # 恢复当前符号表为全局表
    p.parser.current_table = old_table

    func_entry = global_table.add(name, 'FUNC')
    if func_entry:
        func_entry.symtab = func_table
        func_entry.params = params
        func_entry.type = rtype

    p[0] = func_entry


def p_error(p):
    if p:
        print(f"Error: Parser encountered unexpected token: {p.type} ({p.value}) at line {p.lineno}")
    else:
        print("Error: Parser reached unexpected end of input")


# Create parser
lexer = lex.lex()
parser = yacc.yacc()
parser.current_table = global_table

# Main program
if __name__ == '__main__':
    with open('test.c', 'r') as f:
        data = f.read()
    parser.parse(data, lexer=lexer)
