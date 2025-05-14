import ply.lex as lex
import ply.yacc as yacc

# ====================
# 词法分析器部分
# ====================

# 令牌定义
tokens = (
    'ID', 'NUM',  # 标识符和数字
    'INT', 'VOID',  # 数据类型
    'LPAREN', 'RPAREN',  # 括号
    'LBRACK', 'RBRACK',  # 方括号
    'LBRACE', 'RBRACE',  # 大括号
    'SEMI', 'COMMA',  # 分隔符
    'ASSIGN',  # 赋值
    'RETURN'  # 返回语句
)

# 简单的令牌规则
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACK = r'\['
t_RBRACK = r'\]'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_SEMI = r';'
t_COMMA = r','
t_ASSIGN = r'='

# 保留字映射
reserved = {
    'int': 'INT',
    'void': 'VOID',
    'return': 'RETURN'
}

# 忽略的字符
t_ignore = ' \t\r'


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


def t_error(t):
    print(f"Lexical error: Illegal character {t.value[0]} at line {t.lineno}")
    t.lexer.skip(1)


# ====================
# 符号表类定义
# ====================

class SymbolTableHeader:
    """符号表头部信息"""

    def __init__(self, outer=None, name="global"):
        self.outer = outer  # 外层符号表
        self.width = 0  # 作用域大小
        self.rtype = None  # 返回类型（函数用）
        self.argc = 0  # 参数个数（函数用）
        self.level = 0  # 作用域层级
        self.name = name  # 作用域名称

    def __repr__(self):
        return (f"Scope [{self.name}]:\n"
                f"Size={self.width}, Return Type={self.rtype}, "
                f"Param Count={self.argc}, Level={self.level}")


class SymbolTableEntry:
    """符号表条目"""

    def __init__(self, name, kind):
        self.name = name  # 符号名
        self.kind = kind  # 类型（VAR/ARRAY/FUNC）
        self.type = None  # 数据类型
        self.offset = 0  # 偏移量
        self.size = 4  # 大小（默认4字节）
        self.dims = []  # 数组维度
        self.params = []  # 函数参数
        self.symtab = None  # 关联的符号表（函数用）

    def __repr__(self):
        type_str = self.kind if self.kind == 'FUNC' else self.type
        info = [f"name: {self.name}", f"type: {type_str}", f"offset: {self.offset}"]
        if self.kind == 'ARRAY':
            info.append(f"dims: {self.dims}")
        return " ".join(info)


class SymbolTable:
    """符号表主类"""

    def __init__(self, outer=None, name="global"):
        self.header = SymbolTableHeader(outer, name)
        if outer:
            self.header.level = outer.header.level + 1
        self.entries = {}

    def add(self, name, kind, type=None, dims=None):
        """添加符号到符号表"""
        if name in self.entries:
            print(f"Warning: Symbol {name} already defined")
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
        """查找符号（支持作用域链）"""
        if name in self.entries:
            return self.entries[name]
        if self.header.outer:
            return self.header.outer.lookup(name)
        return None

    def __repr__(self):
        result = [str(self.header)]
        for entry in self.entries.values():
            result.append(str(entry))
        return "\n".join(result)


# ====================
# 语法分析器部分
# ====================

def p_program(p):
    '''program : decl_list'''
    p[0] = p[1]
    print("\nGlobal Symbol Table:")
    print(global_table)
    for entry in global_table.entries.values():
        if entry.kind == 'FUNC' and entry.symtab:
            print(f"\nSymbol Table for function {entry.name}:")
            print(entry.symtab)


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


def p_type_spec(p):
    '''type_spec : INT
                | VOID'''
    p[0] = p[1]


def p_params(p):
    '''params : param_list
             | VOID
             | '''
    if len(p) == 2 and p[1] != 'void':
        p[0] = p[1]
    else:
        p[0] = []


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
    param = {'name': p[2], 'type': p[1]}
    if len(p) == 3:
        param['kind'] = 'VAR'
    else:
        param['kind'] = 'ARRAY'
    p[0] = param


def p_func_decl(p):
    '''func_decl : type_spec ID LPAREN params RPAREN LBRACE var_decls stmt_list RBRACE'''
    name = p[2]
    rtype = p[1]
    params = p[4]

    # 创建函数的符号表
    func_table = SymbolTable(outer=global_table, name=name)
    func_table.header.rtype = rtype
    func_table.header.argc = len(params)

    # 将函数添加到全局符号表
    func_entry = global_table.add(name, 'FUNC')
    if func_entry:
        func_entry.type = rtype
        func_entry.params = params
        func_entry.symtab = func_table

    # 保存当前符号表并切换到函数的符号表
    old_table = p.parser.current_table
    p.parser.current_table = func_table

    # 添加参数到函数的符号表
    param_offset = 0
    for param in params:
        entry = func_table.add(param['name'], param['kind'], param['type'])
        if entry:
            entry.offset = param_offset
            param_offset += 4

    # 处理函数内部的局部变量声明
    for var_decl in p[7]:
        pass  # 局部变量已在 p_var_decl 中正确添加

    # 恢复全局符号表
    p.parser.current_table = old_table

    p[0] = func_entry


def p_var_decl(p):
    '''var_decl : type_spec ID SEMI
                | type_spec ID LBRACK NUM RBRACK SEMI'''
    name = p[2]
    type = p[1]
    current_table = p.parser.current_table  # 使用当前符号表

    if len(p) == 4:
        entry = current_table.add(name, 'VAR', type)
    else:
        dims = [p[4]]
        entry = current_table.add(name, 'ARRAY', type, dims=dims)

    p[0] = entry


def p_var_decls(p):
    '''var_decls : var_decls var_decl
                | '''
    if len(p) == 3:
        p[0] = p[1] + [p[2]] if p[1] else [p[2]]
    else:
        p[0] = []


def p_stmt_list(p):
    '''stmt_list : stmt_list stmt
                | '''
    if len(p) == 3:
        p[0] = p[1] + [p[2]] if p[1] else [p[2]]
    else:
        p[0] = []


def p_stmt(p):
    '''stmt : expr_stmt
           | return_stmt'''
    p[0] = p[1]


def p_expr_stmt(p):
    '''expr_stmt : expr SEMI
                | SEMI'''
    if len(p) == 3:
        p[0] = p[1]
    else:
        p[0] = None


def p_return_stmt(p):
    '''return_stmt : RETURN expr SEMI
                  | RETURN SEMI'''
    if len(p) == 4:
        p[0] = ('return', p[2])
    else:
        p[0] = ('return', None)


def p_expr(p):
    '''expr : ID ASSIGN expr
           | ID LBRACK expr RBRACK
           | ID LPAREN args RPAREN
           | ID
           | NUM'''
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 4 and p[2] == '=':
        p[0] = ('assign', p[1], p[3])
    elif len(p) == 5 and p[2] == '[':
        p[0] = ('array_ref', p[1], p[3])
    elif len(p) == 5 and p[2] == '(':
        p[0] = ('call', p[1], p[3])


def p_args(p):
    '''args : arg_list
           | '''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = []


def p_arg_list(p):
    '''arg_list : arg_list COMMA expr
                | expr'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[3]]


def p_error(p):
    if p:
        print(f"Syntax error: Unexpected token: {p.type} ({p.value}) at line {p.lineno}")
    else:
        print("Syntax error: Unexpected end of input")


# ====================
# 主程序部分
# ====================

# 创建词法分析器和语法分析器
lexer = lex.lex()
parser = yacc.yacc()

# 初始化全局符号表
global_table = SymbolTable()
parser.current_table = global_table

if __name__ == '__main__':
    # 读取并解析输入文件
    with open('test.c', 'r') as f:
        data = f.read()
    parser.parse(data, lexer=lexer)
