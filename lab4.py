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
    'SEMI', 'COMMA'  # 分隔符
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

# 保留字映射
reserved = {
    'int': 'INT',
    'void': 'VOID',
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
    print(f"词法错误: 非法字符 {t.value[0]}")
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
        info = [f"{self.name}", f"{type_str}", f"{self.offset}"]
        if self.kind == 'ARRAY':
            info.append(f"dims={self.dims}")
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
        # 检查重复定义
        if name in self.entries:
            print(f"警告: 重复定义 {name}")
            return None

        # 创建新条目
        entry = SymbolTableEntry(name, kind)
        entry.type = type
        entry.offset = self.header.width

        # 处理数组
        if kind == 'ARRAY':
            if dims:
                entry.dims = dims
                entry.size = 4 * dims[0]
            else:
                entry.size = 4

        # 添加到符号表
        self.entries[name] = entry
        if kind != 'FUNC':  # 函数不计入大小
            self.header.width += entry.size

        return entry

    def lookup(self, name):
        """查找符号（支持作用域链）"""
        if name in self.entries:
            return self.entries[name]
        if self.header.outer:
            return self.header.outer.lookup(name)
        return None


# ====================
# 语法分析器部分
# ====================

start = 'program'
global_table = SymbolTable()


def p_program(p):
    '''program : decl_list'''
    # 输出所有符号表
    print("\n全局符号表:")
    print(global_table)
    for entry in global_table.entries.values():
        if entry.kind == 'FUNC' and entry.symtab:
            print(f"\n函数 {entry.name} 的符号表:")
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


def p_var_decl(p):
    '''var_decl : type_spec ID SEMI
                | type_spec ID LBRACK NUM RBRACK SEMI'''
    name = p[2]
    type = p[1]
    current_table = p.parser.current_table

    # 变量声明处理
    if len(p) == 4:  # 普通变量
        entry = current_table.add(name, 'VAR', type)
    else:  # 数组变量
        dims = [p[4]]
        entry = current_table.add(name, 'ARRAY', type, dims=dims)
    p[0] = entry


def p_func_decl(p):
    '''func_decl : type_spec ID LPAREN params RPAREN LBRACE var_decls stmt_list RBRACE'''
    name = p[2]
    rtype = p[1]
    params = p[4]

    # 创建函数符号表
    func_table = SymbolTable(outer=global_table, name=name)
    func_table.header.rtype = rtype
    func_table.header.argc = len(params)

    # 保存并切换当前符号表
    old_table = p.parser.current_table
    p.parser.current_table = func_table

    # 处理参数
    param_offset = 0
    for param in params:
        entry = func_table.add(param['name'], param['kind'], param['type'])
        if entry:
            entry.offset = param_offset
            param_offset += 4

    # 恢复全局符号表
    p.parser.current_table = old_table

    # 将函数添加到全局表
    func_entry = global_table.add(name, 'FUNC')
    if func_entry:
        func_entry.symtab = func_table
        func_entry.type = rtype
        func_entry.params = params

    p[0] = func_entry


# [其他解析器规则保持不变...]

def p_error(p):
    if p:
        print(f"语法错误: 意外的令牌: {p.type} ({p.value}) 在第 {p.lineno} 行")
    else:
        print("语法错误: 意外的输入结束")


# ====================
# 主程序部分
# ====================

# 创建词法分析器和语法分析器
lexer = lex.lex()
parser = yacc.yacc()
parser.current_table = global_table

if __name__ == '__main__':
    # 读取并解析输入文件
    with open('test.c', 'r') as f:
        data = f.read()
    parser.parse(data, lexer=lexer)
