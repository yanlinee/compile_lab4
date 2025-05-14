#!/usr/bin/env python
# -*- coding: UTF-8 -*-
"""
@Project ：compile_lab4 
@File    ：lab4.py
@IDE     ：PyCharm 
@Author  ：ZCTong
@Date    ：2025/5/14 11:13 
"""

import ply.lex as lex
import ply.yacc as yacc

# ========================
# Token 定义
# ========================
tokens = (
    'ID', 'NUM',
    'INT', 'VOID',
    'LPAREN', 'RPAREN', 'LBRACK', 'RBRACK', 'LBRACE', 'RBRACE',
    'SEMI', 'COMMA',
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


def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'ID')
    return t


def t_NUM(t):
    r'\d+'
    t.value = int(t.value)
    return t


t_ignore = ' \t\n\r'


def t_error(t):
    print(f"Illegal character {t.value[0]}")
    t.lexer.skip(1)


lexer = lex.lex()


# ========================
# 符号表结构
# ========================
class SymbolTable:
    def __init__(self, outer=None, rtype=None):
        self.outer = outer
        self.symbols = {}
        self.width = 0
        self.argc = 0
        self.arglist = []
        self.rtype = rtype

    def add(self, name, entry):
        if name in self.symbols:
            print(f"Warning: Duplicate declaration of {name}")
        entry['offset'] = self.width
        self.symbols[name] = entry
        if entry['type'] not in ('FUNC', 'FUNPTT'):
            self.width += 4  # 假设每个变量占4字节

    def __repr__(self):
        return f"<SymbolTable width={self.width}, argc={self.argc}, arglist={self.arglist}, rtype={self.rtype}, symbols={self.symbols}>"


# ========================
# 分析器规则
# ========================
start = 'program'

global_table = SymbolTable()


def p_program(p):
    '''program : decl_list'''
    print("全局符号表：")
    print(global_table)


def p_decl_list(p):
    '''decl_list : decl_list decl
                 | decl'''
    pass


def p_decl(p):
    '''decl : var_decl
            | func_decl'''
    pass


def p_var_decl(p):
    '''var_decl : type_spec ID SEMI
                | type_spec ID LBRACK NUM RBRACK SEMI'''
    typename = p[1]
    name = p[2]
    if len(p) == 4:
        global_table.add(name, {'name': name, 'type': 'VAR'})
    else:
        dims = [p[4]]
        global_table.add(name, {'name': name, 'type': 'ARRAY', 'etype': typename, 'dims': dims})


def p_type_spec(p):
    '''type_spec : INT
                 | VOID'''
    p[0] = p[1]


def p_func_decl(p):
    '''func_decl : type_spec ID LPAREN params RPAREN compound_stmt'''
    name = p[2]
    return_type = p[1]
    params = p[4]
    local_decls = p[6]

    func_symtab = SymbolTable(outer=global_table)

    # 加入参数
    for param in params:
        func_symtab.add(param['name'], param)

    # 加入局部变量
    for decl in local_decls:
        func_symtab.add(decl['name'], decl)

    func_symtab.argc = len(params)
    func_symtab.arglist = [param['name'] for param in params]
    func_symtab.rtype = return_type

    global_table.add(name, {
        'name': name,
        'type': 'FUNC',
        'rtype': return_type,
        'symtab': func_symtab
    })

    print(f"函数 {name} 的符号表：")
    print(func_symtab)


def p_params(p):
    '''params : param_list
              | VOID'''
    if p[1] == 'void':
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
    typename = p[1]
    name = p[2]
    if len(p) == 3:
        p[0] = {'name': name, 'type': 'VAR', 'etype': typename}
    else:
        p[0] = {'name': name, 'type': 'ARRPTT', 'etype': typename}


def p_compound_stmt(p):
    '''compound_stmt : LBRACE local_decls RBRACE'''
    p[0] = p[2]  # 返回 local_decls 给 func_decl


def p_local_decls(p):
    '''local_decls : local_decls var_decl_in_func
                   | empty'''
    if len(p) == 2:
        p[0] = p[1]  # empty -> []
    else:
        p[0] = p[1] + [p[2]]


def p_var_decl_in_func(p):
    '''var_decl_in_func : type_spec ID SEMI
                        | type_spec ID LBRACK NUM RBRACK SEMI'''
    typename = p[1]
    name = p[2]
    if len(p) == 4:
        p[0] = {'name': name, 'type': 'VAR', 'etype': typename}
    else:
        dims = [p[4]]
        p[0] = {'name': name, 'type': 'ARRAY', 'etype': typename, 'dims': dims}


def p_empty(p):
    'empty :'
    p[0] = []


def p_error(p):
    if p:
        print(f"Syntax error at {p.value}")
    else:
        print("Syntax error at EOF")


parser = yacc.yacc()

# ========================
# 主程序入口
# ========================
if __name__ == '__main__':
    with open('test.c', 'r') as f:
        code = f.read()
    parser.parse(code)
