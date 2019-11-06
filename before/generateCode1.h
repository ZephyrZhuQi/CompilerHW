#ifndef __GENCODE_H__
#define __GENCODE_H__

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include "header.h"
#include "symbolTable.h"

#define forEach(iter) for(;iter;iter=iter->rightSibling)
#define ID_Name(idNode) (idNode->semantic_value.identifierSemanticValue.identifierName)
#define ID_Kind(idNode) (idNode->semantic_value.identifierSemanticValue.kind)
#define ID_SymTab(idNode) (idNode->semantic_value.identifierSemanticValue.symbolTableEntry)
#define getIDAttr(idNode) (ID_SymTab(idNode)->attribute)
#define getIDTypeDescriptor(idNode) (getIDAttr(idNode)->attr.typeDescriptor)

#define getIDGlobal(idNode) (getIDAttr(idNode)->global)
#define setIDGlobal(idNode, val) (getIDAttr(idNode)->global = (val))
#define getIDOffset(idNode) (getIDAttr(idNode)->offset)
#define setIDOffset(idNode, val) (getIDAttr(idNode)->offset = (val))
#define SymTab_ArrayProperty(symtabEntry) (symtabEntry->attribute->attr.typeDescriptor->properties.arrayProperties)

#define Decl_Kind(declNode) (declNode->semantic_value.declSemanticValue.kind)
#define Stmt_Kind(stmtNode) (stmtNode->semantic_value.stmtSemanticValue.kind)
#define getTypeEntry(idNode) ID_SymTab(idNode)
#define getHeadFunctionSignature(idNode) (getTypeEntry(idNode)->attribute->attr.functionSignature)

#define EXPRKIND(exprNode) (exprNode->semantic_value.exprSemanticValue.kind)
#define EXPRBINOP(exprNode) (exprNode->semantic_value.exprSemanticValue.op.binaryOp)
#define EXPRUNIOP(exprNode) (exprNode->semantic_value.exprSemanticValue.op.unaryOp)
#define EXPRCONSTEVAL(exprNode) (exprNode->semantic_value.exprSemanticValue.isConstEval)
#define EXPRCONSTU(exprNode) (exprNode->semantic_value.exprSemanticValue.constEvalValue)
#define CONSTTYPE(constNode) (constNode->semantic_value.const1->const_type)
#define CONSTU(constNode) (constNode->semantic_value.const1->const_u)

#define SYMFUNCSIGN(symtabEntry) (symtabEntry->attribute->attr.functionSignature)
#define SYMTYPEDESC(symtabEntry) (symtabEntry->attribute->attr.typeDescriptor)

#define isConstExpr(exprNode) (exprNode->nodeType == CONST_VALUE_NODE || \
        (exprNode->nodeType == EXPR_NODE && exprNode->semantic_value.exprSemanticValue.isConstEval))

#define getExprType(exprNode) (exprNode->dataType)
#define getExprValue(exprNode) ( \
        getExprType(exprNode) == INT_TYPE ? \
            ( exprNode->nodeType == CONST_VALUE_NODE ? \
                exprNode->semantic_value.const1->const_u.intval : \
                exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue ) : \
            ( exprNode->nodeType == CONST_VALUE_NODE ? \
                exprNode->semantic_value.const1->const_u.fval : \
                exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue ) )
#define findParentDecl(iter, type) \
        for(;iter;iter=iter->parent){ \
            if (iter->nodeType == DECLARATION_NODE && Decl_Kind(iter) == type) \
                break; \
        }

void emit(char *fmt, ...);
void gen_head(char *name);
void gen_prologue(char *name);
void gen_epilogue(char *name, int size);


int get_reg();
void free_reg(int reg);
int genIntLiteral(int i);
int genFloatLiteral(float f);
int genStrLiteral(char* s);
int genConstValue(AST_NODE *constValueNode);
int genExpr(AST_NODE *node);
int genArrayRef(AST_NODE *idNode);
int ArraySize(AST_NODE *idNode);

void genGlobalVarDecl(AST_NODE *varDeclListNode);
void genArrayAssign(AST_NODE *idNode, int val);
void genAssign(AST_NODE *stmtNode);
void genWhile(AST_NODE *whileNode);
void genIf(AST_NODE *ifNode);
void genWrite(AST_NODE *functionCallNode);
void genFor(AST_NODE *forNode);
void genPushParam(AST_NODE *param, int *size);
void genPopParam(int size);
void genFuncCall(AST_NODE *funcCallStmtNode);
void genReturn(AST_NODE *returnNode);
int genExprRelated(AST_NODE *exprRelatedNode);

void genStmt(AST_NODE *stmtNode);
void genStmtList(AST_NODE *stmtListNode);
void genDeclList(AST_NODE *declList);

void gen_block(AST_NODE *blockNode);
int getVariableSize(TypeDescriptor *typeDescriptor);
int getArrayCount(AST_NODE *dim);
void genFunctionDecl(AST_NODE *funcDeclNode);
int genVariableRef(AST_NODE *idNode);

void generateCode(AST_NODE *root);

#endif