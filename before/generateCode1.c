#include "generateCode1.h"

FILE *output;

char *currentFuncName;
int const_label_no;
int while_label_no;
int for_label_no;
int if_label_no;
int short_circuit_label;
const int prologue_stack_size = 176;

void emit(char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    vfprintf(output, fmt, args);
    fprintf(output, "\n");
    va_end(args);
}

void gen_head(char *name)
{
    emit(".text");
    emit("_start_%s:", name);
}

void gen_prologue(char *name)
{
    emit("str x30, [sp, #0]");  // store return address
    emit("str x29, [sp, #-8]"); // save old fp
    emit("add x29, sp, #-8");   // new fp
    emit("add sp, sp, #-16");   // new sp
    emit("ldr x30, =_frameSize_%s", name);
    emit("ldr w30, [x30, #0]");
    emit("sub sp, sp, w30"); // push new AR
    int offset = 0;
    for (int i = 9; i <= 29; ++i)
    {
        if(i == 16 || i==17||i==18)continue;
        offset += 8;
        emit("str x%d, [sp, #%d]", i, offset);
    }
    for (int i = 16; i <= 23; ++i)
    {
        if (i == 16)
            offset += 8;
        else
            offset += 4;
        emit("str s%d, [sp, #%d]", i, offset);
    }
}

void gen_epilogue(char *name, int size)
{
    emit("_end_%s:", name);
    int offset = 0;
    for (int i = 9; i <= 29; ++i)
    {
        if(i == 16 || i==17||i==18)continue;
        offset += 8;
        emit("ldr x%d, [sp, #%d]", i, offset);
    }
    for (int i = 16; i <= 23; ++i)
    {
        if (i == 16)
            offset += 8;
        else
            offset += 4;
        emit("ldr s%d, [sp, #%d]", i, offset);
    }
    emit("ldr x30, [x29, #8]"); // restore return address
    emit("mov sp, x29");
    emit("add sp, sp, #8");    // pop AR "add sp, x29, #8"
    emit("ldr x29, [x29,#0]"); // restore caller (old) fp
    emit("RET x30");
    emit(".data");
    emit("_framesize_%s: .word %d", name, size);
}

int busy_reg[29];

int get_reg()
{
    for (int i = 19; i < 29; i++)
    {
        if (!busy_reg[i])
        {
            busy_reg[i] = 1;
            return i;
        }
    }
    printf("no free registers\n");
}

void free_reg(int reg)
{
    assert(reg >= 19 && reg < 29);
    busy_reg[reg] = 0;
}

int genIntLiteral(int i)
{
    emit(".data");
    emit("_CONSTANT_%d: .word %d", const_label_no, i);
    emit(".align 3");
    emit(".text");
    int reg = get_reg();
    emit("ldr x%d, _INT_CONST_%d", reg, const_label_no);
    const_label_no++;
    return reg;
}

int genFloatLiteral(float f)
{
    emit(".data");
    emit("_CONSTANT_%d: .float %f", const_label_no, f);
    emit(".align 3");
    emit(".text");
    int reg = get_reg();
    emit("ldr s%d, _FLOAT_CONST_%d", reg, const_label_no);
    const_label_no++;
    return reg;
}

int genStrLiteral(char *s)
{
    emit(".data");

    char buf[256];
    int i;
    strncpy(buf, s, 256);
    for (i = 1; buf[i] != '"'; i++)
        ;
    buf[i] = '\0';
    emit("_CONSTANT_%d: .ascii \"%s\\000\"", const_label_no, buf + 1);

    emit(".align 3");
    emit(".text");
    int reg = get_reg();
    emit("ldr x%d, =_STR_CONST_%d", reg, const_label_no);
    const_label_no++;
    return reg;
}

int genConstValue(AST_NODE *constValueNode)
{
    assert(constValueNode->nodeType == CONST_VALUE_NODE ||
           (constValueNode->nodeType == EXPR_NODE && isConstExpr(constValueNode)));

    int reg;

    if (constValueNode->dataType == INT_TYPE)
    {
        reg = genIntLiteral(getExprValue(constValueNode));
    }
    else if (constValueNode->dataType == FLOAT_TYPE)
    {
        reg = genFloatLiteral(getExprValue(constValueNode));
    }
    else
    {
        reg = genStrLiteral(constValueNode->semantic_value.const1->const_u.sc);
    }

    return reg;
}

int genExpr(AST_NODE *node)
{

    if (isConstExpr(node))
        return genConstValue(node);
    node = node->child;
    if (EXPRKIND(node) == BINARY_OPERATION)
    {
        AST_NODE *lvalue = node->child;
        AST_NODE *rvalue = lvalue->rightSibling;
        int lreg = genExprRelated(node->child);
        int rreg;
        if (EXPRBINOP(node) == BINARY_OP_AND || EXPRBINOP(node) == BINARY_OP_OR)
        {
            int label = short_circuit_label++;
            if (node->child->dataType == INT_TYPE)
                emit("cmp w%d, #0", lreg);
            else
                emit("fcmp s%d, #0", lreg);
            switch (EXPRBINOP(node))
            {
            case BINARY_OP_AND:
                emit("beq _BOOLEAN_FALSE_%d", label);
                break;
            case BINARY_OP_OR:
                emit("bne _BOOLEAN_TRUE_%d", label);
                break;
            }
            rreg = genExprRelated(node->child->rightSibling);
            if (node->child->rightSibling->dataType == INT_TYPE)
                emit("cmp w%d, #0", rreg);
            else
                emit("fcmp s%d, #0", rreg);
            emit("bne _BOOLEAN_TRUE_%d", label);
            emit("_BOOLEAN_FALSE_%d:", label);
            emit("mov w%d, #0", rreg);
            emit("b _BOOLEAN_END_%d", label);
            emit("_BOOLEAN_TRUE_%d:", label);
            emit("mov w%d, #1", rreg);
            emit("_BOOLEAN_END_%d:", label);
        }
        else
        {
            rreg = genExprRelated(rvalue);

            if (lvalue->dataType == INT_TYPE && rvalue->dataType == INT_TYPE)
            {
                switch (EXPRBINOP(node))
                {
                case BINARY_OP_ADD:
                    emit("add w%d, w%d, w%d", lreg, lreg, rreg);
                    break;
                case BINARY_OP_SUB:
                    emit("sub w%d, w%d, w%d", lreg, lreg, rreg);
                    break;
                case BINARY_OP_MUL:
                    emit("mul w%d, w%d, w%d", lreg, lreg, rreg);
                    break;
                case BINARY_OP_DIV:
                    emit("sdiv w%d, w%d, w%d", lreg, lreg, rreg);
                    break;
                case BINARY_OP_EQ:
                    emit("cmp w%d, w%d", lreg, rreg);
                    emit("cset w%d, eq", lreg);
                    break;
                case BINARY_OP_GE:
                    emit("cmp w%d, w%d", lreg, rreg);
                    emit("cset w%d, ge", lreg);
                    break;
                case BINARY_OP_LE:
                    emit("cmp w%d, w%d", lreg, rreg);
                    emit("cset w%d, le", lreg);
                    break;
                case BINARY_OP_NE:
                    emit("cmp w%d, w%d", lreg, rreg);
                    emit("cset w%d, ne", lreg);
                    break;
                case BINARY_OP_GT:
                    emit("cmp w%d, w%d", lreg, rreg);
                    emit("cset w%d, gt", lreg);
                    break;
                case BINARY_OP_LT:
                    emit("cmp w%d, w%d", lreg, rreg);
                    emit("cset w%d, lt", lreg);
                    break;
                }
            }
            else
            {
                // Float expr
                if (lvalue->dataType == INT_TYPE)
                    emit("scvtf s%d, w%d", lreg, lreg); //Signed fixed-point convert to floating-point.
                if (rvalue->dataType == INT_TYPE)
                    emit("scvtf s%d, w%d", rreg, rreg);
                switch (EXPRBINOP(node))
                {
                case BINARY_OP_ADD:
                    emit("fadd s%d, s%d, s%d", lreg, lreg, rreg);
                    break;
                case BINARY_OP_SUB:
                    emit("fsub s%d, s%d, s%d", lreg, lreg, rreg);
                    break;
                case BINARY_OP_MUL:
                    emit("fmul s%d, s%d, s%d", lreg, lreg, rreg);
                    break;
                case BINARY_OP_DIV:
                    emit("fdiv s%d, s%d, s%d", lreg, lreg, rreg);
                    break;
                case BINARY_OP_EQ:
                    emit("fcmp s%d, s%d", lreg, rreg);
                    emit("cset w%d, eq", lreg);
                    break;
                case BINARY_OP_GE:
                    emit("fcmp s%d, s%d", lreg, rreg);
                    emit("cset w%d, ge", lreg);
                    break;
                case BINARY_OP_LE:
                    emit("fcmp s%d, s%d", lreg, rreg);
                    emit("cset w%d, le", lreg);
                    break;
                case BINARY_OP_NE:
                    emit("fcmp s%d, s%d", lreg, rreg);
                    emit("cset w%d, ne", lreg);
                    break;
                case BINARY_OP_GT:
                    emit("fcmp s%d, s%d", lreg, rreg);
                    emit("cset w%d, gt", lreg);
                    break;
                case BINARY_OP_LT:
                    emit("fcmp s%d, s%d", lreg, rreg);
                    emit("cset w%d, lt", lreg);
                    break;
                }
            }
        }
        free_reg(rreg);
        return lreg;
    }
    else
    { //unary operation
        AST_NODE *value = node->child;
        int operand_reg = genExprRelated(node->child);
        if (value->dataType == INT_TYPE)
        {
            switch (EXPRUNIOP(node))
            {
            case UNARY_OP_POSITIVE:
                //emit("mov %s, %s", REG[dst], REG[lhs]);
                break;
            case UNARY_OP_NEGATIVE:
                emit("neg w%d, w%d", operand_reg, operand_reg);
                break;
            case UNARY_OP_LOGICAL_NEGATION:
                emit("cmp w%d, #0", operand_reg);
                emit("cset w%d, eq", operand_reg);
                break;
            }
        }
        else
        {
            switch (EXPRUNIOP(node))
            {
            case UNARY_OP_POSITIVE:
                //emit("mov %s, %s", REG[dst], REG[lhs]);
                break;
            case UNARY_OP_NEGATIVE:
                emit("fneg s%d, s%d", operand_reg, operand_reg);
                break;
            case UNARY_OP_LOGICAL_NEGATION:
                emit("fcmp s%d, #0", operand_reg);
                emit("cset w%d, eq", operand_reg);
                break;
            }
        }
        return operand_reg;
    }
}

int genArrayRef(AST_NODE *idNode)
{
    assert(idNode->nodeType == IDENTIFIER_NODE);
    assert(ID_SymTab(idNode) != NULL);
    assert(getIDAttr(idNode)->attributeKind == VARIABLE_ATTRIBUTE);
    assert(ID_Kind(idNode) == ARRAY_ID);
    TypeDescriptor *typeDescriptor = getIDTypeDescriptor(idNode);

    int varReg;
    if (getIDGlobal(idNode))
    {
        varReg = get_reg();
        emit("ldr x%d, =_g_%s", varReg, ID_Name(idNode));
    }
    else
    {
        int stackOffset = getIDOffset(idNode);
        varReg = genIntLiteral(stackOffset);
        emit("sub x%d, x29, x%d", varReg, varReg);
        fprintf(stderr, "Array ref name: %s, offset: %d\n", ID_Name(idNode), stackOffset);
    }

    int i = 0;
    int *sizes = typeDescriptor->properties.arrayProperties.sizeInEachDimension;
    AST_NODE *dimNode = idNode->child;

    int offsetReg = get_reg();
    emit("mov x%d, #0", offsetReg);
    for (; dimNode != NULL; dimNode = dimNode->rightSibling)
    {
        int sizeReg = genIntLiteral(sizes[i]);
        emit("mul x%d, x%d, x%d", offsetReg, offsetReg, sizeReg);
        free_reg(sizeReg);

        int indexReg = genExprRelated(dimNode);
        emit("lsl x%d, x%d, #2", indexReg, indexReg);
        emit("add x%d, x%d, x%d", offsetReg, offsetReg, indexReg);
        free_reg(indexReg);

        ++i;
    }

    if (getIDGlobal(idNode))
        emit("add x%d, x%d, x%d", varReg, varReg, offsetReg);
    else
        emit("sub x%d, x%d, x%d", varReg, varReg, offsetReg);

    free_reg(offsetReg);
    if (idNode->dataType == INT_TYPE)
    {
        emit("ldr w%d, [x%d, #0]", varReg, varReg);
    }
    else
    {
        emit("ldr s%d, [x%d, #0]", varReg, varReg);
    }

    return varReg;
}

int ArraySize(AST_NODE *idNode)
{
    int i, product = 4;
    for (i = 0; i < SymTab_ArrayProperty(ID_SymTab(idNode)).dimension; i++)
    {
        product *= SymTab_ArrayProperty(ID_SymTab(idNode)).sizeInEachDimension[i];
    }
    return product;
}

void genGlobalVarDecl(AST_NODE *varDeclListNode)
{
    AST_NODE *declNode = varDeclListNode->child;
    while (declNode)
    {
        if (Decl_Kind(declNode) == VARIABLE_DECL)
        {
            AST_NODE *typeNode = declNode->child, *idNode = typeNode->rightSibling;
            while (idNode)
            {
                setIDGlobal(idNode, 1);//global variable
                SymbolTableEntry *idSymTab = ID_SymTab(idNode);
                TypeDescriptor *idTypeDesc = idSymTab->attribute->attr.typeDescriptor;
                if (idTypeDesc->kind == SCALAR_TYPE_DESCRIPTOR)
                {
                    if (idTypeDesc->properties.dataType == INT_TYPE)
                    {
                        emit("_g_%s: .word 0", idSymTab->name);
                    }
                    else if (idTypeDesc->properties.dataType == FLOAT_TYPE)
                    {
                        emit("_g_%s: .float 0.0", idSymTab->name);
                    }
                }
                else if (idTypeDesc->kind == ARRAY_TYPE_DESCRIPTOR)
                {
                    int variableSize = getVariableSize(idTypeDesc);
                    emit("_g_%s: .space %d", idSymTab->name, variableSize);
                }
                idNode = idNode->rightSibling;
            }
        }
        declNode = declNode->rightSibling;
    }
    return;
}

void genArrayAssign(AST_NODE *idNode, int val)
{
    assert(idNode->nodeType == IDENTIFIER_NODE);
    assert(ID_SymTab(idNode) != NULL);
    assert(getIDAttr(idNode)->attributeKind == VARIABLE_ATTRIBUTE);
    assert(ID_Kind(idNode) == ARRAY_ID);
    TypeDescriptor *typeDescriptor = getIDTypeDescriptor(idNode);

    int varReg;
    if (getIDGlobal(idNode))
    {
        varReg = get_reg();
        emit("ldr x%d, =_g_%s", varReg, ID_Name(idNode));
    }
    else
    {
        int stackOffset = getIDOffset(idNode);
        varReg = genIntLiteral(stackOffset);
        emit("sub x%d, x29, x%d", varReg, varReg);
        fprintf(stderr, "Array assign name: %s, offset: %d\n", ID_Name(idNode), stackOffset);
    }

    int i = 0;
    int *sizes = typeDescriptor->properties.arrayProperties.sizeInEachDimension;
    AST_NODE *dimNode = idNode->child;

    int offsetReg = get_reg();
    emit("mov x%d, #0", offsetReg);
    forEach(dimNode)
    {
        int sizeReg = genIntLiteral(sizes[i]);
        emit("mul x%d, x%d, x%d", offsetReg, offsetReg, sizeReg);
        free_reg(sizeReg);

        int indexReg = genExprRelated(dimNode);
        emit("lsl x%d, x%d, #2", indexReg, indexReg);
        emit("add x%d, x%d, x%d", offsetReg, offsetReg, indexReg);
        free_reg(indexReg);

        ++i;
    }

    if (getIDGlobal(idNode))
        emit("add x%d, x%d, x%d", varReg, varReg, offsetReg);
    else
        emit("sub x%d, x%d, x%d", varReg, varReg, offsetReg);

    free_reg(offsetReg);
    if (idNode->dataType == INT_TYPE)
    {
        emit("str w%d, [x%d, #0]", val, varReg);
    }
    else
    {
        emit("str s%d, [x%d, #0]", val, varReg);
    }
    free_reg(varReg);
}

void genAssign(AST_NODE *stmtNode)
{
    AST_NODE *id = stmtNode->child;
    AST_NODE *expr = id->rightSibling;
    int val = genExprRelated(expr);

    if (id->dataType == FLOAT_TYPE && expr->dataType == INT_TYPE) //int to float
    {
        emit("scvtf s%d, w%d", val, val);
    }
    if (id->dataType == INT_TYPE && expr->dataType == FLOAT_TYPE) //float to int
    {
        emit("fcvtzs w%d, s%d", val, val);
    }

    TypeDescriptor *typeDescriptor = getIDTypeDescriptor(id);

    if (ID_Kind(id) == ARRAY_ID)
    {
        genArrayAssign(id, val);
        return;
    }
    else
    {
        int addr;
        if (getIDGlobal(id))
        { //global variable
            addr = get_reg();
            emit("ldr x%d, =_g_%s", addr, ID_Name(id));
        }
        else
        { //local variable
            int offset = getIDOffset(id);
            addr = genIntLiteral(offset);
            emit("sub x%d, x29, x%d", addr, addr);
            fprintf(stderr, "Var assign name: %s, offset: %d\n", ID_Name(id), offset);
        }

        if (id->dataType == INT_TYPE)
        {
            emit("str w%d, [x%d, #0]", val, addr);
        }
        else
        {
            emit("str s%d, [x%d, #0]", val, addr);
        }
        free_reg(addr);
    }
}

void genWhile(AST_NODE *whileNode)
{
    AST_NODE *cond = whileNode->child;
    AST_NODE *stmt = cond->rightSibling;
    int while_label = while_label_no++;

    emit("_WHILE_%d:", while_label);
    if (cond->nodeType == STMT_NODE && Stmt_Kind(cond) == ASSIGN_STMT)
    {
        genAssign(cond);
        cond = cond->child;
    }
    int reg = genExprRelated(cond);
    if (cond->dataType == FLOAT_TYPE)
        emit("fcvtzs w%d, s%d", reg, reg);

    emit("cmp w%d, #0", reg);
    free_reg(reg);
    emit("beq _WHILE_END_%d", while_label);
    if (stmt->nodeType == STMT_NODE)
        genStmt(stmt);
    else if (stmt->nodeType == BLOCK_NODE)
        gen_block(stmt);
    emit("b _WHILE_%d", while_label);
    emit("_WHILE_END_%d:", while_label);
}

void genIf(AST_NODE *ifNode)
{
    AST_NODE *condNode = ifNode->child;
    AST_NODE *thenNode = condNode->rightSibling;
    AST_NODE *elseNode = thenNode->rightSibling;
    int if_label = if_label_no++;

    emit("_IF_%d:", if_label);
    if (condNode->nodeType == STMT_NODE && Stmt_Kind(condNode) == ASSIGN_STMT)
    {
        genAssign(condNode);
        condNode = condNode->child;
    }
    int reg = genExprRelated(condNode);
    if (condNode->dataType == FLOAT_TYPE)
        emit("fcvtzs w%d, s%d", reg, reg);

    emit("cmp w%d, #0", reg);
    free_reg(reg);
    emit("beq _elseLabel_%d", if_label);
    if (thenNode->nodeType == STMT_NODE)
    { //then
        genStmt(thenNode);
    }
    else if (thenNode->nodeType == BLOCK_NODE)
    {
        gen_block(thenNode);
    }
    emit("b _ifExitLabel_%d", if_label);
    emit("_elseLabel_%d:", if_label);
    if (elseNode->nodeType != NUL_NODE)
    {
        if (elseNode->nodeType == STMT_NODE)
        {
            genStmt(elseNode);
        }
        else if (elseNode->nodeType == BLOCK_NODE)
        {
            gen_block(elseNode);
        }
    }
    emit("_ifExitLabel_%d:", if_label);
}

void genWrite(AST_NODE *functionCallNode)
{
    AST_NODE *it = functionCallNode;
    AST_NODE *id = it->child;
    AST_NODE *paramList = id->rightSibling;

    AST_NODE *param = paramList->child;

    int reg = genExprRelated(param);
    switch (param->dataType)
    {
    case INT_TYPE:
        emit("mov w0, w%d", reg);
        emit("bl _write_int");
        break;
    case FLOAT_TYPE:
        emit("fmov s0, s%d", reg);
        emit("bl _write_float");
        break;
    case CONST_STRING_TYPE:
        emit("mov x0, x%d", reg);
        emit("bl _write_str");
        break;
    }
    free_reg(reg);
}

void genFor(AST_NODE *forNode)
{
    int for_label = for_label_no++;
    AST_NODE *initNode, *condNode, *incrNode, *loopNode;
    initNode = forNode->child;
    condNode = initNode->rightSibling;
    incrNode = condNode->rightSibling;
    loopNode = incrNode->rightSibling;

    emit("_FOR_start_%d:", for_label);

    if (initNode->nodeType == NONEMPTY_ASSIGN_EXPR_LIST_NODE)
    {
        AST_NODE *assignNode = initNode->child;
        for (; assignNode != NULL; assignNode = assignNode->rightSibling)
        {
            genAssign(assignNode);
        }
    }

    emit("__for_loop_%d:", for_label);
    if (condNode->nodeType == NONEMPTY_RELOP_EXPR_LIST_NODE)
    {
        AST_NODE *exprNode = condNode->child;
        AST_NODE *last_cond;
        int cond;
        for (; exprNode != NULL; exprNode = exprNode->rightSibling)
        {
            last_cond = exprNode;
            cond = genExprRelated(exprNode);
        }
        if (last_cond->dataType == INT_TYPE)
        {
            emit("cmp w%d, #0", cond);
        }
        else
        {
            emit("fcmp s%d, #0", cond);
        }
        free_reg(cond);
        emit("beq _FOR_END_%d", for_label);
    }
    if (loopNode->nodeType == STMT_NODE)
    {
        genStmt(loopNode);
    }
    else if (loopNode->nodeType == BLOCK_NODE)
    {
        gen_block(loopNode);
    }
    else if (loopNode->nodeType == NUL_NODE)
    {
        // do nothing
    }
    if (incrNode->nodeType == NONEMPTY_ASSIGN_EXPR_LIST_NODE)
    {
        AST_NODE *assignNode = incrNode->child;
        for (; assignNode != NULL; assignNode = assignNode->rightSibling)
        {
            if (assignNode->nodeType == STMT_NODE)
            {
                genAssign(assignNode);
            }
            else if (assignNode->nodeType == EXPR_NODE)
            {
                genExpr(assignNode);
            }
        }
    }
    emit("b __for_loop_%d", for_label);
    emit("__for_end_%d:", for_label);
}

void genPushParam(AST_NODE *param, int *size)
{
    AST_NODE *it = param->child;
    *size = 0;
    forEach(it)
    {
        /*
        DATA_TYPE dataType = getExprType(it);
        //__asm__("int3;");
        if (dataType == INT_TYPE || dataType == FLOAT_TYPE) *size += 4;
        else *size += 8;
        */
        *size += 8;
    }

    it = param->child;
    int size_tmp = 0;
    Parameter *params = param->parent->child->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.functionSignature->parameterList;

    forEach(it)
    {
        DATA_TYPE dataType = getExprType(it);
        // fill stack
        int reg = genExprRelated(it);
        /*
        if (dataType == INT_TYPE || dataType == FLOAT_TYPE){
            size_tmp += 4;
            emit("str w%d, [sp, #%d]", reg, size_tmp);
        }
        else {
            size_tmp += 8;
            emit("str x%d, [sp, #%d]", reg, size_tmp);
        }
        */
        size_tmp += 8;
        assert(params);
        DATA_TYPE t = params->type->properties.dataType;
        if (t == INT_TYPE && dataType == FLOAT_TYPE)
        {
            emit("fcvtzs w%d, s%d", reg, reg);
        }
        else if (t == FLOAT_TYPE && dataType == INT_TYPE)
        {
            emit("scvtf s%d, w%d", reg, reg);
        }
        params = params->next;
        if (t == FLOAT_TYPE)
            emit("str s%d, [sp, #-%d]", reg, size_tmp);
        else
            emit("str x%d, [sp, #-%d]", reg, size_tmp);
        free_reg(reg);
        // set offset in function decl
        //setIDOffset(it, -*size-prologue_stack_size + size_tmp);
    }
    printf("param size: %d\n", *size);
    emit("add sp, sp, #-%d", *size);
}

void genPopParam(int size)
{
    emit("add sp, sp, #%d", size);
}

void genFuncCall(AST_NODE *funcCallStmtNode)
{
    AST_NODE *id, *param, *exprNode;
    id = funcCallStmtNode->child;
    param = id->rightSibling;

    char *name = ID_Name(id);
    if (!strcmp(name, "write"))
    {
        genWrite(funcCallStmtNode);
    }
    else if (!strcmp(name, "read"))
    {
        emit("bl _read_int");
    }
    else if (!strcmp(name, "fread"))
    {
        emit("bl _read_float");
    }
    else
    {
        // proceed param
        int size = 0;
        genPushParam(param, &size);
        emit("bl _start_%s", name);
        genPopParam(size);
    }
}

void genReturn(AST_NODE *returnNode)
{
    AST_NODE *exprRelated = returnNode->child;

    int reg;
    if (exprRelated->nodeType != NUL_NODE)
    {
        reg = genExprRelated(exprRelated);
    }

    AST_NODE *parent = returnNode;
    findParentDecl(parent, FUNCTION_DECL);
    FunctionSignature *fs = getHeadFunctionSignature(parent->child);
    switch (fs->returnType)
    {
    case INT_TYPE:
        if (returnNode->dataType == INT_TYPE)
        {
            printf("[DEBUG] %s int to int\n", ID_Name(parent->child->rightSibling));
            emit("mov w0, w%d", reg);
        }
        else if (returnNode->dataType == FLOAT_TYPE)
        {
            emit("fmov s0, s%d", reg);
            emit("fcvtzs w0, s0");
            printf("[DEBUG] %s float to int\n", ID_Name(parent->child->rightSibling));
        }
        else
        {
            puts("return type error");
        }
        break;
    case FLOAT_TYPE:
        if (returnNode->dataType == INT_TYPE)
        {
            emit("mov w0, w%d", reg);
            emit("scvtf s0, w0");
            printf("[DEBUG] %s int to float\n", ID_Name(parent->child->rightSibling));
        }
        else if (returnNode->dataType == FLOAT_TYPE)
        {
            emit("fmov s0, s%d", reg);
            printf("[DEBUG] %s float to float\n", ID_Name(parent->child->rightSibling));
        }
        else
        {
            puts("return type error");
        }
        break;
    case VOID_TYPE:
        break;
    default:
        puts("Undefined return type");
        break;
    }
    emit("b _epilogue_%s", ID_Name(parent->child->rightSibling));
    free_reg(reg);
}

int genExprRelated(AST_NODE *exprRelatedNode)
{
    int reg;
    switch (exprRelatedNode->nodeType)
    {
    case EXPR_NODE:
        return genExpr(exprRelatedNode);
        break;
    case STMT_NODE:
        //function call
        genFuncCall(exprRelatedNode->child);
        reg = get_reg();
        switch (exprRelatedNode->dataType)
        {
        case INT_TYPE:
            emit("mov w%d, w0", reg);
            break;
        case FLOAT_TYPE:
            emit("fmov s%d, s0", reg);
            break;
        }
        return reg;
        break;
    case IDENTIFIER_NODE:
        return genVariableRef(exprRelatedNode);
        break;
    case CONST_VALUE_NODE:
        return genConstValue(exprRelatedNode);
        break;
    default:
        printf("Unhandle case in void genExprRelated(AST_NODE* exprRelatedNode)\n");
        break;
    }
}

void genStmt(AST_NODE *stmtNode)
{
    switch (Stmt_Kind(stmtNode))
    {
    case ASSIGN_STMT: // 1)	Assignment	statements
        genAssign(stmtNode);
        break;
    case WHILE_STMT: // 3)	Control	statements:	while,	if-then-else
        genWhile(stmtNode);
        break;
    case IF_STMT: // 3)	Control	statements:	while,	if-then-else
        genIf(stmtNode);
        break;
    case FOR_STMT: // 9)	For	loops
        genFor(stmtNode);
        break;
    case FUNCTION_CALL_STMT:
        genFuncCall(stmtNode);
        break;
    case RETURN_STMT:
        genReturn(stmtNode);
        break;
    default:
        printf("unknown stmt kind\n");
    }
}

void genStmtList(AST_NODE *stmtListNode)
{
    AST_NODE *child = stmtListNode->child;
    for (; child != NULL; child = child->rightSibling)
    {
        if (child->nodeType == STMT_NODE)
            genStmt(child);
        else if (child->nodeType == BLOCK_NODE)
            gen_block(child);
        else if (child->nodeType == NUL_NODE)
            ; // do nothing
    }
}

void genDeclList(AST_NODE *declList)
{
    AST_NODE *declIter = declList->child;
    for (; declIter != NULL; declIter = declIter->rightSibling)
    {
        AST_NODE *type = declIter->child;
        AST_NODE *it = type->rightSibling;
        for (; it != NULL; it = it->rightSibling)
        {
            if (it->child)
            {
                int reg = genExprRelated(it->child);
                if (type->dataType == INT_TYPE && it->child->dataType == FLOAT_TYPE)
                {
                    emit("fcvtzs w%d, s%d", reg, reg);
                }
                else if (type->dataType == FLOAT_TYPE && it->child->dataType == INT_TYPE)
                {
                    emit("scvtf s%d, w%d", reg, reg);
                }
                int addr = genIntLiteral(getIDOffset(it));
                emit("sub x%d, x29, x%d", addr, addr);
                if (type->dataType == INT_TYPE)
                {
                    emit("str w%d, [x%d, #0]", reg, addr);
                }
                else if (type->dataType == FLOAT_TYPE)
                {
                    emit("str s%d, [x%d, #0]", reg, addr);
                }
                free_reg(addr);
                free_reg(reg);
            }
        }
    }
}

void gen_block(AST_NODE *blockNode)
{

    AST_NODE *child = blockNode->child;
    // block -> decl_list stmt_list
    if (child->rightSibling)
    {
        AST_NODE *decl_list = child->rightSibling;
        AST_NODE *stmt_list = decl_list->rightSibling;
        assert(decl_list->nodeType == VARIABLE_DECL_LIST_NODE);
        assert(stmt_list->nodeType == STMT_LIST_NODE);
        genDeclList(decl_list);
        genStmtList(stmt_list);
    }
    // block -> decl_list | stmt_list
    else
    {
        if (child->nodeType == VARIABLE_DECL_LIST_NODE)
        {
            genDeclList(child);
        }
        else if (child->nodeType == STMT_LIST_NODE)
        {
            genStmtList(child);
        }
        else
        {
            puts("Undefined block child nodeType");
        }
    }
}


int getVariableSize(TypeDescriptor *typeDescriptor)
{
    if(typeDescriptor->kind == SCALAR_TYPE_DESCRIPTOR)
    {
        //INT_TYPE and FLOAT_TYPE
        return 4;
    }
    else if(typeDescriptor->kind == ARRAY_TYPE_DESCRIPTOR)
    {
        ArrayProperties* arrayPropertiesPtr = &(typeDescriptor->properties.arrayProperties);
        
        int arrayElementCount = 1;
        int index = 0;
        for(index = 0; index < arrayPropertiesPtr->dimension; ++index)
        {
            arrayElementCount *= arrayPropertiesPtr->sizeInEachDimension[index];
        }

        return arrayElementCount * 4;
    }
    else
    {
        printf("Error in int getVariableSize(TypeDescriptor *typeDescriptor)\n");
        return -1;
    }
}

int getArrayCount(AST_NODE *dim){
    int size = 1;
    forEach(dim){
        size *= getExprValue(dim);
    }
    return size;
}

void genFunctionDecl(AST_NODE *funcDeclNode)
{
    //TODO child or not
    AST_NODE *typeNode, *idNode, *paramListNode, *blockNode;
    typeNode = funcDeclNode->child;
    idNode = typeNode->rightSibling;
    currentFuncName = ID_Name(idNode);
    paramListNode = idNode->rightSibling;
    blockNode = paramListNode->rightSibling;
    gen_head(currentFuncName);

     // proceed param
    AST_NODE *it = paramListNode->child;
    int size = 0;
    forEach(it){
        size += 8;
    }
    it = paramListNode->child;
    int size_tmp = 0;
    forEach(it){
        AST_NODE *itt = it->child;
        AST_NODE *head = itt->child;
        AST_NODE *id = head->rightSibling;
        size_tmp += 8;
        setIDOffset(id, -size-prologue_stack_size + size_tmp);
    }
    
    int ARSize = abs(getIDAttr(idNode)->offset) + 26 * 4;

	while(ARSize%8 != 0){
		ARSize=ARSize+4;	
	}
	ARSize = ARSize +18*4;
	while(ARSize%8 != 0){
		ARSize=ARSize+4;	
	}
	if (ARSize + 8 < 93)
        ARSize = 92-8;
    
    printf("local stack size %d\n", ARSize);
    
    gen_prologue(ID_Name(idNode));
    //gen_block(blockNode);
    gen_epilogue(ID_Name(idNode), ARSize+8);
}

int genVariableRef(AST_NODE *idNode)
{
    assert(idNode->nodeType == IDENTIFIER_NODE);
    assert(ID_SymTab(idNode) != NULL);
    assert(getIDAttr(idNode)->attributeKind == VARIABLE_ATTRIBUTE);
    TypeDescriptor *typeDescriptor = getIDTypeDescriptor(idNode);

    int reg;

    if (ID_Kind(idNode) == ARRAY_ID)
    {
        return genArrayRef(idNode);
    }
    else
    {
        if (getIDGlobal(idNode))
        {
            reg = get_reg();
            emit("ldr x%d, =_g_%s", reg, ID_Name(idNode));
        }
        else
        {
            int offset = getIDOffset(idNode);
            fprintf(stderr, "Var ref name: %s, offset: %d\n", ID_Name(idNode), offset);
            reg = genIntLiteral(offset);
            emit("sub x%d, x29, x%d", reg, reg);
        }
        if (idNode->dataType == INT_TYPE)
        {
            emit("ldr w%d, [x%d, #0]", reg, reg);
        }
        else
        {
            emit("ldr s%d, [x%d, #0]", reg, reg);
        }
    }
    return reg;
}

void generateCode(AST_NODE *root)
{
    output = fopen("output.s", "w");
    if (!output)
    {
        printf("error opening output.s!\n");
        exit(1);
    }
    AST_NODE *traverse = root->child;
    
    while (traverse)
    {
        if (traverse->nodeType == VARIABLE_DECL_LIST_NODE)
        {
            emit(".data");
            genGlobalVarDecl(traverse);
            emit(".text");
        }
        else if (traverse->nodeType == DECLARATION_NODE)
        {
            genFunctionDecl(traverse);
        }
        traverse = traverse->rightSibling;
    }

    fclose(output);
}