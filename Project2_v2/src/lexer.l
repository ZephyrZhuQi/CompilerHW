%option noyywrap 
%{
#include <stdio.h>
#include "header.h" 
int linenumber;
symtab * lookup();
symtab * ptr;
void     insertID();
void 	 printSymTab();
/* You need to define for all tokens in C--, here are some examples */
/* operators */
#define OP_PLUS      13
#define OP_MINUS     14
#define OP_MUL       15
#define OP_DIV       16
#define OP_BIGGER    1
#define OP_SMALLER   1
#define OP_BIGGERE   1
#define OP_SMALLERE  1
#define OP_EQUAL     1
#define OP_NEQUAL    1
#define OP_AND       1
#define OP_OR 		   13 
#define OP_NOT       1
#define OP_ASSIGN 	12 
/* separators */
#define MK_LBRACE    16
#define MK_RBRACE    17
#define MK_LBRACKET  1
#define MK_RBRACKET  1
#define MK_LPAREN 	14 
#define MK_RPAREN 	15 
#define MK_SEMICOLON 1
#define MK_COMMA     1
#define MK_PERIOD    1
/* identifiers */
/* reserved words */
#define INT		      10
#define FLOAT 		   11 
#define RETURN       1
#define TYPEDEF      1
#define IF           1
#define ELSE         1
#define FOR          1
#define VOID         1
#define WHILE        1
/* constants */
#define INT_CONSTANT 1
#define FLOAT_CONSTANT        1
#define STRING_CONSTANT       12

#define ERROR 		   100 

%}

letter            [A-Za-z]
digit             [0-9]
newline           "\n"
WS	               [ \n\t\r]+
String_constant   \"({letter}|{WS})*\"
ID	               {letter}({letter}|{digit}|"_")*
Pos_Int_constant  {digit}+
Int_constant      [-]?{Pos_Int_constant}


/* reserved words */
RWInt             "int"
RWFloat           "float"  
RWReturn          "return"
RWTypedef         "typedef"
RWIf              "if"
RWElse            "else"
RWFor             "for"
RWVoid            "void"
RWWhile           "while"

/* You need to define the following RE's */
Float_constant    -?({Pos_Int_constant}?(\.)?{Pos_Int_constant}([eE][+-]?{Pos_Int_constant})?|{Pos_Int_constant}\.{Pos_Int_constant}?([eE][+-]?{Pos_Int_constant})?)
/*    .12e+10     12.e+10   12e+10       */

/* operators */
OP_Plus           "+"
OP_Minus          "-"
OP_Mul            "*"
OP_Div            "/"

OP_bigger         ">"
OP_smaller        "<"
OP_biggere        ">="
OP_smallere       "<="
OP_nequal         "!="
OP_equal          "=="

OP_or             "||"
OP_and            "&&"
OP_not            "!"

OP_assign         "="

DL_lparen         "("
DL_rparen         ")"
DL_lbrace         "{"
DL_rbrace         "}"
DL_lbracket       "["
DL_rbracket       "]"
DL_comma          ","
DL_semicol        ";"
DL_dot            "."

/* Other separators appear here */
error             .
%x                comment 
%%
<INITIAL>{
"/*"              {
                     printf("%s",yytext);
                     BEGIN(comment);
                  }
}
<comment>{
[^*\n]*           printf("%s",yytext);
"*"+"[^/]"        printf("%s",yytext);
\n                {
                     printf("%s",yytext);
                     yylineno++;
                  }
"*/"              {
                     printf("%s\n",yytext);
                     BEGIN(INITIAL);
                  }
}

{Int_constant}    /*return INT_CONSTANT;*/
{Float_constant}  /*return FLOAT_CONSTANT;*/
{String_constant} /*return STRING_CONSTANT;*/
                  

{WS}              {}

{RWInt}            /* return INT; */ 
{RWFloat}          /* return FLOAT; */
{RWReturn}         /*return RETURN;*/ 
{RWTypedef}        /*return TYPEDEF;*/ 
{RWIf}             /*return IF;*/ 
{RWElse}           /*return ELSE;*/ 
{RWFor}            /*return FOR;*/ 
{RWVoid}           /*return VOID;*/ 
{RWWhile}          /*return WHILE;*/ 


{ID}              { 
			ptr = lookup(yytext);
			if (ptr == NULL)
			     insertID(yytext);	
			else 
			     ptr->counter++;
		            }
{OP_Plus}         /*return OP_PLUS;*/
{OP_Minus}        /*return OP_MINUS;*/
{OP_Mul}          /*return OP_MUL;*/
{OP_Div}          /*return OP_DIV;*/
{OP_bigger}       /*return OP_BIGGER;*/  
{OP_smaller}      /*return OP_SMALLER;*/  
{OP_biggere}      /*return OP_BIGGERE;*/  
{OP_smallere}     /*return OP_SMALLERE;*/  
{OP_nequal}       /*return OP_NEQUAL;*/  
{OP_equal}        /*return OP_EQUAL;*/  
{OP_and}          /*return OP_AND;*/ 
{OP_or}           /*return OP_OR;     */ 
{OP_not}          /*return OP_NOT;*/  
{OP_assign}       /*return OP_ASSIGN; */

{DL_lparen}       /*return MK_LPAREN; */
{DL_rparen}       /*return MK_RPAREN; */
{DL_lbrace}       /*return MK_LBRACE; */
{DL_rbrace}       /*return MK_RBRACE; */
{DL_lbracket}     /*return MK_LBRACKET; */
{DL_rbracket}     /*return MK_RBRACKET; */
{DL_comma}        /*return MK_COMMA; */
{DL_semicol}      /*return MK_SEMICOL;*/
{DL_dot}          /*return MK_PERIOD; */
{newline}         linenumber += 1;
{error}           printf("ERR:%s \n",yytext);/* return ERROR; */


%%

main(int argc, char **argv)
{
   argc--; ++argv;
   if (argc > 0)
      yyin = fopen(argv[0], "r");
   else
      yyin = stdin;
   yylex();
   printf("Frequency of identifiers:\n");
   printSymTab();
}

