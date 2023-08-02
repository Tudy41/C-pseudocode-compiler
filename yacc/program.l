%{
#include <stdio.h>
#include "y.tab.h"
%}
%option noyywrap
%%
"int"|"float"|"char"|"string"|"bool" {yylval.sir = strdup(yytext);return TIP;}
"const" {yylval.sir = strdup(yytext);return CONST;}
"return" {return RETURN;}
"if" {return IF;}
"else" {return ELSE;}
"while" {return WHILE;}
"for" {return FOR;}

"=" {return ASSIGN;}

"+" {return PLUS;}
"-" {return MINUS;}
"*" {return MUL;}
"/" {return DIV;}
"++" {return INC;}
"--" {return DEC;}


"<" {return MAI_MIC;}
">" {return MAI_MARE;}
"<=" {return MAI_MIC_E;}
">=" {return MAI_MARE_E;}
"==" {return EGAL;}
"&&" {return SI;}
"||" {return SAU;}
"!=" {return DIF;}

"TypeOf" {return TYPEOF;}

"inceput" {return BGIN;}
"sfarsit" {return END;}
"inceput_functii" {return INCEPUT_FUNCTII;}
"sfarsit_functii" {return SFARSIT_FUNCTII;}
"inceput_struct" {return INCEPUT_STRUCT;}
"sfarsit_struct" {return SFARSIT_STRUCT;}
"inceput_variabile_globale" {return INCEPUT_VARIABILE_GLOBALE;}
"sfarsit_variabile_globale" {return SFARSIT_VARIABILE_GLOBALE;}

"struct"  {return STRUCT;}

adevarat                           {yylval.logica=strdup(yytext);return BOOLEAN;}
fals                             {yylval.logica=strdup(yytext); return BOOLEAN;}
[a-zA-Z]+([a-zA-Z]*[_]*[0-9]*)*	     {yylval.sir=strdup(yytext); return ID;}
\"[^\"]*\"                           {yylval.sir=strdup(yytext); return STRING;}
([1-9][0-9]*|0)  {yylval.intreg=atoi(yytext);return NR;}
([1-9][0-9]*|0)\.([0-9]*)  {yylval.real=atof(yytext); return NR_REAL;}
\'[^\']\' {yylval.litera= strdup(yytext); return CHAR;}  
[ \t] ;
\n {yylineno++;}
. {return yytext[0];}

 