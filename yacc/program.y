%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdbool.h>

extern FILE* yyin;
extern char* yytext;
extern int yylineno;

struct variabile{
      char* tip;
      char* denumire;
      char* val;
      short constanta;
}v[100];

struct list_parametru{
      char* tip;
      char* denumire;
};

struct functie{
      char* ret;//return ca tip
      int nrp;
      char* denumire;
      struct list_parametru pf[32];

}f[100];

struct structura{
        char tip[20];
        int nv;
        struct variabile variabile_c[10];
}structuri[100];

struct expresie
{
char* denumire;
bool este_int;
bool este_real;
bool este_id;
bool este_char;
bool este_string;
}ex[100];

int nr_ex=0;
char tip1[8];
char p[32][8];
int nr_p=0;

int nrs=0;bool ok=false;
struct list_parametru aux[100];
int numarv=0,numarf=0,numaraux=0;
char fisier_variabile[]="symbol_table.txt";
char fisier_functii[]="symbol_table_functions.txt ";
void lipsa_init(char* tip,char* denumire, int este_const);
void init_atrib(char* tip,char* denumire,int val,int este_const);
void adauga_in_tab_simb();
void adauga_in_tab_simb_func();
void insert_in_pf(char* tip, char* denumire,struct list_parametru *aux);
void adauga_functie(char* tip, char* denumire,struct list_parametru *aux);
void adauga_structura(char* tip);
void adauga_parametru_structura(char* tip, char* denumire);
void noua_structura(char* tip, char* denumire ,char* clasa, char* membru,int este_const);
int verificare_exista_variabila(char* denumire);
void vector_nou(char* tip, char* denumire);
void exista_variabila (char* denumire);
void exista_functie (char* denumire);
void variabila_deja_declarata(char* denumire);
void verificam_tip_exp();
void adaugam_in_ex(char* denumire,int tip ) ;
void verificam_stanga(char* id);
void verificam_param_apel(char* denumire);
void verificare_constant(char * denumire);
int cauta_tip_return(char * denumire);
%}

%union
{
    int intreg;
    char* logica;
    double ratio;
    float real;  
    char* sir;
    char* litera;
}

%token  BGIN END ASSIGN  CONST RETURN WHILE FOR IF INCEPUT_FUNCTII SFARSIT_FUNCTII PLUS MINUS MUL DIV  MAI_MIC MAI_MARE  MAI_MIC_E MAI_MARE_E EGAL INC DEC SI SAU DIF ELSE STRUCT   INCEPUT_STRUCT SFARSIT_STRUCT SFARSIT_VARIABILE_GLOBALE INCEPUT_VARIABILE_GLOBALE
%token<logica> BOOLEAN
%token<sir> STRING  
%token<real> NR_REAL
%token<intreg> NR
%token<sir> ID TIP
%token<litera> CHAR
%token TYPEOF
%start progr

%right ASSIGN
%left MAI_MIC_E MAI_MARE_E MAI_MIC MAI_MARE EGAL

%left PLUS MINUS 
%left DIV MUL

%left SAU
%left SI
%left DIF
%left INC DEC

%%
progr: INCEPUT_VARIABILE_GLOBALE declaratii SFARSIT_VARIABILE_GLOBALE INCEPUT_STRUCT structuri SFARSIT_STRUCT INCEPUT_FUNCTII functii SFARSIT_FUNCTII bloc {printf("program corect sintactic\n"); adauga_in_tab_simb();adauga_in_tab_simb_func();}
     ;


declaratii :  declaratie_globala ';'  
  |  declaratii declaratie_globala ';'
  ;

declaratie_globala : TIP ID {variabila_deja_declarata($2);lipsa_init($1,$2,0);}
                   | TIP ID  ASSIGN start_expresie {variabila_deja_declarata($2);init_atrib($1,$2,0,0); verificam_stanga($2);}
                   | TIP ID '[' NR ']' ASSIGN start_expresie {variabila_deja_declarata($2);vector_nou($1,$2);   verificam_stanga($2);} 
                   | TIP ID '[' NR ']'  {variabila_deja_declarata($2);vector_nou($1,$2);}
                   | CONST TIP ID  ASSIGN start_expresie {variabila_deja_declarata($3);init_atrib($2,$3,0,1); verificam_stanga($3);}
                   | ID  ASSIGN start_expresie {exista_variabila($1); verificam_stanga($1);}
                   | TIP ID ASSIGN STRING{variabila_deja_declarata($2);init_atrib($1,$2,0,0);strcpy(tip1,"string");verificam_stanga($2);}
                   | TIP ID ASSIGN CHAR{variabila_deja_declarata($2);init_atrib($1,$2,0,0);strcpy(tip1,"char");verificam_stanga($2);}
                   | CONST TIP ID ASSIGN STRING{variabila_deja_declarata($3);init_atrib($2,$3,0,1);strcpy(tip1,"string");verificam_stanga($3);}
                   | CONST TIP ID ASSIGN CHAR{variabila_deja_declarata($3);init_atrib($2,$3,0,1);strcpy(tip1,"char");verificam_stanga($3);}
                   | CONST TIP ID '[' NR ']' ASSIGN CHAR {variabila_deja_declarata($3);init_atrib($2,$3,0,1);strcpy(tip1,"char");verificam_stanga($3);}
                   | CONST TIP ID '[' NR ']' ASSIGN STRING {variabila_deja_declarata($3);init_atrib($2,$3,0,1);strcpy(tip1,"string");verificam_stanga($3);}
                   ;

functii: functie
       | functii functie
       | functii CONST functie
       | CONST functie
       ;  
     
structuri:  def_clasa
           | structuri def_clasa
           ;    
       
def_clasa : STRUCT ID '{' clasa_declaratii '}' ';' {adauga_structura($2);}
          ;

clasa_declaratii : clasa_declaratie
                 | clasa_declaratii clasa_declaratie
                 ;

clasa_declaratie : declaratie_clasa ';'
                 |  functie
                 ;
                 
declaratie_clasa: TIP ID {variabila_deja_declarata($2);adauga_parametru_structura($1,$2);   }
                   | TIP ID  ASSIGN start_expresie  {variabila_deja_declarata($2);adauga_parametru_structura($1,$2); verificam_stanga($2);  }
                   | TIP ID '[' NR ']' ASSIGN start_expresie {variabila_deja_declarata($2);adauga_parametru_structura($1,$2);  verificam_stanga($2); }
                   | TIP ID '.' ID ASSIGN start_expresie {variabila_deja_declarata($2);adauga_parametru_structura($1,$2);  verificam_stanga($4); }
                   | TIP ID '.' ID '[' NR ']' ASSIGN start_expresie {variabila_deja_declarata($2);adauga_parametru_structura($1,$2); verificam_stanga($4);  }
                   | TIP ID '[' NR ']'   {variabila_deja_declarata($2);adauga_parametru_structura($1,$2);   }
                   | CONST TIP ID  ASSIGN start_expresie {variabila_deja_declarata($3);init_atrib($2,$3,0,1); verificam_stanga($3);}
                   | CONST TIP ID '[' NR ']' ASSIGN start_expresie {variabila_deja_declarata($3);init_atrib($2,$3,0,1); verificam_stanga($3);}
                   | TIP ID ASSIGN STRING{variabila_deja_declarata($2);init_atrib($1,$2,0,0);strcpy(tip1,"string");verificam_stanga($2);}
                   | TIP ID ASSIGN CHAR{variabila_deja_declarata($2);init_atrib($1,$2,0,0);strcpy(tip1,"char");verificam_stanga($2);}
                   | CONST TIP ID ASSIGN STRING{variabila_deja_declarata($3);init_atrib($2,$3,0,1);strcpy(tip1,"string");verificam_stanga($3);}
                   | CONST TIP ID ASSIGN CHAR{variabila_deja_declarata($3);init_atrib($2,$3,0,1);strcpy(tip1,"char");verificam_stanga($3);}
                   | CONST TIP ID '[' NR ']' ASSIGN CHAR {variabila_deja_declarata($3);init_atrib($2,$3,0,1);strcpy(tip1,"char");verificam_stanga($3);}
                   | CONST TIP ID '[' NR ']' ASSIGN STRING {variabila_deja_declarata($3);init_atrib($2,$3,0,1);strcpy(tip1,"string");verificam_stanga($3);}
                   ;

functie: TIP ID '(' lista_param ')' '{' list_functii '}'{ adauga_functie($1,$2,aux);}
       | TIP ID '('  ')' '{' list_functii '}'{ adauga_functie($1,$2,NULL);}
       ;
       
lista_param : param
            | lista_param ','  param
            ;

param : TIP ID { insert_in_pf($1,$2,aux);}
      | TIP ID '[' NR ']' { insert_in_pf($1,$2,aux);}
      ;

list_functii:  list_functii list_functie
            |  list_functie
            ;
list_functie:  declaratie_local ';'
         | bucla_in_functie
         | ID  ASSIGN start_expresie ';' {exista_variabila($1);}
         | TYPEOF '(' expresie ')' ';'{ verificam_tip_exp(); printf("Tipul expresiei de la linia %d este:%s \n",yylineno,tip1);}
         | TYPEOF '(' STRING ')'';' { strcpy(tip1,"string"); printf("Tipul expresiei de la linia %d este:%s \n",yylineno,tip1);}
         | TYPEOF '(' CHAR ')' ';'{ strcpy(tip1,"char"); printf("Tipul expresiei de la linia %d este:%s \n",yylineno,tip1);}
         | ID INC
         | ID DEC
            ;

bucla_in_functie: WHILE '('start_expresie')' '{' list_functii  '}'
                | FOR '(' assign_a ';' conditie_if  ';' expresie ')' '{' list_functii '}'
                | IF '('conditie_if')' '{' list_functii '}'  
                | IF '('conditie_if')' '{' list_functii '}' ELSE '{' list_functii'}'
                ;



/* bloc */
bloc : BGIN list END  
     ;
     
/* lista instructiuni */
list :  statement ';'
     | list statement ';'
     | bucla
     | list bucla
     | list declaratie_local ';'
     | declaratie_local ';'
     ;

declaratie_local : TIP ID {variabila_deja_declarata($2);lipsa_init($1,$2,0);}
                   | TIP ID  ASSIGN start_expresie {variabila_deja_declarata($2);init_atrib($1,$2,0,0); verificam_stanga($2);}
                   | TIP ID '[' NR ']' ASSIGN start_expresie {variabila_deja_declarata($2);vector_nou($1,$2); verificam_stanga($2);}
                   | TIP ID '.' ID ASSIGN start_expresie {variabila_deja_declarata($2);noua_structura($1,$2, $4,0,0); verificam_stanga($4);}
                   | ID ID {variabila_deja_declarata($2);lipsa_init($1,$2,0); }
                   | ID ID '[' NR ']' {variabila_deja_declarata($2);lipsa_init($1,$2,0);}
                   | TIP ID '.' ID '[' NR ']' ASSIGN start_expresie {variabila_deja_declarata($2);noua_structura($1,$2, $4,0,0); verificam_stanga($4);}
         	    | TIP ID '[' NR ']'  {variabila_deja_declarata($2);vector_nou($1,$2);}
                   | CONST TIP ID  ASSIGN start_expresie {variabila_deja_declarata($3);init_atrib($2,$3,0,1); verificam_stanga($3);}
                   | CONST TIP ID '[' NR ']' ASSIGN start_expresie{variabila_deja_declarata($3); init_atrib($2,$3,0,1); verificam_stanga($3);            }
                   | CONST TIP ID '.' ID ASSIGN start_expresie  {variabila_deja_declarata($3);noua_structura($2,$3, $5,0,1); verificam_stanga($5);}
                   | CONST TIP ID '.' ID '[' NR ']' ASSIGN start_expresie {variabila_deja_declarata($3);noua_structura($2,$3, $5,0,1); verificam_stanga($5);}
                   | TIP ID ASSIGN STRING{variabila_deja_declarata($2);init_atrib($1,$2,0,0);strcpy(tip1,"string");verificam_stanga($2);}
                   | TIP ID ASSIGN CHAR{variabila_deja_declarata($2);init_atrib($1,$2,0,0);strcpy(tip1,"char");verificam_stanga($2);}
                   | CONST TIP ID ASSIGN STRING{variabila_deja_declarata($3);init_atrib($2,$3,0,1);strcpy(tip1,"string");verificam_stanga($3);}
                   | CONST TIP ID ASSIGN CHAR{variabila_deja_declarata($3);init_atrib($2,$3,0,1);strcpy(tip1,"char");verificam_stanga($3);}
                   | CONST TIP ID '[' NR ']' ASSIGN CHAR {variabila_deja_declarata($3);init_atrib($2,$3,0,1);strcpy(tip1,"char");verificam_stanga($3);}
                   | CONST TIP ID '[' NR ']' ASSIGN STRING {variabila_deja_declarata($3);init_atrib($2,$3,0,1);strcpy(tip1,"string");verificam_stanga($3);}
                   ;
                   
                   

bucla: WHILE '('conditie_if')' '{' list  '}'
     | FOR '(' assign_a ';' conditie_if  ';' expresie ')' '{' list '}'
     | IF '('conditie_if')' '{' list '}'  
     | IF '('conditie_if')' '{' list '}' ELSE '{' list'}'
     ;

/* instructiune */
statement: assign_a
         | ID '(' lista_apel ')' {exista_functie($1); verificam_param_apel($1); nr_p=0;}
         | ID '(' ')'{exista_functie($1); verificam_param_apel($1); nr_p=0;}
         | TYPEOF '(' expresie ')' { verificam_tip_exp(); printf("Tipul expresiei de la linia %d este:%s \n",yylineno,tip1);}
         | TYPEOF '(' STRING ')' { strcpy(tip1,"string"); printf("Tipul expresiei de la linia %d este:%s \n",yylineno,tip1);}
         | TYPEOF '(' CHAR ')' { strcpy(tip1,"char"); printf("Tipul expresiei de la linia %d este:%s \n",yylineno,tip1);}
         | ID INC
         | ID DEC
         ;
assign_a : ID  ASSIGN start_expresie {exista_variabila($1);verificam_stanga($1);verificare_constant($1);}
         | ID '[' NR ']' ASSIGN start_expresie {exista_variabila($1); verificam_stanga($1);verificare_constant($1);}
         | ID '.' ID ASSIGN start_expresie {exista_variabila($1);exista_variabila($3); verificam_stanga($3);verificare_constant($3);}
         | ID '.' ID '[' NR ']' ASSIGN start_expresie {exista_variabila($1);exista_variabila($3); verificam_stanga($3);verificare_constant($3);}
         | ID ASSIGN STRING{exista_variabila($1);verificam_stanga($1);verificare_constant($1);}
         | ID ASSIGN CHAR{exista_variabila($1);verificam_stanga($1);verificare_constant($1);}
         | ID '[' NR ']' ASSIGN CHAR {exista_variabila($1);strcpy(tip1,"char");verificam_stanga($1);verificare_constant($1);}
         | ID '[' NR ']' ASSIGN STRING {exista_variabila($1);strcpy(tip1,"string");verificam_stanga($1);verificare_constant($1);}
         ;


start_expresie : expresie  {verificam_tip_exp();}
               ;

expresie : expresie PLUS expresie    
         | expresie MINUS expresie  //{$$=$1-$3;}
         | expresie MUL expresie    //{$$=$1*$3;}
         | expresie DIV expresie    //{$$=$1/$3;}
         | expresie INC
         | expresie DEC
         | '(' expresie ')'          
         | NR                       {char nr1[8]; sprintf(nr1,"%d",$1); adaugam_in_ex(nr1,1);}
         | ID                       { exista_variabila($1);adaugam_in_ex($1,3);}
         | ID '[' NR ']'            { exista_variabila($1);}
         | ID '(' lista_apel ')'    {exista_functie($1);  verificam_param_apel($1);adaugam_in_ex($1,cauta_tip_return($1)); nr_p=0;}
         | ID '.' ID                { exista_variabila($1); exista_variabila($3);}
         | ID '.' ID '[' NR ']'     { exista_variabila($1); exista_variabila($3);}
         | NR_REAL                  {char nr1[8]; snprintf(nr1,9,"%f",$1); adaugam_in_ex(nr1,2);}
         ;




conditie_if : expresie_if {verificam_tip_exp();}
            ;

expresie_if: expresie_if EGAL expresie_if  
	 | expresie_if MAI_MARE expresie_if
         | expresie_if MAI_MIC expresie_if
         | expresie_if MAI_MIC_E expresie_if
         | expresie_if MAI_MARE_E expresie_if
         | expresie_if DIF expresie_if
         | expresie_if SAU expresie_if
         | expresie_if SI expresie_if
         | expresie_if PLUS expresie_if    
         | expresie_if MINUS expresie_if  //{$$=$1-$3;}
         | expresie_if MUL expresie_if    //{$$=$1*$3;}
         | expresie_if DIV expresie_if    //{$$=$1/$3;}
         | expresie_if INC
         | expresie_if DEC 
         | '(' expresie_if ')'          
         | NR                       {char nr1[8]; sprintf(nr1,"%d",$1); adaugam_in_ex(nr1,1);}
         | ID                       { exista_variabila($1);adaugam_in_ex($1,3);}
         | ID '[' NR ']'            { exista_variabila($1);}
         | ID '(' lista_apel ')'    {exista_functie($1);  verificam_param_apel($1);adaugam_in_ex($1,cauta_tip_return($1)); nr_p=0;}
         | ID '.' ID                { exista_variabila($1); exista_variabila($3);}
         | ID '.' ID '[' NR ']'     { exista_variabila($1); exista_variabila($3);}
         | NR_REAL                  {char nr1[8]; snprintf(nr1,9,"%f",$1); adaugam_in_ex(nr1,2);}
         ;

lista_apel : start_expresie {strcpy(p[nr_p],tip1); nr_p++; }
           | lista_apel ',' start_expresie {strcpy(p[nr_p],tip1); nr_p++;}
           ;




%%
int yyerror(char * s){
printf("eroare: %s la linia:%d\n",s,yylineno);
}

int main(int argc, char** argv){
yyin=fopen(argv[1],"r");

for(int i=0 ;i<100;i++)
{
ex[i].este_int=false;
ex[i].este_real=false;
ex[i].este_id=false;
}

yyparse();
}

void lipsa_init(char* tip2,char* denumire, int este_const){

        if(este_const==1){
                char eroare[100]="constanta fara initializare!!!  ";
                strcat(eroare,denumire);
                yyerror(eroare);
                exit(1);
        }
     
        v[numarv].tip=strdup(tip2);
        v[numarv].denumire=strdup(denumire);
        v[numarv].constanta=0;
        numarv++;
}
void init_atrib(char* tip,char* denumire,int val,int este_const){
       
        v[numarv].tip=strdup(tip);
        v[numarv].denumire=strdup(denumire);
        char valoare[50];
        snprintf(valoare,50,"%d",val);
        v[numarv].val=strdup(valoare);
        v[numarv].constanta=este_const;
        numarv++;
     
}


void adauga_functie(char* tip, char* denumire,struct list_parametru *aux)
{       //mutam valorile din structura auxiliara in functie
        f[numarf].nrp=numaraux;
        f[numarf].ret=strdup(tip);
        f[numarf].denumire=strdup(denumire);
        for(int i =0;i<numaraux;i++) 
        {
                f[numarf].pf[i].denumire=strdup(aux[i].denumire);
                f[numarf].pf[i].tip=strdup(aux[i].tip);
        }
        numarf++; 
        numaraux=0; // resetare aux

}


void insert_in_pf(char* tip, char* denumire,struct list_parametru *aux)
{
       
        aux[numaraux].denumire=(char*)malloc(strlen(denumire));
        strcpy(aux[numaraux].denumire,denumire);
        aux[numaraux].tip=(char*)malloc(strlen(tip));
        strcpy(aux[numaraux].tip,tip);
        numaraux++;    
}

void adauga_structura(char* tip){
        strcpy(structuri[nrs].tip,tip);
        structuri[nrs].nv=0;
        nrs++;
}

void adauga_parametru_structura(char* tip, char* denumire){

        structuri[nrs].variabile_c[structuri[nrs].nv].tip=strdup(tip);
        structuri[nrs].variabile_c[structuri[nrs].nv].denumire=strdup(denumire);
        char buf[20];
        lipsa_init(tip,denumire, 0);
        structuri[nrs].nv++;
}



void noua_structura(char* tip, char* denumire ,char* clasa, char* membru,int este_const){
        char id_data_membru[20];
        bzero(id_data_membru,20);
        strcat(id_data_membru,clasa);
        strcat(id_data_membru,".");
        strcat(id_data_membru,membru);
        int index=verificare_exista_variabila(id_data_membru);
     
       
         if(strcmp(tip,v[index].tip)==0){
                       
         	v[numarv].tip=strdup(tip);
         	v[numarv].denumire=strdup(denumire);
         	v[numarv].val=v[index].val;
         	v[numarv].constanta=este_const;
          	numarv++;
          }  
       
}

int verificare_exista_variabila(char* denumire){
        for (int i = 0; i < numarv; i++){
               if(strcmp(v[i].denumire,denumire)==0)
               {
                       return i;
               }
        }
        return -1;
}



void vector_nou(char* tip, char* denumire){
        int index=verificare_exista_variabila(denumire);
        if(index!=-1){
                char error_msg[250];
                strcat(error_msg, "Vector deja declarat");
                yyerror(error_msg);
                exit(0);  
        }
        else{  
                v[numarv].tip=strdup(tip);
                v[numarv].denumire=strdup(denumire);
                v[numarv].constanta=0;
                numarv++;  
        }
}




void adauga_in_tab_simb()
{
      FILE* var_fisier_ptr;
      var_fisier_ptr=fopen(fisier_variabile,"w+"); 
      fprintf(var_fisier_ptr,"tip <> denumire  <>  val \n");
     
   
      for(int i=0;i<numarv;i++){
              if(!(v[i].constanta))
                fprintf(var_fisier_ptr,"%s  <>  %s  <>  %s  \n", v[i].tip, v[i].denumire,v[i].val);
              else
                fprintf(var_fisier_ptr,"const %s  <>  %s  <>  %s  \n", v[i].tip, v[i].denumire,v[i].val);
      }
     

      fclose(var_fisier_ptr);
}
void adauga_in_tab_simb_func()
{
        
        FILE* functii_fisier_ptr;
        functii_fisier_ptr=fopen(fisier_functii,"w+");
        fprintf(functii_fisier_ptr,"tip <> denumire  <>  parametrii \n");
     
        for(int i=0;i<numarf;i++) {
          
          fprintf(functii_fisier_ptr,"%s <> %s <>  ",f[i].ret,f[i].denumire);

          
          for(int j=0; j<f[i].nrp; j++) {
                  fprintf(functii_fisier_ptr, "%s %s ", f[i].pf[j].tip, f[i].pf[j].denumire);
          }

          fprintf(functii_fisier_ptr, "\n");
        }


        fclose(functii_fisier_ptr);
}

void exista_variabila (char* denumire)
{
	bool ok=false;
	for(int i=0; i<numarv;i++)
	{
		if(strcmp(v[i].denumire,denumire)==0)
		{
			ok=true;
			break;
		}
	}
	if(ok==false)
	{
		char eroare[]="variabila ";
		strcat(eroare,denumire);
		strcat(eroare," nedeclarata!!!!");
		yyerror(eroare);
		exit(1);
	}


}


void exista_functie (char* denumire)
{
	bool ok=false;
	for(int i=0; i<numarf;i++)
	{
		if(strcmp(f[i].denumire,denumire)==0)
		{
			ok=true;
			break;
		}
	}
	if(ok==false)
	{	
		char eroare[]="functia ";
		strcat(eroare,denumire);
		strcat(eroare," nedeclarata!!!!");
		yyerror(eroare);
		exit(1);
	}
}


void variabila_deja_declarata (char* denumire)
{
	bool ok=false;
	for(int i=0; i<numarv;i++)
	{
		if(strcmp(v[i].denumire,denumire)==0)
		{
			ok=true;
			break;
		}
	}

	if(ok==true)
	{
		char eroare[]="variabila ";
		strcat(eroare,denumire);
		strcat(eroare," deja declarata!!!!");
		yyerror(eroare);
		exit(1);
	}

}


void adaugam_in_ex(char* denumire,int tip ) //tip 1-int 2-real 3-id 4-char 5-string
{

	ex[nr_ex].denumire=denumire;
	if(tip==1)
		ex[nr_ex].este_int=true;
	if(tip==2)
		ex[nr_ex].este_real=true;
	if(tip==3)
		ex[nr_ex].este_id=true;
	if(tip==4)
		ex[nr_ex].este_char=true;
	if(tip==5)
		ex[nr_ex].este_string=true;
	nr_ex++;
}

void verificam_tip_exp()
{
       bzero(tip1,0);
	bool ok=true;
	char tip[8];
	if(ex[0].este_int==true)
	strcpy(tip,"int");
	if(ex[0].este_real==true)
		strcpy(tip,"float");
	if(ex[0].este_char==true)
		strcpy(tip,"char");
	if(ex[0].este_string==true)
		strcpy(tip,"string");
	if(ex[0].este_id==true)
		for(int j=0;j<numarv;j++)
		{
			if(strcmp(v[j].denumire,ex[0].denumire)==0)
			{
				strcpy(tip,v[j].tip);
				break;
			}

		}
	for(int i=1;i<nr_ex;i++)
	{

		if(ex[i].este_id==true)
			for(int j=0;j<numarv;j++)
			{
				if(strcmp(v[j].denumire,ex[i].denumire)==0)
					if(strcmp(tip,v[j].tip)!=0)
					{
						ok=false;
						break;
					}
			}
		if(ex[i].este_int==true)
		{	
			if(strcmp(tip,"int")!=0)
			{
				ok=false;
				break;
			}
		}

		if(ex[i].este_real==true)
		{
			if(strcmp(tip,"float")!=0)
			{
				ok=false;
				break;
			}
		}

		if(ex[i].este_char==true)
		{
			if(strcmp(tip,"char")!=0)
			{
				ok=false;
				break;
			}			
		}

		if(ex[i].este_string==true)
		{
			if(strcmp(tip,"string")!=0)
			{
				ok=false;
				break;
			}
		}
	}

	if(ok==false)
	{
		char eroare[]="expresie cu tipuri diferite (in partea dreapta)!!!!";
		yyerror(eroare);
		exit(1);
	}

	for(int i=0;i<nr_ex;i++)
	{
		bzero(ex[i].denumire,0);
		ex[i].este_int=false;
		ex[i].este_real=false;
		ex[i].este_id=false;
		ex[i].este_char=false;
		ex[i].este_string=false;

	}
	nr_ex=0;

	strcpy(tip1,tip);
}

void verificam_stanga(char* id)
{
	int index=-1;
	for(int i=0;i<numarv;i++)
	{
		if(strcmp(v[i].denumire,id)==0)
		{
			index=i;
			break;
		}
	}

	if(strcmp(v[index].tip,tip1)!=0)
	{
		char eroare[]="tipuri diferite (stanga cu dreapta)!!!!";
		yyerror(eroare);
		exit(1);
	}
	bzero(tip1,0);
}


void verificam_param_apel(char* denumire)
{
	/*for(int i=0;i<nr_p;i++)
	{
	printf("%s  ",p[i]);
	}

	printf("\n");
	*/
	bool ok=false;
	int index=-1;
	for(int i=0; i<numarf; i++)
	{
		if(strcmp(denumire,f[i].denumire)==0)
		{
			index=i;
			if(nr_p==f[i].nrp){

				for(int j=0; j<f[index].nrp; j++)
				{
					if(strcmp(f[index].pf[j].tip,p[j])==0)
					{
						ok=true;
						break;
					}
				}
				if(ok==true){
					break;
				}

			}
		}		
	}
	/*for(int i=0;i<f[index].nrp;i++)
	{
	printf("%s  ",f[i].tip);
	}
	*/


	if(ok==false)
	{
		char eroare[]="parametrii apelului de funcție nu are tipurile din definiția funcției !!!!";
		yyerror(eroare);
		exit(1);
	}


}

void verificare_constant(char * denumire){

	for(int i=0;i<numarv;i++){

		if(strcmp(v[i].denumire,denumire)==0){

			if(v[i].constanta==1){

				char eroare[]="incercare de modificare a variabilei constante!!!!";
				yyerror(eroare);
				exit(1);


			}


		}

	}

}

int cauta_tip_return(char * denumire){

	for(int i=0;i<numarf;i++){

	if(strcmp(f[i].denumire,denumire)==0){


		if(strcmp(f[i].ret,"int")==0){

		return 1;
		}

		if(strcmp(f[i].ret,"float")==0){

		return 2;
		}


		if(strcmp(f[i].ret,"char")==0){

		return 4;
		}

		if(strcmp(f[i].ret,"string")==0){

		return 5;
		}

		}


	}

	return 0;

}