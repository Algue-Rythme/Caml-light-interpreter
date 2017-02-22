%{
    open Expr
%}

%token TRUE FALSE
%token <int> INT
%token EQUIVALENT IMPLIES XOR OR AND NOT
%token LPARENT RPARENT
%token EOL

%left EQUIVALENT
%right IMPLIES
%left XOR
%left OR
%left AND
%nonassoc NOT

%start main

%type <Expr.propFormula> main

%%

main:
    propFormula EOL     { $1 }
;
propFormula:
    | TRUE                                  {         Const(true)  }
    | FALSE                                 {         Const(false) }
    | INT                                   {       Literal($1)    }
    | LPARENT propFormula RPARENT           {               $2     }
    | propFormula AND propFormula           {           And($1,$3) }
    | propFormula OR propFormula            {            Or($1,$3) }
    | propFormula XOR propFormula           {           Xor($1,$3) }
    | propFormula IMPLIES propFormula       {       Implies($1,$3) }
    | propFormula EQUIVALENT propFormula    {    Equivalent($1,$3) }
    | NOT propFormula                       {           Not($2)    }
;
