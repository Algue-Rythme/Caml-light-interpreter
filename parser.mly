%{
    open Expr
%}

%token <int>
%token EQUIVALENT IMPLIES XOR OR AND NOT
%token LPARENT RPARENT
%token EOL

%left EQUIVALENT
%right IMPLIES
%left XOR
%left OR
%left AND
%nonassoc UMINUS

%start main

%type <Expr.propFormula> main

%%
