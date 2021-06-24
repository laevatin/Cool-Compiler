/*
 *  cool.y
 *              Parser definition for the COOL language.
 *
 */
%{
#include <iostream>
#include "cool-tree.h"
#include "stringtab.h"
#include "utilities.h"

/* Add your own C declarations here */


/************************************************************************/
/*                DONT CHANGE ANYTHING IN THIS SECTION                  */

extern int yylex();           /* the entry point to the lexer  */
extern int curr_lineno;
extern char *curr_filename;
Program ast_root;            /* the result of the parse  */
Classes parse_results;       /* for use in semantic analysis */
int omerrs = 0;              /* number of errors in lexing and parsing */

/*
   The parser will always call the yyerror function when it encounters a parse
   error. The given yyerror implementation (see below) justs prints out the
   location in the file where the error was found. You should not change the
   error message of yyerror, since it will be used for grading puproses.
*/
void yyerror(const char *s);

/*
   The VERBOSE_ERRORS flag can be used in order to provide more detailed error
   messages. You can use the flag like this:

     if (VERBOSE_ERRORS)
       fprintf(stderr, "semicolon missing from end of declaration of class\n");

   By default the flag is set to 0. If you want to set it to 1 and see your
   verbose error messages, invoke your parser with the -v flag.

   You should try to provide accurate and detailed error messages. A small part
   of your grade will be for good quality error messages.
*/
extern int VERBOSE_ERRORS;
extern char* yytext;

%}

/* A union of all the types that can be the result of parsing actions. */
%union {
  Boolean boolean;
  Symbol symbol;
  Program program;
  Class_ class_;
  Classes classes;
  Feature feature;
  Features features;
  Formal formal;
  Formals formals;
  Case case_;
  Cases cases;
  Expression expression;
  Expressions expressions;
  char *error_msg;
}

/* 
   Declare the terminals; a few have types for associated lexemes.
   The token ERROR is never used in the parser; thus, it is a parse
   error when the lexer returns it.

   The integer following token declaration is the numeric constant used
   to represent that token internally.  Typically, Bison generates these
   on its own, but we give explicit numbers to prevent version parity
   problems (bison 1.25 and earlier start at 258, later versions -- at
   257)
*/
%token CLASS 258 ELSE 259 FI 260 IF 261 IN 262 
%token INHERITS 263 LET 264 LOOP 265 POOL 266 THEN 267 WHILE 268
%token CASE 269 ESAC 270 OF 271 DARROW 272 NEW 273 ISVOID 274
%token <symbol>  STR_CONST 275 INT_CONST 276 
%token <boolean> BOOL_CONST 277
%token <symbol>  TYPEID 278 OBJECTID 279 
%token ASSIGN 280 NOT 281 LE 282 ERROR 283

/*  DON'T CHANGE ANYTHING ABOVE THIS LINE, OR YOUR PARSER WONT WORK       */
/**************************************************************************/
 
   /* Complete the nonterminal list below, giving a type for the semantic
      value of each non terminal. (See section 3.6 in the bison 
      documentation for details). */

/* Declare types for the grammar's non-terminals. */
%type <program> program
%type <classes> class_list
%type <class_> class
%type <features> feature_list
%type <feature> feature
%type <formals> formal_list
%type <formal> formal
%type <cases> case_list
%type <case_> case
%type <expressions> expression_list1
%type <expressions> non_empty_list1
%type <expressions> empty_list1
%type <expressions> expression_list2
%type <expression> expression
%type <expression> let_expr
%type <expression> maybe_set

/* Precedence declarations */
%precedence IN
%right ASSIGN
%precedence NOT
%nonassoc LE '=' '<'
%left '+' '-'
%left '*' '/'
%precedence ISVOID
%precedence '~'
%precedence '@'
%precedence '.'

%%
/* 
   Save the root of the abstract syntax tree in a global variable.
*/
program : class_list { ast_root = program($1); }
        | error      { ast_root = NULL; }
        ;

class_list
        : class            /* single class */
                { $$ = single_Classes($1); }
        | class_list class /* several classes */
                { $$ = append_Classes($1, single_Classes($2)); }
        ;

class_head              
        : CLASS
        | { yyerror("syntax error"); }  /* empty class head is error */
        ;

/* If no parent is specified, the class inherits from the Object class. */
class   : class_head TYPEID '{' feature_list '}' ';'                 /* with the non-empty feature list */
                { $$ = class_($2, idtable.add_string("Object"), $4,
                              stringtable.add_string(curr_filename)); }
        | class_head TYPEID INHERITS TYPEID '{' feature_list '}' ';' /* with the non-empty feature list and inherit */
                { $$ = class_($2, $4, $6, stringtable.add_string(curr_filename)); }
        | class_head TYPEID '{' '}' ';'                              /* with the empty feature list */
                { $$ = class_($2, idtable.add_string("Object"), nil_Features(),
                              stringtable.add_string(curr_filename)); }
        | class_head TYPEID INHERITS TYPEID '{' '}' ';'              /* with the empty feature list and inherit */
                { $$ = class_($2, $4, nil_Features(), 
                              stringtable.add_string(curr_filename)); }
        | class_head TYPEID error ';'
                { yyclearin; $$ = NULL; }
        | class_head TYPEID INHERITS TYPEID error ';'
                { yyclearin; $$ = NULL; }
        ;

feature_list
        : feature ';'   /* single feature */
                { $$ = single_Features($1); }
        | feature_list feature ';' /* several features */
                { $$ = append_Features($1, single_Features($2)); }
        | error ';'     /* error recovery */
                { yyclearin; $$ = NULL; };
        ;

feature : OBJECTID '(' formal_list ')' ':' TYPEID '{' expression '}' /* Feature as a method */
                { $$ = method($1, $3, $6, $8); }
        | OBJECTID ':' TYPEID ASSIGN expression  /* Feature as an attribute */
                { $$ = attr($1, $3, $5); }
        | OBJECTID ':' TYPEID   /* Feature as an attribute */
                { $$ = attr($1, $3, no_expr()); }
        ;

formal_list 
        : formal  /* single formal */
                { $$ = single_Formals($1); }
        | formal_list ',' formal  /* list of formals seperated by commas */
                { $$ = append_Formals($1, single_Formals($3)); }
        | /* %empty */  /* epsilon for the empty formal */
                { $$ = nil_Formals(); }
        ;

formal  : OBJECTID ':' TYPEID  /* rule for formal */
                { $$ = formal($1, $3); }
        ;

/* the expression list "expr,*" */
expression_list1
        : non_empty_list1
                { $$ = $1; }
        | empty_list1
                { $$ = $1; }

/* at least one expression */
non_empty_list1
        : expression    /* single expression */ 
                { $$ = single_Expressions($1); }
        | expression_list1 ',' expression       /* list of expressions seperated by commas */
                { $$ = append_Expressions($1, single_Expressions($3)); }
        ;

/* empty expression list */
empty_list1
        : /* %empty */
                { $$ = nil_Expressions(); }
        

/* the expression list expr;+, should be non-empty */
expression_list2
        : expression ';'
                { $$ = single_Expressions($1); }
        | expression_list2 expression ';'
                { $$ = append_Expressions($1, single_Expressions($2)); }
        | error ';'     /* error recovery */
                { yyclearin; $$ = NULL; };
        ;

/* case_list should be non-empty */
case_list
        : case  /* single case */
                { $$ = single_Cases($1); }
        | case_list case /* list of cases */
                { $$ = append_Cases($1, single_Cases($2)); }
        ;

case    : OBJECTID ':' TYPEID DARROW expression ';'
                { $$ = branch($1, $3, $5); }
        | error ';'     /* error recovery */
                { yyclearin; $$ = NULL; };
        ;

expression
        : OBJECTID ASSIGN expression      /* assignment */
                { $$ = assign($1, $3); }
        | expression '.' OBJECTID '(' expression_list1 ')'      /* dispatch */
                { $$ = dispatch($1, $3, $5); }
        | expression '@' TYPEID '.' OBJECTID '(' expression_list1 ')'    /* static dispatch */
                { $$ = static_dispatch($1, $3, $5, $7); }
        | OBJECTID '(' expression_list1 ')'     /* dispatch with expr self */
                { $$ = dispatch(object(idtable.add_string("self")), $1, $3); }
        | IF expression THEN expression ELSE expression FI      /* if-else */
                { $$ = cond($2, $4, $6); }
        | IF error FI 
                { $$ = NULL; }
        | WHILE expression LOOP expression POOL /* while-loop */
                { $$ = loop($2, $4); }
        | WHILE error POOL 
                { $$ = NULL; }
        | '{' expression_list2 '}'      /* { expr;+ } */
                { $$ = block($2); }
        | LET let_expr
                { $$ = $2; }
        | CASE expression OF case_list ESAC     /* case */
                { $$ = typcase($2, $4); }
        | NEW TYPEID                    /* new */
                { $$ = new_($2); }      
        | ISVOID expression             /* isvoid */
                { $$ = isvoid($2); }
        | expression '+' expression     /* + */
                { $$ = plus($1, $3); }
        | expression '-' expression     /* - */
                { $$ = sub($1, $3); }
        | expression '*' expression     /* * */
                { $$ = mul($1, $3); }
        | expression '/' expression     /* / */
                { $$ = divide($1, $3); }
        | '~' expression                /* ~ */
                { $$ = neg($2); }
        | expression '<' expression     /* < */
                { $$ = lt($1, $3); }
        | expression LE expression      /* <= */
                { $$ = leq($1, $3); }
        | expression '=' expression     /* == */
                { $$ = eq($1, $3); }
        | NOT expression                /* not */
                { $$ = comp($2); }
        | '(' expression ')'            /* (expr) */ 
                { $$ = $2; }
        | INT_CONST                     /* integer */
                { $$ = int_const($1); }
        | STR_CONST                     /* string */
                { $$ = string_const($1); }
        | BOOL_CONST                    /* boolean */
                { $$ = bool_const($1); }
        | OBJECTID                      /* ID */
                { $$ = object($1); }
        | '{' error '}' 
                { $$ = NULL; }
        ;

let_expr
        : OBJECTID ':' TYPEID maybe_set IN expression   /* the last expression */
                { $$ = let($1, $3, $4, $6); }
        | OBJECTID ':' TYPEID maybe_set ',' let_expr    /* not the last expression */
                { $$ = let($1, $3, $4, $6); }
        | error IN expression                  /* error recovery */
                { yyclearin; $$ = NULL; }
        | error ',' let_expr                   /* error recovery */
                { yyclearin; $$ = NULL; }

/* make the line number same as the reference */
maybe_set
        : ASSIGN expression
                { $$ = $2; }
        | /* %empty */
                { $$ = no_expr(); }


/* end of grammar */
%%

/* This function is called automatically when Bison detects a parse error. */
void yyerror(const char *s)
{
  cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " \
    << s << " at or near ";
  print_cool_token(yychar);
  cerr << endl;
  omerrs++;

  if(omerrs>20) {
      if (VERBOSE_ERRORS)
         fprintf(stderr, "More than 20 errors\n");
      exit(1);
  }
}

