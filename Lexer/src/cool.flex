/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
  if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
    YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
int comment_depth = 0;

bool err_state = false;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */
int length_err();
%}

%option noyywrap

/*
 * Define names for regular expressions here.
 */

digit       [0-9]
typeid      [A-Z]([a-zA-Z0-9_])*
objid       [a-z]([a-zA-Z0-9_])*
integer     {digit}+

/* Keywords */
class       (?i:class)
else        (?i:else)
false       f(?i:alse)
fi          (?i:fi)
if          (?i:if)
in          (?i:in)
inherits    (?i:inherits)
isvoid      (?i:isvoid)
let         (?i:let)
loop        (?i:loop)
pool        (?i:pool)
then        (?i:then)
while       (?i:while)
case        (?i:case)
esac        (?i:esac)
new         (?i:new)
of          (?i:of)
not         (?i:not)
true        t(?i:rue)

/* States for comment and string */
%x COMMENT
%x DASHCOMMENT
%x STRING 

%%

 /*
  * Define regular expressions for the tokens of COOL here. Make sure, you
  * handle correctly special cases, like:
  *   - Nested comments
  *   - String constants: They use C like systax and can contain escape
  *     sequences. Escape sequence \c is accepted for all characters c. Except
  *     for \n \t \b \f, the result is c.
  *   - Keywords: They are case-insensitive except for the values true and
  *     false, which must begin with a lower-case letter.
  *   - Multiple-character operators (like <-): The scanner should produce a
  *     single token for every such operator.
  *   - Line counting: You should keep the global variable curr_lineno updated
  *     with the correct line number
  */

 /* Count the line number */
"\n" {
  curr_lineno++;
}

 /* Deal with the comments in (*...*) */
"(*" {
  BEGIN(COMMENT);
  comment_depth++;
}

"*)" {
  yylval.error_msg = "Unmatched *)";
  return ERROR;
}
 
 /* Support nested comment */
<COMMENT>"(*" {
  comment_depth++;
}

<COMMENT>"*)" {
  comment_depth--;
  if (comment_depth == 0) {
    BEGIN(INITIAL);
  }
}
 /* Count the line number in comment */
<COMMENT>\n {
  curr_lineno++;
}

 /* Eat everything in the comments */
<COMMENT>. { }

<COMMENT><<EOF>> {
  yylval.error_msg = "EOF in comment";
  BEGIN(INITIAL);
  return ERROR;
}

 /* Deal with the dash comments */
"--" {
  BEGIN(DASHCOMMENT);
}
 /* Eat everything in the comments */
<DASHCOMMENT>. { }

<DASHCOMMENT>\n {
  curr_lineno++;
  BEGIN(INITIAL);
}

 /* Deal with the string */
\" {
  string_buf_ptr = string_buf;
  BEGIN(STRING);
}
 
 /* End of string, change state and put the string into the string table. */
<STRING>\" { 
  BEGIN(INITIAL);
  *string_buf_ptr = '\0';
  if (!err_state) {
    cool_yylval.symbol = stringtable.add_string(string_buf);
    return STR_CONST;
  }
  err_state = false;
}
 /* Match the escaped newline character in string */
<STRING>\\\n {
  curr_lineno++;
  *string_buf_ptr++ = '\n';
  if (string_buf_ptr - string_buf >= MAX_STR_CONST - 1) 
    return length_err(); 
}
 /* Match the escape characters */
<STRING>\\b { 
  *string_buf_ptr++ = '\b';
  if (string_buf_ptr - string_buf >= MAX_STR_CONST - 1) 
    return length_err(); 
}

<STRING>\\t { 
  *string_buf_ptr++ = '\t';
  if (string_buf_ptr - string_buf >= MAX_STR_CONST - 1) 
    return length_err(); 
}

<STRING>\\n { 
  *string_buf_ptr++ = '\n';
  if (string_buf_ptr - string_buf >= MAX_STR_CONST - 1) 
    return length_err(); 
}

<STRING>\\f { 
  *string_buf_ptr++ = '\f';
  if (string_buf_ptr - string_buf >= MAX_STR_CONST - 1) 
    return length_err(); 
}
 /* If there is a null character in the string, return ERROR */
<STRING>(\0|\\\0) {
  yylval.error_msg = "String contains null character.";
  err_state = true;
  return ERROR; 
}
 /* Match the character starts with a '\' */
<STRING>\\[^btnf] { 
  if (string_buf_ptr - string_buf >= MAX_STR_CONST - 2) return length_err(); 
  *string_buf_ptr++ = *(yytext + 1); 
}
 /* If there is a non-escaped newline character in the string, return ERROR */
<STRING>\n {
  curr_lineno++;
  yylval.error_msg = "Unterminated string constant";
  if (err_state) err_state = false;
  BEGIN(INITIAL);
  return ERROR;
}
 
<STRING><<EOF>> {
  yylval.error_msg = "EOF in string constant";
  BEGIN(INITIAL);
  return ERROR;
}
 /* Any other character in the string is included */
<STRING>. {
  *string_buf_ptr++ = *yytext;
  if (string_buf_ptr - string_buf >= MAX_STR_CONST - 1) 
    return length_err(); 
}

 /* Eat the white spaces */
[ \f\r\t\v]+ { }

 /* Match the keywords */
{class}     { return CLASS;    }
{else}      { return ELSE;     }
{fi}        { return FI;       }
{if}        { return IF;       }
{in}        { return IN;       }
{inherits}  { return INHERITS; }
{let}       { return LET;      }
{loop}      { return LOOP;     }
{pool}      { return POOL;     }
{then}      { return THEN;     }
{while}     { return WHILE;    }
{case}      { return CASE;     }
{esac}      { return ESAC;     }
{of}        { return OF;       }
{new}       { return NEW;      }
{isvoid}    { return ISVOID;   }
{not}       { return NOT;      }
{true}      { yylval.boolean = 1; return BOOL_CONST; }
{false}     { yylval.boolean = 0; return BOOL_CONST; }

 /* Match the identifiers */
{typeid} {
  yylval.symbol = idtable.add_string(yytext);
  return TYPEID;
}

{objid} {
  yylval.symbol = idtable.add_string(yytext);
  return OBJECTID;
}

 /* Match the integers */
{integer} {
  yylval.symbol = inttable.add_string(yytext);
  return INT_CONST;
}

 /* Match the syntactic symbols */
"}"    { return '}';    }
"{"    { return '{';    }
"("    { return '(';    }
")"    { return ')';    }
":"    { return ':';    }
";"    { return ';';    }
"*"    { return '*';    }
"+"    { return '+';    }
"-"    { return '-';    }
"/"    { return '/';    }
"~"    { return '~';    }
"<"    { return '<';    }
"="    { return '=';    }
"@"    { return '@';    }
"."    { return '.';    }
","    { return ',';    }
"<-"   { return ASSIGN; }
"<="   { return LE;     }
"=>"   { return DARROW; }

 /* Unexpected symbols are errors */
. {
  yylval.error_msg = yytext;
  return ERROR;
}
%%

 /* Helper function for returning the length error */
int length_err() {
  yylval.error_msg = "String constant too long";
  BEGIN(INITIAL);
  return ERROR;
}
