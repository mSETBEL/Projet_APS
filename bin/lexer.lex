/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright (C) 2000 Gerwin Klein <lsf@jflex.de>                          *
 * All rights reserved.                                                    *
 *                                                                         *
 * Thanks to Larry Bell and Bob Jamison for suggestions and comments.      *
 *                                                                         *
 * License: BSD                                                            *
 *                                                                         *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* ========================================================================== */
/* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == */
/* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == */
/* == Analyse des programmes et sémantiques                                == */
/* ========================================================================== */
/* == hello-APS Syntaxe JAVA                                               == */
/* == Fichier: lexer.lex                                                   == */
/* ==  Lexique                                                             == */
/* ========================================================================== */

%%

%byaccj

%{
  private Parser yyparser;

  public Yylex(java.io.Reader r, Parser yyparser) {
    this(r);
    this.yyparser = yyparser;
  }
%}

nls  = \n | \r | \r\n
nums = -?[0-9]+
ident = [a-z][a-zA-Z0-9]*

%%

{nls}   { return 0; }
[ \t]+ { }

"["  { return Parser.LBRA; }
"]"  { return Parser.RBRA; }
"("  { return Parser.LPAR; }
")"  { return Parser.RPAR; }

"ECHO" { return Parser.ECHO; }

{nums}  { yyparser.yylval = new ParserVal(Integer.parseInt(yytext()));
         return Parser.NUM; }

{ident} { yyparser.yylval = new ParserVal(yytext());
  return Parser.IDENT;
}

\b     { System.err.println("Sorry, backspace doesn't work"); }

/* error fallback */
[^]    { System.err.println("Error: unexpected character '"+yytext()+"'"); return -1; }
