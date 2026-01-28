/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright (C) 2001 Gerwin Klein <lsf@jflex.de>                          *
 * All rights reserved.                                                    *
 *                                                                         *
 * This is a modified version of the example from                          *
 *   http://www.lincom-asg.com/~rjamison/byacc/                            *
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
/* == Fichier: parser.y                                                    == */
/* ==  Grammaire                                                           == */
/* ========================================================================== */

%{

  import java.io.*;
  import java.util.ArrayList;

%}
      
%token <ival> NUM            
%token <sval> IDENT
%token ECHO
%token LPAR RPAR             
%token LBRA RBRA

%type <obj> expr
%type <obj> exprs
%type <obj> stat
%type <obj> cmds
%type <obj> prog

%start prog      
%%


prog:  LBRA cmds RBRA   { $$ = new AstProg((ArrayList<Ast>)$2); }
;

cmds:
stat                    { ArrayList<Ast>r = new ArrayList<Ast>();
                          r.add((Ast)$1);
                          $$ = r;
                        }
;

stat:
ECHO expr               { $$ = new AstEcho((Ast)$2); }
;

expr:
  NUM                   { $$ = new AstNum($1); }
| IDENT                 { $$ = new AstId($1); }
| LPAR expr exprs RPAR  { $$ = new AstApp((Ast)$2,(ArrayList<Ast>)$3); }
;

exprs:
  expr                  { ArrayList<Ast> r = new ArrayList<Ast>();
                          r.add((Ast)$1);
			  $$ = r; }
| exprs expr            { ((ArrayList<Ast>)$1).add((Ast)$2); $$ = $1; }
;
%%

  public Ast prog;
  
  private Yylex lexer;


  private int yylex () {
    int yyl_return = -1;
    try {
      yylval = new ParserVal(0);
      yyl_return = lexer.yylex();
    }
    catch (IOException e) {
      System.err.println("IO error :"+e);
    }
    return yyl_return;
  }


  public void yyerror (String error) {
    System.err.println ("Error: " + error);
  }


  public Parser(Reader r) {
    lexer = new Yylex(r, this);
  }
