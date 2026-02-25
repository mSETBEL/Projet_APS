
main :- read(user_input, X), type_check(X).

type_check(prog(P)) :- type_prog(prog(P),void), write("OK\n").
type_check(_) :- write("KO\n").

type_prog(prog(BK),void) :- ctx_init(G), type_block(G,BK, void). %prog

ctx_init(
	[
		(true,bool),
		(false,bool),
		(not,arrow((bool),(bool))),
		(eq,arrow([int,int],bool)),
		(lt, arrow([int,int],bool)),
		(add, arrow([int,int],int)),
		(sub, arrow([int,int],int)),
		(mul, arrow([int,int],int)),
		(div, arrow([int,int],int))
	]).

arrow(_,_).

find([(X,T)|_], X, T).
find([(_,_)|Ls],X,T) :- find(Ls,X,T).


%Blocks
type_block(G, block(CS), void) :- type_cmds(G, CS, void).

%Suite de commandes
type_cmds(G, end(S), void) :- type_stat(G, S, void). %end
type_cmds(G, stat(S,CS), void) :- type_stat(G, S, void), type_cmds(G,CS,void). %stats
type_cmds(G,dec(D,CS),void) :- type_def(G,D,G1), type_cmds(G1,CS,void). %defs

%Définitions
type_def(G,const(X,T,E),[(X,T)|G]) :- type_expr(G,E,T). %const
type_def(G,fun(X,T,ARGS,E),[(X,arrow(TARGS,T))|G]) :- add_args_ctx(G,ARGS,G1), build_types(ARGS,TARGS), type_expr(G1,E,T). %fun
type_def(G,funrec(X,T,ARGS,E),[(X,arrow(TARGS,T))|G]) :- build_types(ARGS,TARGS), add_args_ctx(G,[arg(X,arrow(TARGS,T))],G1), add_args_ctx(G1,ARGS,G2), type_expr(G2,E,T). %fun_rec
type_def(G, var(X,int),[(X,int)|G]). %var
type_def(G, var(X,bool),[(X,bool)|G]).
type_def(G, proc(X,ARGS,BK),[(X,arrow(TARGS,void))|G]) :- build_types(ARGS,TARGS), add_args_ctx(G,ARGS,G1), type_block(G1,BK,void). %proc
type_def(G, procrec(X,ARGS,BK),[(X,arrow(TARGS,void))|G]) :- build_types(ARGS,TARGS), add_args_ctx(G,[arg(X,arrow(TARGS,void))],G1), add_args_ctx(G1,ARGS,G2), type_block(G2,BK,void). %proc_rec

%instructions
type_stat(G, echo(E), void) :- type_expr(G, E, int). %echo
type_stat(G, set(X,E), void) :- find(G,X,T), type_expr(G,E,T). %set
type_stat(G, ifS(E,BK1,BK2), void) :- type_expr(G,E,bool), type_block(G,BK1,void), type_block(G,BK2,void). %ifS
type_stat(G, while(E,BK), void) :- type_expr(G,E,bool), type_block(G,BK,void). %while
type_stat(G, call(X,ES), void) :- find(G,X,arrow(TS,void)), type_exprs(G,ES,TS). %call

%expression
type_expr(_,num(_),int). %num
type_expr(G,ident(X),T) :-  find(G,X,T). %id
type_expr(G, if(E1, E2, E3), T) :- type_expr(G,E1, bool), type_expr(G,E2,T), type_expr(G,E3,T). %if
type_expr(G, and(E1, E2), bool) :- type_expr(G,E1,bool), type_expr(G,E2,bool). %and
type_expr(G, or(E1, E2), bool) :- type_expr(G,E1,bool), type_expr(G,E2,bool). %or
type_expr(G, app(E,ENS), T) :-  type_expr(G,E,arrow(TENS,T)), type_exprs(G,ENS,TENS). %app
type_expr(G, abs(ARGS,E), arrow(TARGS,T)) :- add_args_ctx(G,ARGS,G1), build_types(ARGS,TARGS), type_expr(G1,E,T). %abs

type_exprs(_,[],[]).
type_exprs(G,[E|ES],[T|TS]) :- type_expr(G,E,T), type_exprs(G,ES,TS).


add_args_ctx(G,[],G).
add_args_ctx(G,[arg(X,types(TS,T))|ARGS],G1) :- add_args_ctx([(X,arrow(TS,T))|G],ARGS,G1).
add_args_ctx(G,[arg(X,T)|ARGS],G1) :- add_args_ctx([(X,T)|G],ARGS,G1).


build_types([],_).
build_types([arg(_,T)|ARGS], TARGS) :- build_types(ARGS,[T|TARGS]).
