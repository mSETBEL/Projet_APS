
main :- read(user_input, X), type_check(X).

type_check(prog(P)) :- type_prog(prog(P),_), write("OK\n").
type_check(_) :- write("KO\n").

type_prog(prog(BK),T) :- ctx_init(G), type_block(G,BK, T). %prog

ctx_init(
	[
		(true,bool),
		(false,bool),
		(not,arrow([bool],bool)),
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


%ref ou pas ref
a([], []).

a([(var(X),T)|Ls], [arg(X,ref(T))|ARGS]) :-
    a(Ls, ARGS).

a([arg(X,T)|Ls], [arg(X,T)|ARGS]) :-
    a(Ls, ARGS).



%Blocks
type_block(G, block(CS), T) :- type_cmds(G, CS, T).

%Suite de commandes
type_cmds(G, end(S), T) :- type_stat(G, S, T). %end
type_cmds(G, stat(S,CS), T) :- type_stat(G, S, void), type_cmds(G,CS,T). %stat0
type_cmds(G, stat(S,CS), T) :- T\=void, type_stat(G, S, union(T,void)), type_cmds(G,CS,T). %stat1
type_cmds(G,dec(D,CS),T) :- type_def(G,D,G1), type_cmds(G1,CS,T). %defs
type_cmds(G, ret(R), T) :- type_expr(G,R,T). %ret

%Définitions
type_def(G,const(X,T,E),[(X,T)|G]) :- type_expr(G,E,T). %const
type_def(G,fun(X,T,ARGS,BK),[(X,arrow(TARGS,T))|G]) :- add_args_ctx(G,ARGS,G1), build_types(ARGS,TARGS), type_block(G1,BK,T). %fun
type_def(G,funrec(X,T,ARGS,BK),[(X,arrow(TARGS,T))|G]) :- build_types(ARGS,TARGS), add_args_ctx(G,[arg(X,arrow(TARGS,T))],G1), add_args_ctx(G1,ARGS,G2), type_block(G2,BK,T). %fun_rec
type_def(G, var(X,T),[(X,ref(T))|G]). %var

type_def(G, proc(X,PARGS,BK),[(X,arrow(TARGS,void))|G]) :- a(PARGS,ARGS), build_types(ARGS,TARGS), add_args_ctx(G,ARGS,G1), type_block(G1,BK,void). %proc
type_def(G, procrec(X,PARGS,BK),[(X,arrow(TARGS,void))|G]) :- a(PARGS,ARGS), build_types(ARGS,TARGS), add_args_ctx(G,[arg(X,arrow(TARGS,void))],G1), add_args_ctx(G1,ARGS,G2), type_block(G2,BK,void). %proc_rec

%instructions
type_stat(G, echo(E), void) :- type_expr(G, E, int). %echo
type_stat(G, set(LV,E), void) :- type_expr(G, LV, T), type_expr(G, E, T) . %set
type_stat(G, ifS(E,BK1,BK2), T) :- type_expr(G,E,bool), type_block(G,BK1,T), type_block(G,BK2,T). %ifS0
type_stat(G, ifS(E,BK1,BK2), union(T,void)) :- T\=void, type_expr(G,E,bool), type_block(G,BK1,void), type_block(G,BK2,T). %ifS1
type_stat(G, ifS(E,BK1,BK2), union(T,void)) :- T\=void, type_expr(G,E,bool), type_block(G,BK1,T), type_block(G,BK2,void). %ifS2
type_stat(G, while(E,BK), void) :- type_expr(G,E,bool), type_block(G,BK,void). %whileVoid cas void+void=void (on simplifie ici)
type_stat(G, while(E,BK), union(T,void)) :- type_expr(G,E,bool), type_block(G,BK,T). %while
type_stat(G, call(X,ES), void) :- type_expars(G,ES,TS), find(G,X,arrow(TS,void)). %call

%expression
type_expr(_,num(_),int). %num
type_expr(G,ident(X),T) :-  find(G,X,T). %idv
type_expr(G,ident(X),T) :-  find(G,X,ref(T)). %idvr
type_expr(G, if(E1, E2, E3), T) :- type_expr(G,E1, bool), type_expr(G,E2,T), type_expr(G,E3,T). %if
type_expr(G, and(E1, E2), bool) :- type_expr(G,E1,bool), type_expr(G,E2,bool). %and
type_expr(G, or(E1, E2), bool) :- type_expr(G,E1,bool), type_expr(G,E2,bool). %or
type_expr(G, app(E,ENS), T) :-  type_expr(G,E,arrow(TENS,T)), type_exprs(G,ENS,TENS). %app
type_expr(G, abs(ARGS,E), arrow(TARGS,T)) :- add_args_ctx(G,ARGS,G1), build_types(ARGS,TARGS), type_expr(G1,E,T). %abs
type_expr(G, alloc(E), vec(_)) :- type_expr(G, E, int). %alloc
type_expr(G, nth(E1, E2), T) :- type_expr(G, E1, vec(T)), type_expr(G, E2, int). %nth
type_expr(G, len(E), int) :- type_expr(G,E, vec(_)). %len
type_expr(G, vset(E1, E2, E3), vec(T)) :- type_expr(G, E1, vec(T)), type_expr(G, E2, int), type_expr(G, E3, T). %vset


type_exprs(_,[],[]).
type_exprs(G,[E|ES],[T|TS]) :- type_expr(G,E,T), type_exprs(G,ES,TS).


%expar
type_expar(G, E, T) :- type_expr(G, E, T).
type_expar(G, adr(X), ref(T)) :- find(G, X, ref(T)).

type_expars(_,[],[]).
type_expars(G,[E|ES],[T|TS]) :- type_expar(G,E,T), type_expars(G,ES,TS).


add_args_ctx(G,[],G).
add_args_ctx(G,[arg(X,types(TS,T))|ARGS],G1) :- add_args_ctx([(X,arrow(TS,T))|G],ARGS,G1).
add_args_ctx(G,[arg(X,T)|ARGS],G1) :- add_args_ctx([(X,T)|G],ARGS,G1).


build_types([], []).

build_types([arg(_,T)|ARGS], [T|TARGS]) :-
    build_types(ARGS, TARGS).