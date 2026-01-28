
main :- read(user_input, X), type_check(X).

type_check(prog(P)) :- type_prog(prog(P),void), write("OK\n").
type_check(_) :- write("KO\n").

type_prog(prog(CS),void) :- ctx_init(G), type_cmds(G,CS, void). #prog

ctx_init(_,true,bool).
ctx_init(_,false,bool).
ctx_init(_,not,bool -> bool).
ctx_init(_,eq,int * int -> bool).

type_cmds(G, stat(S), void) :- type_expr(G, S, void). #end

type_expr(_,num(_),int). #num

type_expr(G,ident(X),T) :-  ctx_init(G,X,T). #id

type_expr(G, if(E1, E2, E3), t) :- type_expr(G,E1, bool), type_expr(G,E2,T), type_expr(G,E3,T). #if

type_expr(G, app(E,[EN]), T) :-  type_expr(e,t1), 

type_stat(G, echo(E), void) :- type_expr(G, E, int). #echo

