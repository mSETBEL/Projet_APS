
Prog :- [Cmds]


Cmds :- Stat
Cmds :- Def ; Cmds


Def :- CONST ident Type Expr
Def :- FUN ident Type [ Args ] Expr
Def :- FUN REC ident Type [ Args ] Expr


Type :- bool | int
Type :- ( Types -> Type )


Types :- Type
Types :- Type * Types


Args :- Arg
Args :- Arg , Args


Arg :- ident : Type


Stat :- ECHO Expr


Expr :- num
Expr :- ident
Expr :- (if Expr Expr Expr )
Expr :- ( and Expr Expr )
Expr :- ( or Expr Expr )
Expr :- ( Expr Exprs )
Expr :- [ Args ] Expr


Exprs :- Expr
Exprs :- Expr Exprs