/* task 1.1 */
wff(p(N)) :- integer(N).
wff(neg(X)) :- wff(X).
wff(impl(X,Y)) :- wff(X) , wff(Y).
wff(and(X,Y)) :- wff(X) , wff(Y).
wff(or(X,Y)) :- wff(X) , wff(Y).
wff(equiv(X,Y)) :- wff(X) , wff(Y).
wff(xor(X,Y)) :- wff(X) , wff(Y).

/* task 1.2 */
/* strange behaviour when empty list due to negation */
satisfies(V,p(N)) :- member(V,N).
/* negation \+ ruins backtracking */
satisfies(V,neg(X)) :- \+satisfies(V,X).

satisfies(V,impl(X,Y)) :- satisfies(V,X) , satisfies(V,Y), !. 
satisfies(V,impl(X,_)) :- \+satisfies(V,X), !. 

satisfies(V,and(X,Y)) :- satisfies(V,X) , satisfies(V,Y). 

satisfies(V,or(X,_)) :- satisfies(V,X), !.
satisfies(V,or(_,Y)) :- satisfies(V,Y), !.  

satisfies(V,equiv(X,Y)) :- satisfies(V,X) , satisfies(V,Y) , !.
satisfies(V,equiv(X,Y)) :- \+satisfies(V,X) , \+satisfies(V,Y) , !.

satisfies(V,xor(X,Y)) :- satisfies(V,X) , \+satisfies(V,Y) , !.
satisfies(V,xor(X,Y)) :- \+satisfies(V,X) , satisfies(V,Y) , !.

/** task 1.3
* relevant valuations is dependent on the amount of p(N) in the formula. no need to have 6 in V if no p(6) exists. */
find_val_tt(F,V) :- get_v_list(F,Vlist) , sort(Vlist,Vclean) , subset(Vclean,V) ,   satisfies(V,F). 
/* look through F and give a list of all N's from p(N) in the list unsorted and with duplicates */
get_v_list(p(X),V) :- append([],X,V).
get_v_list(neg(X),V) :- get_v_list(X,V).
get_v_list(impl(X,Y),V) :- get_v_list(X,V1), get_v_list(Y,V2), concatenate(V1,V2,V).
get_v_list(and(X,Y),V) :- get_v_list(X,V1), get_v_list(Y,V2), concatenate(V1,V2,V).
get_v_list(or(X,Y),V) :- get_v_list(X,V1), get_v_list(Y,V2), concatenate(V1,V2,V).
get_v_list(equiv(X,Y),V) :- get_v_list(X,V1), get_v_list(Y,V2), concatenate(V1,V2,V).
get_v_list(xor(X,Y),V) :- get_v_list(X,V1), get_v_list(Y,V2), concatenate(V1,V2,V).

/** task 1.4 */
unsat_tt(F) :- \+find_val_tt(F,_).
sat_tt(F) :- \+unsat_tt(F) , get_v_list(F,Vlist) , sort(Vlist,Vclean) , subset(Vclean,V) , findall(X,find_val_tt(F,X),Z) , nonmember(V,Z).
taut_tt(F) :- \+unsat_tt(F) , \+sat_tt(F).
/* stuff that didnt work
taut_tt1(F,V) :- get_v_list(F,Vlist) , sort(Vlist,Vclean) , subset(Vclean,V).
taut_tt2(F,Z) :- findall(X,find_val_tt(F,X),Z).
taut_tt(F) :- get_v_list(F,Vlist) , sort(Vlist,Vclean) , subset(Vclean,V) , findall(X,find_val_tt(F,X),Z) , member(V,Z).
taut_tt_check(V,Z) :- nonmember(V,Z). */

/** task 2.5   */
tableau(p(N),p(N)).
tableau(neg(p(N)),neg(p(N))).

tableau(neg(neg(X)),L) :- tableau(X,L).

tableau(impl(X,_),L) :- tableau(neg(X),L).
tableau(impl(_,X),L) :- tableau(X,L).

tableau(neg(impl(X,Y)),V) :- tableau(X,X1) , tableau(neg(Y),Y1) , flatten([X1,Y1],V).

tableau(and(X,Y),V) :- tableau(X,X1) , tableau(Y,Y1)  , flatten([X1,Y1],V).

tableau(neg(and(X,_Y)),X1) :- tableau(neg(X),X1).
tableau(neg(and(_X,Y)),Y1) :- tableau(neg(Y),Y1).

tableau(or(X,_Y),X1) :- tableau(X,X1).
tableau(or(_X,Y),Y1) :- tableau(Y,Y1).

tableau(neg(or(X,Y)),V) :- tableau(neg(X),X1) , tableau(neg(Y),Y1) , flatten([X1,Y1],V).

tableau(equiv(X,Y),V) :- tableau(X,X1) , tableau(Y,Y1) , flatten([X1,Y1],V).
tableau(equiv(X,Y),V) :- tableau(neg(X),X1) , tableau(neg(Y),Y1) , flatten([X1,Y1],V).

tableau(neg(equiv(X,Y)),V) :- tableau(X,X1) , tableau(neg(Y),Y1) , flatten([X1,Y1],V).
tableau(neg(equiv(X,Y)),V) :- tableau(neg(X),X1) , tableau(Y,Y1) , flatten([X1,Y1],V).

tableau(xor(X,Y),V) :- tableau(X,X1) , tableau(neg(Y),Y1) , flatten([X1,Y1],V).
tableau(xor(X,Y),V) :- tableau(neg(X),X1) , tableau(Y,Y1) , flatten([X1,Y1],V).

tableau(neg(xor(X,Y)),V)  :- tableau(X,X1) , tableau(Y,Y1) , flatten([X1,Y1],V).
tableau(neg(xor(X,Y)),V)  :- tableau(neg(X),X1) , tableau(neg(Y),Y1) , flatten([X1,Y1],V).

/** task 2.6  */
/*make two lists, one for neg, and one for normal. If one exists in both, fail*/
find_val_tab(F,Vclean) :- tableau(F,Leaf) , leaf_to_list(Leaf,Listp), leaf_to_list_neg(Leaf,Listn) , no_match(Listp,Listn) , sort(Listp,Vclean).
/* make a leaf into a list with the p(N)'s N value(s) */
leaf_to_list(p(N),V) :- append([],N,V). 
leaf_to_list(neg(p(_N)),[]).
leaf_to_list([],[]).
leaf_to_list([p(N)|L],V) :- append([],N,List1) , leaf_to_list(L,List2) , concatenate(List1,List2,V).
leaf_to_list([neg(p(_N))|L],V) :- leaf_to_list(L,V).

leaf_to_list_neg(neg(p(N)),V) :- append([],N,V).
leaf_to_list_neg(p(_N),[]). 
leaf_to_list_neg([],[]).
leaf_to_list_neg([neg(p(N))|L],V) :- append([],N,List1) , leaf_to_list_neg(L,List2) , concatenate(List1,List2,V).
leaf_to_list_neg([p(_N)|L],V) :- leaf_to_list_neg(L,V).

/** task 2.7 */

taut_tab(F) :- unsat_tab(neg(F)).
unsat_tab(F) :- \+find_val_tab(F,_).
sat_tab(F) :- \+taut_tab(F) , \+unsat_tab(F).

/**lists , some functions defined by default in swi-prolog.*/
/* length already defined by default */
/* sort defined by default */
/* findall defined by default */
append([],Y,[Y]).
append([X|L],Y,[X|W]) :- append(L,Y,W).
member([X|_L],X).
member([_X|L],Y) :- member(L,Y).
remove([],_Y,[]).
remove([X|L],X,L).
remove([_|L],Y,W) :- remove(L,Y,W).
concatenate([],L,L).
concatenate([X|L],W,[X|Z]) :- concatenate(L,W,Z).
subset([], []).
subset([X|L], [X|Lnew]):- subset(L, Lnew).
subset([_|L], Lnew):- subset(L, Lnew).
nonmember(E,[E|_]) :- ! , fail.
nonmember(E,[_|L]) :- ! , nonmember(E,L).
nonmember(_,[]).
flatten(L,F) :- flatten(L,[],F1), !, F = F1.
flatten(X,L,[X|L]) :- var(X), !.
flatten([],L,L) :- !.
flatten([X|L],T,List) :- !, flatten(X,L1,List), flatten(L,T,L1).
flatten(X,L,[X|L]).
no_match([],_).
no_match([X|L], List2) :- nonmember(X,List2) , no_match(L,List2).
