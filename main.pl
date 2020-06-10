:- use_module("tables.pl").
:- use_module("check_predicates.pl").

%afisare tabel

%tanspose
trans([], []).
trans([X|Xs], Ys) :- trans_check(X, [X|Xs], Ys).
trans_check([], _, []).
trans_check([_|Rs], Ms, [Ts|Tss]) :- trans_helper(Ms, Ts, R1), trans_check(Rs, R1, Tss).
trans_helper([], [], []).
trans_helper([[X | Ys] | Rest], [X | Xs], [Ys | Yssp]) :- trans_helper(Rest, Xs, Yssp).


%string2listaCuLungime
% [String] -> [Int]
string2lista([], []).   
string2lista([H | T], [MaxPeH | Rez]) :- string2lista(T, Rez), string_length(H, MaxPeH).

% [[String]] -> [[Int]]
matrice2lista([], []).
matrice2lista([H | T], [MaxPeH | Rez]) :- matrice2lista(T, Rez), string2lista(H, MaxPeH).

%lista cu maxime
%listaCuMaxime :: [[String]] -> [Integer]
listaCuMaxime([], []).
listaCuMaxime([H | T], [MaxPeH | Rez]) :- listaCuMaxime(T, Rez), max_list(H, MaxPeH).

%print_table
print_table_op(IntrariTabel) :- matrice2lista(IntrariTabel, Rezultat),
                            trans(Rezultat, IntrariTabelP),
                            listaCuMaxime(IntrariTabelP, ListaFormat), 
                            make_format_string(ListaFormat, StringFormat),
                            printTabel(IntrariTabel, StringFormat).

printTabel([], _).
printTabel([H | T], StringFormat) :- format(StringFormat, H), printTabel(T, StringFormat).

%join
join_op(Op, Coloane, [_ | Tabel1], [_ | Tabel2], R) :- maplist(Op, Tabel1, Tabel2, Rezultat), append([Coloane], Rezultat, R).

%select
indexOf([String|_], String, 0) :- !.
indexOf([_|Tail], String, Index):- indexOf(Tail, String, Indexp), Index is Indexp+1. 

headerTabel(Intrari, Header) :- nth0(0, Intrari, Header).
tailTabel([_ | R], R). 

indecsi([], _, []).
indecsi([X | Xs], HeaderTabel, [Index | R]) :- indecsi(Xs, HeaderTabel, R), indexOf(HeaderTabel, X, Index).

takeIndex([], _, []) :- !.
takeIndex([X | Xs], Intrari, [Rez | R]) :- takeIndex(Xs, Intrari, R), nth0(X, Intrari, Rez).

select_op(Tabel, Coloane, [Coloane | R]) :- headerTabel(Tabel, Header),
                                tailTabel(Tabel, IntrariFaraHeader),
                                indecsi(Coloane, Header, Indecsi),
                                trans(IntrariFaraHeader, Col),
                                takeIndex(Indecsi, Col, Rp),
                                trans(Rp, R).

%filter
csp([], _, _, []) :- !.
csp([Header | Intrari], Variabile, Predicat, R) :- not((Variabile = Header, Predicat)), csp(Intrari, Variabile, Predicat, R), !.
csp([Rez | Intrari], Variabile, Predicat, [Rez | R]) :- csp(Intrari, Variabile, Predicat, R).
filter_op([H | Tbl], Vars, Predicat, R) :- csp(Tbl, Vars, Predicat, Tail), append([H], Tail, R).

%eval
eval(table(Str), R) :- table_name(Str, R).
eval(tprint(Tabel), _) :- eval(Tabel, T), print_table_op(T).
eval(select(Coloane, Tabel), R) :- eval(Tabel, Rp), select_op(Rp, Coloane, R).
eval(join(Pred, Cols, Q1, Q2), R) :- eval(Q1, Tabel1), eval(Q2, Tabel2), join_op(Pred, Cols, Tabel1, Tabel2, R).
eval(tfilter(Schema, Goal, Nume), R) :- eval(Nume, Tabel), filter_op(Tabel, Schema, Goal, R).

%complex Q
eval(complex_query1(Nume), R) :- eval(Nume, Tabel), 
                                filter_op(Tabel, [_,_,AA,PP,_,_,_], ((AA + PP) / 2 > 6), R1),
                                filter_op(R1, [_,_,AA,PP,PC,PA,POO], ((AA + PP + PC + PA + POO) / 5 > 5), R2),
                                filter_op(R2, [_,L,_,_,_,_,_], (sub_string(L, _, _, _, "escu")), R).

%anexa
plus5(X,Y):- Y is X + 5.

make_format_string(MaxRowLen,Str) :- maplist(plus5,MaxRowLen,Rp), aux_format(Rp,Str).

aux_format([H],R) :- string_concat("~t~w~t~",H,R1),
                    string_concat(R1,"+~n",R),!.

aux_format([H|T],R) :- string_concat("~t~w~t~",H,R1),
                    string_concat(R1,"+ ",R2),
                    aux_format(T,Rp),
                    string_concat(R2,Rp,R).
%anexa end
