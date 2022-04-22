% -*- mode: prolog; -*- select prolog mode in the emacs text editor

% Convenient shortcuts, especially during steps 1 to 6.

% Similar to "assert", but raise an non-fatal error.
check(Condition, _, _) :- call(Condition), !.
check(_, Format, Arguments) :- throwf(Format, Arguments).

throwf(Format, Arguments) :-
    format(string(Message), Format, Arguments),
    throw(mal_error(Message)).

% Convenient shortcut: unbox(+Sequence, -List).

unbox_seq(Sequence, Forms) :- list(Forms, Sequence).
unbox_seq(Sequence, Forms) :- vector(Forms, Sequence).

% Abstract some loops.

% foldr(Goal, Vn, [X1, X2,...,Xn], V0) :-
%     Goal(Xn, Vn, Vn-1),
%     ...
%     Goal(X2, V2, V1),
%     Goal(X1, V1, V0),
foldr(_,    Vn, [],     Vn).
foldr(Goal, Vn, [X|Xs], V0) :-
    foldr(Goal, Vn, Xs, V1),
    call(Goal, X, V1, V0).

% foldl_keyvals(Goal, Init, [K1, V1, K2, V2, K3, V3], Acc3) :-
%     Goal(Init, K1, V1, Acc1),
%     Goal(Acc1, K2, V2, Acc2),
%     Goal(Acc2, K3, V3, Acc3).
foldl_keyvals(_,    Init, [], Init).
foldl_keyvals(Goal, Init, [K, V | KVs], Res) :-
    call(Goal, Init, K, V, Acc),
    foldl_keyvals(Goal, Acc, KVs, Res).

% map_keyvals(Goal, [K1, V1, K2, V2, K3, V3]) :-
%     Goal(K1, V1),
%     Goal(K2, V2),
%     Goal(K3, V3).
map_keyvals(_, []).
map_keyvals(Goal, [K, V | KVs]) :-
    call(Goal, K, V),
    map_keyvals(Goal, KVs).
