implement main /*heureka*/

domains
    item = r(string, string).
    it = item*.
    node = t(real, real, it).
    tree = node*.

class facts
    xy : (string, integer, integer).
    op : (string, string).
    start : string := "a".
    finish : string := "s".

class predicates
    getXY : (string, integer [out], integer [out]) determ.
    cost : (string, string) -> real.
    hn : (string) -> real.
    not_in_circle : (string, it) determ.
    theGoal : (string [out]).
    toDaughter : (node, node [out]) nondeterm.
    init : (string [out]).
    goalReached : (node) determ.
    search : (tree, node [out]) determ.
    prtSolution : (node).
    solve : ().
    cmp : (node, node) -> compareResult.

clauses
    cmp(t(A, _, _), t(B, _, _)) = greater() :-
        A > B,
        !.
    cmp(t(A, _, _), t(B, _, _)) = equal() :-
        A = B,
        !.
    cmp(_, _) = less().
    op("a", "b").
    op("b", "m").
    op("m", "f").
    op("f", "q").
    op("q", "p").
    op("p", "n").
    op("p", "s").
    op("b", "c").
    op("c", "d").
    op("d", "q").
    op("d", "n").
    op("d", "g").
    op("n", "h").
    op("n", "s").
    op("h", "g").
    init(start).

    goalReached(t(_, _, [r(_, M) | _])) :-
        theGoal(R),
        R = M.
    theGoal(finish).
    not_in_circle(Stt, Path) :-
        not(list::isMember(r("", Stt), Path)).
    xy("a", 2, 4).
    xy("b", 5, 6).
    xy("c", 4, 2).
    xy("d", 7, 4).
    xy("f", 7, 8).
    xy("g", 8, 2).
    xy("h", 10, 1).
    xy("m", 9, 6).
    xy("n", 11, 3).
    xy("p", 12, 6).
    xy("q", 11, 7).
    xy("s", 13, 2).
    getXY(M, X, Y) :-
        xy(M, X, Y),
        !.
    cost(No, NoFilho) = C :-
        if getXY(No, XN, YN) and getXY(NoFilho, XF, YF) then
            C = math::sqrt((XN - XF) * (XN - XF) + (YN - YF) * (YN - YF))
        else
            C = 0.0
        end if.
    hn(N) = HN :-
        theGoal(S),
        if getXY(S, XS, YS) and getXY(N, XN, YN) then
            HN = math::sqrt((XN - XS) * (XN - XS) + (YN - YS) * (YN - YS))
        else
            HN = 0.0
        end if.
    search([T | Queue], S) :-
        if goalReached(T) then
            S = T
        else
            Extension = [ E || toDaughter(T, E) ],
            NewQueue = list::append(Queue, Extension),
            BestFirst = list::sortBy(cmp, NewQueue),
            search(BestFirst, S)
        end if.
    toDaughter(t(_F, G, [r(B, N) | Path]), t(F1, G1, [r(Op, Child), r(B, N) | Path])) :-
        op(N, Child),
        Op = string::format("%s to %s", N, Child),
        not_in_circle(Child, Path),
        G1 = G + cost(N, Child),
        F1 = G1 + hn(Child).

    prtSolution(t(_, _, T)) :-
        foreach X = list::getMember_nd(list::reverse(T)) do
            stdio::write(X),
            stdio::nl
        end foreach.
    solve() :-
        if init(E) and search([t(hn(E), 0, [r("root", E)])], S) then
            prtSolution(S)
        else
            stdio::write("Помилка! Немає рішень.")
        end if.

    run() :-
        console::init(),
        solve().

end implement main

goal
    console::runUtf8(main::run).
