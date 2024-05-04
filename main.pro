implement main
    open core

domains
    node = r(string, stt).
    path = node*.
    queue = path*.
    piece = e; b; w.
    stt = st(piece, piece, piece, piece, piece).

class facts
    start : stt := st(w, w, e, b, b).
    finish : stt := st(w, b, b, w, e).

class predicates
    searchWidth : (queue, path [out], integer) determ.
    searchDepth : (queue, path [out], integer) determ.
    nextLevel : (path, path [out]) nondeterm.
    solved : (path) determ.
    prtSolution : (path).
    operator : (string [out], stt, stt [out]) nondeterm.

clauses
    solved(L) :-
        L = [r(_, finish) | _].
    prtSolution(L) :-
        foreach P = list::getMember_nd(list::reverse(L)) do
            stdio::write(P),
            stdio::nl
        end foreach.

    operator("slide", st(e, A, B, C, D), st(A, e, B, C, D)).
    operator("jump", st(e, A, B, C, D), st(B, A, e, C, D)).
    operator("slide", st(A, e, B, C, D), st(A, B, e, C, D)).
    operator("slide", st(A, e, B, C, D), st(e, A, B, C, D)).
    operator("jump", st(A, e, B, C, D), st(A, C, B, e, D)).
    operator("slide", st(A, B, e, C, D), st(A, e, B, C, D)).
    operator("slide", st(A, B, e, C, D), st(A, B, C, e, D)).
    operator("jump", st(A, B, e, C, D), st(e, B, A, C, D)).
    operator("jump", st(A, B, e, C, D), st(A, B, D, C, e)).
    operator("slide", st(A, B, C, e, D), st(A, B, e, C, D)).
    operator("jump", st(A, B, C, e, D), st(A, e, C, B, D)).
    operator("slide", st(A, B, C, e, D), st(A, B, C, D, e)).
    operator("slide", st(A, B, C, D, e), st(A, B, C, e, D)).
    operator("jump", st(A, B, C, D, e), st(A, B, e, D, C)).

    searchWidth([T | Queue], Solution, Step) :-
        if solved(T) then
            Solution = T,
            stdio::write("1. Кількість кроків для розв'язку головоломки вшир: ", Step, "."),
            stdio::nl
        else
            Step2 = Step + 1,
            Extentions = [ Daughter || nextLevel(T, Daughter) ],
            ExtendedQueue = list::append(Queue, Extentions),
            searchWidth(ExtendedQueue, Solution, Step2)
        end if.

    searchDepth([T | Queue], Solution, Step) :-
        if solved(T) then
            Solution = T,
            stdio::write("2. Кількість кроків для розв'язку головоломки вглиб: ", Step, "."),
            stdio::nl
        else
            Step2 = Step + 1,
            Extentions = [ Daughter || nextLevel(T, Daughter) ],
            ExtendedQueue = list::append(Extentions, Queue),
            searchDepth(ExtendedQueue, Solution, Step2)
        end if.

    nextLevel([r(Branch, N) | Path], [r(Op, Daughter), r(Branch, N) | Path]) :-
        operator(Op, N, Daughter),
        not(list::isMember(r(Op, Daughter), Path)).

    run() :-
        console::init(),
        if searchWidth([[r("0", start)]], L, 0) then
            prtSolution(L)
        else
            stdio::write("Помилка! За заданими параметрами нема рішення."),
            stdio::nl
        end if,
        if searchDepth([[r("0", start)]], L2, 0) then
            prtSolution(L2)
        else
            stdio::write("Помилка! За заданими параметрами нема рішення."),
            stdio::nl
        end if.

end implement main

goal
    console::runUtf8(main::run).
