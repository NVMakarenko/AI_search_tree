%Файл main.pro ()

implement main

domains
    node = r(integer, string).
    path = node*.
    queue = path*.

class facts
    operator : (integer, string, string).
    start : string := "b".
    finish : string := "k".

class predicates
    searchWidth : (queue, path [out], integer) determ.
    searchDepth : (queue, path [out], integer) determ.
    nextLevel : (path, path [out]) nondeterm.
    solved : (path) determ.
    prtSolution : (path).

clauses
    solved(L) :-
        L = [r(_, finish) | _].
    prtSolution(L) :-
        foreach P = list::getMember_nd(list::reverse(L)) do
            stdio::write(P),
            stdio::nl
        end foreach.

    operator(1, "b", "c").
    operator(2, "b", "d").
    operator(3, "b", "e").
    operator(4, "c", "f").
    operator(5, "c", "g").
    operator(6, "c", "h").
    operator(7, "e", "i").
    operator(8, "e", "j").
    operator(9, "g", "k").
    operator(10, "g", "m").

    searchWidth([T | Queue], Solution, Step) :-
        if solved(T) then
            Solution = T,
            stdio::write("1. Кількість кроків для пошуку вшир на графі станів: ", Step, "."),
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
            stdio::write("2. Кількість кроків для пошуку вглиб на графі станів: ", Step, "."),
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
        if searchWidth([[r(0, start)]], L, 0) then
            stdio::write("Пошук вшир на графі станів від '", start, "' до '", finish, "' наступний:"),
            stdio::nl,
            prtSolution(L)
        else
            stdio::write("Помилка! Відповідного маршруту не знайдено"),
            stdio::nl
        end if,
        stdio::nl,
        if searchDepth([[r(0, start)]], L2, 0) then
            stdio::write("Пошук вглиб на графі станів від '", start, "' до '", finish, "' наступний:"),
            stdio::nl,
            prtSolution(L2)
        else
            stdio::write("Помилка! Відповідного маршруту не знайдено"),
            stdio::nl
        end if.

end implement main

goal
    console::runUtf8(main::run).
