%Define valid moves
move([X,Y], [X1,Y1], Width, Height) :- moveRight([X,Y], [X1,Y1], Width, Height).
move([X,Y], [X1,Y1], Width, Height) :- moveLeft([X,Y], [X1,Y1], Width, Height).
move([X,Y], [X1,Y1], Width, Height) :- moveUp([X,Y], [X1,Y1], Width, Height).
move([X,Y], [X1,Y1], Width, Height) :- moveDown([X,Y], [X1,Y1], Width, Height).


moveRight([X,Y], [X1,Y1], Width, Height) :- X1 is X + 1, Y1 is Y, valid(X1,Y1, Width, Height).
moveLeft([X,Y], [X1,Y1], Width, Height) :- X1 is X - 1, Y1 is Y, valid(X1,Y1, Width, Height).
moveUp([X,Y], [X1,Y1], Width, Height) :- X1 is X, Y1 is Y + 1, valid(X1,Y1, Width, Height).
moveDown([X,Y], [X1,Y1], Width, Height) :- X1 is X, Y1 is Y - 1, valid(X1,Y1, Width, Height).

valid(X, Y, Width, Height) :- between(0, Width, X), between(0, Height, Y).

%Fill Board with coordinates
% base case: empty board
generate_coordinates(Array, Width, Height, Coordinates) :-
    generate_coordinates(Array, 0, 0, Width, Height, Coordinates).

generate_coordinates(_, _, Height, _, Height, []).

generate_coordinates(Array, Width, Row, Width, Height, Coordinates) :-
    NewRow is Row + 1,
    generate_coordinates(Array, 0, NewRow, Width, Height, Coordinates).

generate_coordinates(Array, Col, Row, Width, Height, [[Row, Col, Element] | Coordinates]) :-
    nth0(Row, Array, RowArray),
    nth0(Col, RowArray, Element),
    NewCol is Col + 1,
    generate_coordinates(Array, NewCol, Row, Width, Height, Coordinates).

% Main predicate for finding a cycle
findCyclePath(Width, Height,Board) :-
    generate_coordinates(Board, Height,Width,Initial),
    search([[Initial, null]], [], Path, Width, Height),
    printPath(Path).

% Predicates for searching
search(Open, Closed, Path, Width, Height) :-
    getState(Open, [CurrentNode,Parent], RestOpen),
    hasSquare(CurrentNode, Path, Width,Height).

search(Open, Closed, Path, Width, Height) :-
    getState(Open, CurrentNode, TmpOpen),
    getAllValidChildren(CurrentNode, TmpOpen, Closed, Children, Width, Height),
    addChildren(Children, TmpOpen, NewOpen),
    append(Closed, [CurrentNode], NewClosed),
    search(NewOpen, NewClosed, Path, Width, Height).

getAllValidChildren(Node, Open, Closed, Children, Width, Height) :-
    findall(Next, getNextState(Node, Open, Closed, Next, Width, Height), Children).

getNextState([State,Parent], Open, Closed, [Next,State], Width, Height) :-
    move(State, Next, Width, Height),
    \+ member([Next,_], Open),
    \+ member([Next,_], Closed).

% BFS
getState([[CurrentNode,Parent]|RestOpen], [CurrentNode,Parent], RestOpen).
addChildren(Children, Open, NewOpen) :-
    append(Open, Children, NewOpen).

% Predicate to check if a circle with the same color exists
hasSquare(State, Path, Width, Height) :-
    member([X,Y,Color], State),
    findSquare(State, [X,Y,Color], Path, Width, Height).

findSquare(State, [X,Y,Color], Path, Width, Height) :-
    moveUp([X,Y], [X1,Y1], Width, Height),
    member([X1,Y1,Color], State),

    moveRight([X1,Y1], [X2,Y2], Width, Height),
    member([X2,Y2,Color], State),

    moveDown([X2,Y2], [X3,Y3], Width, Height),
    member([X3,Y3,Color], State),

    % Ensure distinct coordinates for each corner
    [X,Y] \= [X1,Y1], [X,Y] \= [X2,Y2], [X,Y] \= [X3,Y3],
    [X1,Y1] \= [X2,Y2], [X1,Y1] \= [X3,Y3],
    [X2,Y2] \= [X3,Y3],
    Path = [[X,Y],[X1,Y1],[X2,Y2],[X3,Y3]].

% Print the path
% Design Output
printPath([]) :- nl.
printPath([[X,Y]|T]) :-
    format('~w,~w', [X,Y]),
    (   T == [] -> nl
    ;   write(' -> '),
        printPath(T)
    ).







