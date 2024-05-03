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
board_coordinates(_, _, []).

board_coordinates(Board, Width, Coords) :-
    board_coordinates_row(Board, Width, 0, Coords).

board_coordinates_row([], _, _, []).
board_coordinates_row([Row|Rest], Width, RowIndex, Coords) :-
    NextRowIndex is RowIndex + 1,
    board_coordinates_row(Rest, Width, NextRowIndex, RestCoords),
    board_coordinates_row_helper(Row, Width, RowIndex, 0, RowCoords),
    append(RowCoords, RestCoords, Coords).

board_coordinates_row_helper([], _, _, _, []).
board_coordinates_row_helper([Color|Rest], Width, RowIndex, ColIndex, [[RowIndex, ColIndex, Color]|RestCoords]) :-
    NextColIndex is ColIndex + 1,
    ColIndex < Width,
    !,
    board_coordinates_row_helper(Rest, Width, RowIndex, NextColIndex, RestCoords).

board_coordinates(Width, Height, Board, Coords) :-
    length(Board, Height),
    maplist(same_length(Board), Board),
    board_coordinates(Board, Width, Coords).

% Main predicate for finding a cycle
findCyclePath(Width, Height,Board) :-
    board_coordinates(Width,Height,Board,Initial),
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








