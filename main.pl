board([
    [red, blue, yellow],
    [red, blue, yellow],
    [red, blue, yellow]
]).

% Predicate to get the color of a cell at position (X, Y) on the board
cell_color(X, Y, Board, Color) :-
    nth0(X, Board, Row),     % Get the row at position X
    nth0(Y, Row, Color).     % Get the element at position Y in the row

same_color(X1, Y1, X2, Y2, Board) :-
    cell_color(X1, Y1, Board, Color1),
    cell_color(X2, Y2, Board, Color2),
    Color1 = Color2.

check_range(X, Y, Board) :-
    length(Board, RowsCount),
    RowsCount > 0,
    nth0(0, Board, Row),
    length(Row, ColsCount),
    X >= 0, X < RowsCount,
    Y >= 0, Y < ColsCount.
    
up(X, Y, NewX, NewY, Board) :-
    NewX is X - 1,
    NewY is Y,
    check_range(NewX, NewY, Board),
    same_color(X, Y, NewX, NewY, Board).

down(X, Y, NewX, NewY, Board) :-
    NewX is X + 1,
    NewY is Y,
    check_range(NewX, NewY, Board),
    same_color(X, Y, NewX, NewY, Board).

left(X, Y, NewX, NewY, Board) :-
    NewY is Y - 1,
    NewX is X,
    check_range(NewX, NewY, Board),
    same_color(X, Y, NewX, NewY, Board).

right(X, Y, NewX, NewY, Board) :-
    NewY is Y + 1,
    NewX is X,
    check_range(NewX, NewY, Board),
    same_color(X, Y, NewX, NewY, Board).
    

move(X, Y, NewX, NewY, Board) :-
    up(X, Y, NewX, NewY, Board);
    down(X, Y, NewX, NewY, Board);
    left(X, Y, NewX, NewY, Board);
    right(X, Y, NewX, NewY, Board).

% Usage: right(0, 0, NewX, NewY, [[red, red, blue], [yellow, red, yellow]).
% Expected: NewX = 1, NewY = 0.
% Usage: up(0, 0, NewX, NewY, [[red, red, blue], [yellow, red, yellow]).
% Expected: false.
% Usage: right(0, 1, NewX, NewY, [[red, red, blue], [yellow, red, yellow]).
% Expected: false. (because the color doesn't match)