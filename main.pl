board([
    [red, blue, blue],
    [red, red, yellow],
    [blue, red, red]
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
    
up(X, Y, NewX, NewY, Board, Visited) :-
    NewX is X - 1,
    NewY is Y,
    check_range(NewX, NewY, Board),
    same_color(X, Y, NewX, NewY, Board),
    \+ member((NewX, NewY), Visited).

down(X, Y, NewX, NewY, Board, Visited) :-
    NewX is X + 1,
    NewY is Y,
    check_range(NewX, NewY, Board),
    same_color(X, Y, NewX, NewY, Board),
    \+ member((NewX, NewY), Visited).

left(X, Y, NewX, NewY, Board, Visited) :-
    NewY is Y - 1,
    NewX is X,
    check_range(NewX, NewY, Board),
    same_color(X, Y, NewX, NewY, Board),
    \+ member((NewX, NewY), Visited).

right(X, Y, NewX, NewY, Board, Visited) :-
    NewY is Y + 1,
    NewX is X,
    check_range(NewX, NewY, Board),
    same_color(X, Y, NewX, NewY, Board),
    \+ member((NewX, NewY), Visited).

search(X, Y, GoalX, GoalY) :-
    board(Board),
    search(X, Y, GoalX, GoalY, Board, [], Path),
    (last(Path, (GoalX, GoalY)) -> % Check if the last point in the path is the goal point
        write(Path) % If it is, print the path
    ;
        write('not found') % If it's not, print "not found"
    ).

search(X, Y, GoalX, GoalY, Board, Visited, Path) :-
    check_range(X, Y, Board),
    \+ member((X, Y), Visited),
    (X = GoalX, Y = GoalY) ->
        Path = [(X, Y)]; % If goal is reached, return the path
        (
            append(Visited, [(X, Y)], NewVisited),
            (up(X, Y, NewX, NewY, Board, NewVisited) ->
                search(NewX, NewY, GoalX, GoalY, Board, NewVisited, TempPath),
                Path = [(X,Y)|TempPath]
            ;
                (down(X, Y, NewX, NewY, Board, NewVisited) ->
                    search(NewX, NewY, GoalX, GoalY, Board, NewVisited, TempPath),
                    Path = [(X,Y)|TempPath]
                ;
                    (left(X, Y, NewX, NewY, Board, NewVisited) ->
                        search(NewX, NewY, GoalX, GoalY, Board, NewVisited, TempPath),
                        Path = [(X,Y)|TempPath]
                    ;
                        (right(X, Y, NewX, NewY, Board, NewVisited) ->
                            search(NewX, NewY, GoalX, GoalY, Board, NewVisited, TempPath),
                            Path = [(X,Y)|TempPath]
                        ;
                            Path = []
                        )
                    )
                )
            )
        ).

% Test cases
% search(0, 0, 2, 2). % [(0,0),(1,0),(1,1),(2,1),(2,2)]
% search(0, 0, 1, 2). % not found
% search(0, 0, 0, 2). % not found
% search(0, 0, 0, 0). % [(0,0)]
% search(0, 0, 1, 1). % [(0,0),(1,0),(1,1)]
