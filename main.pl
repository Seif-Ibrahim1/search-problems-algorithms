board([
    [red, blue, red, red, red],
    [red, red, red, yellow, red],
    [blue, yellow, red, red, red]
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

search(X, Y, GoalX, GoalY) :-
    board(Board),
    heuristic(X, Y, GoalX, GoalY, H),
    (astar([(X, Y, H)], GoalX, GoalY, Board, [], Path)) ->
        write(Path);
        write('not found').
    

astar([], _, _, _, _, _) :- !, fail. % If the priority queue is empty, fail

astar([(X, Y, _) | RestQueue], GoalX, GoalY, Board, Visited, Path) :-
    (X = GoalX, Y = GoalY) ->
        append(Visited, [(GoalX, GoalY)], Path); % Append the goal node to the Visited list
    (
        (   member((X, Y), Visited) -> astar(RestQueue, GoalX, GoalY, Board, Visited, Path)
        ;   astar_expand(X, Y, GoalX, GoalY, Board, Visited, NextNodes),
            append(Visited, [(X, Y)], NewVisited),
            append(NextNodes, RestQueue, NewQueue),
            astar_sort(NewQueue, Sorted),
            astar(Sorted, GoalX, GoalY, Board, NewVisited, Path)
        )
    ).

astar_expand(X, Y, GoalX, GoalY, Board, Visited, NextNodes) :-
    findall((NX, NY, H),
            (   (up(X, Y, NX, NY, Board), \+ member((NX, NY), Visited), heuristic(NX, NY, GoalX, GoalY, H1), H = H1)
            ;   (down(X, Y, NX, NY, Board), \+ member((NX, NY), Visited), heuristic(NX, NY, GoalX, GoalY, H2), H = H2)
            ;   (left(X, Y, NX, NY, Board), \+ member((NX, NY), Visited), heuristic(NX, NY, GoalX, GoalY, H3), H = H3)
            ;   (right(X, Y, NX, NY, Board), \+ member((NX, NY), Visited), heuristic(NX, NY, GoalX, GoalY, H4), H = H4)
            ),
            NextNodes
    ).

astar_sort(Nodes, Sorted) :-
    predsort(compare_node, Nodes, Sorted).

compare_node(Order, (X1, Y1, H1), (X2, Y2, H2)) :-
    (   H1 < H2 -> Order = <
    ;   H1 > H2 -> Order = >
    ;   (X1, Y1) @< (X2, Y2) -> Order = <
    ;   Order = >
    ).

heuristic(X, Y, GoalX, GoalY, H) :-
    H is abs(X - GoalX) + abs(Y - GoalY).

% Test cases
% search(0, 0, 2, 4). % [(0,0),(1,0),(1,1),(1,2),(2,2),(2,3),(2,4)]
% search(0, 0, 1, 3). % not found
% search(0, 0, 0, 1). % not found
% search(0, 0, 0, 0). % [(0,0)]
% search(0, 0, 1, 1). % [(0,0),(1,0),(1,1)]



% the old search code will remove it but not now

% search(X, Y, GoalX, GoalY) :-
%     board(Board),
%     search(X, Y, GoalX, GoalY, Board, [], Path),
%     (last(Path, (GoalX, GoalY)) -> % Check if the last point in the path is the goal point
%         write(Path) % If it is, print the path
%     ;
%         write('not found') % If it's not, print "not found"
%     ).

% search(X, Y, GoalX, GoalY, Board, Visited, Path) :-
%     check_range(X, Y, Board),
%     \+ member((X, Y), Visited),
%     (X = GoalX, Y = GoalY) ->
%         Path = [(X, Y)]; % If goal is reached, return the path
%         (
%             append(Visited, [(X, Y)], NewVisited),
%             (up(X, Y, NewX, NewY, Board, NewVisited) ->
%                 search(NewX, NewY, GoalX, GoalY, Board, NewVisited, TempPath),
%                 Path = [(X,Y)|TempPath]
%             ;
%                 (down(X, Y, NewX, NewY, Board, NewVisited) ->
%                     search(NewX, NewY, GoalX, GoalY, Board, NewVisited, TempPath),
%                     Path = [(X,Y)|TempPath]
%                 ;
%                     (left(X, Y, NewX, NewY, Board, NewVisited) ->
%                         search(NewX, NewY, GoalX, GoalY, Board, NewVisited, TempPath),
%                         Path = [(X,Y)|TempPath]
%                     ;
%                         (right(X, Y, NewX, NewY, Board, NewVisited) ->
%                             search(NewX, NewY, GoalX, GoalY, Board, NewVisited, TempPath),
%                             Path = [(X,Y)|TempPath]
%                         ;
%                             Path = []
%                         )
%                     )
%                 )
%             )
%         ).