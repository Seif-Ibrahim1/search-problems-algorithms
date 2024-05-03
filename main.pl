% some test cases are in the bottom of the file for input 

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



% board([
%     [red, blue, red, red, red],
%     [red, red, red, yellow, red],
%     [blue, yellow, red, red, red]
% ]).

% Predicate to get the color of a cell
cell_color(X, Y, Board, Color) :-
    nth0(X, Board, Row),
    nth0(Y, Row, Color).

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

find_goal_path(X, Y, GoalX, GoalY, Board) :-
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
% find_goal_path(0,0,2,2,[         
%               [red, blue, red, red, red],   
%               [red, red, red, yellow, red],
%               [blue, yellow, red, red, red]
%             ]).    % output -> [(0,0),(1,0),(1,1),(1,2),(2,2)]
% find_goal_path(0,0,1,3,[         
%               [red, blue, red, red, red],   
%               [red, red, red, yellow, red],
%               [blue, yellow, red, red, red]
%             ]).  % output -> not found
% find_goal_path(0,0,0,1,[         
%               [red, blue, red, red, red],   
%               [red, red, red, yellow, red],
%               [blue, yellow, red, red, red]
%             ]).  % output -> not found
% find_goal_path(0,0,0,0,[         
%               [red, blue, red, red, red],   
%               [red, red, red, yellow, red],
%               [blue, yellow, red, red, red]
%             ]).     % output -> [(0,0)]
% find_goal_path(0,0,1,1,[         
%               [red, blue, red, red, red],   
%               [red, red, red, yellow, red],
%               [blue, yellow, red, red, red]
%             ]).     % output -> [(0,0),(1,0),(1,1)]


% findCyclePath(3,3, [[b,b,r],[b,b,y],[r,y,b]]). % output -> 0,0 -> 0,1 -> 1,1 -> 1,0
% findCyclePath(3,3, [[b,b,r],[b,y,y],[r,y,r]]). % output -> not found
% findCyclePath(4,4, [[b,b,r,r],[b,b,y,r],[r,y,b,r],[r,r,r,r]]). % output -> 0,0 -> 0,1 -> 1,1 -> 1,0
% findCyclePath(4,4, [[b,r,r,r],[b,b,y,r],[r,y,b,r],[r,r,b,r]]). % output -> not found
    