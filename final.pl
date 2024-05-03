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
    generate_coordinates(Board, Width,Height,Initial),
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

% Predicate to check if two cells have the same color
same_color(X1, Y1, X2, Y2, Board) :-
    cell_color(X1, Y1, Board, Color1),
    cell_color(X2, Y2, Board, Color2),
    Color1 = Color2.

% Predicate to check if a cell is within the board's range
check_range(X, Y, Board) :-
    length(Board, RowsCount),
    RowsCount > 0,
    nth0(0, Board, Row),
    length(Row, ColsCount),
    X >= 0, X < RowsCount, % Check if X is within the range
    Y >= 0, Y < ColsCount.

% Define valid moves    
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

% Predicate to check if a pair should be removed based on its coordinates
should_remove(_, []). 

% remove a pair if its coordinates match the first pair in the list.
should_remove((X1, Y1), [(X2, Y2)|_]) :-
    (X1 =:= X2, Y1 =:= Y2);
    (X1 =:= X2, Y1 =\= Y2);
    (X1 =\= X2, Y1 =:= Y2). 


clean_path([], []).
clean_path([X], [X]).

% If the first pair should be removed, skip it and continue with the rest of the list.
clean_path([X, Y|T], [X|Result]) :-
    should_remove(X, [Y|T]), % Check if X should be removed based on Y and the rest of the list.
    clean_path([Y|T], Result). 

% If the first pair should not be removed, keep it and continue with the rest of the list.
clean_path([X, Y|T], Result) :-
    \+ should_remove(X, [Y|T]), % Check if X should not be removed.
    clean_path([Y|T], Result).

find_goal_path(X, Y, GoalX, GoalY, Board) :-
    heuristic(X, Y, GoalX, GoalY, H),
    (astar([(X, Y, H)], GoalX, GoalY, Board, [], Path)) ->
        clean_path(Path, CleanPath), 
        write(CleanPath);
        write('not found').


astar([], _, _, _, _, _) :- !, fail. % If the priority queue is empty, fail

% Main predicate for A* search
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
% Expand the current node
astar_expand(X, Y, GoalX, GoalY, Board, Visited, NextNodes) :-
    findall((NX, NY, H),
            (   (up(X, Y, NX, NY, Board), \+ member((NX, NY), Visited), heuristic(NX, NY, GoalX, GoalY, H1), H = H1)
            ;   (down(X, Y, NX, NY, Board), \+ member((NX, NY), Visited), heuristic(NX, NY, GoalX, GoalY, H2), H = H2)
            ;   (left(X, Y, NX, NY, Board), \+ member((NX, NY), Visited), heuristic(NX, NY, GoalX, GoalY, H3), H = H3)
            ;   (right(X, Y, NX, NY, Board), \+ member((NX, NY), Visited), heuristic(NX, NY, GoalX, GoalY, H4), H = H4)
            ),
            NextNodes
    ).

% Sort the nodes based on their heuristic values
astar_sort(Nodes, Sorted) :-
    predsort(compare_node, Nodes, Sorted).

% Compare nodes based on their heuristic values
compare_node(Order, (X1, Y1, H1), (X2, Y2, H2)) :-
    (   H1 < H2 -> Order = <
    ;   H1 > H2 -> Order = >
    ;   (X1, Y1) @< (X2, Y2) -> Order = <
    ;   Order = >
    ).

% Calculate the heuristic value
heuristic(X, Y, GoalX, GoalY, H) :-
    H is abs(X - GoalX) + abs(Y - GoalY).

% Test cases (Assignment tests)


% find_goal_path(0,0,1,3,[[red, red, yellow, yellow],
%                          [red, blue, red, red],
%                        [red, red, red, yellow],
%                         [blue, red, blue, yellow]]). % output -> [(0,0),(1,0),(2,0),(2,1),(2,2),(1,2),(1,3)]

% findCyclePath(4,4, [[y,y,y,r],[b,y,b,y],[b,b,b,y],[b,b,b,y]]). % output -> 2,0 -> 2,1 -> 3,1 -> 3,0


% other tests

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