move([X,Y], [X1,Y1]) :- moveRight([X,Y], [X1,Y1]).
move([X,Y], [X1,Y1]) :- moveLeft([X,Y], [X1,Y1]).
move([X,Y], [X1,Y1]) :- moveUp([X,Y], [X1,Y1]).
move([X,Y], [X1,Y1]) :- moveDown([X,Y], [X1,Y1]).


moveRight([X,Y], [X1,Y1]) :- X1 is X + 1, Y1 is Y, valid(X1,Y1).
moveLeft([X,Y], [X1,Y1]) :- X1 is X - 1, Y1 is Y, valid(X1,Y1).
moveUp([X,Y], [X1,Y1]) :- X1 is X, Y1 is Y + 1, valid(X1,Y1).
moveDown([X,Y], [X1,Y1]) :- X1 is X, Y1 is Y - 1, valid(X1,Y1).

valid(X, Y) :- between(0, 3, X), between(0, 3, Y).

initialState([
    [0,0,yellow], [0,1,yellow], [0,2,yellow], [0,3,red],
    [1,0,blue], [1,1,yellow], [1,2,blue],   [1,3,yellow],
    [2,0,blue],   [2,1,blue],   [2,2,blue],   [2,3,yellow],
    [3,0,blue],   [3,1,blue],   [3,2,blue],   [3,3,yellow]
]).

% Main predicate for finding a circle with the same color
findCirclePath() :-
    initialState(Initial),
    search([[Initial, null]], [], Path),   % open is initial (null is parent), close is empty
    printPath(Path).

% Predicates for searching
search(Open, Closed, Path) :-
    getState(Open, [CurrentNode,Parent], RestOpen), % Step 1
    hasSquare(CurrentNode, Path).
    
search(Open, Closed, Path) :-
    getState(Open, CurrentNode, TmpOpen),
    getAllValidChildren(CurrentNode, TmpOpen, Closed, Children), % Step 3
    addChildren(Children, TmpOpen, NewOpen), % Step 4
    append(Closed, [CurrentNode], NewClosed), % Step 5.1
    search(NewOpen, NewClosed, Path). % Step 5.2

% Implementation of step 3 to get the next states
getAllValidChildren(Node, Open, Closed, Children) :-
    findall(Next, getNextState(Node, Open, Closed, Next), Children).

getNextState([State,Parent], Open, Closed, [Next,State]) :-
    move(State, Next),
    \+ member([Next,_], Open),
    \+ member([Next,_], Closed).

% Implementation of getState and addChildren determine the search algorithm.
% BFS
getState([[CurrentNode,Parent]|RestOpen], [CurrentNode,Parent], RestOpen). % retrieve current node and its parent
addChildren(Children, Open, NewOpen) :-
    append(Open, Children, NewOpen).

% Predicate to check if a circle with the same color exists
hasSquare(State, Path) :-
    member([X,Y,Color], State),
    findSquare(State, [X,Y,Color], Path).

findSquare(State, [X,Y,Color], Path) :-
    moveUp([X,Y], [X1,Y1]),
    member([X1,Y1,Color], State),
    
    moveRight([X1,Y1], [X2,Y2]),
    member([X2,Y2,Color], State),
    
    moveDown([X2,Y2], [X3,Y3]),
    member([X3,Y3,Color], State),
    
    % Ensure distinct coordinates for each corner
    [X,Y] \= [X1,Y1], [X,Y] \= [X2,Y2], [X,Y] \= [X3,Y3],
    [X1,Y1] \= [X2,Y2], [X1,Y1] \= [X3,Y3],
    [X2,Y2] \= [X3,Y3],
    Path = [[X,Y],[X1,Y1],[X2,Y2],[X3,Y3]].

% print the path
printPath([]) :- nl.
printPath([[X,Y]|T]) :-
    format('~w,~w', [X,Y]),
    (   T == [] -> nl
    ;   write(' -> '),
        printPath(T)
    ).







