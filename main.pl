% Define valid moves for a 2D board 
move([X,Y], [X1,Y1]) :- X1 is X + 1, Y1 is Y, valid(X1,Y1).
move([X,Y], [X1,Y1]) :- X1 is X - 1, Y1 is Y, valid(X1,Y1).
move([X,Y], [X1,Y1]) :- X1 is X, Y1 is Y + 1, valid(X1,Y1).
move([X,Y], [X1,Y1]) :- X1 is X, Y1 is Y - 1, valid(X1,Y1).

valid(X, Y) :- between(0, 3, X), between(0, 3, Y).

% Define the initial state and color of the cells
initialState([
    [0,0,yellow], [0,1,yellow], [0,2,yellow], [0,3,red],
    [1,0,blue],   [1,1,yellow], [1,2,blue],   [1,3,yellow],
    [2,0,blue],   [2,1,blue],   [2,2,blue],   [2,3,yellow],
    [3,0,blue],   [3,1,blue],   [3,2,blue],   [3,3,yellow]
]).

%BFS Algorithm

% Predicate to find a cycle path of the same color
search(CyclePath) :-
    initialState(InitialState),
    member(StartNode, InitialState),
    %bfs([StartNode], CyclePath, [StartNode]).






