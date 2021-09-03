/**********************| Main Comment |**************************

  Programmer  - Or Waichman
  
  FileName    - mmn17.pl
  
  Description - Implementation of AI that is capable of playing
                 a custom game using Alpha-Beta algorithm.
                 
                In the game, the player needs to choose between
                 moving or placing barriers that limit the
                 movement of the opponent. The goal is to reach
                 to the opposite side from the start.
                For elaboration on the gameplay, enter help
                 screen during run.
                For more information on the project, read
                 attached file.
                 
  Input       - Specified during run.
  
  Output      - In essence, Board overview for each turn and
                 announcement of the winner.

  Synopsys    - start_game.  % For default
                start_game(Player). % For specifying whether the
                                    % player or the computer
                                    % should start.
                                    % Player is 1 or 2
                                    
 ****************************************************************/
 
 
% Cache
:- dynamic known/1.
:- dynamic cache_size/1.


/***********************
 Definition of Constants
 ***********************/

/* Recommended adjustable values:
   (Note that these values have direct effect on performence and quality of opponent)
   
   * cache size --> 5x5: ~50000, 7x7: ~200000
   * board dimensions (make sure to change players' definitions as well)
     * 5x5: board_definition(5, 5, 2), player_definition(1, 3/1, 5, 3), player_definition(2, 3/5, 1, 3)
     * 7x7: board_definition(7, 7, 2), player_definition(1, 4/1, 7, 6), player_definition(2, 4/7, 1, 6)
   * depth limit - must be at least 3 --> 5x5: 4, 7x7: 3
   * Barrier options percentage --> ~1/10  (Higher for a higher chance that the computer will place clever barriers)
*/

max_cache_size(65536).

% player_definition(Sign, StartRow/StartColumn, GoalColumn, StartBarrierAmount)
:- dynamic player_definition/4.
% See definition in set_constants

% board_definition(Rows, Columns, BarrierLength)
board_definition(5, 5, 2).

% alphabeta_definition(Alpha, Beta, MaxDepth)
:- dynamic alphabeta_definition/3.
% See definition in set_constants

% barrier_random_percentage(PercentageOfOptionsTaken)
% Optimaziation for randomely pruning of branches represent barrier placing
barrier_random_percentage(1/10).


/****
 Main
 ****/

% start_game
% Main function, call to start a game
start_game :-
    % Whoever starts has a big adventage, as the board is smaller than I intended it to be.
    % As a result, in order to make it a close match the computer should start
    start_game(2).

% start_game(StartingPlayer)
start_game(1) :-
    game_preperations,
    write('Started game.'), nl,
    player_definition(1, P1Pos, _, P1Barriers),
    player_definition(2, P2Pos, _, P2Barriers),
    print_board(pos(state(P1Pos, P2Pos, P1Barriers, P2Barriers, []), 1)),
    play_opponent_turn(pos(state(P1Pos, P2Pos, P1Barriers, P2Barriers, []), 1)).
    
start_game(2) :-
    game_preperations,
    write('Started game.'), nl,
    player_definition(1, P1Pos, _, P1Barriers),
    player_definition(2, P2Pos, _, P2Barriers),
    print_board(pos(state(P1Pos, P2Pos, P1Barriers, P2Barriers, []), 2)),
    play_computer_turn(pos(state(P1Pos, P2Pos, P1Barriers, P2Barriers, []), 2)).


% game_preperations
% Clears cache, displays help screen and allows player to choose difficulty
game_preperations :-
    % Initialization
    clean_cache,
    set_constants,
    
    % Displaying help screen if requested
    write('Type "help" for instructions, or any other expression to start.'), nl,
    read(Consent),
    (Consent = 'help', !,
    print_help
    ;
    true),
    
    choose_difficulty.


% set_constants
% Sets constants that might get changed after choosing difficulty level
% Used as predicate so it could be run every time a game starts
set_constants :-
    retractall(player_definition(_, _, _, _)),
    asserta(player_definition(1, 3/1, 5, 3)),
    asserta(player_definition(2, 3/5, 1, 3)),  % Computer is always second
    
    retractall(alphabeta_definition(_, _, _)),
    asserta(alphabeta_definition(-9999, 9999, 4)).


% choose_difficulty
choose_difficulty :-
    repeat,
    write('Choose difficulty level: "easy", "medium" or "hard"'), nl,
    read(Difficulty),
    set_difficulty(Difficulty), !.


% set_difficulty(Difficulty)
% Adjusts constants based on difficulty level chosen
set_difficulty(easy) :-
    % Setting recursion depth to 1
    retract(alphabeta_definition(Alpha, Beta, _)),
    asserta(alphabeta_definition(Alpha, Beta, 1)).

set_difficulty(medium).  % Leaving settings as is

set_difficulty(hard) :-
    % Advancing computer one column closer to goal and giving it unlimited barriers
    % Note that the board dimensions should be 4x4 or bigger for this to be meaningful
    retract(player_definition(2, Row/Col, Goal, _)),
    NewCol is Col - 1,
    asserta(player_definition(2, Row/NewCol, Goal, 999)).


/********
 Gameplay
 ********/

% play_opponent_turn(GameState)
% Inputs a move from the user, and updates the game accordingly
play_opponent_turn(pos(state(P1Pos, P2Pos, P1Barriers, P2Barriers, BarriersPlacement), 1)) :-
    repeat,  % Until valid move received
    
    % Input move
    write('Player 1 turn (available barriers: '), write(P1Barriers), write(')    '),
    read(Move),

    % Case 1: Valid move, Continue game
    (validate_input(Move),
    interpret_validate_and_perform_move_of_opponent(Move, pos(state(P1Pos, P2Pos, P1Barriers, P2Barriers, BarriersPlacement), 1), NewGameState), !,

    print_board(NewGameState),
    % Stops if victory achieved
    (check_victory(NewGameState, 1), !,
    write('Player 1 won!'), nl
    ;
    % Computer's turn
    play_computer_turn(NewGameState))
    ;
    
    % Case 2: Invalid input, try again
    \+ validate_input(Move),
    write('Invalid input'), nl,
    fail
    ;
    
    % Case 3: Move is not legal, try again
    validate_input(Move),  % (we cannot use cut as it will break out of loop)
    write('Illegal move'), nl,
    fail).


% play_computer_turn(GameState)
% Finds best move to perform given game's state and updates it accordingly
play_computer_turn(pos(state(P1Pos, P2Pos, P1Barriers, P2Barriers, BarriersPlacement), 2)) :-
    write('Player 2 turn (available barriers: '), write(P2Barriers), write('):'), nl,
    
    % Calculating best move
    alphabeta(pos(state(P1Pos, P2Pos, P1Barriers, P2Barriers, BarriersPlacement), 2), NewGameState),

    print_board(NewGameState),

    % Stops if victory achieved
    (check_victory(NewGameState, 2), !,
    write('Player 2 won!'), nl
    ;

    % User's turn
    play_opponent_turn(NewGameState)).


% interpret_validate_and_perform_move_of_opponent(MoveRepr, GameState, NewGameState)
% Interprets, validates and performs move of opponent
% Case 1: Advance move
interpret_validate_and_perform_move_of_opponent(Move, pos(state(P1Pos, P2Pos, P1Barriers, P2Barriers, BarriersPlacement), 1), pos(state(NewP1Pos, P2Pos, P1Barriers, P2Barriers, BarriersPlacement), 2)) :-
    % Validate input
    is_advance_move(Move), !,
    % Interpret
    interpret_direction(Move, Direction),
    % Validate move, fail if illegal move
    valid_advance(P1Pos, pos(state(P1Pos, P2Pos, P1Barriers, P2Barriers, BarriersPlacement), 1), Direction, NewP1Pos).

% Case 2: Barrier placement move
interpret_validate_and_perform_move_of_opponent(Move, pos(state(P1Pos, P2Pos, P1Barriers, P2Barriers, BarriersPlacement), 1), pos(state(P1Pos, P2Pos, NewP1Barriers, P2Barriers, [Barrier| BarriersPlacement]), 2)) :-
    % Validate input
    is_barrier_move(Move), !,
    
    % Checks that player has barriers to start with
    P1Barriers > 0,
    
    % Interpret
    interpret_barrier(Move, Barrier),
    
    % Validate move, fail if illegal
    valid_barrier(Barrier, pos(state(P1Pos, P2Pos, P1Barriers, P2Barriers, BarriersPlacement), 1)),
    NewP1Barriers is P1Barriers - 1.


% validate_input(MoveRepr)
% Checks that input is in expected format
validate_input(MoveRepr) :-
    is_advance_move(MoveRepr), !
    ;
    is_barrier_move(MoveRepr).


% is_advance_move(AdvanceRepr)
% Checks that a move is advance move
is_advance_move(AdvanceRepr) :-
    ValidTerms = [e, east, s, south, w, west, n, north],
    member(AdvanceRepr, ValidTerms).


% is_barrier_move(BarrierMove)
% Checks that a move is barrier placing move
is_barrier_move(bv-Row/Col) :-
    integer(Row), integer(Col), !.
is_barrier_move(bh-Row/Col) :-
    integer(Row), integer(Col).


% interpret_direction(Expression, Direction)
% Translate shortened direction to unified format
% Assumes expression is valid
interpret_direction(e, east) :- !.
interpret_direction(s, south) :- !.
interpret_direction(w, west) :- !.
interpret_direction(n, north) :- !.
interpret_direction(Expression, Expression).


% interpret_barrier(MoveRepr, Barrier)
% Translate input of barrier placement to the format we work with
interpret_barrier(Orientation-Row/Col, Barrier) :-
    board_definition(_, _, BarrierLen),
    interpret_barrier(Orientation-Row/Col, Barrier, BarrierLen).
    
interpret_barrier(_, [], 0):- !.

interpret_barrier(bv-Row/Col, [Row/Col-NextCol| Barrier], Remains) :-
    NextCol is Col + 1,
    NewRemains is Remains - 1,
    NextRow is Row + 1,
    interpret_barrier(bv-NextRow/Col, Barrier, NewRemains).

interpret_barrier(bh-Row/Col, [Row-NextRow/Col| Barrier], Remains) :-
    NextRow is Row + 1,
    NewRemains is Remains - 1,
    NextCol is Col + 1,
    interpret_barrier(bh-Row/NextCol, Barrier, NewRemains).


% check_victory(GameState, Player)
% Checks whether a player has reached his goal column
check_victory(pos(state(_/GoalCol, _, _, _, _), _), 1) :-
    player_definition(1, _, GoalCol, _).
check_victory(pos(state(_, _/GoalCol, _, _, _), _), 2) :-
    player_definition(2, _, GoalCol, _).


/********
 Graphics
 ********/

% print_help
% Displays a tutorial for the game
print_help :-
    write('The game board is 5x5. The starting position for each player is the middle tile of the outermost column.'), nl,
    write('The goal for each player is to reach the other outermost column. However, a player cannot move freely as placed barriers can limit his movement.'), nl,
    write('Each turn, a player can choose between moving 1 tile on any direction, or place a barrier of length 2 anywhere on the board except its outer borders.'), nl,
    write('The barriers are placed on the border between tiles, and cannot be placed if crossing another barrier or in a way that traps player with no way of exit.'), nl,
    write('Each player has a limit of 3 barrier placing.'), nl, nl,
    write('Enter any expression to continue to input instruction, or enter "quit" to proceed to game'), nl,
    read(Consent),

    (Consent \= 'quit', !,
    write('Lets demonstrate input on this empty board'), nl,
    print_board_of_size(pos(state(2/1, 2/3, _, _, []), _), 1, 3/3),
    write('Enter any expression to continue'), nl, read(_),

    write('Player can move east, south, west and north. specify advancing in direction by typing the direction, or its first letter.'), nl,
    write('For example, by typing "s" when it is player 1 turn, the board would look like this:'), nl,
    print_board_of_size(pos(state(3/1, 2/3, _, _, []), _), 1, 3/3),
    write('Enter any expression to continue'), nl, read(_),

    write('Player can lay a barrier, which limits the movement of both players. The barrier can be horizontal or vertical.'), nl,
    write('To place a horizontal barrier, f.e., between lines 1 and 2, in columns 2 and 3, we type bh-1/2:'), nl,
    print_board_of_size(pos(state(3/1, 2/3, _, _, [[1-2/2, 1-2/3]]), _), 1, 3/3),
    write('Enter any expression to continue'), nl, read(_),

    write('To place a vertical barrier, f.e., between columns 1 and 2, in rows 2 and 3, we type bv-2/1:'), nl,
    print_board_of_size(pos(state(3/1, 2/3, _, _, [[2/1-2, 3/1-2]]), _), 1, 3/3),
    write('Finished displaying instructions. Enter any expression to continue'), nl, read(_)
    ;
    true).


% print_board(GameState)
% Prints game's board
print_board(pos(state(P1Pos, P2Pos, _, _, BarriersPlacement), _)) :-
    board_definition(Rows, Cols, _),  % Retrieve board dimensions
    print_board_of_size(pos(state(P1Pos, P2Pos, _, _, BarriersPlacement), _), 1, Rows/Cols), nl.


% print_board_of_size(GameState, StartingRow, BoardDimensions)
% Prints a game board described by it's state and dimensions
% Base: reached end
print_board_of_size(_, Row, Rows/_) :-
    Row > Rows, !.

% Main recursion
print_board_of_size(pos(state(P1Pos, P2Pos, _, _, BarriersPlacement), _), CurrentRow, BoardDimensions) :-
    % Print current row
    print_board_row(pos(state(P1Pos, P2Pos, _, _, BarriersPlacement), _), CurrentRow, 1, BoardDimensions),
    nl,
    
    % Print underlying barrier (demanded by the display format)
    print_board_barrier_line(pos(state(P1Pos, P2Pos, _, _, BarriersPlacement), _), CurrentRow, 1, BoardDimensions),
    nl,
    
    % Move to next row
    NextRow is CurrentRow + 1,
    print_board_of_size(pos(state(P1Pos, P2Pos, _, _, BarriersPlacement), _), NextRow, BoardDimensions).


% print_board_row(GameState, CurrentRow, StartingCol, BoardDimensions)
% Prints a single row of the board
% Base: reached end
print_board_row(_, _, Col, _/Cols) :-
    Col > Cols, !.

% Main recursion
print_board_row(pos(state(P1Pos, P2Pos, _, _, BarriersPlacement), _), Row, Col, BoardDimensions) :-
    print_board_location(pos(state(P1Pos, P2Pos, _, _, BarriersPlacement), _), Row/Col),
    NextCol is Col + 1,
    print_board_row(pos(state(P1Pos, P2Pos, _, _, BarriersPlacement), _), Row, NextCol, BoardDimensions).


% print_board_location(GameState, Location)
% Prints a single square in the board
print_board_location(pos(state(P1Pos, P2Pos, _, _, BarriersPlacement), _), Row/Col) :-
    % First, display the location itself, and if it is occupied by one of the players
    (P1Pos = Row/Col, !,
    write(' 1 ')
    ;
    P2Pos = Row/Col, !,
    write(' 2 ')
    ;
    write(' . ')),  % No player is standing in this location
    
    % Now, display barriers around this location
    (NextCol is Col + 1,
    member(Barrier, BarriersPlacement),
    member(Row/Col-NextCol, Barrier),
    write('|')
    ;
    write(' ')).


% print_board_barrier_line(GameState, CurrentRow, StartingCol, BoardDimensions)
% Print underlying barriers under row (demanded by the display format)
% Base: reached end
print_board_barrier_line(_, _, Col, _/Cols) :-
    Col > Cols, !.

% Main recursion
print_board_barrier_line(pos(state(P1Pos, P2Pos, _, _, BarriersPlacement), _), Row, Col, BoardDimensions) :-
    print_board_barrier_line_location(pos(state(P1Pos, P2Pos, _, _, BarriersPlacement), _), Row/Col),
    NextCol is Col + 1,
    print_board_barrier_line(pos(state(P1Pos, P2Pos, _, _, BarriersPlacement), _), Row, NextCol, BoardDimensions).


% print_board_barrier_line_location(GameState, Location)
% Prints a single position in underlying barrier line
print_board_barrier_line_location(pos(state(_, _, _, _, BarriersPlacement), _), Row/Col) :-
    % print a horizontal barrier
    NextRow is Row + 1,
    (member(Barrier1, BarriersPlacement),
    member(Row-NextRow/Col, Barrier1), !,
    write('---')
    ;
    write('   ')),
    
    % Connect the barrier if it continues
    (NextCol is Col + 1,
    member(Barrier2, BarriersPlacement),
    member(Row-NextRow/Col, Barrier2),
    member(Row-NextRow/NextCol, Barrier2), !,
    write('-')
    ;
    
    % Connect a vertical barrier that crosses this line
    NextCol is Col + 1,
    member(Barrier3, BarriersPlacement),
    member(Row/Col-NextCol, Barrier3),
    member(NextRow/Col-NextCol, Barrier3), !,
    write('|')
    ;
    write(' ')).


/******************
 Moves Calculations
 ******************/

% advance(StartPos, Direction, NewPos)
% Translates direction to position relative to a given one (without any checks)
advance(Row/Col, east, Row/NewCol) :-
    NewCol is Col + 1.
advance(Row/Col, west, Row/NewCol) :-
    NewCol is Col - 1.
advance(Row/Col, south, NewRow/Col) :-
    NewRow is Row + 1.
advance(Row/Col, north, NewRow/Col) :-
    NewRow is Row - 1.


% within_board(Pos)
% Checks if position is valid based on board dimensions
within_board(Row/Col) :-
    board_definition(RowsNum, ColsNum, _),
    Row =< RowsNum, Row >= 1,
    Col =< ColsNum, Col >= 1.


% not_blocked(PositionOfPlayer, NewPosition, BarriersPlacement)
% Checks whether advancing from one position to a neighboring one is not blocked by barriers
not_blocked(Row/Col, NewRow/NewCol, BarriersPlacement) :-
    % Check if there's no border between current and new position
    ( Col = NewCol ;
    \+ (member(Barrier, BarriersPlacement), (member(NewRow/Col-NewCol, Barrier) ; member(NewRow/NewCol-Col, Barrier)))),
    ( Row = NewRow ;
    \+ (member(Barrier, BarriersPlacement), (member(Row-NewRow/NewCol, Barrier) ; member(NewRow-Row/NewCol, Barrier)))),
    !.


% valid_advance(StartPos, Direction, NewPos)
% Translates direction to position relative to a given one, succeeds only if the movement is valid
valid_advance(Row/Col, pos(state(P1Pos, P2Pos, _, _, BarriersPlacement), _), Direction, NewRow/NewCol) :-
    % Calculate new position
    advance(Row/Col, Direction, NewRow/NewCol),

    % Position is not occupied
    NewRow/NewCol \= P1Pos,
    NewRow/NewCol \= P2Pos,

    % Position is within board dimensions
    within_board(NewRow/NewCol),

    % Position is not seperated from current one by barrier
    not_blocked(Row/Col, NewRow/NewCol, BarriersPlacement).
    
    
% movement_options(PlayerPosition, GameState, Positions)
% Finds all possible advancements from a given position, given a game state (4 at most).
movement_options(Pos, GameState, Positions) :-
    Directions = [east, north, south, west],
    findall(NewPos,
            (   member(Direction, Directions),
                valid_advance(Pos, GameState, Direction, NewPos)),
            Positions).


% vertical_barrier_block(StartPosition, BarrierLen, Barrier)
% Creates vertical barrier representation from start position (without any checks)
vertical_barrier_block(_/_, 0, []) :- !.
vertical_barrier_block(Row/Col, BarrierLen, [Row/Col-NextCol| Borders]) :-
    NextCol is Col + 1,
    RestOfBarrierLen is BarrierLen - 1,
    NextRow is Row + 1,
    vertical_barrier_block(NextRow/Col, RestOfBarrierLen, Borders).


% horizontal_barrier_block(StartPosition, BarrierLen, Barrier)
% Creates horizontal barrier representation from start position (without any checks)
horizontal_barrier_block(_/_, 0, []) :- !.
horizontal_barrier_block(Row/Col, BarrierLen, [Row-NextRow/Col| Borders]) :-
    NextRow is Row + 1,
    RestOfBarrierLen is BarrierLen - 1,
    NextCol is Col + 1,
    horizontal_barrier_block(Row/NextCol, RestOfBarrierLen, Borders).


% all_barriers(Barriers)
% Calculates all possible placements for barriers on empty board
% Yields [(r-1)*(c-b) + (c-1)*(r-b) < 2rc] results, when r is RowNum, c is ColNum, b is BarrierLen
all_barriers(Barriers) :-
    known(all_barriers(Barriers)), !.

all_barriers(Barriers) :-
    board_definition(RowNum, ColNum, BarrierLen),
    
    % Generate vertical barriers
    RowNumForVertical is RowNum - BarrierLen + 1,
    ColNumForVertical is ColNum - 1,
    range(1, RowNumForVertical, RowsForVertical),
    range(1, ColNumForVertical, ColsForVertical),
    
    findall(Barrier,
            (   member(Row, RowsForVertical),
                member(Col, ColsForVertical),
                vertical_barrier_block(Row/Col, BarrierLen, Barrier)),
            VerticalBarriers),

    % Generate horizontal barriers
    RowNumForHorizontal is RowNum - 1,
    ColNumForHorizontal is ColNum - BarrierLen + 1,
    range(1, RowNumForHorizontal, RowsForHorizontal),
    range(1, ColNumForHorizontal, ColsForHorizontal),

    findall(Barrier,
            (   member(Row, RowsForHorizontal),
                member(Col, ColsForHorizontal),
                horizontal_barrier_block(Row/Col, BarrierLen, Barrier)),
            HorizontalBarriers),

    % Yield the union of those lists
    conc(VerticalBarriers, HorizontalBarriers, Barriers),
    add_to_cache(all_barriers(Barriers)).


% valid_barrier(Barrier, GameState)
% Checks whether placement of new barrier is legal
valid_barrier(Barrier, pos(state(P1Pos, P2Pos, _, _, BarriersPlacement), _)) :-
    % Check whether is in inner board
    barrier_within_board(Barrier),

    % Does not override existing barrier
    not_collides(Barrier, BarriersPlacement),
    
    % Does not trap a player and prevent him to reach his goal
    player_definition(1, _, P1GoalCol, _),
    player_definition(2, _, P2GoalCol, _),
    check_route_to_column(P1Pos, pos(state(P1Pos, P2Pos, _, _, [Barrier| BarriersPlacement]), _), P1GoalCol),
    check_route_to_column(P2Pos, pos(state(P1Pos, P2Pos, _, _, [Barrier| BarriersPlacement]), _), P2GoalCol).


% barrier_within_board(Barrier)
% Checks that barrier is within board
barrier_within_board([]).
barrier_within_board([Row/Col-NextCol| Barrier]) :-
    within_board(Row/Col),
    within_board(Row/NextCol),
    barrier_within_board(Barrier).
barrier_within_board([Row-NextRow/Col| Barrier]) :-
    within_board(Row/Col),
    within_board(NextRow/Col),
    barrier_within_board(Barrier).


% barrier_placement_options(GameState, Barriers)
% Finds all possible barrier placements on board, given game state
% Assures that do not collide with existing or block one of the players
barrier_placement_options(pos(state(P1Pos, P2Pos, _, _, BarriersPlacement), _), Barriers) :-
    % Calculate every possible placement
    all_barriers(PossibleBarriers),

    % Filter in only those that do not collide with another barrier and do not block a player
    findall(Barrier,
            (   member(Barrier, PossibleBarriers),
                valid_barrier(Barrier, pos(state(P1Pos, P2Pos, _, _, BarriersPlacement), _))),
            AllBarriers),

    % Pruning barnches randomely by diluting this list
    barrier_random_percentage(Prob),
    random_sublist(AllBarriers, Prob, Barriers).


% not_collidees(Barrier, BarriersPlacement)
% Checks whether a barrier placement is available based on existing layout
not_collides(Barrier, BarrierPlacement) :-
    % Case 1: Vertical barrier
    % Checks that barrier does not overrides existing barrier
    \+ (member(Row/LCol-RCol, Barrier),
        member(Barrier1, BarrierPlacement),
        member(Row/LCol-RCol, Barrier1)),
    % Checks that is does not cross a horizontal barrier
    \+ (member(Row1/LCol-RCol, Barrier),
        member(Row2/LCol-RCol, Barrier),
        member(Barrier1, BarrierPlacement),
        member(Row1-Row2/LCol, Barrier1)),
    
    % Case 2: Horizontal - similar (yet adjusted) checks as before
    % Note that if the barrier is vertical, those checks will succeed without
    % computing them all the way through (so we need to use AND and not OR)
    \+ (member(LRow-RRow/Col, Barrier),
        member(Barrier1, BarrierPlacement),
        member(LRow-RRow/Col, Barrier1)),

    \+ (member(LRow-RRow/Col1, Barrier),
        member(LRow-RRow/Col2, Barrier),
        member(Barrier1, BarrierPlacement),
        member(Row/Col1-Col2, Barrier1)).


/************
 Path Finding
 ************/

% shortest_route_to_column(Pos, GameState, GoalCol, Route, RouteLen)
% Finds shortest path from position to column considering barriers. Fails if there is not any
shortest_route_to_column(Pos, GameState, GoalCol, Route, RouteLen) :-
   % Start with the minimal route length if there were no barriers
   distance_to_col(Pos, GoalCol, StartMaxDepth),
   deepening_route_to_column(Pos, GameState, GoalCol, Route, RouteLen, StartMaxDepth), !.


% deepening_route_to_column(Pos, GameState, GoalCol, Route, RouteLen, MaxDepth)
% The iterative deepening of shortest_route_to_column
deepening_route_to_column(Pos, GameState, GoalCol, Route, RouteLen, MaxDepth) :-
    (route_to_column(Pos, GameState, GoalCol, Route, RouteLen, [], MaxDepth), !
    ;
    % If solution was not found for MaxDepth, raise it by 1 and try again
    NewMaxDepth is MaxDepth + 1,
    
    % Limit path's length with number of positions on board
    board_definition(Rows, Cols, _),
    MaxDepthLimit is Rows * Cols,
    NewMaxDepth =< MaxDepthLimit,
    
    deepening_route_to_column(Pos, GameState, GoalCol, Route, RouteLen, NewMaxDepth)).


% check_route_to_column(Pos, GameState, GoalCol)
% Checks if a route exists from position to column
% Uses shortest_route_to_column but ignores actual route
check_route_to_column(Pos, GameState, GoalCol) :-
    shortest_route_to_column(Pos, GameState, GoalCol, _, _).


% route_to_column(StartPos, GoalCol, Route, RouteLen, Accumulated, MaxDepth)
% Finds route from a position to column, with depth restraint
% Base: reached target
route_to_column(Row/Col, _, Col, [Row/Col], 0, _, _) :- !.

% Main recursion
route_to_column(Pos, GameState, GoalCol, [Pos| Route], RouteLen, Accumulated, MaxDepth) :-
    % Validate we're in recursion depth limit
    MaxDepth > 0,
    NewMaxDepth is MaxDepth - 1,
    
    % If position has already been visited during run, it means it doesn't lead anywhere
    \+ member(Pos, Accumulated),

    % Pass on every neighboring position from current
    bagof(Pos, (movement_options(Pos, GameState, Positions), member(NewPos, Positions)), _),
    route_to_column(NewPos, GameState, GoalCol, Route, SubRouteLen, [Pos| Accumulated], NewMaxDepth),

    RouteLen is SubRouteLen + 1.


% distance_to_col(Pos, Col, Distance)
% Calculates distance from location to column
distance_to_col(Row/Col, GoalCol, Distance) :-
    distance(Row/Col, Row/GoalCol, Distance).

% distance(Pos1, Pos2, Distance)
% Calculates distance between two locations in grid
distance(Row1/Col1, Row2/Col2, Distance) :-
   absolute(Row1-Row2, RowDiff),
   absolute(Col1-Col2, ColDiff),
   Distance is RowDiff + ColDiff.


/***************************
 Huristic Evaluation & Moves
 ***************************/

% staticval(GameState, Val)
% Huristic estimation of a situation
% Defined as aerial distance to goal column
% Note: computer (min) is P2
% Note: we assume the position is VALID (in particular: barriers do not lock access for player to his goal
staticval(pos(state(P1Row/P1Col, P2Row/P2Col, P1BarrierCount, P2BarrierCount, BarriersPlacement), _), Val) :-
    known(staticval(pos(state(P1Row/P1Col, P2Row/P2Col, P1BarrierCount, P2BarrierCount, BarriersPlacement), _), Val)).
    
staticval(pos(state(P1Row/P1Col, P2Row/P2Col, P1BarrierCount, P2BarrierCount, BarriersPlacement), _), Val) :-
    % Add distance traveled by P1 and subtract distance left for P2.
    % Lower value is better for P2. Prefers it when P2 is close to goal.
    player_definition(1, _, P1GoalCol, _),
    
    (shortest_route_to_column(P1Row/P1Col, pos(state(P1Row/P1Col, P2Row/P2Col, P1BarrierCount, P2BarrierCount, BarriersPlacement), _), P1GoalCol, _, P1Distance), !
    ;
    % Sometimes there's an uncommon situation when a player cannot reach his
    % goal column after the opponent moves and blocks the only bottle-neck
    % leads there. This is not an illegal situation, but we can't calculate
    % route length to the goal. As a solution, a special value is calculated
    % consists of aerial distance and penalty for this situation
    distance_to_col(P1Row/P1Col, P1GoalCol, P1AerialDistance),
    P1Distance is P1AerialDistance + 5),
    
    player_definition(2, _, P2GoalCol, _),
    
    % Same trick for P2
    (shortest_route_to_column(P2Row/P2Col, pos(state(P1Row/P1Col, P2Row/P2Col, P1BarrierCount, P2BarrierCount, BarriersPlacement), _), P2GoalCol, _, P2Distance), !
    ;
    distance_to_col(P2Row/P2Col, P2GoalCol, P2AerialDistance),
    P2Distance is P2AerialDistance + 5),
    
    DistanceIndex is 1.1 * P2Distance - P1Distance,
    
    % Use adventage in unused barriers as tie-breaker
    BarrierIndex is (P2BarrierCount - P1BarrierCount) * 0.1,

    Val is DistanceIndex + BarrierIndex,

    add_to_cache(staticval(pos(state(P1Row/P1Col, P2Row/P2Col, P1BarrierCount, P2BarrierCount, BarriersPlacement), _), Val)).


% Succeeds if a move is of the opponent
max_to_move(pos(_,1)).


% Succeeds if a move is of the computer
min_to_move(pos(_,2)).


% moves(GameState, Moves)
% Case 1: Calculate all possible moves for opponent given a game state
moves(pos(state(P1Pos, P2Pos, P1BarrierCount, P2BarrierCount, BarriersPlacement), 1), Moves) :-
    % First, check if the opponent has already beaten the game, if so there are no moves
    \+ check_victory(pos(state(P1Pos, P2Pos, P1BarrierCount, P2BarrierCount, BarriersPlacement), 1), 2),

    % Cast result from movement_options to a format that describes game states
    movement_options(P1Pos, pos(state(P1Pos, P2Pos, P1BarrierCount, P2BarrierCount, BarriersPlacement), 1), NewPositions),
    findall(pos(state(Position, P2Pos, P1BarrierCount, P2BarrierCount, BarriersPlacement), 2),
           (member(Position, NewPositions)),
           AdvanceMoves),

    % Same with barrier placements, but deduct one from potential
    % If potential is empty, do not calculate these moves
    (P1BarrierCount > 0, !,
    barrier_placement_options(pos(state(P1Pos, P2Pos, P1BarrierCount, P2BarrierCount, BarriersPlacement), 1), NewBarriers),
    NewP1BarrierCount is P1BarrierCount - 1,
    findall(pos(state(P1Pos, P2Pos, NewP1BarrierCount, P2BarrierCount, [Barrier| BarriersPlacement]), 2),
           (member(Barrier, NewBarriers)),
           BarrierMoves)
    ;
    BarrierMoves = []),

    % Combine the two types of optional moves to one list
    conc(AdvanceMoves, BarrierMoves, Moves),

    % Fails if none found
    Moves \= [].

% Case 2: Calculate all possible moves for computer given a game state
moves(pos(state(P1Pos, P2Pos, P1BarrierCount, P2BarrierCount, BarriersPlacement), 2), Moves) :-
    % First, check if the opponent has already beaten the game, if so there are no moves
    \+ check_victory(pos(state(P1Pos, P2Pos, P1BarrierCount, P2BarrierCount, BarriersPlacement), 2), 1),
    
    % Cast result from movement_options to a format that describes game states
    movement_options(P2Pos, pos(state(P1Pos, P2Pos, P1BarrierCount, P2BarrierCount, BarriersPlacement), 2), NewPositions),
    findall(pos(state(P1Pos, Position, P1BarrierCount, P2BarrierCount, BarriersPlacement), 1),
           (member(Position, NewPositions)),
           AdvanceMoves),

    % Same with barrier placements, but deduct one from potential
    % If potential is empty, do not calculate these moves
    (P2BarrierCount > 0, !,
    barrier_placement_options(pos(state(P1Pos, P2Pos, P1BarrierCount, P2BarrierCount, BarriersPlacement), 2), NewBarriers),
    NewP2BarrierCount is P2BarrierCount - 1,
    findall(pos(state(P1Pos, P2Pos, P1BarrierCount, NewP2BarrierCount, [Barrier| BarriersPlacement]), 1),
           (member(Barrier, NewBarriers)),
           BarrierMoves)
    ;
    BarrierMoves = []),
    
    % Combine the two types of optional moves to one list
    conc(AdvanceMoves, BarrierMoves, Moves),

    % Fails if none found
    Moves \= [].


/********************
 Alpha-Beta Algorithm
 ********************/

% alphabeta(Pos, GoodPos)
% Wrapper for the classical Alpha-Beta algorithm, restricted by depth limit
alphabeta(Pos, GoodPos) :-
    alphabeta_definition(Alpha, Beta, MaxDepth),
    alphabeta(Pos, Alpha, Beta, GoodPos, _, MaxDepth).


alphabeta(Pos, _, _, GoodPos, Val, _) :-
    % Check cached value if already computed
    known(alphabeta(Pos, GoodPos, Val)), !.

alphabeta(Pos, Alpha, Beta, GoodPos, Val, MaxDepth) :-
    % Validate we're in recursion depth limit
    MaxDepth > 0,
    NewMaxDepth is MaxDepth - 1,
    
    % If there are any moves, take the best one out of them
    moves(Pos, PosList), !,
    boundedbest(PosList, Alpha, Beta, GoodPos, Val, NewMaxDepth),
    % Add the result to cache
    add_to_cache(alphabeta(Pos, GoodPos, Val))
    ;
    
    % If there are no moves, or reached MaxDepth, assess static value of current position
    staticval(Pos, Val).
    
% Finds the best value from a list (withinbounds)
boundedbest([Pos| PosList], Alpha, Beta, GoodPos, GoodVal, MaxDepth) :-
    % Assess value and best move of the current position
    alphabeta(Pos, Alpha, Beta, _, Val, MaxDepth),

    % Find the better move out of all the positions
    goodenough(PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal, MaxDepth).

% Case 1: No other candidates - Take Pos
goodenough([], _, _, Pos, Val, Pos, Val, _) :- !.

% Case 2: Pos is better than bounds - Take Pos
goodenough(_, Alpha, Beta, Pos, Val, Pos, Val, _) :-
    min_to_move(Pos), Val > Beta, !
    ;
    max_to_move(Pos), Val < Alpha, !.

% Case 3: Pos is within bounds, redefine bounds and rerun boundedbest for
% the remaining positions and hope to encounter a better position than Pos.
goodenough(PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal, MaxDepth) :-
    newbounds(Alpha, Beta, Pos, Val, NewAlpha, NewBeta),
    boundedbest(PosList, NewAlpha, NewBeta, Pos1, Val1, MaxDepth),
    betterof(Pos, Val, Pos1, Val1, GoodPos, GoodVal).


% Redfines the bounds if needed, based on given position
% Case 1: Narrower - increase lower bound
newbounds(Alpha, Beta, Pos, Val, Val, Beta) :-
    min_to_move(Pos), Val > Alpha, !.

% Case 2: Narrower - decrease upper bound
newbounds(Alpha, Beta, Pos, Val, Alpha, Val) :-
    max_to_move(Pos), Val < Beta, !.

% Case 2: Unchanged
newbounds(Alpha, Beta, _, _, Alpha, Beta).


% Compares 2 positions and yields the better one
betterof(Pos, Val, _, Val1, Pos, Val) :-
    min_to_move(Pos), Val > Val1, !
    ;
    max_to_move(Pos), Val < Val1, !.

betterof(_, _, Pos1, Val1, Pos1, Val1).


/********************
 Additional Utilities
 ********************/

conc([], List, List).
conc([Item|List1], List2, [Item|List]) :-
    conc(List1, List2, List).

% member is already defined in SWI-Prolog

% Absolute value of X
absolute(X,Y) :-
    Y is X, Y >= 0, !.
absolute(X,Y) :-
    Y is -(X).

% Generate sequence of following numbers
% f.e., range(1, 5) --> [1,2,3,4,5]
range(Start, Start, [Start]) :- !.
range(Start, End, [Start| Range]) :-
    Second is Start + 1,
    range(Second, End, Range).


% Randomely remove items from a list with probability of (Y-X)/Y
random_sublist([], _, []).
random_sublist(List, 1/1, List) :- !.
random_sublist([_| List], X/Y, DilutedList) :-
    random(1, Y, Random),
    X1 is X + 1,
    range(X1, Y, ChancesToDiscard),
    member(Random, ChancesToDiscard), !,
    random_sublist(List, X/Y, DilutedList).
random_sublist([Item| List], X/Y, [Item| DilutedList]) :-
    random_sublist(List, X/Y, DilutedList).


/******************
 LRU Cache Handling
 ******************/

% add_to_cache(Fact)
% Adds a fact to cache
% Case 1: fact already in cache - move it to end
add_to_cache(Fact) :-
    retract(known(Fact)), !,
    assertz(known(Fact)).

% Case 2: Size limit has not been reached
add_to_cache(Fact) :-
    cache_size(CacheSize),
    max_cache_size(MaxCacheSize),
    CacheSize < MaxCacheSize, !,
    assertz(known(Fact)),
    retract(cache_size(CacheSize)),
    UpdatedCacheSize is CacheSize + 1,
    asserta(cache_size(UpdatedCacheSize)).

% Case 3: Reached size limit
add_to_cache(Fact) :-
    retract_first(known(_)),
    assertz(known(Fact)).


% retract_first(Fact)
% Validated retraction of the first fact that matches
retract_first(Fact) :-
    call(Fact), !,
    retract(Fact).


% retract_facts_from_cache(Fact)
% Retract all facts of specific structure from cache
retract_facts_from_cache(Fact) :-
    retractall(known(Fact)).


% clean_cache
% As the name suggests...
clean_cache :-
    retractall(known(_)),
    retractall(cache_size(_)),
    asserta(cache_size(0)).
