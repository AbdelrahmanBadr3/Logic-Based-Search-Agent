:- include('KB.pl').
% the length of the path to the goal (number of states in the solution). 
path_length(s0,1).
path_length(result(_,S),D):-
    path_length(S,D1), D is D1 + 1.
% is goal if  list of the remaining soldiers is zero (empty list), the capacity is full (as the start state has the same value) and the position of the 
% ethan (X,Y) as same as the position of submarine  if it is the case then set the current state to final state.
is_goal(X,Y,C,[],FS,FS):-
    capacity(C) , submarine(X,Y).
% goal helper it takes the current X , Y of ethan, current capacity of the truck and the remaining soldiers list (list of remaing soldiers positions),
% current state (CS) and the final state (solution). it checks doing an action on current state leads to a goal state or to a state which will lead to 
% goal state. 
% conditions for ever action:
% right it can be done if the Y positon of the ethan less than the right border of grid.
% left it can be done if the Y positon of the ethan greater than the left border of grid.
% down it can be done if the X positon of the ethan less than the lower border of grid.
% up it can be done if the X positon of the ethan greater than the upper border of grid.
% carry it can be done if the remaing capacity is greater than zero and there is a soldier in the same position of ethnan. 
% drop it can be done if ethan and the submarine in the same postion and the capacity is not full.
goal_hepler(X,Y,C,RS,CS,S):-
    (
        (A = right , Y1 is Y+1 , Y < 3 , (is_goal(X,Y1,C,RS,result(A,CS),S); goal_hepler(X,Y1,C,RS,result(A,CS),S)));
        (A = up    , X1 is X-1 , X > 0 , (is_goal(X1,Y,C,RS,result(A,CS),S); goal_hepler(X1,Y,C,RS,result(A,CS),S)));
        (A = down  , X1 is X+1 , X < 3 , (is_goal(X1,Y,C,RS,result(A,CS),S); goal_hepler(X1,Y,C,RS,result(A,CS),S)));
        (A = left  , Y1 is Y-1 , Y > 0 , (is_goal(X,Y1,C,RS,result(A,CS),S); goal_hepler(X,Y1,C,RS,result(A,CS),S)));
        (A = carry , C1 is C-1 , C > 0 , select([X,Y],RS,RS1) , (is_goal(X,Y,C1,RS1,result(A,CS),S); goal_hepler(X,Y,C1,RS1,result(A,CS),S)));
        (A = drop  , submarine(X,Y) , \+capacity(C) , capacity(C1) , (is_goal(X,Y,C1,RS,result(A,CS),S); goal_hepler(X,Y,C1,RS,result(A,CS),S)))        
    ). 
% iterative deepening search, it starts from intial (X,Y) position of ethan, the capacity of the truck, the remaining soldiers list and the initial state (s0),
% the final state S and the depth to search. it searchs to find a goal till a certain depth or increament the search deoth limit by 1
ids_goal(X,Y,C,RS,s0,S,D):-
    ((call_with_depth_limit(goal_hepler(X,Y,C,RS,s0,S),D,Result), number(Result));
    (D1 is D + 1, ((\+var(S), path_length(S,L), D < (L+5)); var(S)), ids_goal(X,Y,C,RS,s0,S,D1))). 
% initialize the state with the values in Knowledge Base position of ethan (X,Y), the full capacity and the Remaining soldiers.
% call IDS with s0 and depth 1 or it's a goal state.
goal(S):-
    ethan_loc(X,Y) , capacity(C), members_loc(RS),
    (is_goal(X,Y,C,RS,s0,S); ids_goal(X,Y,C,RS,s0,S,1)).
