-module(region).
-compile(export_all).

-record(region,
        { point = {0, 0},
          side_length,
          neighbors = [],
          specks = [],
          particle_threshold,
          subdivision_counter }).

occuppied_char(true) -> 42; % Star
occuppied_char(false) -> 32. % Space

generate_row(0, Xs) ->
  occuppied_char(lists:member(0, Xs));
generate_row(Length, Xs) ->
  %[ occuppied_char(lists:member(Length, Xs)) | generate_row(Length - 1, Xs) ].
[generate_row(Length - 1, Xs) | [occuppied_char(lists:member(Length, Xs))]].

print_row(Length, []) ->
  io:fwrite("|"),
  io:fwrite("~*c", [Length, 32]),
  io:fwrite("|~n");
print_row(Length, Points) ->
  io:fwrite("|"),
  io:fwrite("~s", [generate_row(Length - 1, x_only_coordinates(Points))]),
  io:fwrite("|~n").

print_rows(0, Length, Points) ->
  %io:fwrite("~w", [0]),
  print_row(Length, points_for_row(0, Points));
print_rows(Row_Count, Length, Points) ->
  %io:fwrite("~w", [Row_Count]),
  print_row(Length, points_for_row(Row_Count, Points)),
  print_rows(Row_Count - 1, Length, Points).

x_only_coordinates(Points) ->
  lists:map(fun({_, {X,_}}) -> X end, Points).

points_for_row(Y_Cor, Points) ->
  lists:filter(fun({_, {_,Y}}) -> Y == Y_Cor end, Points).

box(Side, Points) ->
  Dash = 45,
  io:fwrite("+~*c+~n",[Side,Dash]),
  print_rows(Side - 1, Side, Points),
  io:fwrite("+~*c+~n",[Side,Dash]).

broadcast(State) ->
  Side_Length = State#region.side_length,
  box(Side_Length, State#region.specks),
  io:format("~p~n", [State]).


loop(State) ->
  broadcast(State),
  receive
    {move, Pid, Direction} ->
      {_, XY} = find_particle(Pid, State#region.specks),
      NewXY = new_xy(XY, Direction),
      case in_self(NewXY, State) of
        true ->
          NewSpecks = internal_update({Pid, NewXY}, State#region.specks),
          loop(State#region{specks=NewSpecks});
        false ->
          % Implement later: send external_speck speck request to neighbor
          Pid ! collision,
          loop(State)
      end;
    {external_speck, RegionPid, SpeckPid, XY} ->
      ok;
    {new_speck, Move, XY} ->
      % Need to check here whether XY is already occupied.
      NewSpecks = [{spawn(speck, init, [Move, self()]), XY} |
        State#region.specks],
      loop(State#region{specks=NewSpecks});
    stop -> ok
  end.


find_particle(Pid, Particles) ->
  lists:keyfind(Pid, 1, Particles).


new_xy({X, Y}, {x, Sign}) -> {X + Sign, Y};
new_xy({X, Y}, {y, Sign}) -> {X, Y + Sign}.


in_self({X, Y},
    #region{point={OriginX, OriginY}, side_length=SideLength}) ->
  (X < OriginX + SideLength andalso Y < OriginY + SideLength)
    andalso (X >= OriginX andalso Y >= OriginY).


internal_collision(NewXY, Specks) ->
  lists:keyfind(NewXY, 2, Specks).


internal_update({Pid, NewXY}, Specks) ->
  Collision = internal_collision(NewXY, Specks),
  internal_update(Collision, {Pid, NewXY}, Specks).

internal_update(false, {Pid, NewXY}, Specks) ->
  lists:keyreplace(Pid, 1, Specks, {Pid, NewXY});
internal_update({OtherPid, OtherXY}, {Pid, NewXY}, Specks) ->
  OtherPid ! Pid ! collision,
  Specks.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%             Future partition stuff             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% can_subdivide(Region) ->
%%   Region#region.subdivision_counter > 0.
%% 
%% need_to_subdivide(Region) ->
%%   length(Region#region.specks) >= Region#region.particle_threshold.
%% 
%% collision_detected(Region) ->
%%   need_to_subdivide(Region) and not(can_subdivide(Region)).
%% 
%% subdivide(Region) ->
%%   % spawn four new processes
%%   % 
%%   % stuff
%%   0.
%% 
%% notify_neighbor_of_divide(Neighbor, NewRegions) ->
%%   % stuff
%%   0.
