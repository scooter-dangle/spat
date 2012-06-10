-module(region).
-compile(export_all).

-record(region,
        { point = {0, 0},
          side_length,
          neighbors = [],
          specks = [],
          particle_threshold,
          subdivision_counter }).

char_or_space(empty) -> 32;
char_or_space(point) -> 42.

generate_mat(Size) ->
  [{{X, Y}, empty} || X <- lists:seq(0, Size-1), Y <- lists:seq(0, Size-1)].

populate_mat(Mat, []) -> Mat;
populate_mat(Mat, [{_, XY} | MoreSpecks]) ->
  populate_mat(lists:keyreplace(XY, 1, Mat, {XY, point}), MoreSpecks).

as_string(Points) ->
  [ char_or_space(Entry) || {_, Entry} <- Points].

display_mat(Row_Length, []) -> ok;
display_mat(Row_Length, Mat) ->
  {Curr_Row, Remaining_Rows} = lists:split(Row_Length, Mat),
  display_mat(Row_Length, Remaining_Rows),
  io:fwrite("|~s|~n", [as_string(Curr_Row)]).

box(Side, Points) ->
  Dash = 45,
  io:fwrite("+~*c+~n",[Side,Dash]),
  display_mat(Side, populate_mat(generate_mat(Side), Points)),
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
