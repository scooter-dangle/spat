-module(region).
-compile(export_all).

-record(region,
        { point = {0, 0},
          side_length,
          neighbors,
          specks = [],
          particle_threshold,
          subdivision_counter }).


broadcast(State) ->
  io:format("~p~n", [State]).


wait_on_neighbors(State) ->
  broadcast(State),
  receive
    {hello, Neighbor} ->
      NewState = fleem,
      wait_on_neighbors(NewState);
    neighborhood_ready -> loop(State)
  end.


loop(State) ->
  broadcast(State),
  receive
    {move, Pid, Direction={Axis, _}} ->
      {_, XY} = find_particle(Pid, State#region.specks),
      NewXY = new_xy(XY, Direction),
      case in_self(NewXY, State) of
        true ->
          NewSpecks = internal_update({Pid, NewXY}, State#region.specks, Axis),
          loop(State#region{specks=NewSpecks});
        false ->
          % Implement later: send external_speck speck request to neighbor
          Pid ! {collision, Axis},
          loop(State)
      end;
    {external_speck, RegionPid, SpeckPid, XY} ->
      ok;
    {new_speck, Move, XY} ->
      % Need to check here whether XY is already occupied.
      NewSpecks = [{spawn(speck, init, [Move, self()]), XY} |
        State#region.specks],
      loop(State#region{specks=NewSpecks});
    kill ->
      kill_specks(State#region.specks),
      ok
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


internal_update({Pid, NewXY}, Specks, Axis) ->
  Collision = internal_collision(NewXY, Specks),
  internal_update(Collision, {Pid, NewXY}, Specks, Axis).

internal_update(false, {Pid, NewXY}, Specks, _) ->
  lists:keyreplace(Pid, 1, Specks, {Pid, NewXY});
internal_update({OtherPid, OtherXY}, {Pid, NewXY}, Specks, Axis) ->
  OtherPid ! Pid ! {collision, Axis},
  Specks.

kill_specks([]) -> ok;
kill_specks([{Speck, _} | TheRest ]) ->
  Speck ! kill,
  kill_specks(TheRest).


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
