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
% {
%    points: [ [x, y, id], ... ],
%    length: integer,
%    width: integer,
%    updates: [
%     { time: counter,
%       points: [ [new_x, new_y, id], [new_x, new_y, id], ...] },
%    ]
%  }
start_json(Fd, Mat_Size, Points) ->
  io:fwrite(Fd, "{", []),
  io:format("~p~n", [Points]),
  print_points_json(Fd, Points),
  io:fwrite(Fd, ",length:~w,width:~w,updates:[", [Mat_Size, Mat_Size]).

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


log_update(Fd, Counter, Points) ->
  case Counter of
    0 -> io:fwrite(Fd, "{time:~w,", [Counter]);
    _ -> io:fwrite(Fd, ",{time:~w,", [Counter])
  end,
  print_points_json(Fd, Points),
  io:fwrite(Fd, "}", []).


print_points_json(Fd, Points) ->
  io:fwrite(Fd, "points:[",[]),
  points_to_json(Fd, Points),
  io:fwrite(Fd, "]",[]).


points_to_json(Fd, []) -> ok;
points_to_json(Fd, [{Pid, {X, Y}} | MorePoints]) ->
  io:fwrite(Fd, "[~w,~w,\"~w\"]", [X, Y, Pid]),
  case MorePoints of
    [] -> ok;
    _ -> io:fwrite(Fd, ",", [])
  end,
  points_to_json(Fd, MorePoints).


close(Fd) ->
  io:fwrite(Fd, "]}~n",[]),
  file:close(Fd).


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


% box(Side, Points) ->
%   Dash = 45,
%   io:fwrite("+~*c+~n",[Side,Dash]),
%   display_mat(Side, populate_mat(generate_mat(Side), Points)),
%   print_rows(Side - 1, Side, Points),
%   io:fwrite("+~*c+~n",[Side,Dash]).

broadcast(State, false) ->
  % Side_Length = State#region.side_length,
  % box(Side_Length, State#region.specks),
  % clear_screen(),
  % io:format("~p~n", [State]);
  ok;
broadcast(State, Printer) ->
  Printer ! {update, State}.

broadcast(State) ->
  box(State#region.side_length, State#region.specks),
  io:format("~p~n", [State]).

clear_screen() -> clear_screen(130).

clear_screen(0) -> ok;
clear_screen(N) ->
  clear_screen(N - 1),
  io:format("~n").

wait_on_neighbors(State, Printer) ->
  broadcast(State, Printer),
  receive
    {hello, Neighbor} ->
      NewState = fleem,
      wait_on_neighbors(NewState, Printer);
    neighborhood_ready -> loop(State, Printer)
  end.

init(Filename, State, FullTime, TimeInterval) ->
  {ok, Fd} = file:open(Filename, [write]),
  start_json(Fd, State#region.side_length, State#region.specks),
  timer:send_after(TimeInterval, print_out),
  timer:send_after(FullTime, end_print),
  logger_loop(Fd, State, TimeInterval, 0).


logger_loop(Fd, State, TimeInterval, Counter) ->
  receive
    {update, NewState} -> logger_loop(Fd, NewState, TimeInterval, Counter);
    print_out ->
      log_update(Fd, Counter, State#region.specks),
      timer:send_after(TimeInterval, print_out),
      logger_loop(Fd, State, TimeInterval, Counter + 1);
    end_print -> 
      close(Fd),
      io:format("File closed.~n")
  end.


loop(State, Printer) ->
  broadcast(State, Printer),
  receive
    {move, Pid, Direction={Axis, _}} ->
      {_, XY} = find_particle(Pid, State#region.specks),
      NewXY = new_xy(XY, Direction),
      case in_self(NewXY, State) of
        true ->
          NewSpecks = internal_update({Pid, NewXY}, State#region.specks, Axis),
          loop(State#region{specks=NewSpecks}, Printer);
        false ->
          % Implement later: send external_speck speck request to neighbor
          Pid ! {collision, Axis},
          loop(State, Printer)
      end;
    {external_speck, RegionPid, SpeckPid, XY} ->
      ok;
    {new_speck, Move, XY} ->
      % Need to check here whether XY is already occupied.
      NewSpecks = [{spawn(speck, init, [Move, self()]), XY} |
        State#region.specks],
      loop(State#region{specks=NewSpecks}, Printer);
    {start_log, Filename, FullTime, TimeInterval} ->
      NewPrinter = spawn(region, init, [Filename, State, FullTime, TimeInterval]),
      loop(State, NewPrinter);
    kill ->
      case Printer of
        false -> ok;
        Pid   -> Pid ! end_print
      end,
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
