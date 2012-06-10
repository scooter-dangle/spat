-module(speck).
-compile(export_all).

-record(move, {slope, dir, state, time}).


init(Move, Region) ->
  timer:send_after(Move#move.time, move),
  loop(Move, Region).


loop(Move, Region) ->
  receive
    {collision, Axis} ->
      loop(switch_sign(Move, Axis), Region);
    move ->
      {MoveMsg, NewMove} = move(Move),
      Region ! {move, self(), MoveMsg},
      timer:send_after(Move#move.time, move),
      loop(NewMove, Region);
    kill ->
      ok
  end.


move(#move{slope=Slope, state={0,0}}=Move) ->
  move(Move#move{state=Slope});

move(#move{dir={_, YSign}, state={0, Y}}=Move) ->
  { {y, YSign}, Move#move{state={0, Y-1}} };

move(#move{dir={XSign, _}, state={X, Y}}=Move) ->
  { {x, XSign}, Move#move{state={X-1, Y}} }.


switch_sign(#move{dir={X, Y}} = Move, x) ->
  Move#move{dir={X * -1, Y}};

switch_sign(#move{dir={X, Y}} = Move, y) ->
  Move#move{dir={X, Y * -1}}.

