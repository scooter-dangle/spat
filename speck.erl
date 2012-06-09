-module(speck).
-compile(export_all).

-record(move, {slope, dir, state, time}).


move(Move=#move{Slope=slope, SlopeSign=dir, {0, 0}=state}) ->
  move(Move#move{state=Slope});

move(Move=#move{{_, YSign}=dir, {0, Y}=state}) ->
  { {y, YSign}, Move#move{state={0, Y-1}} };

move(Move=#move{{XSign, _}=dir, {X, Y}=state}) ->
  { {x, XSign}, Move#move{state={X-1, Y}} }.


switch_sign(#move{sign={X, Y}} = Move) ->
  Move#move{sign={X * -1, Y * -1}}.

 
loop(Move, WaitTime, Region) ->
  timer:send_after(WaitTime, move),
  receive
    collision ->
      loop(switch_sign(Move), WaitTime, Region);
    move ->
      {Msg, NewMove#move{}} = move(Move),
      Region ! Msg,
      loop(NewMove, WaitTime, Region)
  end.
