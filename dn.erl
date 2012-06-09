-module(dn).
-compile(export_all).

-record(move, {slope, dir, state, time}).
-record(speck, {pid, xy, move = #move{}}).


move(Move=#move{Slope=slope, SlopeSign=dir, {0, 0}=state}) ->
  move(Move#move{state=Slope});

move(Move=#move{{_, YSign}=dir, {0, Y}=state}) ->
  { {y, YSign}, Move#move{state={0, Y-1}} };

move(Move=#move{{XSign, _}=dir, {X, Y}=state}) ->
  { {x, XSign}, Move#move{state={X-1, Y}} }.




internal_collision(_, []) -> false;
internal_collision(NewXY, SpeckList) ->
  lists:keyfind(NewXY, 2, SpeckList).

internal_update(false, {Pid, NewXY}, SpeckList) ->
  lists:keyreplace(Pid, 1, SpeckList, {Pid, NewXY});

internal_update({OtherPid, OtherXY}, {Pid, NewXY}, SpeckList) ->
  OtherPid ! Pid ! collision,
  SpeckList.

new_xy({X, Y}, x, Sign) -> {X + Sign, Y};
new_xy({X, Y}, y, Sign) -> {X, Y + Sign}.

in_self(NewXY, #region{Origin=point, SideLength=side_length}) ->
  

%% loop(State) ->
%%   receive
%%     %% Hunt for the most recent address message.
%%     {recent_msg, Pid} ->
%%       Pid ! most_recent_message(),
%%       loop(State);
%%     kill -> true
%%   end.
 
 
loop(Move, SleepTime, Region) ->
  CurrentTime = time:now(),
  receive
    collision ->
      %%stuff
      loop(T - time:now());
  after SleepTime ->
      {Msg, NewMove#move{}} = move(Move),
      Region ! Msg,
      loop(NewMove, SleepTime, Region)
  end.




most_recent_message() ->
  RefTup = {ref, make_ref()},
  self() ! RefTup,
  receive
    RefTup -> false;
    Message ->
      most_recent_message(RefTup, [Message])
  end.


most_recent_message(RefTup, MsgCache=[MostRecentMsg|TheRest]) ->
  receive
    RefTup ->
      sendBack(TheRest),
      MostRecentMsg;
    Message ->
      most_recent_message(RefTup, [Message|MsgCache])
  end.


sendBack([]) -> ok;
sendBack([H|T]) ->
  self() ! H,
  sendBack(T).


