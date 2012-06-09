-module(dn).
-compile(export_all).


move(Slope, SlopeSign, {0, 0}) ->
  move(Slope, SlopeSign, Slope);

move(_, {_, YSign}, {0, Y}) ->
  {{y, YSign}, {0, Y-1}};

move(_, {XSign, _}, {X, Y}) ->
  {{x, XSign}, {X-1, Y}}.


loop(State) ->
  receive %% Hunt for the most recent address message.
    {recent_msg, Pid} ->
      Pid ! most_recent_message(),
      loop(State);
    kill -> true
  end.


sleep(T) ->
  receive
  after T -> true
  end.


most_recent_message() ->
  RefTup = {ref, make_ref()},
  self() ! RefTup,
  receive
    RefTup2 when RefTup =:= RefTup2 -> false;
    Message ->
      most_recent_message(RefTup, [Message])
  end.

most_recent_message(RefTup, MsgCache=[MostRecentMsg|TheRest]) ->
  receive
    RefTup2 when RefTup =:= RefTup2 ->
      sendBack(TheRest),
      MostRecentMsg;
    Message ->
      most_recent_message(RefTup, [Message|MsgCache])
  end.

sendBack([]) -> ok;
sendBack([H|T]) ->
  self() ! H,
  sendBack(T).


