-module(dn).
-compile(export_all).

-record(speck, {pid, xy, move = #move{}}).



in_self(NewXY, #region{Origin=point, SideLength=side_length}) ->
  .

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


