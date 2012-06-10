-module(dn).
-compile(export_all).

-record(speck, {pid, xy, move = #move{}}).







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


