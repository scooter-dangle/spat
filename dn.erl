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
    _ -> true
  end.


sleep(T) ->
  receive
  after T -> true
  end.

