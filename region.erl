-module(region).
-compile(export_all).

-record(region,
        { origin = {0, 0},
          side_length,
          neighbors,
          particles = [],
          particle_threshold,
          subdivision_counter }).

loop(State) ->
  receive
    {move, Pid, Direction} ->
      {_, XY} = find_particle(Pid, State#region.particles),
      NewXY = new_xy(XY, Direction),
      % if NewXY in self then check internal_collision
      %   if no collision, update particle coordinate
      %   else notify_particles of collision
      % else notify neighbor;
  end.


find_particle(Pid, Particles) ->
  lists:keymap(Pid, 1, Particles).


new_xy({X, Y}, {x, Sign}) -> {X + Sign, Y};
new_xy({X, Y}, {y, Sign}) -> {X, Y + Sign}.


in_self(NewXY={X, Y},
        #region{point={OriginX, OriginY}, side_length=SideLength}) ->
  (X < OriginX + SideLength andalso Y < OriginY + SideLength)
    andalso (X >= OriginX andalso Y >= OriginY).

can_subdivide(Region) ->
  Region#region.subdivision_counter > 0.

need_to_subdivide(Region) ->
  length(Region#region.particles) >= Region#region.particle_threshold.

collision_detected(Region) ->
  need_to_subdivide(Region) and not(can_subdivide(Region)).

subdivide(Region) ->
  % spawn four new processes
  % 
  % stuff
  0.

notify_neighbor_of_divide(Neighbor, NewRegions) ->
  % stuff
  0.
