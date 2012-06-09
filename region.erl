-module(region).
-compile(export_all).

-record(region,
        { point = {0, 0},
          side_length,
          neighbors,
          particles = [],
          particle_threshold,
          subdivision_counter }
).

loop() ->
  receive
    {move, Pid, {Axis, Sign}} ->
      {_, XY} = find_particle(Pid),
      NewXY = new_xy(XY, Axis, Sign),
      % if NewXY in self then check internal_collision
      %   if no collision, update particle coordinate
      %   else notify_particles of collision
      % else notify neighbor;
  end.

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
