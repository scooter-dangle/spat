-module(region).
-compile(export_all).

-record(region, {neighbors, particles=[], particle_threshold, subdivision_counter}).

can_subdivide(Region) ->
  Region#region.subdivision_counter > 0.

need_to_subdivide(Region) ->
  length(Region#region.particles) >= Region#region.particle_threshold.

collision_detected(Region) ->
  need_to_subdivide(Region) and not(can_subdivide(Region)).
