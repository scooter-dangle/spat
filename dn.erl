-module(dn).
-compile(export_all).

-record(move, {slope, dir, state, time}).

-record(region,
        { point = {0, 0},
          side_length,
          neighbors,
          specks = [],
          particle_threshold,
          subdivision_counter }).


foo() ->
  RegionState = #region{ point = {0, 0},
                         side_length = 16,
                         neighbors = [],
                         specks = [],
                         particle_threshold = 10,
                         subdivision_counter = 3 },
  Region = spawn(region, loop, [RegionState]),
  SpeckState = #move{ slope = {2, 1},
                      dir   = {1, -1},
                      state = {0, 0},
                      time  = 1500 },
  Region ! {new_speck, SpeckState, {2, 2}}.

