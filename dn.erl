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
                         side_length = 256,
                         neighbors = [],
                         specks = [],
                         particle_threshold = 10,
                         subdivision_counter = 3 },
  Region = spawn(region, loop, [RegionState, false]),
  SpeckStates = [{#move{ slope = { 2,  1},
                         dir   = { 1, -1},
                         state = { 0,  0},
                         time  =  75     }, { 2,  2}},
                 {#move{ slope = { 2,  1},
                         dir   = { 1, -1},
                         state = { 0,  0},
                         time  = 100     }, { 2,  4}},
                 {#move{ slope = { 2,  1},
                         dir   = { 1, -1},
                         state = { 0,  0},
                         time  =  50     }, { 2,  6}},
                 {#move{ slope = { 2,  1},
                         dir   = { 1, -1},
                         state = { 0,  0},
                         time  =  65     }, { 2,  8}},
                 {#move{ slope = { 2,  1},
                         dir   = { 1, -1},
                         state = { 0,  0},
                         time  =  80     }, { 2, 10}} | make_so_many(250)],
  add_specks(Region, SpeckStates),
  Region ! {start_log, "fleem.json", 13000, 25},
  timer:send_after(14500, Region, kill),
  Region.

make_so_many(N) -> make_so_many(N, 40, []).

make_so_many(N, 80, List) ->
        make_so_many(N, 40, List);
make_so_many(0, Sleep, List) ->
        List;
make_so_many(Count, Sleep, List) ->
        NewList = [{#move{ slope={2, 3}, dir = {1, 1}, state={0, 0}, time=Sleep}, {4, Count}} | List],
        make_so_many(Count - 1, Sleep + 1, NewList).

add_specks(_, []) -> ok;
add_specks(Region, [{SpeckState, XY} | SpeckStates]) ->
  Region ! {new_speck, SpeckState, XY},
  add_specks(Region, SpeckStates).


