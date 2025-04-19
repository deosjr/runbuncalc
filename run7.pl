:- dynamic box/1.

run7box1 :-
    retractall(box(_)),
    Piplup = #{ability:"Clear Body", ivs:_{atk:31, def:14, hp:16, spa:31, spd:24, spe:31}, level:12, moves:["Pound", "Growl", "Bubble", "Pluck"], name:"Piplup", nature:"Bashful"},
    Surskit = #{ability:"Swift Swim", ivs:_{atk:7, def:20, hp:17, spa:8, spd:21, spe:31}, level:12, moves:["Bubble", "Quick Attack", "Sweet Scent", "Bubble Beam"], name:"Surskit", nature:"Bashful"},
    Starly = #{ability:"Keen Eye", ivs:_{atk:4, def:31, hp:18, spa:3, spd:15, spe:22}, level:12, moves:["Tackle", "Growl", "Quick Attack", "Aerial Ace"], name:"Starly", nature:"Brave"},
    Houndour = #{ability:"Early Bird", ivs:_{atk:25, def:9, hp:5, spa:29, spd:20, spe:4}, level:12, moves:["Leer", "Ember", "Smog", "Bite"], name:"Houndour", nature:"Lonely"},
    Lotad = #{ability:"Rain Dish", ivs:_{atk:11, def:1, hp:14, spa:22, spd:11, spe:29}, level:12, moves:["Natural Gift", "Growl", "Absorb", "Bubble"], name:"Lotad", nature:"Serious"},
    Psyduck = #{ability:"Swift Swim", ivs:_{atk:9, def:28, hp:25, spa:8, spd:24, spe:15}, level:12, moves:["Confusion", "Scratch", "Tail Whip", "Water Gun"], name:"Psyduck", nature:"Lax"},
    assertz(box([Piplup, Surskit, Starly, Houndour, Lotad, Psyduck])).

:- begin_tests(run7_first_box, [setup(run7box1)]).

test(youngster_calvin, [nondet]) :-
    box(Box),
    opponent('Youngster Calvin', Calvin),
    find_line_naive(Box, Calvin, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    % This is very naive. Piplup first because it (everyone) sees a kill on Poochyena, then Houndour because only it has a range on the rest.
    % In practise, for this fight there is not really a problem, though Keen Eye Starly and Clear Body Piplup are nice against Sand-Attacks.
    assertion(Names == ["Piplup", "Houndour", "Houndour"]).
    % ACTUAL: Houndour just solos. 

test(bug_catcher_rick, [nondet]) :-
    box(Box),
    opponent('Bug Catcher Rick', Rick),
    find_line_naive(Box, Rick, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    % Here we see the difference between AI switchin logic and Nuzlocke runner logic.
    % Piplup sees a possible fast-kill on Grubbin, while Starly has a guaranteed fast-kill and should therefore lead.
    % Houndour is the other contender for lead with a fast-kill using Ember, but Starly is the best choice vs all three
    assertion(Names == ["Piplup", "Starly", "Piplup"]),
    find_line_less_naive(Box, Rick, Line2),
    maplist([X,Y]>>get_dict(name,X,Y), Line2, Names2),
    % NOTE: we dont see Pinecos Sturdy yet
    assertion(Names2 == ["Starly", "Starly", "Surskit"]).

test(youngster_allen, [nondet]) :-
    box(Box),
    opponent('Youngster Allen', Allen),
    find_line_naive(Box, Allen, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    % Starly has a fast kill on Skiddo. Piplup is a great switchin vs the rest.
    % Lotad is arguably better vs Psyduck, but nice line!
    assertion(Names == ["Starly", "Piplup", "Piplup"]).

test(lass_tiana, [nondet]) :-
    box(Box),
    opponent('Lass Tiana', Tiana),
    find_line_naive(Box, Tiana, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    % Metronome is such a wildcard it is ignored in calc, so (almost) everyone outspeeds/outdamages and we go in party order.
    % Here it would be interesting if line calculation could take average damage taken into account, ie Piplup will be damaged
    assertion(Names == ["Piplup", "Piplup"]).

:- end_tests(run7_first_box).

run7box2 :-
    retractall(box(_)),
    Piplup = #{ability:"Clear Body", ivs:_{atk:31, def:14, hp:16, spa:31, spd:24, spe:31}, level:12, moves:["Pound", "Growl", "Bubble", "Pluck"], name:"Piplup", nature:"Bashful"},
    Surskit = #{ability:"Swift Swim", ivs:_{atk:7, def:20, hp:17, spa:8, spd:21, spe:31}, level:12, moves:["Bubble", "Quick Attack", "Sweet Scent", "Bubble Beam"], name:"Surskit", nature:"Bashful"},
    Starly = #{ability:"Keen Eye", ivs:_{atk:4, def:31, hp:18, spa:3, spd:15, spe:22}, level:12, moves:["Tackle", "Growl", "Quick Attack", "Aerial Ace"], name:"Starly", nature:"Brave"},
    Houndour = #{ability:"Early Bird", ivs:_{atk:25, def:9, hp:5, spa:29, spd:20, spe:4}, level:12, moves:["Leer", "Ember", "Smog", "Bite"], name:"Houndour", nature:"Lonely"},
    Lotad = #{ability:"Rain Dish", ivs:_{atk:11, def:1, hp:14, spa:22, spd:11, spe:29}, level:12, moves:["Natural Gift", "Growl", "Absorb", "Bubble"], name:"Lotad", nature:"Serious"},
    Psyduck = #{ability:"Swift Swim", ivs:_{atk:9, def:28, hp:25, spa:8, spd:24, spe:15}, level:12, moves:["Confusion", "Scratch", "Tail Whip", "Water Gun"], name:"Psyduck", nature:"Lax"},
    Beedrill = #{ability:"Sniper", ivs:_{atk:4, def:14, hp:0, spa:5, spd:4, spe:12}, level:12, moves:["Bug Bite", "Fury Attack", "String Shot", "Twineedle"], name:"Beedrill", nature:"Bashful"},
    Shellos = #{ability:"Sticky Hold", ivs:_{atk:17, def:6, hp:29, spa:13, spd:24, spe:2}, level:12, moves:["Mud-Slap", "Mud Sport", "Water Pulse", "Mud Shot"], name:"Shellos", nature:"Relaxed"},
    assertz(box([Piplup, Surskit, Starly, Houndour, Lotad, Psyduck, Beedrill, Shellos])).

:- begin_tests(run7_petalburg, [setup(run7box2)]).

test(triathlete_mikey, [nondet]) :-
    box(Box),
    opponent('Triathlete Mikey', Mikey),
    find_line_naive(Box, Mikey, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    % Here we completely fail. Lotad should lead into Krabby, Piplup vs Clobbopus.
    % Starly/Houndour each get a >50% hit in vs Yanma with an Oran Berry.
    assertion(Names == ["Piplup", "Piplup", "Piplup"]).

%test(fisherman_darian, [nondet]) :-
    % TODO: Darian data is broken, Im only getting one Magikarp..
    % I lead Shellos and it solos the first one, then chips the second so Lotad can kill with Absorb.

test(lady_cindy, [nondet]) :-
    box(Box),
    opponent('Lady Cindy', Cindy),
    find_line_less_naive(Box, Cindy, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    % Not great, not terrible. I actually want to lead with Beedrill and Bug Bite all the berries.
    % Piplup can clean up with Pluck doing the same if Beedrill gets low, and can Bubble Phanpy.
    % Surskit can be backup in case we get attracted.
    assertion(Names == ["Surskit", "Piplup", "Piplup"]).

test(team_aqua_grunt, [nondet]) :-
    box(Box),
    opponent('Team Aqua Grunt Petalburg Woods', Grunt),
    find_line_less_naive(Box, Grunt, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    % I like Beedrill first and last (though note we think we are faster because of a speed tie).
    % Surskit again is not that great of a pick, Piplup should do much better vs Croagunk.
    % Switching becomes more interesting here, making sure we have healthy lead matchups.
    % Psyduck can tank FakeOut from Croagunk (it will come out, Beedrill outdamages/outspeeds both).
    % Scratch + Confusion will kill without proccing Salac berry (I almost missed that!).
    % This means Beedrill can come in on a Bullet Seed and kill with Bug Bite
    assertion(Names == ["Beedrill", "Surskit", "Beedrill"]).
    % ACTUAL: learned that bug bite triggers before rough skin.
    % Psyduck wasnt super low but still got a Confusion instead of Bullet Seed.

:- end_tests(run7_petalburg).