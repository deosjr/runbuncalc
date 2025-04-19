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
    assertion(Names == ["Piplup", "Starly", "Piplup"]).

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