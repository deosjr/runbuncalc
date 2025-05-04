:- dynamic box/1.

run10box1 :-
    retractall(box(_)),
    Chimchar = #{ability:"Vital Spirit",ivs:_{atk:31,def:23,hp:31,spa:3,spd:3,spe:31},level:12,moves:["Scratch","Leer","Ember","Mach Punch"],name:"Chimchar",nature:"Quirky"},
    Lillipup = #{ability:"Vital Spirit",ivs:_{atk:19,def:0,hp:15,spa:11,spd:31,spe:26},level:12,moves:["Leer","Tackle","Baby-Doll Eyes","Bite"],name:"Lillipup",nature:"Gentle"},
    Finneon = #{ability:"Swift Swim",ivs:_{atk:26,def:9,hp:5,spa:18,spd:22,spe:4},level:12,moves:["Pound","Water Gun","Gust"],name:"Finneon",nature:"Timid"},
    Growlithe = #{ability:"Intimidate",ivs:_{atk:29,def:18,hp:5,spa:2,spd:16,spe:24},level:12,moves:["Ember","Bite","Covet","Flame Wheel"],name:"Growlithe",nature:"Mild"},
    Gossifleur = #{ability:"Cotton Down",ivs:_{atk:20,def:10,hp:5,spa:15,spd:1,spe:5},level:12,moves:["Leafage","Sing","Rapid Spin","Magical Leaf"],name:"Gossifleur",nature:"Relaxed"},
    Rookidee = #{ability:"Unnerve",ivs:_{atk:4,def:5,hp:13,spa:24,spd:13,spe:30},level:12,moves:["Peck","Leer","Fury Attack","Sand Attack"],name:"Rookidee",nature:"Bashful"},
    assertz(box([Chimchar, Lillipup, Finneon, Growlithe, Gossifleur, Rookidee])).

:- begin_tests(run10_start, [setup(run10box1)]).

test(youngster_calvin, [nondet]) :-
    box(Box),
    opponent('Youngster Calvin', Calvin),
    find_line_less_naive(Box, Calvin, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Chimchar", "Growlithe", "Growlithe"]),
    % Okay, Chimchar does guaranteed one-shot Poochyena, but this line needs a switch
    % What if we want to minimize the total damage taken on the entire team?
    % Perhaps Chimchar/Growlithe soloing would be better. Can they?
    % And what if want to account for sand-attack/swagger making us switch?
    Calvin = [Poochyena, Lillipup, Rookidee],
    get_pokemon_by_name("Chimchar", Box, Chimchar),
    lines_1v1(Chimchar, Poochyena, Lines),
    % Chimchar mach punches Poochyena to death. Note it sees its dead and tries to Quick Attack
    % but Punch is also +1 speed. This is _not_ reflected in lines_1v1 logic yet though!
    assertion(Lines = [[res(_, "Mach Punch", _, none)]]),
    post_ko_switch_in(Chimchar, [Lillipup, Rookidee], [Next|_]),
    assertion(Next == Lillipup),    % both are outsped/outdamaged, so party order applies
    lines_1v1(Chimchar, Lillipup, Lines2),
    % Chimchar cannot OHKO Lillipup, so it gets either Tackle/Bite off.
    % We dont consider status moves yet; they just count as 0-damage moves atm.
    % Duplicate lines are due to Lillipup trying to Tackle/Bite at the end and fainting before it can
    assertion(Lines2 = [[res(_, "Mach Punch", _, "Tackle"), res(_, "Mach Punch", _, none)],
                        [res(_, "Mach Punch", _, "Tackle"), res(_, "Mach Punch", _, none)],
                        [res(_, "Mach Punch", _, "Bite"), res(_, "Mach Punch", _, none)],
                        [res(_, "Mach Punch", _, "Bite"), res(_, "Mach Punch", _, none)]]),
    % TODO: Chimchar starts damaged vs Rookidee (but it wont matter)!
    lines_1v1(Chimchar, Rookidee, Lines3),
    assertion(Lines3 = [[res(_, "Ember", _, "Wing Attack"), res(_, "Ember", _, none)]]).
    % ACTUAL: we took some sand attacks but never missed, and never had to switch

test(bug_catcher_rick, [nondet]) :-
    box(Box),
    opponent('Bug Catcher Rick', Rick),
    find_line_less_naive(Box, Rick, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Growlithe", "Chimchar", "Chimchar"]),
    % This one is the other way around: Growlithe should solo here.
    % The first switch is especially dumb, since Growlithe also solo kills but Chimchar is first in party order
    Rick = [Grubbin, Pineco, Sizzlipede],
    get_pokemon_by_name("Growlithe", Box, Growlithe),
    lines_1v1(Growlithe, Grubbin, Lines),
    assertion(Lines = [[res(_, "Flame Wheel", _, none)],[res(_, "Flame Wheel", _, none)]]),
    post_ko_switch_in(Growlithe, [Pineco, Sizzlipede], [Next|_]),
    assertion(Next == Pineco),    % Sizzlipede is not OHKOd, but Pineco isnt either because it is Sturdy
    lines_1v1(Growlithe, Pineco, Lines2),
    assertion(Lines2 = [[res(_, "Ember", _, "Pin Missile"), res(_, "Ember", _, none)],
                        [res(_, "Ember", _, "Pin Missile"), res(_, "Flame Wheel", _, none)],
                        [res(_, "Flame Wheel", _, "Pin Missile"), res(_, "Ember", _, none)],
                        [res(_, "Flame Wheel", _, "Pin Missile"), res(_, "Flame Wheel", _, none)]]).
    % TODO: Growlithe starts damaged vs Sizzlipede (but it wont matter)!
    % The amount of lines vs Sizzlipede gets large, and we arent considering crits nor Oran Berries yet!
    % But all lines have one thing in common: spam Bite until it faints.
    % ACTUAL: Growlithe indeed solos the fight

test(youngster_allen, [nondet]) :-
    box(Box),
    opponent('Youngster Allen', Allen),
    find_line_less_naive(Box, Allen, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Growlithe", "Chimchar", "Gossifleur"]).
    % Growlithe should stay in vs Litleo because it can just spam Bite, otherwise no notes.

% Lass Tiana: solod by Growlithe

:- end_tests(run10_start).

run10box2 :-
    retractall(box(_)),
    Chimchar = #{ability:"Vital Spirit",ivs:_{atk:31,def:23,hp:31,spa:3,spd:3,spe:31},level:12,moves:["Scratch","Leer","Ember","Mach Punch"],name:"Chimchar",nature:"Quirky"},
    Lillipup = #{ability:"Vital Spirit",ivs:_{atk:19,def:0,hp:15,spa:11,spd:31,spe:26},level:12,moves:["Leer","Tackle","Baby-Doll Eyes","Bite"],name:"Lillipup",nature:"Gentle"},
    Finneon = #{ability:"Swift Swim",ivs:_{atk:26,def:9,hp:5,spa:18,spd:22,spe:4},level:12,moves:["Pound","Water Gun","Gust"],name:"Finneon",nature:"Timid"},
    Growlithe = #{ability:"Intimidate",ivs:_{atk:29,def:18,hp:5,spa:2,spd:16,spe:24},level:12,moves:["Ember","Bite","Covet","Flame Wheel"],name:"Growlithe",nature:"Mild"},
    Gossifleur = #{ability:"Cotton Down",ivs:_{atk:20,def:10,hp:5,spa:15,spd:1,spe:5},level:12,moves:["Leafage","Sing","Rapid Spin","Magical Leaf"],name:"Gossifleur",nature:"Relaxed"},
    Rookidee = #{ability:"Unnerve",ivs:_{atk:4,def:5,hp:13,spa:24,spd:13,spe:30},level:12,moves:["Peck","Leer","Fury Attack","Sand Attack"],name:"Rookidee",nature:"Bashful"},
    Lotad = #{ability:"Swift Swim",ivs:_{atk:14,def:9,hp:6,spa:27,spd:19,spe:1},level:12,moves:["Astonish","Natural Gift","Absorb","Bubble"],name:"Lotad",nature:"Hasty"},
    Vivillon = #{ability:"Shield Dust",ivs:_{atk:25,def:3,hp:18,spa:26,spd:20,spe:10},level:12,moves:["Air Cutter","String Shot","Stun Spore","Protect"],name:"Vivillon",nature:"Hardy"},
    assertz(box([Chimchar, Lillipup, Finneon, Growlithe, Gossifleur, Rookidee, Lotad, Vivillon])).

:- begin_tests(run10_petalburg, [setup(run10box2)]).

test(triathlete_mikey, [nondet]) :-
    box(Box),
    opponent('Triathlete Mikey', Mikey),
    find_line_less_naive(Box, Mikey, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Vivillon", "Vivillon", "Vivillon"]).
    % easiest fight ever

% Fisherman Darian still bugged to only have one Magikarp...
% Vivillon solos with Protect vs Bounce

test(lady_cindy, [nondet]) :-
    box(Box),
    opponent('Lady Cindy', Cindy),
    find_line_less_naive(Box, Cindy, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Vivillon", "Growlithe", "Finneon"]).
    % TODO

:- end_tests(run10_petalburg).