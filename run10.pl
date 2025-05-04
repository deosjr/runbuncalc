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
    assertion(Lines = [[Chimchar, "Mach Punch", _]]),
    post_ko_switch_in(Chimchar, [Lillipup, Rookidee], [Next|_]),
    assertion(Next == Lillipup),    % both are outsped/outdamaged, so party order applies
    lines_1v1(Chimchar, Lillipup, Lines2),
    % Chimchar cannot OHKO Lillipup, so it gets either Tackle/Bite off.
    % We dont consider status moves yet; they just count as 0-damage moves atm.
    assertion(Lines2 = [[_, "Mach Punch", _, "Tackle"], [_, "Mach Punch", _, "Bite"]]),
    % TODO: Chimchar starts damaged vs Rookidee!
    lines_1v1(Chimchar, Rookidee, Lines3),
    assertion(Lines3 = [[_, "Ember", _, "Wing Attack"]]).

:- end_tests(run10_start).