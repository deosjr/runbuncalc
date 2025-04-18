:- begin_tests(run5).

test(youngster_calvin, [nondet]) :-
    Ponyta = #{ability:"Flame Body", ivs:_{atk:0, def:19, hp:9, spa:20, spd:29, spe:21}, level:12, moves:["Growl", "Stomp", "Tail Whip", "Ember"], name:"Ponyta", nature:"Impish"},
    Chimchar = #{ability:"Vital Spirit", ivs:_{atk:26, def:31, hp:2, spa:29, spd:31, spe:31}, level:12, moves:["Scratch", "Leer", "Ember", "Mach Punch"], name:"Chimchar", nature:"Modest"},
    Delcatty = #{ability:"Normalize", ivs:_{atk:7, def:9, hp:15, spa:20, spd:28, spe:14}, level:12, moves:["Fake Out", "Double Slap", "Wake-Up Slap", "Sing"], name:"Delcatty", nature:"Serious"},
    Tympole = #{ability:"Poison Touch", ivs:_{atk:17, def:29, hp:29, spa:1, spd:5, spe:26}, level:12, moves:["Bubble", "Growl", "Mud-Slap", "Echoed Voice"], name:"Tympole", nature:"Mild"},
    Exeggcute = #{ability:"Chlorophyll", ivs:_{atk:29, def:27, hp:10, spa:13, spd:18, spe:6}, level:12, moves:["Bullet Seed", "Confusion", "Hypnosis", "Stun Spore"], name:"Exeggcute", nature:"Jolly"},
    Fletchling = #{ability:"Keen Eye", ivs:_{atk:4, def:19, hp:8, spa:18, spd:10, spe:1}, level:12, moves:["Tackle", "Growl", "Quick Attack", "Aerial Ace"], name:"Fletchling", nature:"Timid"},
    Box = [Ponyta, Chimchar, Delcatty, Tympole, Exeggcute, Fletchling],
    opponent('Youngster Calvin', YoungsterCalvinTeam),
    % find Delcatty as the only pokemon that fast-kills the entire team (with Wake-Up Slap, but we dont know that ahead of time)
    include([P]>>(maplist([X]>>(fast_kill_guaranteed(P,X)), YoungsterCalvinTeam)), Box, Solo),
    assertion(Solo == [Delcatty]).

test(bug_catcher_rick, [nondet]) :-
    Ponyta = #{ability:"Flame Body", ivs:_{atk:0, def:19, hp:9, spa:20, spd:29, spe:21}, level:12, moves:["Growl", "Stomp", "Tail Whip", "Ember"], name:"Ponyta", nature:"Impish"},
    opponent('Bug Catcher Rick', [Grubbin, Pineco, Sizzlipede]),
    assertion(fast_kill_guaranteed(Ponyta, Grubbin, "Ember")),
    post_ko_switch_in(Ponyta, [Pineco, Sizzlipede], SwitchinsPostGrubbin),
    assertion(SwitchinsPostGrubbin == [Sizzlipede]).
    % Ponyta then 3-shots Sizzlipede with Stomp (remember, Oran Berry, so no 2HKO) and 2-shots Pineco with Ember (remember, Sturdy, unless we burn it)
    % ACTUAL: Pineco comes out before Sizzlipede ?!? We do burn it but it has an Oran Berry too.
    % (from the docs): No, the AI does not understand it has stat lowering abilities like Intimidate when determining the switch in.
    % It looks at the damage you do to it as if it were to be unchanged if it entered battle. It does however understand that Sturdy prevents it from getting OHKOed. (!)

:- end_tests(run5).