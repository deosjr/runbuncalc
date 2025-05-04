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
    % Thunder Wave / Attract makes this harder to plan
    % Vivillon needs 3 turns to kill Minccino due to Oran Berry
    % Growlithe actually has a chance to 2HKO if it lowrolls its first Flame Wheel and highrolls the second
    % Similarly for Vivillon on Jigglypuff, so I might switch those two around.
    % Finneon vs Panphy is fine. Gossifleur is a nice second, though it is very slow.
    % Chimchar can come in on the first two mons if we get paralyzed/infatuated
    % Since only Vivillon is faster, I think only it should have a Cheri berry? Since T-wave will be more likely?

test(team_aqua_grunt_petalburg, [nondet]) :-
    box(Box),
    opponent('Team Aqua Grunt Petalburg Woods', Grunt),
    find_line_less_naive(Box, Grunt, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Gossifleur", "Vivillon", "Vivillon"]),
    % Gossifleur has a 50% chance to slow-kill Carvanha with Magical Leaf, taking no Rough Skin damage.
    % If Carvanha lives, it will trigger Cotton Down and be at -1 speed afterwards.
    % It will see that it is dead and therefore Aqua Jet. Finneon can switch in and finish up if needed.
    % Gossifleur gets Croagunk while Finneon baits Exeggcute, I think.
    % Croagunk gets Rookidee chipping with Peck, then Vivillon to OHKO it before berry can trigger
    % Exeggcute gets Vivillon immediately. If Croagunk is left, still Rookidee+Vivillon finisher as the plan.
    % ACTUAL: Carvanha Bites, we flinch, and we have to steer.. almost lost a few pokemon here
    % My mistake here was thinking Carvanha always goes for Poison Fang!
    % So lets do some after-the-fact calculations:
    Grunt = [Carvanha, Croagunk, Exeggcute],
    get_pokemon_by_name("Gossifleur", Box, Gossifleur),
    lines_1v1(Gossifleur, Carvanha, Lines),
    % NOTE that this shouldve told me I missed something, but doesnt take flinching into account yet
    assertion(Lines = [[res(_, "Magical Leaf", _, "Bite"), res(_, none, _, "Bite")],
                       [res(_, "Magical Leaf", _, "Bite"), res(_, none, _, "Poison Fang")],
                       [res(_, "Magical Leaf", _, "Poison Fang"), res(_, none, _, "Bite")],
                       [res(_, "Magical Leaf", _, "Poison Fang"), res(_, none, _, "Poison Fang")]]),
    post_ko_switch_in(Gossifleur, [Croagunk, Exeggcute], [Next|_]),
    assertion(Next == Croagunk).    % even if Gossifleur is heavily damaged, this doesnt change
    % Not sure about Croagunk Belch damage counting when looking at switch-in logic?

:- end_tests(run10_petalburg).

run10box3 :-
    retractall(box(_)),
    Box = [
    #{ability:"Swift Swim",ivs:_{atk:26,def:9,hp:5,spa:18,spd:22,spe:4},level:17,moves:["Pound","Water Gun","Gust","Water Pulse"],name:"Finneon",nature:"Timid"},
    #{ability:"Intimidate",ivs:_{atk:19,def:0,hp:15,spa:11,spd:31,spe:26},level:17,moves:["Leer","Covet","Baby-Doll Eyes","Bite"],name:"Herdier",nature:"Gentle"},
    #{ability:"Swift Swim",ivs:_{atk:14,def:9,hp:6,spa:27,spd:19,spe:1},level:17,moves:["Fake Out","Natural Gift","Mega Drain","Bubble"],name:"Lombre",nature:"Hasty"},
    #{ability:"Guts",ivs:_{atk:22,def:24,hp:24,spa:17,spd:11,spe:14},level:17,moves:["Pound","Leer","Rock Throw","Low Kick"],name:"Timburr",nature:"Hardy"},
    #{ability:"Oblivious",ivs:_{atk:26,def:21,hp:5,spa:5,spd:25,spe:27},level:17,moves:["Charm","Brine","Powder Snow","Rollout"],name:"Spheal",nature:"Hardy"},
    #{ability:"Rough Skin",ivs:_{atk:26,def:20,hp:5,spa:28,spd:14,spe:26},level:17,moves:["Water Pulse","Bite","Rage","Aqua Jet"],name:"Carvanha",nature:"Modest"},
    #{ability:"Shield Dust",ivs:_{atk:25,def:3,hp:18,spa:26,spd:20,spe:10},level:17,moves:["Air Cutter","Struggle Bug","Stun Spore","Protect"],name:"Vivillon",nature:"Hardy"},
    #{ability:"Cotton Down",ivs:_{atk:20,def:10,hp:5,spa:15,spd:1,spe:5},level:17,moves:["Round","Sing","Rapid Spin","Magical Leaf"],name:"Gossifleur",nature:"Relaxed"},
    #{ability:"Vital Spirit",ivs:_{atk:31,def:23,hp:31,spa:3,spd:3,spe:31},level:17,moves:["Low Sweep","Leer","Flame Wheel","Mach Punch"],name:"Monferno",nature:"Quirky"},
    #{ability:"Intimidate",ivs:_{atk:29,def:18,hp:5,spa:2,spd:16,spe:24},level:17,moves:["Ember","Bite","Covet","Fire Fang"],name:"Growlithe",nature:"Mild"},
    #{ability:"Unnerve",ivs:_{atk:4,def:5,hp:13,spa:24,spd:13,spe:30},level:17,moves:["Pluck","Leer","Fury Attack","Sand Attack"],name:"Rookidee",nature:"Bashful"}
    ],
    assertz(box(Box)).

:- begin_tests(run10_dewford, [setup(run10box3)]).

test(fisherman_elliot, [nondet]) :-
    box(Box),
    opponent('Fisherman Elliot', Elliot),
    find_line_less_naive(Box, Elliot, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Lombre", "Vivillon", "Vivillon"]),
    Elliot = [Staryu, OppLombre, Arrokuda],
    % Lombre vs Staryu should only ever be FakeOut + Mega Drain, but our logic doesnt know about flinch
    % Vivillon vs Lombre is also interesting, lines_1v1 suggests Air Cutter or Struggle Bug
    % but only Air Cutter is a range to kill so we should never go for Struggle Bug here.
    % Same thing vs Arrokuda
    get_pokemon_by_name("Vivillon", Box, Vivillon),
    lines_1v1(Vivillon, Arrokuda, Lines),
    assertion(Lines = [[res(_, "Air Cutter", _, "Peck"), res(_, "Air Cutter", _, none)],
                       [res(_, "Air Cutter", _, "Peck"), res(_, "Struggle Bug", _, none)],
                       [res(_, "Struggle Bug", _, "Peck"), res(_, "Air Cutter", _, none)],
                       [res(_, "Struggle Bug", _, "Peck"), res(_, "Struggle Bug", _, none)]]),
    % lets check to make sure who comes out after Staryu. All its attacks deal exactly the same range of damage
    get_pokemon_by_name("Lombre", Box, Lombre),
    highest_damage_move(Staryu, Lombre, Move),
    lowRoll(Staryu, Lombre, false, Move, Low),
    highRoll(Staryu, Lombre, false, Move, High),
    lowRoll(Staryu, Lombre, true, Move, LowCrit),
    highRoll(Staryu, Lombre, true, Move, HighCrit),
    calculate(Staryu, Lombre, Move, Data),
    LombreDmgLow is Data.defender.originalCurHP - Low,
    LombreLowDmgd = Lombre.put(_{curHP: LombreDmgLow}),
    post_ko_switch_in(LombreLowDmgd, [OppLombre, Arrokuda], [NextLow|_]),
    assertion(NextLow == OppLombre),
    LombreDmgHigh is Data.defender.originalCurHP - High,
    LombreHighDmgd = Lombre.put(_{curHP: LombreDmgHigh}),
    post_ko_switch_in(LombreHighDmgd, [OppLombre, Arrokuda], [NextHigh|_]),
    assertion(NextHigh == OppLombre),
    LombreDmgLowCrit is Data.defender.originalCurHP - LowCrit,
    LombreLowCritDmgd = Lombre.put(_{curHP: LombreDmgLowCrit}),
    post_ko_switch_in(LombreLowCritDmgd, [OppLombre, Arrokuda], [NextLowCrit|_]),
    assertion(NextLowCrit == OppLombre),
    LombreDmgHighCrit is Data.defender.originalCurHP - HighCrit,
    LombreHighCritDmgd = Lombre.put(_{curHP: LombreDmgHighCrit}),
    post_ko_switch_in(LombreHighCritDmgd, [OppLombre, Arrokuda], [NextHighCrit|_]),
    assertion(NextHighCrit == OppLombre).
    % its all Lombre, no matter the damage. Our Lombre outdamages both but doesnt outspeed any.
    % A Staryu crit deals 27% max, nowhere near enough for Arrokuda to see a kill with Peck
    % And that is not even accounting for Mega Drain regaining health (probably guaranteed to get all back)

test(ruin_maniac_georgie, [nondet]) :-
    box(Box),
    opponent('Ruin Maniac Georgie', Georgie),
    find_line_less_naive(Box, Georgie, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Carvanha", "Lombre", "Growlithe", "Timburr"]).
    % I think Mawile comes out on Carvanha, but other than that this team should work fine.
    % I do prefer Monferno over Timburr as a counter to Munchlax since its faster, but shouldnt matter.

test(tuber_chandler, [nondet]) :-
    box(Box),
    opponent('Tuber Chandler', Chandler),
    find_line_less_naive(Box, Chandler, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Carvanha", "Herdier", "Monferno"]).
    % This should work. Monferno also fast-kills Smoochum so Carvanha isnt even needed.

test(tuber_lola, [nondet]) :-
    box(Box),
    opponent('Tuber Lola', Lola),
    find_line_less_naive(Box, Lola, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Carvanha", "Monferno"]),
    % Carvanha has a range to slow-kill Fletchinder with Water Pulse, made easier because it will also trigger Rough Skin.
    % It lives a crit, but might die to crit + burn damage if it tries to finish with Aqua Jet and Flame Body triggers.
    % If that happens we would like to pivot to Monferno but without taking more Flying attacks.
    Lola = [Fletchinder, Herdier],
    get_pokemon_by_name("Carvanha", Box, Carvanha),
    get_pokemon_by_name("Monferno", Box, Monferno),
    get_pokemon_by_name("Spheal", Box, Spheal),
    get_pokemon_by_name("Gossifleur", Box, Gossifleur),
    get_pokemon_by_name("Vivillon", Box, Vivillon),
    pivot(Fletchinder, Carvanha, Monferno, Box, Via),
    assertion(Via == Spheal), % unfortunately, there is no one like that. Spheal can at least tank/finish
    pivot(Herdier, Carvanha, Monferno, Box, Via2),
    assertion(Via2 == Gossifleur),
    select(Spheal, Box, NoSphealBox),       % should remove others from Box too, but Im lazy. This was the only clash
    pivot(Herdier, Spheal, Monferno, NoSphealBox, Via3),
    assertion(Via3 == Gossifleur),
    select(Spheal, Box, NoSphealBox),       % should remove others from Box too, but Im lazy. This was the only clash
    % Pivotting via Gossifleur is possible but baits Ice Fang, and risks being frozen.
    select(Gossifleur, Box, NoGossifleurBox),
    pivot(Herdier, Carvanha, Monferno, NoGossifleurBox, Via4),
    assertion(Via4 == Vivillon).            % that doesnt help. We dont have a better way it seems
    % Oh well, Low Sweep + Mach Punch should kill before 2x Headbutt does unless both maxroll crit

test(sailor_edmond, [nondet]) :-
    box(Box),
    opponent('Sailor Edmond', Edmond),
    find_line_less_naive(Box, Edmond, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Vivillon", "Vivillon", "Lombre"]).
    % maybe? Id like to go for something slightly more complicated:
    % Rookidee tanks a Shock Wave and Plucks the oran berry.
    % Switch to Gossifleur to tank another Shock Wave, lowering speed, then Herdier to finish
    % Buizel should be next, Herdier can chip, then switch to Lombre to finish. Monferno in the back if needed, but risky
    % Palpitoad is hard countered by Vivillon.
    % ACTUAL: Rookidee solos Wingull as it goes for Rain Dance. Lombre solos Buizel in the rain.
    % Lombre can stay in vs Palpitoad as it has just enough HP to survive crit Sludge, and kills with Mega Drain.

% Fisherman Bill gets solod by Vivillon.

test(tuber_ricky, [nondet]) :-
    box(Box),
    opponent('Tuber Ricky', Ricky),
    find_line_less_naive(Box, Ricky, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Monferno", "Vivillon", "Monferno"]).
    % Vivillon would be nice vs Aipom, protecting vs Fake Out, but we need it vs Nidorino.
    % Monferno is amazing vs Luxio so we are looking for an Aipom counter.
    % Growlithe with Intimidate does okay, and if needed Herdier with another Intimidate can finish
    % Herdier baits Nidorino with Double Kick which is perfect, so maybe we try that anyways
    % ACTUAL: swapped Growlite and Herdier on Aipom until it was at -4, worked great

test(tuber_hailey, [nondet]) :-
    box(Box),
    opponent('Tuber Hailey', Hailey),
    find_line_less_naive(Box, Hailey, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Vivillon", "Herdier", "Herdier"]).
    % Herdier cannot 1v1 Nidorina, but Vivillon can chip a lot first.
    % Flaaffy can then be handled by Growlithe/Monferno/Lombre
    % ACTUAL: Flaaffy comes out before Nidorina, but we steer just fine.

:- end_tests(run10_dewford).