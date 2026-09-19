:- dynamic box/1.

% this run I save and reload if fights go wrong, so not a real nuzlocke
% lets gather some more info on how to properly handle fights

run11box1 :-
    retractall(box(_)),
    Turtwig = #{ability:"Shell Armor",ivs:_10734{atk:31,def:31,hp:26,spa:18,spd:31,spe:0},level:12,moves:["Bite","Growl","Absorb","Confide"],name:"Turtwig",nature:"Quirky"},
    Lotad = #{ability:"Rain Dish",ivs:_10600{atk:6,def:12,hp:18,spa:5,spd:10,spe:1},level:12,moves:["Natural Gift","Growl","Absorb","Bubble"],name:"Lotad",nature:"Docile"},
    Lillipup = #{ability:"Run Away",ivs:_10468{atk:16,def:29,hp:9,spa:2,spd:1,spe:4},level:12,moves:["Leer","Tackle","Baby-Doll Eyes","Bite"],name:"Lillipup",nature:"Relaxed"},
    Houndour = #{ability:"Early Bird",ivs:_10332{atk:17,def:27,hp:31,spa:4,spd:20,spe:12},level:12,moves:["Leer","Ember","Smog","Bite"],name:"Houndour",nature:"Brave"},
    Skrelp = #{ability:"Poison Point",ivs:_10200{atk:28,def:28,hp:13,spa:28,spd:8,spe:8},level:12,moves:["Water Gun","Smokescreen","Feint Attack","Smog"],name:"Skrelp",nature:"Lax"},
    Rookidee = #{ability:"Unnerve",ivs:_10064{atk:26,def:1,hp:3,spa:4,spd:15,spe:26},level:12,moves:["Peck","Leer","Fury Attack","Sand Attack"],name:"Rookidee",nature:"Lonely"},
    assertz(box([Turtwig, Lotad, Lillipup, Houndour, Skrelp, Rookidee])).

:- begin_tests(run11_start, [setup(run11box1)]).

test(youngster_calvin, [nondet]) :-
    box(Box),
    opponent('Youngster Calvin', Calvin),
    find_line_less_naive(Box, Calvin, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Houndour", "Skrelp", "Houndour"]),
    % the switch to Skrelp takes extra damage for no reason, Houndour should sweep.
    find_line_sticky(Box, Calvin, Sticky),
    maplist([X,Y]>>get_dict(name,X,Y), Sticky, StickyNames),
    assertion(StickyNames == ["Houndour", "Houndour", "Houndour"]).

test(bug_catcher_rick, [nondet]) :-
    box(Box),
    opponent('Bug Catcher Rick', Rick),
    find_line_less_naive(Box, Rick, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Houndour", "Skrelp", "Skrelp"]),
    % I dont understand the fascination with Skrelp. Again Houndour just sweeps.
    % Skrelp only wins the tiebreak on Pineco because it takes a few percent less
    % from a Pineco nobody can OHKO through Sturdy, and the switch is never charged for.
    find_line_sticky(Box, Rick, Sticky),
    maplist([X,Y]>>get_dict(name,X,Y), Sticky, StickyNames),
    assertion(StickyNames == ["Houndour", "Houndour", "Houndour"]),
    % and it knows what staying in costs: Houndour enters Pineco at 36 and Sizzlipede at 32
    Sticky = [_|Switched],
    maplist([X,Y]>>get_dict(curHP,X,Y), Switched, HPs),
    assertion(HPs == [36, 32]).

test(youngster_allen, [nondet]) :-
    box(Box),
    opponent('Youngster Allen', Allen),
    find_line_less_naive(Box, Allen, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Houndour", "Skrelp", "Lillipup"]).
    % Houndour kills the Skiddo and Skrelp deals with the rest

% Lass Tiana: Turtwig solos, safe from crits and spamming Absorb

:- end_tests(run11_start).

run11box2 :-
    retractall(box(_)),
    Turtwig = #{ability:"Shell Armor",ivs:_10734{atk:31,def:31,hp:26,spa:18,spd:31,spe:0},level:12,moves:["Bite","Growl","Absorb","Confide"],name:"Turtwig",nature:"Quirky"},
    Lotad = #{ability:"Rain Dish",ivs:_10600{atk:6,def:12,hp:18,spa:5,spd:10,spe:1},level:12,moves:["Natural Gift","Growl","Absorb","Bubble"],name:"Lotad",nature:"Docile"},
    Lillipup = #{ability:"Run Away",ivs:_10468{atk:16,def:29,hp:9,spa:2,spd:1,spe:4},level:12,moves:["Leer","Tackle","Baby-Doll Eyes","Bite"],name:"Lillipup",nature:"Relaxed"},
    Houndour = #{ability:"Early Bird",ivs:_10332{atk:17,def:27,hp:31,spa:4,spd:20,spe:12},level:12,moves:["Leer","Ember","Smog","Bite"],name:"Houndour",nature:"Brave"},
    Skrelp = #{ability:"Poison Point",ivs:_10200{atk:28,def:28,hp:13,spa:28,spd:8,spe:8},level:12,moves:["Water Gun","Smokescreen","Feint Attack","Smog"],name:"Skrelp",nature:"Lax"},
    Rookidee = #{ability:"Unnerve",ivs:_10064{atk:26,def:1,hp:3,spa:4,spd:15,spe:26},level:12,moves:["Peck","Leer","Fury Attack","Sand Attack"],name:"Rookidee",nature:"Lonely"},
    Sizzlipede = #{ability:"White Smoke",ivs:_10998{atk:16,def:22,hp:24,spa:5,spd:25,spe:29},level:12,moves:["Ember","Smokescreen","Wrap","Bug Bite"],name:"Sizzlipede",nature:"Timid"},
    assertz(box([Turtwig, Lotad, Lillipup, Houndour, Skrelp, Rookidee, Sizzlipede])).

:- begin_tests(run11_petalburg, [setup(run11box2)]).

test(triathlete_mikey, [nondet]) :-
    box(Box),
    opponent('Triathlete Mikey', Mikey),
    find_line_less_naive(Box, Mikey, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Lillipup", "Sizzlipede", "Houndour"]).
    % Turtwig disposes of Krabby, then Yanma comes out. We chip with Bite, then switch to Rookidee with an Oran Berry.
    % on Clobbopus we switch to Skrelp who kills it easily.

% Fisherman Darian: soloed by Turtwig, even with Bounce paralyzing.

test(lady_cindy, [nondet]) :-
    box(Box),
    opponent('Lady Cindy', Cindy),
    find_line_less_naive(Box, Cindy, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Turtwig", "Houndour", "Sizzlipede"]).
    % This fight is kind of risky with so much RNG, but Skrelp w Poison Point allows us to stall it out

test(team_aqua_grunt_petalburg, [nondet]) :-
    box(Box),
    opponent('Team Aqua Grunt Petalburg Woods', Grunt),
    find_line_less_naive(Box, Grunt, Line),
    get_pokemon_by_name(["Sizzlipede", "Skrelp", "Houndour"], Box, [Sizzlipede, Skrelp, Houndour]),
    assertion(Line == [Sizzlipede, Skrelp, Houndour]),
    % Sizzlipede just dies to crit from Carvanha so is a terrible choice.
    % We lead Turtwig, expecting Exeggcute after. Sizzlipede is a better choice to deal with it,
    % setting up for switching Rookidee into Croagunk. Here it becomes slightly dicey but we have a lot of backup
    % ACTUAL: Croagunk comes in first, so we chip with Turtwig and switch to Rookidee. Skrelp survives Belch and finishes
    Grunt = [Carvanha, Croagunk, Exeggcute],
    % i.e. even without crits, Sizzlipede just dies to Carvanha in all lines..
    lines_1v1(Sizzlipede, Carvanha, Lines1),
    not(clear_winner(Lines1)),
    % compare Turtwig just spamming Absorb and winning
    get_pokemon_by_name("Turtwig", Box, Turtwig),
    lines_1v1(Turtwig, Carvanha, Lines2),
    clear_winner(Lines2),
    lines2pivots(Lines2, [Croagunk, Exeggcute], Pivots),
    % assuming opponent always highrolls and we lowroll, Croagunk is guaranteed to come out
    % note: doesnt take into account crits or any other variance!
    assertion(Pivots = [Croagunk]).

:- end_tests(run11_petalburg).

run11box3 :-
    retractall(box(_)),
    Spheal = #{ability:"Oblivious",ivs:_14904{atk:18,def:29,hp:0,spa:11,spd:29,spe:28},level:17,moves:["Charm","Brine","Powder Snow","Rollout"],name:"Spheal",nature:"Docile"},
    Lombre = #{ability:"Rain Dish",item:"Oran Berry",ivs:_14762{atk:6,def:12,hp:18,spa:5,spd:10,spe:1},level:17,moves:["Natural Gift","Fake Out","Mega Drain","Bubble"],name:"Lombre",nature:"Docile"},
    Grotle = #{ability:"Shell Armor",ivs:_14628{atk:31,def:31,hp:26,spa:18,spd:31,spe:0},level:17,moves:["Bite","Razor Leaf","Absorb","Sand Tomb"],name:"Grotle",nature:"Quirky"},
    Houndour = #{ability:"Early Bird",item:"Oran Berry",ivs:_14486{atk:17,def:27,hp:31,spa:4,spd:20,spe:12},level:17,moves:["Fire Fang","Ember","Smog","Bite"],name:"Houndour",nature:"Brave"},
    Skrelp = #{ability:"Poison Point",ivs:_14348{atk:28,def:28,hp:13,spa:28,spd:8,spe:8},level:17,moves:["Water Gun","Smokescreen","Feint Attack","Poison Tail"],name:"Skrelp",nature:"Lax"},
    Rookidee = #{ability:"Unnerve",ivs:_14210{atk:26,def:1,hp:3,spa:4,spd:15,spe:26},level:17,moves:["Pluck","Leer","Fury Attack","Sand Attack"],name:"Rookidee",nature:"Lonely"},
    Sizzlipede = #{ability:"White Smoke",item:"Oran Berry",ivs:_14066{atk:16,def:22,hp:24,spa:5,spd:25,spe:29},level:17,moves:["Ember","Smokescreen","Bite","Bug Bite"],name:"Sizzlipede",nature:"Timid"},
    Herdier = #{ability:"Sand Rush",item:"Oran Berry",ivs:_13918{atk:16,def:29,hp:9,spa:2,spd:1,spe:4},level:17,moves:["Leer","Covet","Baby-Doll Eyes","Bite"],name:"Herdier",nature:"Relaxed"},
    Stufful = #{ability:"Fluffy",ivs:_13778{atk:9,def:22,hp:20,spa:3,spd:22,spe:6},level:17,moves:["Stomp","Baby-Doll Eyes","Rock Smash","Brutal Swing"],name:"Stufful",nature:"Brave"},
    Horsea = #{ability:"Swift Swim",ivs:_13642{atk:6,def:29,hp:25,spa:21,spd:26,spe:14},level:17,moves:["Bubble Beam","Smokescreen","Leer","Twister"],name:"Horsea",nature:"Bashful"},
    assertz(box([Spheal, Lombre, Grotle, Herdier, Houndour, Skrelp, Rookidee, Sizzlipede, Stufful, Horsea])).

:- begin_tests(run11_dewford, [setup(run11box3)]).

test(fisherman_elliot, [nondet]) :-
    box(Box),
    opponent('Fisherman Elliot', Elliot),
    find_line_less_naive(Box, Elliot, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Lombre", "Rookidee", "Grotle"]).
    % Worked out fine with these three.

test(ruin_maniac_georgie, [nondet]) :-
    box(Box),
    opponent('Ruin Maniac Georgie', Georgie),
    find_line_less_naive(Box, Georgie, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Horsea", "Grotle", "Sizzlipede", "Stufful"]).
    % Again, these four suggestions worked out fine.

test(tuber_chandler, [nondet]) :-
    box(Box),
    opponent('Tuber Chandler', Chandler),
    find_line_less_naive(Box, Chandler, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Houndour", "Lombre", "Horsea"]).
    % used Grotle instead of Lombre, but worked out fine either way

test(tuber_lola, [nondet]) :-
    box(Box),
    opponent('Tuber Lola', Lola),
    find_line_less_naive(Box, Lola, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Horsea", "Stufful"]).
    % no notes

test(sailor_edmond, [nondet]) :-
    box(Box),
    opponent('Sailor Edmond', Edmond),
    find_line_less_naive(Box, Edmond, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Herdier", "Lombre", "Lombre"]).
    % didnt even need Grotle as a backup

% Fisherman Bill: I dont want to risk my Houndour, so I let Rookidee and Stufful wall and kill the bugs

test(tuber_ricky, [nondet]) :-
    box(Box),
    opponent('Tuber Ricky', Ricky),
    find_line_less_naive(Box, Ricky, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Herdier", "Skrelp", "Grotle"]).
    % Stufful seems better than Herdier vs a physical normal type
    % Can even stay in vs Nidorino, though Skrelp is useful

test(tuber_hailey, [nondet]) :-
    box(Box),
    opponent('Tuber Hailey', Hailey),
    find_line_less_naive(Box, Hailey, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Skrelp", "Stufful", "Stufful"]).
    % Skrelp is actually fine here, but Stufful needs help. Grotle walls Flaaffy

% lets try Gavi before the museum, we need a good electric type I think
test(camper_gavi_early, [nondet]) :-
    box(Box),
    opponent('Camper Gavi', Gavi),
    find_line_less_naive(Box, Gavi, Line),
    get_pokemon_by_name(["Grotle", "Skrelp", "Houndour", "Sizzlipede"], Box, [Grotle, Skrelp, Houndour, Sizzlipede]),
    assertion(Line == [Grotle, Skrelp, Houndour, Sizzlipede, Houndour]),
    Gavi = [Bibarel, Ponyta, Eelektrik, Sunflora, Dustox],
    lines_1v1(Grotle, Bibarel, Lines1),
    % this is true, but _only_ because Grotle has Shell Armor.
    % It wouldve died to a crit otherwise, so itd be a risky line
    clear_winner(Lines1),
    lines2pivots(Lines1, [Ponyta, Eelektrik, Sunflora, Dustox], Pivots),
    % makes sense, Ponyta is the first in line to see a fast kill on a weakened Grotle
    assertion(Pivots = [Ponyta]),
    % The pivot into Skrelp is fairly free, but our approach doesnt model the free hit the opponent gets in yet (!)
    lines_1v1(Skrelp, Ponyta, Lines2),
    clear_winner(Lines2),
    lines2pivots(Lines2, [Eelektrik, Sunflora, Dustox], Pivots2),
    assertion(Pivots2 = [Eelektrik]).
    % this is a guaranteed Shock Wave I think. Houndour will get blasted
    % Eelektrik hoses us completely, time to take a Granite Cave B1F encounter and pray for a steel type?
    % Or, hear me out, we lower Bibarels attack by 2 stages with Stufful using Baby-Doll Eyes.
    % Then Grotle can switch in and Razor Leaf once, so it will be locked into Aqua Tail and we can heal with Absorbs
    % This lets us switch a healthy Grotle into Eelektrik?
    % Since this is not a real nuzlocke run, I will just try that out now
    % ACTUAL: nope it will continue to use Super Fang? Ah, because it is faster...
    % However, Lombre can come in safely against Bibarel and kill it with Mega Drain to keep Grotle for Eelektrik
    % ACTUAL: yes that can win, but not consistently. Also Stufful can just Rock Smash Bibarel to death
    % Lombre also kills Bibarel with Fake Out + 2x Mega Drain, unless Bibarel crits

% So lets try museum first anyways :)
test(museum_grunts, [nondet]) :-
    box(Box),
    get_pokemon_by_name(["Grotle", "Skrelp", "Houndour", "Herdier", "Lombre", "Stufful"], Box, [Grotle, Skrelp, Houndour, Herdier, Lombre, Stufful]),
    opponent('Team Aqua Grunt Museum #1', Grunt1),
    opponent('Team Aqua Grunt Museum #2', Grunt2),
    find_line_less_naive(Box, Grunt1, Line1),
    find_line_less_naive(Box, Grunt2, Line2),
    assertion(Line1 == [Herdier, Stufful, Grotle]),
    % Stufful may not survive poison spam, Skrelp can help out
    % Lombre seems better vs Tirtouga cause it will heal a lot with Mega Drain
    assertion(Line2 == [Houndour, Lombre, Houndour]),
    % If we lead Herdier, it can chip decently at Mareanie
    % vs Whirlipede we just need to bait out Rollout and switch Stufful in
    % The plan: Herdier kills with 2x Covet, Skrelp kills Skrelp (and Lombre finished if needed), Lombre kills Tirtouga
    % next trainer, Herdier chips Mareanie then Houndour finishes, Lombre + Grotle kill Frillish and Whirlipede gets soloed by Stufful
    % Whirlipede locks itself into Rollout pretty much always, it seems
    Grunt1 = [Murkrow, OppSkrelp, Tirtouga],
    lines_1v1(Herdier, Murkrow, Lines1),
    clear_winner(Lines1), % Herdier would even survive double crit (!)
    lines2pivots(Lines1, [OppSkrelp, Tirtouga], Pivots),
    assertion(Pivots = [OppSkrelp]),
    lines_1v1(Skrelp, OppSkrelp, Lines2),
    not(clear_winner(Lines2)), % OppSkrelp can get lucky, but Lombre can finish while getting in position
    lines_1v1(Lombre, Tirtouga, Lines3),
    clear_winner(Lines3).

% This raised the level cap to where Gavi is way easier

:- end_tests(run11_dewford).

run11box4 :-
    retractall(box(_)),
    Box = [
    #{ability:"Rain Dish",ivs:_16152{atk:6,def:12,hp:18,spa:5,spd:10,spe:1},level:21,moves:["Natural Gift","Fake Out","Mega Drain","Bubble Beam"],name:"Lombre",nature:"Docile"},
    #{ability:"Oblivious",ivs:_16014{atk:18,def:29,hp:0,spa:11,spd:29,spe:28},level:21,moves:["Charm","Brine","Aurora Beam","Rollout"],name:"Sealeo",nature:"Docile"},
    #{ability:"Poison Point",ivs:_15880{atk:6,def:29,hp:25,spa:21,spd:26,spe:14},level:21,moves:["Bubble Beam","Smokescreen","Clear Smog","Twister"],name:"Seadra",nature:"Bashful"},
    #{ability:"Sheer Force",ivs:_15740{atk:16,def:2,hp:17,spa:9,spd:16,spe:13},level:21,moves:["Metal Claw","Aerial Ace","Bubble Beam","Aqua Jet"],name:"Krabby",nature:"Timid"},
    #{ability:"Unnerve",ivs:_15602{atk:26,def:1,hp:3,spa:4,spd:15,spe:26},level:21,moves:["Pluck","Scary Face","Fury Attack","Steel Wing"],name:"Corvisquire",nature:"Lonely"},
    #{ability:"White Smoke",ivs:_15464{atk:16,def:22,hp:24,spa:5,spd:25,spe:29},level:21,moves:["Ember","Flame Wheel","Bite","Bug Bite"],name:"Sizzlipede",nature:"Timid"},
    #{ability:"Sand Rush",ivs:_15328{atk:16,def:29,hp:9,spa:2,spd:1,spe:4},level:21,moves:["Take Down","Covet","Baby-Doll Eyes","Bite"],name:"Herdier",nature:"Relaxed"},
    #{ability:"Poison Point",ivs:_15190{atk:28,def:28,hp:13,spa:28,spd:8,spe:8},level:21,moves:["Water Pulse","Smokescreen","Feint Attack","Poison Tail"],name:"Skrelp",nature:"Lax"},
    #{ability:"Shell Armor",item:"Pecha Berry",ivs:_15044{atk:31,def:31,hp:26,spa:18,spd:31,spe:0},level:21,moves:["Bite","Razor Leaf","Absorb","Sand Tomb"],name:"Grotle",nature:"Quirky"},
    #{ability:"Fluffy",item:"Pecha Berry",ivs:_14898{atk:9,def:22,hp:20,spa:3,spd:22,spe:6},level:21,moves:["Secret Power","Baby-Doll Eyes","Rock Smash","Brutal Swing"],name:"Stufful",nature:"Brave"},
    #{ability:"Early Bird",item:"Oran Berry",ivs:_14748{atk:17,def:27,hp:31,spa:4,spd:20,spe:12},level:21,moves:["Fire Fang","Flame Burst","Smog","Bite"],name:"Houndour",nature:"Brave"}
    ],
    assertz(box(Box)).

:- begin_tests(run11_slateport, [setup(run11box4)]).

test(camper_gavi, [nondet]) :-
    box(Box),
    get_pokemon_by_name(["Grotle", "Seadra", "Herdier", "Corvisquire"], Box, [Grotle, Seadra, Herdier, Corvisquire]),
    opponent('Camper Gavi', Gavi),
    find_line_less_naive(Box, Gavi, Line),
    assertion(Line=[Grotle, Seadra, Herdier, Corvisquire, Seadra]).
    % this leaves room for 2 mons to pivot through. We can use Lombre vs Bibarel again and Grotle vs Eelektrik
    % Herdier can deal with Dustox and finally Skrelp can take Venoshock, baiting smth else, and kill Ponyta if needed
    % Fight is pretty safe with some Pecha Berries, though not guaranteed.
    % Sunflora Grass Whistle + Leech Seed is scary and so is Dustox Infestation on the wrong mon

test(battle_girl_laura, [nondet]) :-
    box(Box),
    opponent('Battle Girl Laura', Laura),
    find_line_less_naive(Box, Laura, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Seadra", "Houndour", "Corvisquire"]).
    % Getting Houndour out is hard, it is very squishy.
    % Instead, we can pivot via Sealeo taking Body Slam and baiting Force Palm from Stufful
    % Corvisquire can Pluck twice, eat its Sitrus Berry to heal, and OHKO the Mankey next

test(sailor_brenden, [nondet]) :-
    box(Box),
    opponent('Sailor Brenden', Brenden),
    find_line_less_naive(Box, Brenden, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Corvisquire", "Corvisquire"]).
    % Having Corvisquire solo is way too risky. Skrelp completely walls Heracross and Sizzlipede is decent too.
    % ACTUAL: Corvi solos with 2hp remaining on Heracross Pin Missile, so I shouldve switched and played safer!

test(battle_girl_lilith, [nondet]) :-
    box(Box),
    opponent('Battle Girl Lilith', Lilith),
    find_line_less_naive(Box, Lilith, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Corvisquire", "Corvisquire", "Seadra"]).
    % Corvisquire is faster than Mankey and takes no damage from it, so it is fine to use it twice.
    % Being faster is a slight issue, but we can chip first without risking Revenge
    % The Ledian in the back is the biggest problem, and Seadra is a weird take. Luxio is way better.

test(black_belt_takao, [nondet]) :-
    box(Box),
    opponent('Black Belt Takao', Takao),
    find_line_less_naive(Box, Takao, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Corvisquire", "Corvisquire", "Skrelp"]).
    % Skrelp is honestly better vs the Mienfoo. Stufful eats Buneary for breakfast
    % ACTUAL: switched Skrelp into the Buneary after Stufful chips it to be in position and be less risky

test(black_belt_cristian, [nondet]) :-
    box(Box),
    opponent('Black Belt Cristian', Cristian),
    find_line_less_naive(Box, Cristian, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Corvisquire", "Seadra", "Seadra"]).
    % Meditite kills Corvisquire with a Rock Throw crit. Lets use Grotle instead.
    % Seadra is actually super risky, if Poison Point procs these Guts pokemon can OHKO
    % We can juggle Luxio and Sizzlipede to stack Intimidate first though

test(battle_girl_jocelyn, [nondet]) :-
    box(Box),
    opponent('Battle Girl Jocelyn', Jocelyn),
    find_line_less_naive(Box, Jocelyn, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Seadra", "Seadra", "Seadra", "Sealeo"]).
    % Seadra is best vs the Pignite, Grotle can take Golett and Kecleon gets baited by Krabby to turn Fighting into its Aerial Ace
    % Luxio with Covet in the back in case Seadra needs help vs Hakamo-o

test(leader_brawly, [nondet]) :-
    box(Box),
    opponent('Leader Brawly', Brawly),
    find_line_less_naive(Box, Brawly, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Seadra", "Seadra", "Seadra", "Seadra", "Skrelp", "Seadra"]).
    % Oh boy. 5x Seadra is a bad start to this fight plan :)

:- end_tests(run11_slateport).