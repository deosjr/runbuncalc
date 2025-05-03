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
    assertion(Names == ["Piplup", "Houndour", "Houndour"]),
    find_line_less_naive(Box, Calvin, Line2),
    maplist([X,Y]>>get_dict(name,X,Y), Line2, Names2),
    assertion(Names2 == ["Houndour", "Houndour", "Houndour"]).
    % ACTUAL: Houndour just solos; less naive line is an improvement!

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
    assertion(Names2 == ["Starly", "Starly", "Starly"]).

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
    % UPDATE: suggesting Psyduck instead of Surskit after bug fix
    % I like Beedrill first and last (though note we think we are faster because of a speed tie).
    % Surskit again is not that great of a pick, Piplup should do much better vs Croagunk.
    % Switching becomes more interesting here, making sure we have healthy lead matchups.
    % Psyduck can tank FakeOut from Croagunk (it will come out, Beedrill outdamages/outspeeds both).
    % Scratch + Confusion will kill without proccing Salac berry (I almost missed that!).
    % This means Beedrill can come in on a Bullet Seed and kill with Bug Bite
    assertion(Names == ["Beedrill", "Psyduck", "Beedrill"]).
    % ACTUAL: learned that bug bite triggers before rough skin.
    % Psyduck wasnt super low but still got a Confusion instead of Bullet Seed.

:- end_tests(run7_petalburg).

% welcome to the party, Pancham, Remoraid and Spheal!
run7box3 :-
    retractall(box(_)),
    Prinplup = #{ability:"Clear Body",item:"Pecha Berry",ivs:_{atk:31,def:14,hp:16,spa:31,spd:24,spe:31},level:17,moves:["Metal Claw","Growl","Bubble Beam","Pluck"],name:"Prinplup",nature:"Bashful"},
    Surskit = #{ability:"Swift Swim",item:"Pecha Berry",ivs:_{atk:7,def:20,hp:17,spa:8,spd:21,spe:31},level:17,moves:["Struggle Bug","Quick Attack","Sweet Scent","Bubble Beam"],name:"Surskit",nature:"Bashful"},
    Psyduck = #{ability:"Swift Swim",ivs:_{atk:9,def:28,hp:25,spa:8,spd:24,spe:15},level:17,moves:["Confusion","Scratch","Tail Whip","Water Pulse"],name:"Psyduck",nature:"Lax"},
    Lombre = #{ability:"Rain Dish",item:"Oran Berry",ivs:_{atk:11,def:1,hp:14,spa:22,spd:11,spe:29},level:17,moves:["Natural Gift","Fake Out","Mega Drain","Bubble"],name:"Lombre",nature:"Serious"},
    Houndour = #{ability:"Early Bird",item:"Oran Berry",ivs:_{atk:25,def:9,hp:5,spa:29,spd:20,spe:4},level:17,moves:["Bite","Ember","Fire Fang","Leer"],name:"Houndour",nature:"Lonely"},
    Shellos = #{ability:"Sticky Hold",ivs:_{atk:17,def:6,hp:29,spa:13,spd:24,spe:2},level:17,moves:["Mud-Slap","Hidden Power Rock","Water Pulse","Mud Shot"],name:"Shellos",nature:"Relaxed"},
    Staravia = #{ability:"Intimidate",item:"Cheri Berry",ivs:_{atk:4,def:31,hp:18,spa:3,spd:15,spe:22},level:17,moves:["Endeavor","Growl","Quick Attack","Aerial Ace"],name:"Staravia",nature:"Brave"},
    Beedrill = #{ability:"Sniper",ivs:_{atk:4,def:14,hp:0,spa:5,spd:4,spe:12},level:17,moves:["Bug Bite","Pluck","String Shot","Pin Missile"],name:"Beedrill",nature:"Bashful"},
    Pancham = #{ability:"Mold Breaker",ivs:_{atk:15,def:12,hp:12,spa:16,spd:12,spe:16},level:17,moves:["Karate Chop","Covet","Arm Thrust","Feint Attack"],name:"Pancham",nature:"Hasty"},
    Remoraid = #{ability:"Hustle",ivs:_{atk:4,def:8,hp:16,spa:28,spd:0,spe:15},level:17,moves:["Water Gun","Lock-On","Psybeam","Aurora Beam"],name:"Remoraid",nature:"Naughty"},
    Spheal = #{ability:"Oblivious",ivs:_{atk:9,def:1,hp:24,spa:8,spd:25,spe:18},level:17,moves:["Charm","Brine","Powder Snow","Rollout"],name:"Spheal",nature:"Modest"},
    assertz(box([Prinplup, Surskit, Psyduck, Lombre, Houndour, Shellos, Staravia, Beedrill, Pancham, Remoraid, Spheal])).

:- begin_tests(run7_dewford, [setup(run7box3)]).

test(fisherman_elliot, [nondet]) :-
    box(Box),
    opponent('Fisherman Elliot', Elliot),
    find_line_less_naive(Box, Elliot, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    % seems good. Arrokuda is speed tied so thinks it is faster, and comes out first, which is even better.
    assertion(Names == ["Lombre", "Beedrill", "Lombre"]).

test(ruin_maniac_georgie, [nondet]) :-
    box(Box),
    opponent('Ruin Maniac Georgie', Georgie),
    find_line_less_naive(Box, Georgie, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    % Oh boy. Prinplup is great here, but Pancham needs to come out to counter Belly Drum shenanigans instead of Staravia.
    % Im expecting Munchlax second, so after Pancham kills it Mawile will want to come out because it outdamages with Covet.
    % Pivot to Prinplup is safest via a mon that takes Covet well and baits Metal Claw or Fire Fang: Beedrill.
    assertion(Names == ["Prinplup", "Prinplup", "Prinplup", "Staravia"]),
    get_pokemon_by_name("Prinplup", Box, Prinplup),
    get_pokemon_by_name("Pancham", Box, Pancham),
    % TODO: trainer pokemon names are atoms..
    get_pokemon_by_name('Mawile', Georgie, Mawile),
    % pivot(Versus, From, To, Box, Via)
    pivot(Mawile, Pancham, Prinplup, Box, Via),
    assertion(Via.name == "Beedrill").
    % ACTUAL: exactly like we drew it up.

test(tuber_chandler, [nondet]) :-
    box(Box),
    opponent('Tuber Chandler', Chandler),
    find_line_less_naive(Box, Chandler, Line),
    Chandler = [_Smoochum, _Elekid, Magby],
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    % OLD: assertion(Names == ["Beedrill", "Prinplup", "Prinplup"]),
    assertion(Names == ["Beedrill", "Lombre", "Prinplup"]),
    % Elekid vs Prinplup seems wrong, rest is fine.
    get_pokemon_by_name("Beedrill", Box, Beedrill),
    post_ko_switch_in(Beedrill, Chandler, [Switchin|_]),
    assertion(Switchin == Magby). % both are faster, but only Magby outdamages Beedrill
    % Prinplup is suggested because no one in the box outspeeds, and it is the first to outdamage Elekid
    % But we want to use it for Magby instead.
    % both Pancham and Lombre resist Elekids attacks overall really well, and have decent damage
    % We can bring both just to be sure.
    % ACTUAL: Magby comes in first, and indeed Magby outdamages but Elekid doesnt. Bug = fixed.
    % Definitely needed both Pancham and Lombre vs Elekid to play safe.

test(tuber_lola, [nondet]) :-
    % Welcome to the squad, Tirtouga!
    box(BoxOld),
    Tirtouga = #{ability:"Solid Rock", ivs:_{atk:28, def:22, hp:25, spa:31, spd:6, spe:8}, level:17, moves:["Bite", "Mud-Slap", "Smack Down", "Aqua Jet"], name:"Tirtouga", nature:"Naive"}, 
    Box = [Tirtouga|BoxOld],
    opponent('Tuber Lola', Lola),
    find_line_less_naive(Box, Lola, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Tirtouga", "Tirtouga"]),
    % Tirtouga slow-kills Fletchinder with Smack Down. It can stay in to chip Herdier but we need a safe finisher
    % Pancham can try but Headbutt flinches are a thing. We can pivot through Staravia, who also serves as a finisher
    % Other options for chip damage are Prinplup, Spheal and Shellos
    get_pokemon_by_name("Pancham", Box, Pancham),
    get_pokemon_by_name('Herdier', Lola, Herdier),
    % using BoxOld because it recommends pivotting through Tirtouga, who baits Rock Smash instead of Ice Fang
    % nice try computer, but it has already seen some action (should have told you by setting its HP low instead)
    pivot(Herdier, Tirtouga, Pancham, BoxOld, Via),
    assertion(Via.name == "Staravia").
    % ACTUAL: Tirtouga solos because it cannot be killed, even Intimidated and at -2 defense from Rock Smashes. No crits involved.

test(sailor_edmond, [nondet]) :-
    box(BoxOld),
    Tirtouga = #{ability:"Solid Rock", ivs:_{atk:28, def:22, hp:25, spa:31, spd:6, spe:8}, level:17, moves:["Bite", "Mud-Slap", "Smack Down", "Aqua Jet"], name:"Tirtouga", nature:"Naive"}, 
    Box = [Tirtouga|BoxOld],
    opponent('Sailor Edmond', Edmond),
    find_line_less_naive(Box, Edmond, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    % OLD: assertion(Names == ["Tirtouga", "Tirtouga", "Lombre"]).
    assertion(Names == ["Tirtouga", "Prinplup", "Lombre"]).
    % Tirtouga again is a great lead slow-killing Wingull, but keeping it in vs Buizel is way too risky.
    % Palpitoad comes out second anyways, unless Wingull gets a crit Water Pulse off.
    % Lombre is great vs both Buizel and Palpitoad, but again cannot safely fight both.
    % Buizel is the greater threat and can be perfectly calculated with Sonic Boom being highest damage, so we save Lombre for him.
    % Both Beedrill and Prinplup decently outdamage and tank Palpitoad. Only Beedrill outspeeds.
    % IF Palpitoad is second, we bait Mud Shot. Clear Body Prinplup should deal with that no problem.
    % Backup plan is a pivot via Pancham onto Beedrill, so we dont get an accuracy drop on it.
    % Beedrill takes a ton of Pursuit damage though! Staravia would be safer.
    % We prefer not to kill with Prinplup either way because it baits Sonic Boom, we would rather switch Lombre in on Water Pulse.
    % IF Buizel is second, Tirtouga still lives a Pursuit crit, and otherwise baits Water Pulse. Lombre kills Buizel and baits Sludge.
    % That would be great for Beedrill to come in on and kill Palpitoad.
    % ACTUAL: Wingull did not crit (its Shock Wave, equally likely) but Buizel came out anyway because were just outside of Palpitoad range.
    % that was actually extremely likely to happen, only one roll on a non-crit would lead to Palpitoad seeing a kill.
    % Nice, because the Buizel line is less complicated. Buizel confuses Lombre on the switchin, so we pivot (again to Lombre) via Staravia
    % I expected that to bait another Water Pulse but it Sonic Boomed. At 25/49 we are still safe to go for Mega Drain because of how much we heal back.
    % Buizel falls. Palpitoad does Sludge onto Beedrill, takes a Pin Missile (x2), Rain Dances and is now very scary, but dies to another Pin Missile (x3).
    % Prinplup would have been the counter if that did not kill, but I did not think of that ahead of time!

test(fisherman_bill, [nondet]) :-
    box(BoxOld),
    Tirtouga = #{ability:"Solid Rock", ivs:_{atk:28, def:22, hp:25, spa:31, spd:6, spe:8}, level:17, moves:["Bite", "Mud-Slap", "Smack Down", "Aqua Jet"], name:"Tirtouga", nature:"Naive"}, 
    Box = [Tirtouga|BoxOld],
    opponent('Fisherman Bill', Bill),
    find_line_less_naive(Box, Bill, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    % OLD: assertion(Names == ["Houndour", "Houndour", "Houndour","Houndour","Houndour"]).
    % NEW: fixed! Beedrill at the end there because the last bug uses Struggle Bug, a Special move.
    assertion(Names == ["Staravia", "Staravia", "Staravia","Staravia","Beedrill"]).
    % funny, Houndour comes before Staravia in party order. Both fast-kill the entire team except for Scatterbug.
    % Id much prefer to take Staravia because it takes at most 31.2% from a crit while Houndour would be dead.
    % Suggests some subsorting within the priority categories for taking less max damage back that we could try.

test(tuber_ricky, [nondet]) :-
    % Welcome to the squad, Cufant!
    box(BoxOld),
    Tirtouga = #{ability:"Solid Rock", ivs:_{atk:28, def:22, hp:25, spa:31, spd:6, spe:8}, level:17, moves:["Bite", "Mud-Slap", "Smack Down", "Aqua Jet"], name:"Tirtouga", nature:"Naive"}, 
    Cufant = #{ability:"Sheer Force",ivs:_{atk:28,def:24,hp:3,spa:24,spd:4,spe:24},level:17,moves:["Bulldoze","Growl","Rock Throw","Rock Smash"],name:"Cufant",nature:"Impish"},
    Box = [Tirtouga, Cufant|BoxOld],
    opponent('Tuber Ricky', Ricky),
    find_line_less_naive(Box, Ricky, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Tirtouga", "Beedrill", "Beedrill"]).
    % Tirtouga keeps going vs Nidorino, only switching because its accuracy is -3. Beedrill fares worse and cant get many hits in. 
    % Tirtouga switches back and kills with Smack Down. Now we switch in Cufant, but get paralyzed immediately. Luckily it still Bulldozes.
    % Luxio is now low enough for Lombre to kill. Pancham and Prinplup were backup options, but neither of them were great at that.

test(tuber_hailey, [nondet]) :-
    box(BoxOld),
    Tirtouga = #{ability:"Solid Rock", ivs:_{atk:28, def:22, hp:25, spa:31, spd:6, spe:8}, level:17, moves:["Bite", "Mud-Slap", "Smack Down", "Aqua Jet"], name:"Tirtouga", nature:"Naive"}, 
    Cufant = #{ability:"Sheer Force",ivs:_{atk:28,def:24,hp:3,spa:24,spd:4,spe:24},level:17,moves:["Bulldoze","Growl","Rock Throw","Rock Smash"],name:"Cufant",nature:"Impish"},
    Box = [Tirtouga, Cufant|BoxOld],
    opponent('Tuber Hailey', Hailey),
    find_line_less_naive(Box, Hailey, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Staravia", "Beedrill", "Lombre"]).
    % Individual choices are okay, but to make it work safely we need to string it together
    % That would be the next thing to do, estimate damage taken 1-on-1, guess switchins, and try to see a kill while coming in safely.
    % Tirtouga leads to break Focus Sash. Staravia comes in on anything but Rock Tomb and kills with Aerial Ace.
    % If it somehow still is Rock Tomb, Prinplup will have to clean up.
    % Either Nidorina or Flaaffy comes in next, depending on range (this would also be interesting to calc!).
    % In both cases we bait an electric-type move. Cufant switches in on Nidorina and solos it.
    % On Flaaffy I want Lombre, can pivot via Beedrill but it takes so much damage that Id rather keep it as backup to fast-kill after chip damage.
    % NOTE TO SELF: I did not consider last pivotting, which ended up not being that important but still!

test(team_aqua_grunts_museum, [nondet]) :-
    box(BoxOld),
    Tirtouga = #{ability:"Solid Rock", ivs:_{atk:28, def:22, hp:25, spa:31, spd:6, spe:8}, level:17, moves:["Bite", "Mud-Slap", "Smack Down", "Aqua Jet"], name:"Tirtouga", nature:"Naive"}, 
    Cufant = #{ability:"Sheer Force",ivs:_{atk:28,def:24,hp:3,spa:24,spd:4,spe:24},level:17,moves:["Bulldoze","Growl","Rock Throw","Rock Smash"],name:"Cufant",nature:"Impish"},
    Box = [Tirtouga, Cufant|BoxOld],
    opponent('Team Aqua Grunt Museum #1', Grunt1),
    find_line_less_naive(Box, Grunt1, Line1),
    maplist([X,Y]>>get_dict(name,X,Y), Line1, Names1),
    opponent('Team Aqua Grunt Museum #2', Grunt2),
    find_line_less_naive(Box, Grunt2, Line2),
    maplist([X,Y]>>get_dict(name,X,Y), Line2, Names2),
    assertion(Names1 == ["Tirtouga", "Prinplup", "Prinplup"]),
    assertion(Names2 == ["Beedrill", "Lombre", "Cufant"]).
    % Most names I had in mind for this fight do come up, only missing one is Staravia. Lets go one by one.
    % Tirtouga is a clear win vs Murkrow. Cufant and Prinplup are considerations but needed elsewhere.
    % Prinplup vs Skrelp looks miserable, it will get toxic stalled out. Id love to use Cufant here, who would need a pivot
    % Prinplup vs Tirtouga is a lot more reasonable, but Lombre looks to be safer. Calc does not look past Rindo Berry being used.
    % Beedrill vs Mareanie looks great, and is probably a free switch from Tirtouga who starts the fight again. Cufant is better though.
    % Lombre vs Frillish looks good but we want a anti-poison berry. Beedrill and Prinplup can help finish if needed. Wants a safe pivot.
    % Cufant vs Whirlipede I hadnt considered, I was thinking Staravia but Cufant does tank a lot. We can start Staravia and let Cufant finish?
    % Now order: Tirtouga always baits Skrelp. Pivot via Lombre to Cufant. Arguably we dont want to finish Murkrow with Aqua Jet so we take more damage and Water Pulse is guaranteed?
    % If we Fake Out on the pivot, Cufant only has to deal one Bulldoze and pivot back to Lombre to let it finish Skrelp with Mega Drain!
    % This also sets up Lombre to already be out for Tirtouga, which makes this fight probably very clean.
    % Cufant switchin on Mareanie is free, and if we get soaked we can pivot via Beedrill and back. Rinse/repeat if needed.
    % Frillish is guaranteed to be next, baiting Hex or Water Pulse. Pivot via Prinplup to bait Shock Wave and kill with Lombre.
    % Whirlipede is last, baiting Pin Missile. Staravia can switch in on that, get Aerial Ace off, and kill or switch to Cufant to be safe.
    % FINISHING TOUCH: Prinplup learns HP Ghost, Tirtouga HP Rock, Houndour HP Bug, Psyduck HP Poison.
    % ACTUAL: Tirtouga slow-kills Murkrow, Lombre gets confused by Skrelp on the pivot so doesnt Fake Out.
    % This whole thing about Lombre FakeOut didnt matter because Cufant outspeeds Skrelp! And Lombre is pretty safe to come in on Tirtouga anyway
    % Lombre comes in on Mud Shot and gets a speed drop. After AncientPower boosts Tirtouga outspeeds, so Lombre is lower than I thought after that fight.
    % Whirlipede comes out on a Soaked Cufant, since it now outspeeds and outdamages! Still baits Pin Missile though.
    % It uses Rollout, perhaps thats hardcoded to be used if it doesnt see a kill? Cufant tanks it and chips with Rock Throw.
    % 120 power Rollout gets tanked by Tirtouga who kills with Aqua Jet. It is very low after Rocky Helmet, and baits random move from Frillish.
    % Pivot via Prinplup who chips with HP Ghost, then to Lombre who kills with Mega Drain. Staravia sits this fight out.
    % My original plan wouldve probably killed Cufant, as it didnt have enough HP to tank all those Rollouts and the switch to Staravia wouldve been too slow.

:- end_tests(run7_dewford).

run7box4 :-
    retractall(box(_)),
    Pancham = #{ability:"Mold Breaker",item:"Oran Berry",ivs:_{atk:15,def:12,hp:12,spa:16,spd:12,spe:16},level:21,moves:["Karate Chop","Covet","Circle Throw","Feint Attack"],name:"Pancham",nature:"Hasty"},
    Surskit = #{ability:"Swift Swim",item:"Oran Berry",ivs:_{atk:7,def:20,hp:17,spa:8,spd:21,spe:31},level:21,moves:["Struggle Bug","Quick Attack","Sweet Scent","Bubble Beam"],name:"Surskit",nature:"Bashful"},
    Sealeo = #{ability:"Oblivious",item:"Oran Berry",ivs:_{atk:9,def:1,hp:24,spa:8,spd:25,spe:18},level:21,moves:["Charm","Brine","Aurora Beam","Rollout"],name:"Sealeo",nature:"Modest"},
    Remoraid = #{ability:"Hustle",ivs:_{atk:4,def:8,hp:16,spa:28,spd:0,spe:15},level:21,moves:["Bubble Beam","Lock-On","Psybeam","Aurora Beam"],name:"Remoraid",nature:"Naughty"},
    Houndour = #{ability:"Early Bird",item:"Oran Berry",ivs:_{atk:25,def:9,hp:5,spa:29,spd:20,spe:4},level:21,moves:["Bite","Flame Burst","Fire Fang","Hidden Power Bug"],name:"Houndour",nature:"Lonely"},
    Psyduck = #{ability:"Swift Swim",item:"Oran Berry",ivs:_{atk:9,def:28,hp:25,spa:8,spd:24,spe:15},level:21,moves:["Psybeam","Hidden Power Poison","Tail Whip","Water Pulse"],name:"Psyduck",nature:"Lax"},
    Cufant = #{ability:"Sheer Force",ivs:_{atk:28,def:24,hp:3,spa:24,spd:4,spe:24},level:21,moves:["Bulldoze","Growl","Rock Throw","Rock Smash"],name:"Cufant",nature:"Impish"},
    Shellos = #{ability:"Sticky Hold",item:"Oran Berry",ivs:_{atk:17,def:6,hp:29,spa:13,spd:24,spe:2},level:21,moves:["Mud-Slap","Hidden Power Rock","Water Pulse","Mud Shot"],name:"Shellos",nature:"Relaxed"},
    Tirtouga = #{ability:"Solid Rock",ivs:_{atk:28,def:22,hp:25,spa:31,spd:6,spe:8},level:21,moves:["Bite","Ancient Power","Brine","Aqua Jet"],name:"Tirtouga",nature:"Naive"},
    Staravia = #{ability:"Intimidate",item:"Oran Berry",ivs:_{atk:4,def:31,hp:18,spa:3,spd:15,spe:22},level:21,moves:["Endeavor","Growl","Quick Attack","Aerial Ace"],name:"Staravia",nature:"Brave"},
    Lombre = #{ability:"Rain Dish",item:"Pecha Berry",ivs:_{atk:11,def:1,hp:14,spa:22,spd:11,spe:29},level:21,moves:["Natural Gift","Fake Out","Mega Drain","Bubble Beam"],name:"Lombre",nature:"Serious"},
    Prinplup = #{ability:"Clear Body",item:"Oran Berry",ivs:_{atk:31,def:14,hp:16,spa:31,spd:24,spe:31},level:21,moves:["Aqua Jet","Hidden Power Ghost","Bubble Beam","Pluck"],name:"Prinplup",nature:"Bashful"},
    Beedrill = #{ability:"Sniper",item:"Oran Berry",ivs:_{atk:4,def:14,hp:0,spa:5,spd:4,spe:12},level:21,moves:["Bug Bite","Pluck","Focus Energy","Pin Missile"],name:"Beedrill",nature:"Bashful"},
    % caught after Camper Gavi fight on Route 110
    Eelektrik = #{ability:"Levitate",ivs:_18272{atk:27,def:26,hp:15,spa:4,spd:3,spe:30},level:21,moves:["Thunder Wave","Spark","Hidden Power Fire","Crunch"],name:"Eelektrik",nature:"Lonely"},
    assertz(box([Pancham,Surskit,Sealeo,Remoraid,Houndour,Psyduck,Cufant,Shellos,Tirtouga,Staravia,Lombre,Prinplup,Beedrill,Eelektrik])).

:- begin_tests(run7_brawly_split, [setup(run7box4)]).

test(camper_gavi, [nondet]) :-
    box(NewBox),
    select(Route110Eelektrik, NewBox, Box),
    Route110Eelektrik.name = "Eelektrik", % we catch this after the fight, but use box for the rest of the split, so we take it out for now
    opponent('Camper Gavi', Gavi),
    find_line_less_naive(Box, Gavi, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Pancham", "Prinplup", "Houndour", "Beedrill", "Beedrill"]),
    % Decent choices, can probably win the fight with some pivot backup.
    % I prefer Cufants guaranteed slow 2HKO while surviving Super Fang + Aqua Jet crit from Bibarel,
    % over Panchams slow potential 1HKO while being dead to Super Fang + Pluck crit. (potential optimisation!)
    % This assumes Bibarel goes for highest damage and not Headbutt flinches. If that happens, Lombre needs to help out.
    % Cufant also guarantees Ponyta next. Prinplup can switch in on it and oneshot with Bubble Beam.
    % Even if Ponyta crits twice Dustox never sees a kill. If Ponyta crits once Eelektrik will see a kill first, otherwise Sunflora comes out.
    % Eelektrik baits Shock Wave and Sunflora baits Energy Ball. Beedrill switches in without issues on the latter.
    % Houndour needs some more help, but its hard to find:
    get_pokemon_by_name('Eelektrik', Gavi, Eelektrik),
    get_pokemon_by_name("Houndour", Box, Houndour),
    get_pokemon_by_name("Prinplup", Box, Prinplup),
    pivot(Eelektrik, Prinplup, Houndour, Box, Via),
    assertion(Via.name == "Tirtouga"). % still takes 94.8% damage on a crit Shock Wave.. and either way both it and Mega Drain are now lethal, so 50/50
    % In the (noncrit) Sunflora line, Im not sure who comes after. Beedrill outdamages/outspeeds both.
    % Dustox we just 2HKO: we could even set up a Focus Energy on it and then continue on Eelektrik.
    % Otherwise we switch to Houndour after a Bug Bite eats its Sitrus berry.
    % In the (crit) Eelektrik line, we can switch Lombre into Shock Wave, FakeOut + Mega Drain (with anti-poison berry) and switch Houndour when its low.
    % If Houndour gets crit on the switchin (or Super Fanged somehow), it needs to switch out again immediately or risk death to another crit.
    % Pancham is the last mon in our box that resists Shock Wave decently well and can deal damage, so it joins as backup.
    % Beedrill cleans up after Eelektrik is dead.
    % ACTUAL: Bibarel went for 2x Super Fang and died. Ponyta goes for Flame Wheel on the switch and doesnt crit with it nor the followup.
    % We are taking the happy path. Beedrill hits 3x Pin Missile to kill Sunflora in one go. Eelektrik is next (so it looks in party order, not from KOd pokemon).
    % Energy Ball did almost nothing so we arent dead to Super Fang + Shock Wave crit. I go for Pin Missile + Bug Bite.
    % That was a bit dumb, obviously Sitrus berry procs, but Pin Missile hits for x4 and crits once, so another one just wins.
    % Dustox dies to Pluck x2 and we win the fight.

% Welcome to the team, Tynamo! Or should I say, Eelektrik? It learns HP Fire too.

test(battle_girl_laura, [nondet]) :-
    box(Box),
    opponent('Battle Girl Laura', Laura),
    find_line_less_naive(Box, Laura, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Staravia", "Houndour", "Staravia"]),
    % Not good enough. Staravia lead is fine, but Houndour cannot come in safely at all and Eelektrik is a much better pick vs Stufful
    % Stufful is guaranteed second since Mankey is fast-killed by Staravia.
    % In order for Staravia to come in vs Mankey after leading on Riolu, Id like to pivot on Mankeys ground move Stomping Tantrum.
    % Cufant is the clear pick to bait that out, I think.
    get_pokemon_by_name('Mankey', Laura, Mankey),
    get_pokemon_by_name("Staravia", Box, Staravia),
    get_pokemon_by_name("Eelektrik", Box, Eelektrik),
    pivot(Mankey, Eelektrik, Staravia, Box, Via),
    assertion(Via.name == "Cufant").
    % In fact, I want Prinplup vs Stufful because that can get rid of Sitrus berry while maintaining momentum
    % Take an anti-para berry so we dont get paralyzed on the switchin (Eelektrik is immune).
    % I did think of Staravia Intimidate this time, but Riolu has Inner Focus..

test(sailor_brenden, [nondet]) :-
    box(Box),
    opponent('Sailor Brenden', Brenden),
    find_line_less_naive(Box, Brenden, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Staravia", "Staravia"]).
    % Cool plan, we need better. Staravia can kill Heracross guaranteed after any chip damage has been done, or finishing with Quick Attack.
    % Heracross is so fast though, and any crit threatens death by a followup crit or highroll. Switching in on it is risky for almost everyone.
    % Backup plan: Pancham and Beedrill can get a hit in, and Tirtouga can finish with Aqua Jet.
    % Prinplup can too, but I want to lead on it since it guaranteed kills Farfetchd with x2 Pluck through Sitrus Berry
    % Cufant can serve as a pivot to bait Rock Smash instead of Pin Missile where needed

test(battle_girl_lilith, [nondet]) :-
    box(Box),
    opponent('Battle Girl Lilith', Lilith),
    find_line_less_naive(Box, Lilith, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Staravia", "Staravia", "Eelektrik"]),
    % Staravia takes FakeOut damage from Makuhita, who then sees it is dead to Aerial Ace and Bullet Punches before it dies.
    % low rolls are 11 damage for each, and 19 on crit, so we could be anywhere from 19-35 HP left on Staravia
    Lilith = [Makuhita, Mankey, Ledian],
    get_pokemon_by_name("Staravia", Box, Staravia),
    % TODO: calculate already counts -1 attack for Intimidate? low values are -2 atk scores according to calc.runandbun.com
    % TODO: the high roll values look like high rolls for noncrit without Intimidate -1 atk? As if Intimidate is counted on crit?
    % TODO: calc.runandbun has a checkbox for these abilities. Ive tried disabling it locally but cant get it to work exactly the same
    % TODO: this should be a -1 atk boost, not a +1 atk boost!
    IntimidatedMakuhita = Makuhita.put(_{boosts:_{atk:(+1)}}),
    lowRoll(IntimidatedMakuhita, Staravia, false, "Fake Out", FakeOutLow),
    highRoll(IntimidatedMakuhita, Staravia, true, "Fake Out", FakeOutHigh),
    lowRoll(IntimidatedMakuhita, Staravia, false, "Bullet Punch", BulletPunchLow),
    highRoll(IntimidatedMakuhita, Staravia, true, "Bullet Punch", BulletPunchHigh),
    MaxLowRolls is FakeOutLow + BulletPunchLow,
    MaxHighRolls is FakeOutHigh + BulletPunchHigh,
    assertion((MaxLowRolls == 22, MaxHighRolls == 38)),
    Staravia19 = Staravia.put(_{curHP:19}),
    Staravia35 = Staravia.put(_{curHP:35}),
    post_ko_switch_in(Staravia19, Lilith, [Switchin19|_]),
    post_ko_switch_in(Staravia35, Lilith, [Switchin35|_]),
    assertion(Switchin19 == Ledian),    % sees a kill with Thunder Punch
    assertion(Switchin35 == Mankey).    % both are faster, Mankey is first in party order 
    % Eelektrik can switchin on Ledian and kill with Spark. If Ice Punch freezes we can stay in and hope to thaw.
    % Backup plan if we remain frozen and get too low is Tirtouga.
    % We are slower than Mankey, and live a crit Power-Up Punch (otherwise Ledian wouldve come out).
    % We can Aerial Ace after taking a hit and then Quick Attack before it can use Reversal.
    % ACTUAL: Makuhita is not preburned, it uses Life Orb at end of round 1. This makes Mankey more likely second.
    % We can switch Eelektrik on a Thunder Punch, do not get frozen by the followup Ice Punch and win with Spark.

test(black_belt_takao, [nondet]) :-
    box(Box),
    opponent('Black Belt Takao', Takao),
    find_line_less_naive(Box, Takao, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Staravia", "Staravia", "Pancham"]).
    % Staravia looks good but then you realise it takes a Wake-Up Slap AND a Mach Punch before getting a hit in.
    % if either of those crit, Staravia dies. Same thing for Mienfoo Fake Out + Rock Slide.
    % Instead we lead Beedrill on Breloom, with anti-para berry in case of Spore. It should Mach Punch the second time we hit Pluck.
    % Mienfoo probably sees a slow-kill, but if we take a lot of damage or almost none, Buneary comes out (its faster, and can see a kill at ~60%)
    % either way we have to switch out Beedrill.
    % IF Mienfoo is next, Eelektrik switches into it and lives even crit Fake Out + crit Drain Punch, then chips a large chunk of HP with Spark.
    % if it is super low we bait a random move, but it is likely to be another Drain Punch, and even crit Rock Slide would be survived by Staravia.
    % it then fast-kills with Aerial Ace.
    % Buneary on either Beedrill or Staravia baits Triple Axel, though Fake Out is more likely.
    % Tirtouga switches into that and spams Ancient Power + finishes with Aqua Jet. If Mienfoo is left, the line is pretty much the same.
    % Contingency planning: we have 2 slots left. Ancient Power does _not_ make contact so we dont risk Infatuation.
    % Pancham and Prinplup are the backup in case I forgot something.
    % ACTUAL: Eelektrik comes in on a Drain Punch. Shouldve pivoted via Beedrill. I pivot via Beedrill anyways, Mienfoo uses Detect.
    % Prinplup switches in on Rock Slide and would just about live a crit Drain Punch. It uses Pluck _after_ Drain Punch, then switches to Staravia.
    % It would live a max roll crit Rock Slide on exactly 1 HP. That doesnt happen, either way we live to Aerial Ace it down. Scary steer!

test(black_belt_cristian, [nondet]) :-
    box(Box),
    opponent('Black Belt Cristian', Cristian),
    find_line_less_naive(Box, Cristian, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Prinplup", "Beedrill", "Prinplup"]).
    % This doesnt work at all, we have to change this line completely.
    % Caught a Buizel in Slateport, a fast Fake Out user that can solo Meditite. Unless it crits Brick Break, but I didnt see a better play.
    % Spoilers: it crit Brick Break. RIP Buizel. We do get a free switchin of Staravia, who Aerial Aces through it and Machoke.
    % Gurdurr needs a lot of chip to be safely taken out, so we pivot through Surskit onto Tirtouga, get a Brine hit in,
    % then through Beedrill onto Prinplup for the kill. Both cases bait Low Sweep onto a bug which baits Rock Throw.
    % If Prinplup was sufficiently low that it could die to a Mach Punch crit, we couldve done that once more and killed with Beedrill

%test(battle_girl_jocelyn, [nondet]) :-
    % Eelektrik solos Kecleon and forces Hakamo-o, who gets paralyzed. Prinplup easily finishes it off.
    % Next is Golett. Switch to Tirtouga to get Brine in, then Prinplup finishes and is out for Pignite.
    % Bubble Beam + Aqua Jet finishes it without risk.

test(leader_brawly, [nondet]) :-
    box(Box),
    opponent('Leader Brawly', Brawly),
    find_line_less_naive(Box, Brawly, Line),
    maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
    assertion(Names == ["Staravia", "Eelektrik", "Prinplup", "Beedrill", "Eelektrik", "Pancham"]).
    % Staravia would get eaten alive. Prinplup is the counter to Kubfu, as it Plucks and then switches out to get rid of confusion (onto who? Masquerain?)
    % Beedrill vs Hitmontop is also.. a choice. It thinks it outspeeds but its just guaranteed dead.
    % I dont see much better, so lets try the same line from last time, with Cufant/Tirtouga being the pivot.
    % Kill Kubfu, Quick Attack Lopunny, get Hitmontop out (unless Kubfu crit or we lowroll and need 2x Gust, then its Combusken and we dont have enough HP to combo off either..)
    % Hitmontop wants to Rock Slide, so we pivot to Cufant, then back to Masquerain, and repeat until Intimidate has stacked high enough.
    % Kill with Prinplup. Lopunny comes out, take Retaliate onto Cufant, hope to chip and switch to Pancham. If Masquerain still lives it can finish.
    % Combusken is next. If Masquerain lives, it can switch in on Double Kick and bait Incinerate, for Tirtouga to come in and Brine + Aqua Jet.
    % Lotad or Eelektrik is the pick vs Poliwhirl. Pancham hopefully can come back in vs Scraggy.
    % NEW IDEA: Beedrill can set up on Poliwhirl with 2x Focus Energy and 100% crit kill it. It is also a bug so has pivot potential and a Heart Scale buys it U-Turn
    % What if we kill Kubfu and then switch Tirtouga into Retaliate immediately? We can do the same Intimidate dance on Lopunny! But it has Eject Button.
    % This way we can switch whoever pretty safely in to bait our next opponent. Tirtouga vs Combusken is tempting, but it would be low..
    % unless we use Cufant for later pivots when Lopunny is already weakened. Very tempting, but Hitmontop comes out after and Pursuits it. Lets not.
    % Masquerain outdamages Combusken so it gets Hitmontop, and we would continue the combo. Since that combo also lets us set up in the same way,
    % maybe we use Tirtouga then vs Combusken? Any crit could ruin this whole plan btw..
    % Beedrill baits Poliwhirl to not use Superpower which is good. Scraggy meets Pancham.
    % Another idea, what if we kill Lopunny as above but with Pancham? This baits Combusken, which we can tank and Circle Throw, softening it up.
    % Prinplup would guaranteed slow-kill it in that case. IF we get Hitmontop we get into the above line, Poliwhirl gets Beedrill and Scraggy would be ideal.
    % Beedrill and Masquerain both bait Incinerate from Combusken (Masquerain after Intimidate only), which lets Prinplup come in safely.
    % I think all three Circle Throw targets are therefore pretty much fine.
    % If we somehow get Beedrill into Scraggy we can U-Turn into Pancham baiting Faint Attack. Tirtouga vs Combusken can only happen if Poliwhirl is already dead.
    % Actually, Prinplup can just solo Kubfu by Plucking twice and using Aqua Jet, because it doesnt get confused by the Iapapa berry.
    % Aqua Jet also gets Lopunny out. This line always baits Combusken who sees a kill with Thunder Punch.
    % Switch to Eelektrik and spam Spark. If it paralyzes, use Thunder Wave (it has a Lum berry). Otherwise just hope it uses Work Up and kill it, or switch to Prinplup/Tirtouga for Aqua Jet.
    % Hitmontop comes out next. We go into Intimidate switch spam. And at some point this line dies, there is simply too much risk of a single crit.

    % I tried it, and wiped. But there is enough here that I want to play around with it a bit longer before resetting.

    % This line sometimes wins, if lucky even without any deaths: 2xPluck+Aqua Jet, Tirtouga, Masquerain, Pancham Circle Throw. This _doesnt_ break Eject Button
    % IF Combusken, go Tirtouga Brine+Aqua Jet. This gets Lopunny. Aqua Jet Again.
    % IF Hitmontop, combo it to -2 or 3 and take it out with Eelektrik Thunder Wave + spam Spark. Switch Tirtouga into Lopunny again and Aqua Jet Eject it
    % IF Poliwhirl, switch Beedrill and kill it. Switch Tirtouga on Lopunny and Aqua Jet Eject it
    % IF Scraggy, kill it with 2x Covet. This is the best timeline. Switch Tirtouga on Lopunny and Aqua Jet Eject it
    % In all cases, Lopunny is now dead to another Aqua Jet. If Tirtouga is above 30HP it is likely to kill Combusken with Brine + Aqua Jet. Incinerate can be baited with Beedrill.
    % Masquerain is not needed apart for Hitmontop; is it a crutch? Can we do without?
    % What if every cycle we first switch to Beedrill, U-Turn into Cufant? Can Staravia do the same trick but worse?
    % When Hitmontop is below 43% HP, it sees Staravia can kill it (if we give it 31 spe) and will Mach Punch?
    % Is there a line where I get Beedrill vs Poliwhirl early, then 2x Bug Bite + guaranteed kill with U-Turn and set up for the next mon?
    % How about Aqua Jet the very first time Tirtouga is in vs Lopunny? That gets Poliwhirl.
    % U-Turn makes me choose first, and the AI just chooses afterwards based on that choice. That can be used against it, maybe.
    % Im starting to think AI calculates with Retaliate at power 70 always. Lopunny should outdamage Pacham on the U-Turn but Combusken comes out instead.
    % Intimidate still goes off when switching in Staravia on a KOd pokemon.
    % IF Tirtouga gets lucky and stays at high HP, Poliwhirl comes in
    % I really want to paralyze Scraggy when I come in with Pancham, or U-Turn into Pancham from Poliwhirl
    % Masquerain learns Giga Drain vs Poliwhirl if we want

    % So the line is: Prinplup uses 2xPluck+Aqua Jet, then Aqua Jets Lopunny. Combusken comes in, switch Tirtouga, Brine+Aqua Jet. (we dont want Incinerate after all)
    % Lopunny Retaliates again into Tirtouga and gets Intimidated again. Now Pancham is safe to come in and kill with 2x Karate Chop.
    % This brings Hitmontop in. Staravia>Prinplup>Staravia>Prinplup>Staravia>Tirtouga>Staravia puts it at -4 attack. Eelektrik can now ThunderWave + spam Spark to kill.
    % Beedrill kills Poliwhirl and finishes with U-Turn into Staravia. (if Poliwhirl lives, Quick Attack it)
    % Staravia Intimidates, Eelektrik comes in and Thunder Wave + Sparks it down. (could intimidate again). Scrappy needs an Intimidate too before Pancham can finish.

    % The line with Masquerain facing Kubfu and regaining health with Mega Drain doesnt need Prinplup nor Beedrill. It can also Quick Attack.
    % We could take Cufant and Staravia and combo on Lopunny, Hitmontop and even Scraggy if needed. Could bring Eelektrik again.
    % Staravia isnt even needed now that Masquerain regains some HP. This line can make it pretty consistently deathless past Brawly with 5 mons!
    % Staravia is still a nice backup to take some initial hits from Hitmontop and Scraggy while comboing, and Endeavor Scraggy when low as a sac.
    % This line is: Masquerain uses 2x Gust on Kubfu, killing it. Quick Attack Lopunny. Combusken comes out, baiting Incinerate or Thunder Punch.
    % pivot to Tirtouga via Eelektrik (no one baits Incinerate. Double Kick is the next best thing, at least we get a berry proc that way).
    % Kill Combusken with Brine + Aqua Jet. Poliwhirl comes out. Masquerain takes an HP Grass and outspeeds with Giga Drain.
    % Hitmontop is next. Masquerain/Cufant combo it to -6 atk and Eelektrik cleans it up with Thunder Wave + spam Spark.
    % Now Lopunny comes back (it comes before Poliwhirl if Tirtouga got low enough). Masquerain/Tirtouga/Cufant can combo it to -6 atk.
    % Pancham then switches in for the kill with 2x Double Kick. It spams Covet trough Rest on Scraggy until it dies.

:- end_tests(run7_brawly_split).