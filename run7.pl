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

:- end_tests(run7_dewford).