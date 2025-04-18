:- begin_tests(run6).
%test(youngster_calvin, [nondet]) :-
    % soloed by Chinchou

%test(bug_catcher_rick, [nondet]) :-
    % soloed by Litleo

%test(youngster_allen, [nondet]) :-
    % lead Pidgey, then Chinchou takes over

%test(lass_tiana, [nondet]) :-
    % soloed by Litleo

%test(traithlete_mikey, [nondet]) :-
    % Chinchou solos Krabbey and Clobbopus, then Vivillon switches in on Yanma and one-shots it.

%test(fisherman_darian, [nondet]) :-
    % Vivillon solos, using Protect vs Bounce and outspeeding the second Magikarp to kill through Focus Sash

%test(lady_cindy, [nondet]) :-
    % Vivillon + Chinchou win again, with use of anti-para berries.

%test(team_aqua_grunt, [nondet]) :-
    % Chinchou kills Carvanha, then we switch and chip into Exeggcute with Pidgey, Fletchling and Litleo.
    % The latter prevents its berry/harvest combo and kills with Ember.
    % Now we chip onto Croagunk and kill with Vivillon.

%test(fisherman_elliot, [nondet]) :-
    % Chinchou kills Staryu, Grotle switches on Lombre and gets Teeter danced, so Vivillon kills it instead
    % Chinchou switches back in on Arrokuda to fast-kill it.

%test(ruin_maniac_georgie, [nondet]) :-
    % Chinchou fast-kills Dwebble with Bubble Beam, Munchlax comes out. Mienfoo switches in on it and kills it.
    % Sandygast is next, Grotle switches in for the kill. Last is Mawile, and Litleo counters it with Incinerate.

%test(tuber_chandler, [nondet]) :-
    % Litleo gets put to sleep and lives long enough to Incinerate Smoochum down.
    % Chinchou cleans up and baits Elekid. Palpitoad on the switchin kills it with Mud Shots.
    % Magby takes another Mud Shot but brings Palpitoad low; Mienfoo switches in and kills with Fake Out.

%test(tuber_chandler, [nondet]) :-
    % Chinchou kills Fletchinder and Confuse Rays Herdier twice. Palpitoad switches in and kills it.

%test(sailor_edmond, [nondet]) :-
    % Chinchou kills Wingull, and Grotle cleans up the rest with Razor Leaf

%test(fisherman_bill, [nondet]) :-
    % Litleo sweeps through the entire team

%test(tuber_ricky, [nondet]) :-
    % Mienfoo kills Aipom, then Palpitoad cleans up

%test(tuber_hailey, [nondet]) :-
    % Vivillon fast-kills Mienfoo, then Palpitoad cleans up

% INTERMEZZO: caught Chewtle, Horsea (with Dragon Scale!!) and Tentacool

% NOT YET FOUGHT
test(camper_gavi, [nondet]) :-
    % Grotle kills Bibarel with 2x Razor Leaf, and cannot get crit.
    % Dont equip a berry because of Pluck; we are uneven HP so Super Fang will not trigger it before
    % Super Fang deals 28 and Pluck 18-22, so we are at 7-11 hp left
    opponent('Camper Gavi', GavisTeam),
    GavisTeam = [Bibarel, Ponyta, Eelektrik, Sunflora, Dustox],
    Grotle = #{ability:"Shell Armor", item:"Oran Berry", ivs:_{atk:25, def:30, hp:31, spa:31, spd:31, spe:7}, level:17, moves:["Bite", "Razor Leaf", "Absorb", "Sand Tomb"], name:"Grotle", nature:"Rash"},
    DamagedGrotle = Grotle.put(_{curHP:7}),
    GavisTeam = [_|GavisTeamMinusBibarel],
    post_ko_switch_in(DamagedGrotle, GavisTeamMinusBibarel, [Next|_]),
    assertion(Next == Ponyta),
    % Okay that doesnt work very well. How about we switch Chinchou into Pluck (so Grotle can have a berry) and kill with Electro Ball?
    % That would be exactly lethal with lowest damage rolls on Razor Leaf and Electro Ball.
    % We are speed tied though, and a Headbutt flinch would end the run... unless we spend our Heart Scale to up Chinchou speed IV from 29 to 31.
    Chinchou = #{ability:"Volt Absorb", item:"Oran Berry", ivs:_{atk:14, def:6, hp:23, spa:30, spd:4, spe:29}, level:17, moves:["Confuse Ray", "Supersonic", "Bubble Beam", "Electro Ball"], name:"Chinchou", nature:"Jolly"},
    HeartScaleChinchou = Chinchou.put(ivs/spe, 31),
    ai_is_faster(Bibarel, Chinchou),
    ai_is_slower(Bibarel, HeartScaleChinchou),
    lowRoll(Grotle, Bibarel, false, "Razor Leaf", Low1),
    lowRoll(Chinchou, Bibarel, false, "Electro Ball", Low2),
    Total is Low1 + Low2,
    assertion(Total == 56), % ie exactly Bibarel max HP
    DamagedChinchou = HeartScaleChinchou.put(_{curHP:36}),  % lets say Pluck deals 10, and Aqua Jet another 10
    post_ko_switch_in(DamagedChinchou, GavisTeamMinusBibarel, [Next1|_]),
    assertion(Next1 == Sunflora),
    % Now Vivillon comes in and kills with 2x Air Cutter, taking one Energy Ball and one Sludge Bomb (if both crit we still live)
    Vivillon = #{ability:"Compound Eyes", item:"Oran Berry", ivs:_{atk:11, def:22, hp:3, spa:28, spd:20, spe:26}, level:17, moves:["Air Cutter", "Struggle Bug", "Stun Spore", "Protect"], name:"Vivillon", nature:"Hasty"},
    DamagedVivillon = Vivillon.put(_{curHP:30}),  % lets say it takes both noncrit and procs its Oran Berry
    post_ko_switch_in(DamagedVivillon, [Ponyta, Eelektrik, Dustox], [Next2|_]),
    assertion(Next2 == Ponyta).
    % Ponyta sees a kill with Flame Wheel. Palpitoad can switch in on that and get off a Mud Shot, which drops its speed _just_ below that of the toad.
    % Another Mud Shot will finish it off. That baits Eelektrik with Mega Drain.
    % From there, Vivillon/Fletchinder taking Mega Drain + Chinchou absorbing another Shock Wave can repeat
    % Litleo can come in on a Mega Drain and Incinerate Eelektriks berry, dealing decent damage too.
    % Grotle can then switch in on a Shock Wave and further chip with Razor Leaf.
    % Finally, Vivillon switches (via Chinchou) on a Mega Drain and finishes with Struggle Bug.
    % Last is Dustox, who Vivillon can 2HKO and Fletchinder can 3HKO.
    
:- end_tests(run6).