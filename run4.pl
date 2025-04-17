:- begin_tests(run4).
%test(youngster_calvin, [nondet]) :-
    %opponent('Youngster Calvin', [Poochyena, Lillipup, Rookidee]),
    % Fletchling kills everything with Aerial Ace.

test(bug_catcher_rick, [nondet]) :-
    Fletchling = #{ability:"Keen Eye", ivs:[29, 27, 23, 9, 26, 26], level:12, moves:["Tackle", "Growl", "Quick Attack", "Aerial Ace"], name:"Fletchling", nature:"Docile"},
    opponent('Bug Catcher Rick', [Grubbin, Pineco, Sizzlipede]),
    assertion(fast_kill_guaranteed(Fletchling, Grubbin, "Aerial Ace")),
    assertion(fast_kill_guaranteed(Fletchling, Pineco, "Aerial Ace")),     % TODO: forgot about Sturdy again!
    assertion(fast_kill_guaranteed(Fletchling, Sizzlipede, "Aerial Ace")).

%test(youngster_allen, [nondet]) :-
    % Fletchling kills everything with Aerial Ace.

% Lass Tiana and her metronomes are too much of a wildcard. Confirmed Covet doesnt steal Berry Juice though.

%test(traithlete_mikey, [nondet]) :-
    % Lotad 2 shots Krabby, Fletchling kills Yanma using Oran Berry to stay alive + Quick Attack to finish.
    % Clobbopus is dealt with by Piplup, even Plucking its berry
    % Actual: Clobbopus traps Piplup and it lives vs Yanma on 1hp

%test(fisherman_darian, [nondet]) :-
    % Exeggcute again only shoots twice with Bullet Seed, needing Fletchling to clean up
    % Lotad destroys second Magikarp, though I missed that Flail would deal a lot after Focus Sash leaves it at 1 hp first.

%test(lady_cindy, [nondet]) :-
    % Lead with Sizzlipede, who is going to eat berries with Bug Bite and has anti-para berry itself.
    % Shellos counters Phanpy, and Jigglypuff can be cleaned up by Fletchling
    % Actual: Fletchling didn't even need to come out.

%test(team_aqua_grunt) :-
    % Lead Piplup and hope not to flinch. Pluck Carvanha's Berry and bring it low.
    % Switch (via Sizzlipede) to Lotad to finish with Absorb (and hope not to get crit)
    % Exeggcute meets Sizzlipede with Bug Bite, or Fletchling with Aerial Ace.
    % Croagunk we can Exeggcute using Confusion
    % Actual: Sizzlipede Flame Body burned Carvanha, making Lotad more safe to Absorb
    % Exeggcute misses its range and puts Croagunk into +1 Speed Belch.
    % Fletchling risks the crit on switchin and lives, Quick Attack kills.
    % Sizzlipede cleans up Exeggcute with Bug Bites

%test(fisherman_elliot) :-
    % Lombre destroys this team on his own.
    % Actual: some switching to get rid of Teeter Dance confusion, but otherwise Lombre owned.

%test(ruin_maniac_georgie) :-
    % Prinplup OHKOs Dwebble with Bubble Beam. I expect Munchlax to switch in, which Timburr will counter with Low Kick on the switch.
    % Timburr baits Mawile with Covet, switch back to Prinplup (so no berry for him!) and kill with Bubble Beam x2
    % Stay in to finish Sandygast, with Shellos and Lombre ready in the back to take over if needed.
    % Actual: Mawile switches after Dwebble (why?). I forgot, Mawile has Leftovers so Covet would not steal a berry here.
    % Now Munchlax comes in and Belly Drums, and Timburr kills with a Low Kick.
    % Sandygast lands a Hypnosis vs Lombre but gets killed by Fletchinder on a switch.

%test(tuber_chandler) :-
    % Fletchinder OHKOs Smoochum with Flame Charge. Baits Elekid Shock Wave, which Lombre can take, then Mega Drain out.
    % Prinplup kills Magby with Bubble Beam. Exeggcute, Kadabra and Shellos in the back.
    % Actual: swapped between Shellos and Prinplup to get rid of confusion, otherwise as planned.

%test(tuber_lola) :-
    % Prinplup handles Fletchinder, and Herdier possibly too. Otherwise Lombre, Fletchinder, Timburr, Shellos, Exeggcute all tank it.

%test(sailer_edmond) :-
    % Prinplup plucks Wingulls berry, then Lotad switches into Shock Wave and finishes with Fake Out + Mega Drain.
    % Buizel comes out and can Pursuit trap, but 2x Mega Drain should live its Sonic Boom
    % Lotad slow-kills Palpitoad but if we are low we can switch via Fletchinder tanking Sludge and baiting Bubble Beam to another water type
    % Actual: Wingull crits Air Cutter, leaving Lotad at 38/51 vs Buizel in Rain. Lotads Rain Dish heals it 3hp per turn.
    % We are roughly 50% hp vs Palpitoad, and Sludge crit would kill. Fletchinder tanks it (it indeed crits!!).
    % Rain has stopped as we switch back to Prinplup, and it kills with Bubble Beams.

%test(fisherman_bill) :-
    % gets destroyed by Fletchinder, fast killing all but Scatterbug (which is a range instead, and deals at most 10% back)

%test(tuber_ricky) :-
    % Things start to get tricky. Lead Spheal into Aipom, to tank Fake Out and Charm it down. If Fake Out crits, Double Hit crit can kill us,
    % so then we switch into Prinplup. Otherwise, we switch to Fletchinder and Aerial Ace it down. Both lines bait Luxio with Spark.
    % Switch Lombre into that, and Fake Out + Mega Drain it. Exeggcute is in the back if things go wrong.
    % Nidorino gets tanked by Prinplup, and if it is even slightly low, a switch to Kadabra will fast kill with Confusion.
    % Actual: we get Charm off and Fletchinder Aerial Aces through multiple Sand Attacks (which it ignores).
    % Luxio Howls, we Mega Drain it low, but now crit Spark kills Lombre. Switching through Exeggcute (take Spark, bait Bite) to Fletchinder and Quick Attack kills.
    % Nidorino Sand Attacks and Aerial Ace again doensnt care.

test(tuber_hailey, [nondet]) :-
    % Kadabra fast-kills Mienfoo with Confusion, bringing out Flaaffy (as Nidorina is a range).
    % It is likely (guaranteed?) to go for speed control with Thunder Wave, so bring anti-para berry and hopefully 2-shot it.
    % if we do, Kadabra sweeps. If we dont, Lombre finishes Flaaffy and we switch to Prinplup for Nidorina (bring anti-poison)
    Kadabra = #{ability:"Synchronize", item:"Oran Berry", ivs:[20, 18, 7, 5, 31, 30], level:17, moves:["Kinesis", "Confusion"], name:"Kadabra", nature:"Quiet"},
    opponent('Tuber Hailey', [Mienfoo, Nidorina, Flaaffy]),
    assertion(fast_kill_guaranteed(Kadabra, Mienfoo, "Confusion")),
    % test that the only option to switch in is Flaaffy
    post_ko_switch_in(Kadabra, [Nidorina, Flaaffy], SwitchinsPostMienfoo),
    assertion(SwitchinsPostMienfoo == [Flaaffy]).
    % Actual: Flaaffy lives on 1hp after 2 confusions, so we confusion again but are paralyzed.
    % Prinplup switches into Nidorina, Bubble Beams it low, then we switch Lombre but get poisoned, so Fletchinder finishes.

test(camper_gavi, [nondet]) :-
    Geodude = #{ability:"Magnet Pull", ivs:_{atk:10, def:10, hp:29, spa:21, spd:1, spe:3}, level:17, moves:["Spark", "Smack Down", "Rock Smash", "Bulldoze"], name:"Geodude-Alola", nature:"Careful"},
    Lombre = #{ability:"Rain Dish", ivs:_{atk:2, def:25, hp:24, spa:30, spd:16, spe:7}, level:17, moves:["Natural Gift", "Fake Out", "Mega Drain", "Bubble"], name:"Lombre", nature:"Bashful"},
    Kadabra = #{ability:"Synchronize", item:"Oran Berry", ivs:_7018{atk:18, def:7, hp:20, spa:5, spd:31, spe:30}, level:17, moves:["Kinesis", "Confusion", "Hidden Power Psychic"], name:"Kadabra", nature:"Quiet"},
    opponent('Camper Gavi', [Bibarel, Ponyta, Eelektrik, Sunflora, Dustox]),
    ai_is_faster(Bibarel, Geodude),
    highest_damage_move(Bibarel, Geodude, "Super Fang"),
    highRoll(Bibarel, Geodude, false, "Super Fang", 22),
    damageRoll(Geodude, Bibarel, "Spark", _-[SparkLow,_]),
    assertion((57.1 < SparkLow, 57.2 > SparkLow)),
    GeodudeDamaged = Geodude.put(#{curHP:23}),  % 45 - 22
    highest_damage_move(Bibarel, GeodudeDamaged, "Aqua Jet"),
    % switch into Lombre to tank Aqua Jet
    % then Fake Out + Mega Drain kills, taking one Pluck (maybe dont Fake Out, so we steal more HP back)
    % TODO: calc that it is actually dead here
    ai_is_faster(Bibarel, Lombre),
    highest_damage_move(Bibarel, Lombre, "Pluck"),
    LombreDamaged = Lombre.put(#{curHP:25}), % lets say, without crits, its around this value
    post_ko_switch_in(LombreDamaged, [Ponyta, Eelektrik, Sunflora, Dustox], SwitchinsPostBibarel),
    SwitchinsPostBibarel = [SwitchIn1|_],
    assertion(SwitchIn1 == Dustox), % it is faster and sees a kill with Venoshock
    % TODO: calc that Aqua Jet + Pluck BOTH crit still do not kill Lombre (it lives on ~1HP)
    % Switch to Fletchinder to tank Venoshock, then Aerial Ace twice. Take anti-poison berry for Toxic.
    % OR switch to Kadabra, do the same. Fletchinder baits Eelektrik, Kadabra baits Ponyta.
    KadabraDamaged = Kadabra.put(#{curHP:35}),
    post_ko_switch_in(KadabraDamaged, [Ponyta, Eelektrik, Sunflora], SwitchinsPostDustox),
    SwitchinsPostDustox = [SwitchIn2|_],
    assertion(SwitchIn2 == Ponyta).
    % Prinplup then takes on Ponyta, Plucking its Sitrus Berry and Bubble Beaming it
    % Fletchinder is a backup, also taking on Sunflora.
    % If Lombre did not take a crit, it can take on Eelektrik. Kadabra can also help out.
    % Exeggcute could come in on a Shock Wave from Eelektrik and bait a Super Fang?
    % Putting it to sleep could be huge.
    % ACTUAL: Lombre is left on 31/51 after Bibarel going exactly as planned.
    % Kadabra is left on 31/44 after Dustox also going exactly as we calculated.
    % Exeggcute lives 2 Shock Waves as it misses Sleep Powder once, then puts Eelektrik to sleep.
    % Exeggcute max lowrolls Bullet Seed twice(!), lives on 2hp as Eelektrik wakes up.
    % A switch to Kadabra brings it lower and uses up its berry, but Exeggcute is sacced in the end.
    % Fletchinder manages to kill with 2x Flame Charge, which was a range, and is at 27/53 vs Sunflora.
    % It lives a Sludge Bomb, but that poisons and takes it out. First 2 deaths of the run. Kadabra cleans up.
    % AFTERWARDS: dammit Kadabra learns HP Psychic, but I didnt use it. That wouldve changed things a little..

%test(team_aqua_grunts_museum) :-
    % lead Geodude, Spark is a range, Night shade is Murkrows highest dmg move.
    % Kadabra switches in on Skrelp and Lombre on Tirtouga.
    % Geodude Sparks again vs Mareanie, and Tympole is backup.
    % Lombre should deal with Frillish and Sizzlipede + Priplup need to take on Whirlipede.
    % ACTUAL: Geodude hits its range. Skrelp comes out and Lombre tanks a Water Pulse -> Fake Outs.
    % switch to Kadabra, tanking Acid. I AGAIN FORGOT HP PSYCHIC but we two-shot and take 50% ish hp dmg back.
    % Lombre tanks a Brine from Tirtouga. Mega Drain kills it in two shots and we are still almost full HP.
    % Mareanie poisons Geodude but takes a lot of damage. Tympole takes a Venoshock and kills with Spark.
    % Whirlipede comes out next.. Mega Drain kills it in two shots and we are still almost full HP.
    % Mareanie poisons Geodude but takes a lot of damage. Tympole takes a Venoshock and kills with Spark.
    % Whirlipede comes out next. Sizzlipede takes 2 hits from Pin Missile. Ember chips, and it lives a crit Rollout.
    % Switch to Priplup while Whirlipede is locked into Rollouts. It misses its last Rollout and dies to 2x Pluck.
    % Lombre switches in on Shock Wave and kills with 2x Mega Drain.

:- end_tests(run4).