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

%test(youngster_allen, [nondet]) :-
    % Delcatty kills everything, though doesnt fast-sweep anymore
    % ACTUAL: FakeOut + WakeUp Slap kill Skiddo and Delcatty hits the range for both other mons, fast-sweeping anyways

%test(lass_tiana, [nondet]) :-
    % FakeOut + WakeUp Slap fast-kills Swirlix (unless crit FakeOut procs Berry Juice), 3x WakeUp Slap should kill Spinda (it has Juice too)
    % ACTUAL: easiest Metronome fight thus far by far

%test(traithlete_mikey, [nondet]) :-
    % I finally figured out why Yanma has Iron Ball! Its not to nerf its Speed Boost a little, its so that Clobbopus is more likely to come out first!
    % So we want to bait Yanma before Clobbopus, to prevent getting trapped by bind and Sonic Boomed to death.
    % Delcatty is our most reliable Yanma counter on the switch, with FakeOut + 2x WakeUp Slap. It can take 2x Sonic Boom with an Oran Berry
    % To bait Yanma, we need to get our lead pokemon to kill Krabby with 20hp or less remaining, and to resist Clobbopus Rock Smash.
    % Exeggcute is the perfect pokemon to do this; it can stall with stun spore and take damage until low enough, and kill with Bullet Seed
    % When Clobbopus finally comes out, we switch Fletchling in and kill with 2x Aerial Ace.
    % NOTE: I forgot, we even get some new encounters first! But we dont need those right now
    % ACTUAL: Krabby is actually guaranteed to Aqua Jet because it sees Exeggcute kiling it with Bullet Seed.
    % Yanma does come out, Delcatty hits a range and has 30/40 hp remaining when it goes down.
    % Clobbopus is still scary with Defense-lowering Rock Smash, but we manage to kill it with a crit.

%test(fisherman_darian, [nondet]) :-
    % Delcatty should solo this.. and does (with a switchin from Ponyta onto Bounce, that burns with Flame Body)

%test(lady_cindy, [nondet]) :-
    % Delcatty deals with Minccino but gets paralyzed twice. Ponyta flinches Jigglypuff repeatedly with Stomp and leaves Phanpy burned, for Tympole to clean up.

%test(team_aqua_grunt, [nondet]) :-
    % Delcatty deals with Carvanha, Croagunk meets Fletchling + Exeggcute and Exeggcute is killed by Ponyta with Ember
    % ACTUAL: Delcatty is 30/40hp vs Croagunk after killing Carvanha, so we tank a FakeOut then switch.
    % Fletchling is a speed-tie with Croagunk (which I forgot) so Exeggcute is needed for the kill. Ponyta gets paralyzed (shouldve brought a berry) but kills anyways.

%test(fisherman_elliot, [nondet]) :-
    % Delcatty FakeOut+WakeUp Slap kills Staryu. Lombre comes out, we slap it and switch to Fletchinder for the kill.
    % Back to Delcatty for the Arrokuda to FakeOut+Slap again for the win.
    % ACTUAL: was Lombre guaranteed to Teeter Dance on Delcatty? Either way it was perfect.

%test(ruin_maniac_georgie, [nondet]) :-
    % First fight where Delcatty is pretty useless!
    % Palpitoad kills Dwebble with two Bubbles. Even if it gets Sticky Web off, Monferno still fast-kills Munchlax after it Belly Drums and eats Salac berry.
    % Mawile gets countered by Ponyta and Sandygast by Exeggcute.
    % ACTUAL: Sandygast comes out after Palpitoad kills Dwebble, who does indeed use Sticky Web.
    % Switch to Exeggcute, it sleeps and lives some Astonishes before bringing it low with Bullet Seed. Palpitoad kills with another Bubble.
    % Now Munchlax comes out, and as planned we kill with Low Sweep. Mawile is last and gets Flame Wheeled to death.

% INTERMEZZO: I want to try fishing everywhere possible to ensure mons like Tirtouga and Quilfish
% Route 106 for Spheal or Chewtle, Route 107 and Dewford for Remoraid/Horsea, then Route 104 for Buizel
% ACTUAL: we get Spheal and Carvanha on 106/107, so this plan is already dead. Lets leave 104 and Dewford for now
% and try our luck on Route 109 immediately. Result: Tentacool. Now we definitely go for the grass in 104.
% But we cant! Not until Brawly! Thats a huge mistake! Dewford gives us Tyrogue.

test(tuber_chandler, [nondet]) :-
    Monferno = #{ability:"Vital Spirit", item:"Oran Berry", ivs:_{atk:26, def:31, hp:2, spa:29, spd:31, spe:31}, level:17, moves:["Flame Wheel", "Leer", "Low Sweep", "Mach Punch"], name:"Monferno", nature:"Modest"},
    opponent('Tuber Chandler', [Smoochum, Elekid, Magby]),
    assertion(fast_kill_guaranteed(Monferno, Smoochum, "Flame Wheel")),
    post_ko_switch_in(Monferno, [Elekid, Magby], SwitchinsPostSmoochum),
    assertion(SwitchinsPostSmoochum == [Elekid]),
    % Elekid dies to 2x Low Sweep, second time should be slower due to speed drop
    ElekidSlowed = Elekid.put(_{boosts:_{spe:(-1)}}),
    ai_is_slower(ElekidSlowed, Monferno).
    % after Elekid, switch to Delcatty to combo off Magby. This line should not need any berries.
    % ACTUAL: Monferno solos the fight.
    
%test(tuber_lola, [nondet]) :-
    % Palpitoad 2-shots Fletchinder with Bubble. Delcatty + Monferno should take out Herdier.
    % ACTUAL: Monferno did not have to come in

%test(sailor_edmond, [nondet]) :-
    % Silk Scarf Delcatty oneshots Wingull, then kills Buizel.
    % Palpitoad is handled by Exeggcute with Spheal as backup.

%test(fisherman_bill, [nondet]) :-
    % Ponyta kills everything, with two more Fire pokemon in the back.

%test(tuber_ricky, [nondet]) :-
    % Delcatty has a faster FakeOut and a high chance to fast-kill Aipom.
    % Nidorino is next, we swap to Monferno to bring it low, then finish with Fletchinder.
    % this baits Spark from Luxio, which we swap Palpitoad into for the win with Mudshot. 

%test(tuber_hailey, [nondet]) :-
    % Exeggcute hard-counters Mienfoo, baiting Nidorina. We switch Delcatty into that, and finish with Fletchinder or Tentacool.
    % This baits Shock Wave from Flaaffy and we swap Palpitoad again.
    % ACTUAL: we can actually pivot to Delcatty via Tentacool tanking Venoshock, baiting Shock Wave.
    % Palpitoad gets confused but Fletchinder tanks a Fire Punch and we switch back.

%test(team_aqua_grunts_museam, [nondet]) :-
    % Delcatty leads into Murkrow. Skrelp does nothing vs Tentacool with HP Electric. Tirtouga meets Exeggcute.
    % Mareanie is again powerless vs Tentacool. Frillish is harder: pivot through Delcatty to Palpitoad, chip it,
    % then switch to Carvanha guaranteed not to eat a Shock Wave and kill with Bite.
    % Whirlipede is last and is killed by Ponyta.
    % ACTUAL: Tirtouga comes out before Skrelp because it outdamages Delcatty.
    % Whirlipede comes out before Frillish again because it outdamages Tentacool and Frillish doesnt.
    % Whirlipede brings Ponyta pretty low with Rollouts, but is burned in the process.
    % We can switch to Palpitoad immediately. It kills Whirlipede tanking Rollouts, and chips Frillish. Carvanha cleans up.

%test(camper_gavi, [nondet]) :-
    % Delcatty now fast-kills Bibarel with FakeOut+Slap. Ponyta gets 2HKOd by Tentacool. Delcatty comes back for Eelektrik.
    % Sunflora and Dustox are killed by Ponyta and Monferno. Lets bring Exeggcute and Hitmonlee as backup.

%test(battle_girl_laura, [nondet]) :-
    % Fletchinder kills Riolu, Exeggcute kills Stufful, and Delcatty fast-kills Mankey with STAB Normal-typed Play Rough

%test(sailor_brenden, [nondet]) :-
    % We want Fletchinder for Heracross, so Delcatty leads and kills Farfetchd. We can pivot via Hitmonlee with FakeOut to guarantee the range.
    % Remember to not wear the Silk Scarf or it gets Knocked Off (and we cant flinch Farfetchd either).

%test(battle_girl_lilith, [nondet]) :-
    % Delcatty combokills Makuhita, then Ledian comes in. Switch to Monferno to kill it, then back to Delcatty to combo through Mankeys Focus Sash.
    % ACTUAL: Mankey does come out first, because it sees we dont OHKO it due to the Sash!
    % A pivot via Fletchinder puts us back on track.

%test(black_belt_takao, [nondet]) :-
    % Ponyta kills Breloom with Bounce, Buneary comes out and gets Delcattyd, which leaves Mienfoo. Exeggcute can take care of that.
    % ACTUAL: Buneary Cute Charm immobilised Delcatty, so Monferno helps out.

%test(black_belt_christian, [nondet]) :-
    % Delcatty has faster Fake Out and a guaranteed kill after on Meditite.
    % Machoke and Gurdurr are both slower but outdamage, so Machoke comes out next.
    % Fletchinder can take it on, and Exeggcute takes care of Gurdurr after.

% INTERMEZZO: Used the Rare Candy to evolve Boltund.

%test(battle_girl_jocelyn, [nondet]) :-
    % Hitmonlee Double Kick is a guaranteed kill on Kecleon. Golett is next.
    % switch through Delcatty (hoping on Shadow Punch) and take a Drain Punch into Boltund. Bite twice and hope on no crits.
    % Hakamo-o is next because it outdamages. Monferno with HP Dragon should take care of that.
    % Last is Pignite, and Palpitoad should be enough to secure the win. Tentacool in the back can help out if needed.
    % ACTUAL: Pignite comes out before Hakamo-o, it actually also outdamages Boltund.
    % Hakamo-o Dragon Tails Monferno on the switch, dragging out Hitmonlee. It FakeOuts+DoubleKicks, and another tail drags out Boltund.
    % Yet another Dragon Tail brings back Monferno, who finally finishes with HP Dragon.

%test(leader_brawly, [nondet]) :-
    % Fletchinder 2HKOs Kubfu with Aerial Ace (it is speed tied). Lopunny comes in for the Retaliate but gets ejected out after Quick Attack.
    % This should bring in Combusken baiting Thunder Punch. Palpitoad switches in and 2x Mud Shot should kill it.
    % Hitmontop is a speed tie and would come in after on a low Palpitoad; would Pursuit kill it for sure.
    % I dont see a better plan, so lets sac Palpitoad. Ponyta Bounce will have to weaken it, get sacced as well, so that Delcatty can finish it off.
    % Poliwhirl comes in, switch Fletchinder(?) into Superpower or immediately to Boltund for Spark.
    % Hitmonlee deals with Scraggy. Pray we dont sac more to Lopunny in the end.
    % ACTUAL: I get screwed by our own crit Aerial Ace, triggering Kubfus berry and healing it. It does get burned hitting us back.
    % Its just out of range for Quick Attack, so I go for Ace but get Zen Headbutt flinched. Burn brings it in range now and Quick Attack kills.
    % Lopunny gets quick ejected and brings out Poliwhirl (ah, because it is faster!).
    % it goes for Ice Beam instead of Bubble Beam on the switch to Boltund and freezes it..
    % Boltund never thaws and dies. Delcatty revenge kills without taking damage. Lopunny is now back.
    % I opt to stay in and chip with Play Rough. switch to Fletchinder to Quick Attack and get sacrificed.
    % Hitmonlee is next, and it crits Fake Out! Combusken is next, switch to Palpitoad. It chips and dies. Delcatty and Hitmonlee take turns to FakeOut.
    % Hitmontop comes out on 28/52hp Hitmonlee. Monferno dies to it while chipping (shouldve brought Ponyta!). Rest of the team wipes to Hitmontop.
    % AFTERWARDS: I play the fight once more, and win with Hitmonlee and Monferno alive. Boltund couldve lived too but it died trying to paralyze a Limber Lopunny..

:- end_tests(run5).