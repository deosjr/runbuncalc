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

% caught an Electrike in route 110

%test(team_aqua_grunts_museum, [nondet]) :-
    % This fight goes horribly wrong. Chewtle kills Murkrow with 2x Ice Fang, then Skrelp comes out
    % Palpitoad kills it with Mud Shots. Tirtouga is countered by Grotle with 2x Razor Leaf.
    % Mareanie takes a Bite from Chewtle, then crits it down. Palpitoad kills it as well.
    % Now Frillish comes out and it is a big problem. I can switch between Tentacool tanking Hex and baiting Shock Wave
    % so that Chinchou can alternate between healing on it and using Electro Ball.
    % Confusion x2 works through some of its uses of Recover before it goes down.
    % Whirlipede comes out and I switch to Pidgeotto. It uses Feather Dance x3 bringing Whirlipede to -6.
    % Then it starts chipping with Air Cutter but I misjudge Rollout damage and die.
    % I try tanking the last rollout with Chewtle but it also dies. Palpitoad is the last to die to Whirlipede.
    % Finally Chinchou manages to bring it down.
    % NOTE TO SELF: a steel type wouldve probably made a big difference. Stop delaying Granite Cave

%test(battle_girl_laura, [nondet]) :-
    % Vivillon sweeps, even hitting all its ranges

%test(sailor_brenden, [nondet]) :-
    % Kingdra 2HKOs Farfetchd with Bubble Beam, and Fletchinder solos Heracross with Vivillon as backup

%test(battle_girl_lilith, [nondet]) :-
    % Vivillon solos Makuhita and Mankey, Kingdra switches in on Ledian and gets Ice Punch frozen instantly..
    % steer by sending out Mienfoo, who solos(!) using Fake Out, 2x Rock Smash, and a 4 hit Double Slap ending in crit.

%test(black_belt_takao, [nondet]) :-
    % Vivillon fast-kills Breloom, Kingdra takes care of Buneary, and Vivillon switches in on a Drain Punch to OHKO Mienfoo

%test(black_belt_christian, [nondet]) :-
    % Grotle solos Meditite and chips Machoke to half with a crit Razor Blade. Vivillon finishes.
    % Gurdurr can kill with Rock Throw but sees that Vivillon has a range to fast-kill it, so it Mach Punches (is this guaranteed??)
    % So maybe the fight couldve ended there, but I played (too?) safe and finished it with Kingdra instead.

%test(battle_girl_jocelyn, [nondet]) :-
    % Mienfoo FakeOut + Force Palm take Kecleon very low. It Dizzy Punches, so I steer via Vivillon but take a Thunder Wave para
    % I had an anti-para berry on Mienfoo but not Vivillon, which was intended to take out Hakamo-o, so we steer from here.
    % Vivillon kills Kecleon through para, then Golett gets Absorbed by Grotle. Kingdra switches without berry on Pignite Incinerate.
    % 2x Bubble Beam kills it. Interestingly it went for Flame Charge over its more damaging moves, so Kingdra is very healthy going into Hakamo-o.
    % This lets me kill it with Twister, which is super effective. Vivillon wasnt needed. Backup was Litleo to incinerate the Sitrus berry.

%test(leader_brawly, [nondet]) :-
    % Vivillon vs Kubfu: scout Sucker Punch with Protect (just to test), Draining Kiss hopefully low roll so berry doesnt proc, then Air Cutter
    % If berry does proc, just Draining Kiss one more time first to regain HP.
    % Grotle takes Retaliate better than Kingdra and cannot get crit by it. Does fine vs Lopunny anyways. Absorb first because we need the HP.
    % Kingdra is the perfect counter to Combusken. Again, no berry switch-in on an Incinerate.
    % Lopunny comes back in because it outspeeds and outdamages. Grotle tanks again. Mienfoo needs to clean it up.
    % Hitmontop will probably claim a sacrifice. What can I roll in Granite Cave to counter it? I dont see anything
    % So then Mienfoo is sacced. Kingdra comes in and brings it to half HP, Vivillon switches in and kills with Air Cutter.
    % Surskit hilariously counters Poliwhirl pretty well, and is my #1 choice to sac anyway, so might come along.
    % It can Struggle-Bug to lower its SpA while dealing decent chip damage and tanking Superpower which lowers Atk and Def too.
    % Even if Surskit gets frozen on the switch-in, tanking multiple Superpowers is worth it.
    % Tentacool can then kill it, and is the last potential sacrifice.
    % Kingdra tanks Scraggy and can clear its PowerUp boost. Vivillon can revenge kill from 55%.
    % HOLD UP: I can evolve Surskit to Masquerain, which gets Gust and HP Ground. 
    % It can solo Kubfu, Quick Attack Lopunny, then bait Hitmontop (because it outspeeds and outdamages Combusken) and chip it with Gust.
    % If I had a mon that could tank Rock Slide (Granite Cave!?) it could even switch between them and stack Intimidate.
    % Caught a Rhyhorn, which is perfect for this plan. It can also take Retaliate and bait Drain Punch from Lopunny.
    % Kingdra taking Ice Beam and Maquerain taking Superpower from Poliwhirl also still works.
    % OR we just sac Masquerain here and revenge-kill with Grotle. Kingdra is needed for Combusken who hasnt come out yet, Grotle sets that up.
    % The plan for Scraggy is unchanged. Mienfoo still takes out Lopunny and might get sacced to setup on Scraggy though.
    % ACTUAL: Inner Focus blocks Intimidate.. lame. Kubfu dies, Lopunny gets QuickAttacked, Hitmontop comes out.
    % The switcheroo combo gets it to -4 atk, Rhyhorn chips it down to half HP, then Vivillon safely comes in to finish it off.
    % Scraggy comes out and want to Rock Tomb, so we get to switcheroo again, but Masquerain is now very low and we only Intimidate once.
    % switch to Grotle and Sand Tomb + Absorb until it is low, then Vivillon comes in to finish with Draining Kiss.
    % Rhyhorn tanks a Retaliate from Lopunny and lives at 3hp. Masquerain is sacced to Intimidate (it couldve tanked that Retaliate!!)
    % now Mienfoo comes in a little more safe. Im afraid of Headbutt flinch, so I switch Grotle into an oncoming attack so Mienfoo gets another FakeOut off.
    % Combusken comes out, and Kingdra finally steps up. I get lucky, both taking an Incinerate and slow-killing with one Bubble Beam.
    % Poliwhirl is last. Vivillon and Kingdra are the only mons left with high HP, and they bait/tank Superpower/IceBeam pretty well.
    % There is no point though, because they are both special attackers, so stalling out Superpower does nothing.
    % Kingdra gets a Twister off before it gets low. I dont risk a crit and switch Vivillon, who Draining Kiss + Air Cutter wins!

%test(bug_catcher_lyle, [nondet]) :-
    % lets work backwards for once, since I really want Litleo to slowkill Ninjask with Incinerate.
    % That means it needs to come in on a Leech Life or ideally on a Giga Drain.
    % Rhyhorn can take out Ariados and bait Giga Drain, but it may need chip damage to be safe. Smack down deals at least 60%.
    % Kingdra could do it with a Bubble Beam, but it baits Pin Missile.
    % Grotle Sand Tomb + Mienfoo (switching in on Pin Missile) with Fake Out is the best I can do without risking deaths to crits.
    % Kricketune is met by Kingdra and if it chips even once, Vivillon will switch in to finish.
    % ACTUAL: only thing that couldve gone better is if I didnt equip a berry to Kingdra, because it got Bug bitten..

%test(bug_maniac_james, [nondet]) :-
    % Kingdra kills Larvesta. Ribombee comes out because it outspeeds, I go for Bubble Beam chip and crit kill instead.
    % Vivillon would have been the answer. Next is Vibrava, for which I brought Grotle without a berry.
    % Grotle 2-shots it and has high HP remaining, so it can stay in to try and kill Shedinja with Bite.
    % That works; Fletchinder was in the back to Aerial Ace through Bright Powder just in case.

%test(rich_boy_winston, [nondet]) :-
    % Kingdra tanks Furfrou and Bubble Beams twice, then Vivillon comes in to kill it and fast-kills Mightyena.

%test(fisherman_ivan, [nondet]) :-
    % Vivillon solos Qwilfish by baiting Dive and Protecting when it hits.
    % Sealeo gets solod by Litleo using Incinerate because it prefers Aurora Beam over Whirlpool.
    % Litleo then baits Seadra to use Brine, so Grotle can switch in and Mega Drain for the win.

%test(twins_gina_and_mia, [nondet]) :-
    % first (true) double battle!
    % Vivillon baits Electroweb and Protects while Grotle uses Razor Leaf. (we could learn Bug Bite to get rid of Sitrus?)
    % Second turn we switch Vivillon into Chinchou and take no damage.
    % Third we switch Chinchou into Litleo to tank Dazzling Gleam. Grotle Razor Leafs one last time.
    % Mienfoo switches in on Abra to Fake Out. Togedemaru is pretty useless vs Chinchou and Grotle.
    % Kingdra is backup if things go wrong. Lets see if I understand Double Battles, its been a while.
    % ACTUAL: second Razor Leaf is a crit on Clefairy. Chinchou stays in for a turn to chip at Clefairy
    % Lots of switching while I figure targetting AI out. I remember cross-positioning, but thats superseded when AI sees a kill?
    % Anyway, Volt Absorb is MVP here as I Protect and Predict. Clefairy gets taken out first, then Abra, so that Mienfoo can come in.
    % In hindsight, Litleo Unnerve/Incinerate wouldve worked great as a lead to deal with Dedenne!

%test(lass_haley, [nondet]) :-
    % Grotle 2HKOs Lumineon, then Gloom comes out. Litleo can take it on and has low Atk stat vs Strength Sap.
    % Staravia is next because we are low HP and it is faster. Kingdra tanks and kills it, and fast-kills Numel through its Passho berry.
    % ACTUAL: Grotle gets flinched twice in a row without dealing damage, so Chinchou has to come in and Shock Wave it x2 instead.
    % This actually works out better because now Litleo comes in guaranteed on Glooms Magical Leaf.
    % Litleo gets put to sleep after one Incinerate but wakes up in time.
    % Numel comes out first because it sees a kill with Earth Power and Staravia doesnt see a kill yet.
    % Kingdra switches in and kills it, then puts Staravia to 1HP with Aurora Beam. Rhyhorn comes in for the kill with Smack Down.

%test(youngster_joey, [nondet]) :-
    % Kingdra handles Raticate (speed drop makes it even easier), Rhyhorn tanks Retaliate from Linoone and baits Seed Bomb.
    % Grotle can come in on that and spam Mega Drain. Pidgeotto is last, and gets countered by Chinchou.

%test(lass_janice, [nondet]) :-
    % Rhyhorn slow-kills Oricorio and should bait Brionne, whose Liquid Voice Echoed Voice sees a kill.
    % Chinchou kills it fast enough, with Kingdra as backup if things go wrong (x4 resisting).
    % Whimsicott has Prankster + Grass Whistle, so time to equip an anti-sleep berry for the first time.
    % one of Vivillon, Fletchinder or Litleo will have to kill it: all 3 have Chesto Berries.
    % ACTUAL: Rhyhorn takes a Feather Dance so has to Smack Down twice. Brionne is dealt with as planned.
    % Chinchou baits Energy Ball so we first switch Vivillon into Whimsicott.
    % It takes a Grass Whistle but wakes up with the berry and brings it low with Air Cutter.
    % Protect sees it going for Dazzling Gleam next, which crits but doesnt kill and so Vivillon Air Cutters again for the win.

%test(rich_boy_dawson, [nondet]) :-
    % Komala gets walled by Grotle, with an anti-para berry to be sure vs Body Slam.
    % Gyarados is fast and always deals 40 damage, so only mons with more than 70hp + Oran Berry can come in.
    % First we teach one of them Bug Bite to eat the Sitrus berry it has. Then Chinchou can chip a large chunk of HP.
    % Rest can finish it off with more chip damage. Carnivine learns Bug Bite, backup is Vivillon, Kingdra, Mienfoo (with FakeOut)
    % ACTUAL: Komala not only paralyzes twice in a row on Grotle, but proceeds to paralyze TWICE MORE after that.
    % Carnivine is sacrificed. Rest of the team is not healthy enough to risklessly kill Gyarados now.
    % Vivillons second Air Cutter critting is its only chance to stay alive: it doesnt and so it is killed too.
    % Mienfoo FakeOut is just enough to not need even more sacrifices here.

%test(school_kids_jerry_and_johnson, [nondet]) :-
    % I just lost my only Protect user, which hurts a lot.
    % I lead Mienfoo and Electrike, switching for Grotle and FakingOut the Simipour.
    % This runs into Super Fang on Grotle + Icy Wind, but Grotle lives through that, and Mienfoo can Drain Punch Purugly.
    % Razor Leaf cleans Purugly up and brings Simipour low (should that have been a Miracle Seed Razor Leaf?)
    % Sneasel comes out. Grotle baits both Ice moves and switches to Kingdra. Mienfoo leaves Sneasel on 1hp with Drain Punch due to Focus Sash.
    % Tentacool switches in to take the hits headed for Mienfoo, while Kingdra takes out Sneasel.
    % Turns out, this is not a true double, and so Simipour is left alone! HP Rock finished the rest of the fight while the other slot baits hits.

%test(bug_catcher_jose, [nondet]) :-
    % Pinsir takes a Smack Down from Rhyhorn, then I pivot via Fletchinder into Mienfoo to FakeOut + Bounce. This brings it super low.
    % I switch to Rhyhorn again, who dies, then chip the last bit of damage with another FakeOut.
    % Fletchinder pivots taking First Impression and baiting Thunder Fang. Chinchou pivots to Litleo and we kill with Incinerate.
    % Switch to Kingdra, get crit with Bug Buzz from Vivillon, and wipe due to lack of speed.
    
:- end_tests(run6).