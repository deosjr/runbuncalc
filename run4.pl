:- begin_tests(run4).
%test(youngster_calvin, [nondet]) :-
    %opponent('Youngster Calvin', [Poochyena, Lillipup, Rookidee]),
    % Fletchling kills everything with Aerial Ace.

test(bug_catcher_rick, [nondet]) :-
    Fletchling = #{ability:"Keen Eye", ivs:[29, 27, 23, 9, 26, 26], level:12, moves:["Tackle", "Growl", "Quick Attack", "Aerial Ace"], name:"Fletchling", nature:"Docile"},
    opponent('Bug Catcher Rick', [Grubbin, Pineco, Sizzlipede]),
    assertion(fast_kill(Fletchling, Grubbin, "Aerial Ace")),
    assertion(fast_kill(Fletchling, Pineco, "Aerial Ace")),     % NOTE: forgot about Sturdy again!
    assertion(fast_kill(Fletchling, Sizzlipede, "Aerial Ace")).

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

:- end_tests(run4).