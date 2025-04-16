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

:- end_tests(run4).