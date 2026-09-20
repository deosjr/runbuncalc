:- [main].
:- [run7].

% The played run7 Brawly line, from the last paragraph of run7.pl, as a turn
% script. Masquerain is not in run7box4 (Surskit evolves at 22), so it is built
% from Surskit's IVs at level 22 with the moves the notes mention.
assume(assume(rolls(low, high), ignore)).

played_box(Box) :-
    run7box4, box(B0),
    get_pokemon_by_name("Surskit", B0, S),
    M = S.put(_{name:"Masquerain", ability:"Intimidate", level:22,
                moves:["Gust","Quick Attack","Giga Drain","Bubble Beam"]}),
    % the notes buy Beedrill U-Turn with a Heart Scale for the Poliwhirl plan
    get_pokemon_by_name("Beedrill", B0, Bd0), Bd = Bd0.put(_{moves:["U-turn","Pluck","Focus Energy","Pin Missile"]}),
    drop_from_party(B0, S, B1),
    drop_from_party(B1, Bd0, B2),
    Box = [M, Bd|B2].

script([
    % Kubfu -- the game's Kubfu Sucker Punched both turns
    follow(a("Gust"), "Sucker Punch"), follow(a("Gust"), "Sucker Punch"),
    they_sent("Lopunny"),
    % Lopunny: Quick Attack, Eject Button sends it back, Combusken comes out
    a("Quick Attack"),
    they_sent("Combusken"),
    % Combusken: pivot Eelektrik -> Tirtouga, Brine + Aqua Jet
    s("Eelektrik"), s("Tirtouga"), a("Brine"), until_gone(a("Aqua Jet")),
    they_sent("Poliwhirl"),
    % Poliwhirl: Masquerain takes HP Grass, outspeeds with Giga Drain
    s("Masquerain"), until_gone(a("Giga Drain")),
    they_sent("Hitmontop"),
    % Hitmontop: Masquerain/Cufant to -6, Eelektrik Thunder Wave + Spark
    cycle("Cufant", "Masquerain", -6),
    s("Eelektrik"), a("Thunder Wave"), until_gone(a("Spark")),
    they_sent("Lopunny"),
    % Lopunny returns: Masquerain/Tirtouga/Cufant to -6, Pancham kills
    cycle("Cufant", "Masquerain", -6),
    s("Pancham"), until_gone(a("Karate Chop")),
    they_sent("Scraggy"),
    % Scraggy: Covet through Rest
    until_gone(a("Covet"))
]).

t :- played_box(Box), opponent('Leader Brawly', Brawly), get_pokemon_by_name("Masquerain", Box, M0),
     member(Cb, Brawly), get_dict(name, Cb, CbN), atom_string(CbN, "Combusken"), member(Lp, Brawly), get_dict(name, Lp, LpN), atom_string(LpN, "Lopunny"),
     highRoll(Cb, M0, false, "Thunder Punch", TP), highRoll(Lp, M0, false, "Retaliate", RT), highRoll(Lp, M0, false, "Headbutt", HB),
     format("Thunder Punch high roll vs Masquerain: ~w   Lopunny Retaliate ~w Headbutt ~w~n", [TP, RT, HB]),
     t2.
t2 :- played_box(Box), opponent('Leader Brawly', [Kubfu|Rest]), assume(Assume),
     get_pokemon_by_name("Masquerain", Box, Lead),
     drop_from_party(Box, Lead, Bench),
     intimidate(Lead, Kubfu, Faced),
     Battle = battle(Lead, Bench, Faced, Rest, 1, ["Masquerain"]),
     format(">> they send out ~w~n<< we lead ~w~n", [Kubfu.name, Lead.name]),
     script(Script),
     catch(once(run(Battle, Script, 1, Assume)), stop(B), summary(B)).

run(Battle, [], _, _) :- summary(Battle), !.
run(Battle, [until_gone(Act)|More], N, Assume) :- !,
    Battle = battle(_, _, Opp, _, _, _), get_dict(name, Opp, Target),
    repeat_until_gone(Battle, Act, Target, N, Assume, After, N2, 0),
    run(After, More, N2, Assume).
run(Battle, [cycle(A, B, Stage)|More], N, Assume) :- !,
    Battle = battle(_, _, Opp, _, _, _), get_dict(name, Opp, Target),
    repeat_cycle(Battle, A, B, Stage, Target, N, Assume, After, N2, 0),
    run(After, More, N2, Assume).
run(Battle, [expect(HP, Bar)|More], N, Assume) :- !,
    Battle = battle(A, Bn, O, R, T, Br),
    max_hp(O, OMax), Theirs is round(Bar * OMax),
    current_hp(O, OGot),
    (   abs(OGot - Theirs) =< 2
    ->  true
    ;   format("   !! their HP DIVERGES: ~w on ~w in the video, model ~w~n", [O.name, Theirs, OGot])
    ),
    Synced = battle(A, Bn, O.put(_{curHP: Theirs}), R, T, Br),
    run(Synced, [expect(HP)|More], N, Assume).
run(Battle, [expect(HP)|More], N, Assume) :- !,
    Battle = battle(A, _, _, _, _, _), current_hp(A, Got),
    % the model plays worst rolls; the game rolled once. Re-step the last turn at
    % both ends of the roll range and accept the video anywhere inside it.
    (   nb_current(last_turn, t(B0, Action, Opt)),
        findall(H, (member(R, [rolls(low,low), rolls(high,high)]),
                    step(B0, assume(R, ignore), Action, Opt, S), S = battle(A1, _, _, _, _, _), current_hp(A1, H)), Hs),
        Hs = [_|_], min_list(Hs, Lo), max_list(Hs, Hi)
    ->  true
    ;   Lo = Got, Hi = Got
    ),
    (   Got =:= HP
    ->  format("   == video agrees: ~w on ~w~n", [A.name, HP])
    ;   HP >= Lo, HP =< Hi
    ->  format("   == video agrees within rolls: ~w on ~w, model ~w..~w~n", [A.name, HP, Lo, Hi])
    ;   format("   !! DIVERGES: ~w on ~w in the video, model ~w (rolls ~w..~w)~n", [A.name, HP, Got, Lo, Hi])
    ),
    % then follow the game: one roll's difference must not cascade into the next check
    Battle = battle(_, Bn, O, R, T, Br),
    Synced = battle(A.put(_{curHP: HP}), Bn, O, R, T, Br),
    run(Synced, More, N, Assume).
run(Battle, [they_sent(Name)|More], N, Assume) :- !,
    Battle = battle(A, B, Opp, Rest, T, Br),
    get_dict(name, Opp, Cur),
    (   atom_string(Cur, Name)
    ->  After = Battle
    ;   member(Actual, Rest), get_dict(name, Actual, AN), atom_string(AN, Name)
    ->  format("   !! GAME sent ~w here, model sent ~w -- overriding to follow the game~n", [Name, Cur]),
        drop_from_party(Rest, Actual, Rest1),
        % the one the model sent goes back unseen, at the front
        After = battle(A, B, Actual, [Opp|Rest1], 1, Br)
    ;   format("   !! GAME sent ~w here but it is not available (~w)~n", [Name, Cur]), After = Battle
    ),
    run(After, More, N, Assume).
run(Battle, [follow(Act, OppMove)|More], N, Assume) :- !,
    turn(Battle, Act, N, Assume, OppMove, After),
    N2 is N + 1,
    run(After, More, N2, Assume).
run(Battle, [Act|More], N, Assume) :-
    turn(Battle, Act, N, Assume, After),
    N2 is N + 1,
    run(After, More, N2, Assume).

repeat_until_gone(Battle, _, Target, N, _, Battle, N, _) :-
    Battle = battle(_, _, Opp, _, _, _), get_dict(name, Opp, Name), Name \== Target, !.
repeat_until_gone(Battle, _, _, N, _, Battle, N, _) :-
    Battle = battle(_, _, Opp, Rest, _, _), opponents_down(Opp, Rest), !.
repeat_until_gone(Battle, _, Target, N, _, Battle, N, K) :- K >= 8, !,
    format("   !! gave up: ~w still standing after 8 repeats~n", [Target]).
repeat_until_gone(Battle, Act, Target, N, Assume, After, N2, K) :-
    turn(Battle, Act, N, Assume, Next),
    N1 is N + 1, K1 is K + 1,
    repeat_until_gone(Next, Act, Target, N1, Assume, After, N2, K1).

repeat_cycle(Battle, _, _, _, Target, N, _, Battle, N, _) :-
    Battle = battle(_, _, Opp, _, _, _), get_dict(name, Opp, Name), Name \== Target, !,
    format("   !! cycle broken: ~w is no longer out~n", [Target]).
repeat_cycle(Battle, _, _, Stage, _, N, _, Battle, N, _) :-
    Battle = battle(_, _, Opp, _, _, _), boosts_of(Opp, B), get_dict(atk, B, Atk), Atk =< Stage, !,
    format("   -- cycle done, atk at ~w~n", [Atk]).
repeat_cycle(Battle, _, _, _, _, N, _, Battle, N, K) :- K >= 8, !,
    Battle = battle(_, _, Opp, _, _, _), boosts_of(Opp, B), get_dict(atk, B, Atk),
    format("   !! cycle stalled after 8 rounds, atk at ~w~n", [Atk]).
repeat_cycle(Battle, A, B, Stage, Target, N, Assume, After, N2, K) :-
    turn(Battle, s(A), N, Assume, Mid), N1 is N + 1,
    turn(Mid, s(B), N1, Assume, Next), N3 is N1 + 1, K1 is K + 1,
    repeat_cycle(Next, A, B, Stage, Target, N3, Assume, After, N2, K1).

% one scripted action against every move the AI might pick; continue on the worst
turn(Battle, Act, N, Assume, After) :- turn(Battle, Act, N, Assume, worst, After).
turn(Battle, Act, N, Assume, Follow, After) :-
    Battle = battle(Active, Bench, Opp, Rest, OppTurn, _),
    format("~nT~w  ~w vs ~w~n", [N, Active.name, Opp.name]),
    mon_line("  us  ", Active), mon_line("  them", Opp),
    (   concrete(Act, Active, Bench, Opp, Action)
    ->  true
    ;   format("   !! cannot express ~w here~n", [Act]), fail
    ),
    ai_options(Active, Opp, OppTurn, Assume, Options),
    (Action = attack(Mv) -> Shown = attack(Mv) ; Action = switch(P) -> Shown = switch(P.name) ; Action = replace(P) -> Shown = replace(P.name)),
    format("   we: ~w   AI may pick: ~w~n", [Shown, Options]),
    findall(Opt-Out,
        (member(Opt, Options),
         (step(Battle, Assume, Action, Opt, S, H) -> Out = ok(S, H) ; Out = failed)),
        Results),
    forall(member(Opt-Out, Results), show(Opt, Out, Active, Opp)),
    include([_-ok(_,_)]>>true, Results, Oks),
    (   Oks == []
    ->  format("   !! no surviving branch. STOP~n"), throw(stop(Battle))
    ;   length(Results, NR), length(Oks, NO),
        (NR =:= NO -> true ; format("   !! ~w of ~w AI options lose us the Pokemon~n", [NR-NO, NR])),
        (   Follow \== worst, memberchk(Follow-ok(After, _), Oks)
        ->  format("   (following the ~w branch)~n", [Follow]), Chosen = Follow
        ;   findall(Health-S, (member(_-ok(S,_), Oks), party_health(S, Health)), Scored),
            keysort(Scored, [_-After|_]),
            memberchk(Chosen-ok(After, _), Oks)
        ),
        nb_setval(last_turn, t(Battle, Action, Chosen)),
        % what does the post-KO AI say vs what the stepper sent
        After = battle(_, _, NewOpp, _, _, _),
        (   (get_dict(curHP, Opp, 0) ; NewOpp.name \== Opp.name)
        ->  check_next(Active, After, Opp, Rest)
        ;   true
        )
    ).

check_next(_, battle(NewActive, _, NewOpp, _, _, _), _, Rest) :-
    Rest \== [],
    findall(S-N, (member(P, Rest), switchin_score(P, NewActive, S), get_dict(name, P, N)), Scores),
    format("   >> they send out ~w   (switch-in scores vs ~w: ~w)~n", [NewOpp.name, NewActive.name, Scores]).

concrete(a(Move), Active, _, _, attack(Move)) :-
    \+ get_dict(curHP, Active, 0),
    get_dict(moves, Active, Ms), memberchk(Move, Ms).
concrete(s(Name), Active, Bench, _, Action) :-
    member(P, Bench), get_dict(name, P, Name),
    (get_dict(curHP, Active, 0) -> Action = replace(P) ; Action = switch(P)).

show(Opt, failed, _, _) :-
    format("     ~w -> branch FAILS (death, or a refused mechanic)~n", [Opt]).
show(Opt, ok(battle(A, _, O, _, _, _), H), Before, OppBefore) :-
    current_hp(A, AH), current_hp(O, OH),
    current_hp(Before, BH), current_hp(OppBefore, OBH),
    (O.name == OppBefore.name -> Them = OH ; Them = gone),
    format("     ~w -> ~w: us ~w->~w  them ~w->~w", [Opt, H, BH, AH, OBH, Them]),
    (AH > BH, A.name == Before.name -> format("  (we HEALED)") ; true),
    nl.

mon_line(Tag, P) :-
    current_hp(P, HP), max_hp(P, Max), boosts_of(P, B), speed_of(P, Spe),
    dict_pairs(B, _, Pairs), include([_-V]>>(V =\= 0), Pairs, NonZero),
    (get_dict(status, P, St) -> true ; St = none),
    (get_dict(item, P, It) -> true ; It = none),
    format("~w ~w ~w/~w spe ~w boosts ~w status ~w item ~w~n", [Tag, P.name, HP, Max, Spe, NonZero, St, It]).

summary(battle(Active, Bench, Opp, Rest, _, Brought)) :-
    format("~n== end of script ==~n"),
    (opponents_down(Opp, Rest) -> format("   fight won~n") ; format("   fight NOT over: ~w still up, ~w in reserve~n", [Opp.name, Rest])),
    format("   brought ~w~n", [Brought]),
    forall(member(P, [Active|Bench]), mon_line("  ", P)).

% the Hitmontop macro on its own, everyone fresh: is the Intimidate cycle + Thunder
% Wave + Spark a thing the stepper agrees with?
t3 :- played_box(Box), opponent('Leader Brawly', Brawly), assume(Assume),
     member(Top, Brawly), get_dict(name, Top, TN), atom_string(TN, "Hitmontop"),
     get_pokemon_by_name("Masquerain", Box, Lead),
     drop_from_party(Box, Lead, Bench),
     Battle = battle(Lead, Bench, Top, [], 1, ["Masquerain"]),
     format(">> Hitmontop vs a fresh Masquerain~n"),
     Script = [cycle("Cufant", "Masquerain", -6), s("Eelektrik"), a("Thunder Wave"), until_gone(a("Spark"))],
     catch(once(run(Battle, Script, 1, Assume)), stop(B), summary(B)).


% ---------------------------------------------------------------------------
% The fight as recorded on video (youtube XHwJMYBe6Cw, logs/brawly-run7-*.tsv),
% replayed through the stepper. follow/2 pins the AI's move to what it clicked,
% they_sent/1 pins its switch-ins, expect/1 checks our HP against the numerals
% on screen after each turn. Every DIVERGES line is a model gap or a bug.
video_script([
    % Kubfu: Masquerain faster, Zen Headbutt then Sucker Punch, Oran at 27
    follow(a("Gust"), "Zen Headbutt"), expect(46),
    follow(a("Gust"), "Sucker Punch"), expect(37),
    they_sent("Lopunny"),
    a("Quick Attack"), expect(37),
    they_sent("Combusken"),
    follow(s("Eelektrik"), "Thunder Punch"), expect(51),
    follow(s("Tirtouga"), "Work Up"), expect(58),
    follow(a("Brine"), "Double Kick"), expect(27),
    a("Aqua Jet"), expect(27),
    they_sent("Hitmontop"),
    % the Intimidate dance: Staravia twice, then Masquerain three times
    follow(s("Staravia"), "Mach Punch"), expect(38),
    follow(s("Cufant"), "Rock Slide"), expect(53),
    follow(s("Staravia"), "Mach Punch"), expect(33),
    follow(s("Cufant"), "Rock Slide"), expect(34),      % video: a crit
    follow(s("Masquerain"), "Mach Punch"), expect(35),
    follow(s("Cufant"), "Rock Slide"), expect(39),      % Oran
    follow(s("Masquerain"), "Mach Punch"), expect(33),
    follow(s("Cufant"), "Rock Slide"), expect(35),
    follow(s("Masquerain"), "Mach Punch"), expect(31),
    follow(s("Eelektrik"), "Rock Slide"), expect(43),
    follow(a("Thunder Wave"), "Mach Punch"), expect(34),
    follow(a("Spark"), "Rock Slide"), expect(35),       % Oran
    follow(a("Spark"), "Rock Slide"), expect(26),
    a("Spark"), expect(17),                             % video: Hitmontop fully paralysed
    a("Spark"),
    they_sent("Lopunny"),
    follow(s("Tirtouga"), "Headbutt"), expect(20),
    follow(s("Staravia"), "Drain Punch"), expect(23),
    follow(s("Tirtouga"), "Retaliate"), expect(15),
    follow(s("Staravia"), "Drain Punch"), expect(15),
    follow(s("Tirtouga"), "Retaliate"), expect(4),      % video: a crit
    follow(s("Masquerain"), "Drain Punch"), expect(30),
    follow(s("Cufant"), "Retaliate"), expect(31),
    follow(s("Pancham"), "Drain Punch"), expect(54),
    follow(a("Karate Chop"), "Headbutt"), expect(44, 0.59),
    follow(a("Karate Chop"), "Headbutt"), expect(34, 0.50),   % video: Pancham flinched, no Karate Chop
    follow(a("Karate Chop"), "Headbutt"), expect(24, 0.50),   % video: flinched again
    follow(a("Karate Chop"), "Retaliate"),
    they_sent("Poliwhirl"),
    follow(s("Masquerain"), "Superpower"), expect(27, 1.00),
    follow(a("Giga Drain"), "Ice Beam"), expect(27, 0.15),
    a("Giga Drain"), expect(30),
    they_sent("Scraggy"),
    follow(s("Cufant"), "Rock Tomb"), expect(24),
    follow(s("Masquerain"), "Power-Up Punch"), expect(28),
    follow(s("Cufant"), "Rock Tomb"), expect(17),
    follow(s("Masquerain"), "Rock Tomb"), expect(15),      % move name cut off on screen; 13 damage is Rock Tomb
    follow(s("Cufant"), "Rock Tomb"), expect(12),
    follow(s("Staravia"), "Feint Attack"),               % video: Staravia fainted
    s("Pancham"), expect(15),
    until_gone(a("Covet"))
]).

% the box as the video shows it: run7box4 with Surskit evolved, and Eelektrik
% holding an Oran Berry ("Eelektrik's Oran Berry restored health!" at 454s;
% run7box4 has it without an item)
video_box(Box) :-
    run7box4, box(B0),
    get_pokemon_by_name("Surskit", B0, S),
    M = S.put(_{name:"Masquerain", ability:"Intimidate", level:22,
                moves:["Gust","Quick Attack","Giga Drain","Bubble Beam"]}),
    get_pokemon_by_name("Eelektrik", B0, E0), E = E0.put(_{item:"Oran Berry"}),
    get_pokemon_by_name("Cufant", B0, C0), C = C0.put(_{item:"Oran Berry"}),   % "Cufant's Oran Berry restored health!" at 344s
    drop_from_party(B0, S, B1), drop_from_party(B1, E0, B2), drop_from_party(B2, C0, B3), Box = [M, E, C|B3].

tv :- video_box(Box), get_pokemon_by_name("Masquerain", Box, M),
      opponent('Leader Brawly', [Kubfu|Rest]), assume(Assume),
      drop_from_party(Box, M, Bench),
      intimidate(M, Kubfu, Faced),
      Battle = battle(M, Bench, Faced, Rest, 1, ["Masquerain"]),
      video_script(Script),
      catch(once(run(Battle, Script, 1, Assume)), stop(B), summary(B)).
