:- begin_tests(run3).
test(youngster_calvin, [nondet]) :-
    % only considering these two, as Growlithe can clearly solo but might need to switch out of Swagger/Sand-Attack
    Growlithe = #{ability:"Intimidate", ivs:[2, 20, 6, 5, 12, 0], level:12, moves:["Ember", "Bite", "Covet", "Flame Wheel"], name:"Growlithe", nature:"Naive"},
    Abra = #{ability:"Synchronize", ivs:[7, 9, 12, 5, 19, 4], level:12, moves:["Kinesis"], name:"Abra", nature:"Naive"},
    opponent('Youngster Calvin', [Poochyena, Lillipup, Rookidee]),
    calculate(Growlithe, Poochyena, "Covet", AfterMove1),
    % TODO: these two assertions together constitute a Fast Kill
    assertion(AfterMove1.attacker.rawStats.spe > AfterMove1.defender.rawStats.spe),
    damageRoll(Growlithe, Poochyena, "Covet", "Covet"-[LowRollPercent|_]),
    assertion(LowRollPercent >= 100),
    fast_kill(Growlithe, Poochyena, "Covet").
    % TODO: Poochyena will Quick Attack!
    % TODO: Lillipup and Rookidee are a range to OHKO and deal very little damage back.

test(bug_catcher_rick, [nondet]) :-
    % Growlithe can solo
    Growlithe = #{ability:"Intimidate", ivs:[2, 20, 6, 5, 12, 0], level:12, moves:["Ember", "Bite", "Covet", "Flame Wheel"], name:"Growlithe", nature:"Naive"},
    opponent('Bug Catcher Rick', [Grubbin, Pineco, Sizzlipede]),
    % Growlithe outspeeds all of them
    damageRoll(Growlithe, Grubbin, "Flame Wheel", "Flame Wheel"-[LowRollPercent1|_]),
    assertion(LowRollPercent1 >= 100),
    % TODO: Pineco is actually a 2HKO due to Sturdy!
    damageRoll(Growlithe, Pineco, "Flame Wheel", "Flame Wheel"-[LowRollPercent2|_]),
    assertion(LowRollPercent2 >= 100).
    % TODO: Sizzlipede is a 3HKO after Oran Berry

test(youngster_allen, [nondet]) :-
    % Growlithe might need Surskit depending on rolls (ie Psyduck crit). Surskit owns Litleo anyways
    Growlithe = #{ability:"Intimidate", ivs:[2, 20, 6, 5, 12, 0], level:12, moves:["Ember", "Bite", "Covet", "Flame Wheel"], name:"Growlithe", nature:"Naive"},
    Surskit = #{ability:"Swift Swim", ivs:[19, 21, 10, 1, 10, 19], level:12, moves:["Bubble", "Quick Attack", "Sweet Scent", "Bubble Beam"], name:"Surskit", nature:"Naive"},
    opponent('Youngster Allen', [Skiddo, Litleo, Psyduck]),
    % Growlithe outspeeds all of them, Surskit does too
    damageRoll(Growlithe, Skiddo, "Flame Wheel", "Flame Wheel"-[LowRollPercent1|_]),
    assertion(LowRollPercent1 >= 100),
    % TODO: we are likely to 2HKO Psyduck with Bite, but it does have an Oran Berry
    % TODO: check that even crit doesnt OHKO us
    damageRoll(Psyduck, Growlithe, "Bubble Beam", "Bubble Beam"-[LowRollPercent2|_]),
    assertion(LowRollPercent2 < 100).
    % TODO: Surskit survives whatever Litleo can throw at it and Fast Kills with Bubble Beam
    % Actual: Litleo switched in after Skiddo! Killed it with Bites
    % Actual: switched to Turtwig and killed Psyduck with Absorb

% Lass Tiana and her metronomes are too much of a wildcard. Confirmed Covet doesnt steal Berry Juice though.

test(traithlete_mikey, [nondet]) :-
    Turtwig = #{ability:"Shell Armor", ivs:[28, 31, 26, 31, 15, 31], level:12, moves:["Bite", "Growl", "Absorb", "Confide"], name:"Turtwig", nature:"Relaxed"},
    Growlithe = #{ability:"Intimidate", ivs:[3, 20, 6, 5, 12, 0], level:12, moves:["Ember", "Bite", "Covet", "Flame Wheel"], name:"Growlithe", nature:"Naive"},
    opponent('Triathlete Mikey', [Krabby, Clobbopus, Yanma]),
    dead_to_crit(Growlithe, Yanma, MovesThatOHKOGrowlithe),
    assertion(MovesThatOHKOGrowlithe==[]).
    % The line: Turtwig kills Krabby with Absorb. Yanma comes out, we switch to Growlithe. It takes 2 Sonic Booms with help of Oran Berry.
    % It kills Yanma with 2x Flame Wheel. Then Turtwig switches back into Clobbopus and Absorbs it to death.
    % Actual: we flinch twice on the speed tie to Stomp, but kill Krabby anyways. We hit the Flame Wheel range on Yanma.
    % Clobbopus Bind+Rock Smash (lowering defense!) is a problem, we switch to Skrelp to finish it off.

% Fisherman Darian: Turtwig just smashes through Magikarps, even though Bounce manages to paralyze.

:- end_tests(run3).