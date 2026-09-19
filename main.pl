:- use_module(library(http/json)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_client)).
:- use_module(library(dcg/basics)).
:- use_module(library(pio)).
:- use_module(library(tabling)).

:- dynamic pok/4.
% table the oracle itself, not calculate/4: highRoll/5 and lowRoll/5 go straight
% to calculate_http/5, so tabling one level up left every roll an uncached request
:- table calculate_http/5.

:- initialization(assertTrainerPokemon).
:- initialization(assertExportedPokemon).

box(Box) :-
    findall(P, pok(-1, you, _, P), Box).

% opponent names are in single quotes as atoms
opponent(Name, OppTeam) :-
    findall(P, pok(_, Name, _, P), Team),
    predsort(byIndex, Team, OppTeam).

byIndex(<, T1, T2) :- T1.index < T2.index.
byIndex(>, T1, T2) :- T1.index > T2.index.

print_box :- box(Box), member(P, Box), format("~w = ~W,", [P.name, P, [quoted(true)]]), nl, fail. %"

get_pokemon_by_name(Name, List, Pokemon) :-
    get_by_name_from_list(List, Name, Pokemon).

get_pokemon_by_name(Names, List, Pokemon) :-
    Names = [_|_],
    maplist(get_by_name_from_list(List), Names, Pokemon).

get_by_name_from_list(List, Name, Pokemon) :-
    string(Name),
    member(Pokemon, List),
    Pokemon.name = Name.

% NOTE: looks at _current HP_ and calculates a percentage of _that_
damageRolls(Attacker, Defender, Data) :-
    maplist(damageRoll(Attacker, Defender), Attacker.moves, Data).
    
damageRoll(Attacker, Defender, Move, Range) :-
    calculate(Attacker, Defender, Move, Data),
    % nature is included in stats
    DefenderMaxHP = Data.defender.originalCurHP,
    (Data.damage = [Min|_] ->
        last(Data.damage, Max),
        MinPercentage is Min / DefenderMaxHP * 100,
        MaxPercentage is Max / DefenderMaxHP * 100,
        Range = Move-[MinPercentage, MaxPercentage]
        %format('~1f - ~1f\n', [MinPercentage, MaxPercentage])
    ; 
        Percentage is Data.damage / DefenderMaxHP * 100,
        Range = Move-[Percentage, Percentage]
    ).

highRoll(Attacker, Defender, Crit, Move, High) :-
    calculate_http(Attacker, Defender, Move, Crit, Out),
    ( Out.damage = [_|_] -> last(Out.damage, High) ; High=Out.damage).
lowRoll(Attacker, Defender, Crit, Move, Low) :-
    calculate_http(Attacker, Defender, Move, Crit, Out),
    ( Out.damage = [Low|_] -> true ; Low=Out.damage).

calculate(Attacker, Defender, Move, Out) :-
    calculate_http(Attacker, Defender, Move, false, Out).
calculate_with_crit(Attacker, Defender, Move, Out) :-
    calculate_http(Attacker, Defender, Move, true, Out).

calculate_http(Attacker, Defender, Move, Crit, Out) :-
    Data = json([
        gen=8,
        attackingPokemon=Attacker.name,
        attackingPokemonOptions=Attacker,
        defendingPokemon=Defender.name,
        defendingPokemonOptions=Defender,
        moveName=Move,
        crit=Crit
    ]),
    http_get('http://localhost:3000/calculate', Out, [method(get), post(json(Data)), json_object(dict)]).

fast_kill_guaranteed(Attacker, Defender) :-
    include(fast_kill_guaranteed(Attacker, Defender), Attacker.moves, [_|_]).

% AI sees speed ties as them being faster than the player
% TODO: this will impact fast_kill tests for Player Attackers on speed ties!
% TODO: if Defender has Sturdy and is full HP, we cannot fast kill (unless double-hit moves, does the AI know about those?)
fast_kill_guaranteed(Attacker, Defender, MoveName) :-
    calculate(Attacker, Defender, MoveName, Data),
    Data.attacker.stats.spe > Data.defender.stats.spe,
    damageRoll(Attacker, Defender, MoveName, MoveName-[LowRollPercent|_]),
    LowRollPercent >= 100,
    not(sturdy(Defender, Data)).

fast_kill_possible(Attacker, Defender, MoveName) :-
    calculate(Attacker, Defender, MoveName, Data),
    % Note the >= here vs > on _guaranteed
    Data.attacker.stats.spe >= Data.defender.stats.spe,
    damageRoll(Attacker, Defender, MoveName, MoveName-[_,HighRollPercent]),
    HighRollPercent >= 100,
    not(sturdy(Defender, Data)).

slow_kill_possible(Attacker, Defender, MoveName) :-
    calculate(Attacker, Defender, MoveName, Data),
    Data.attacker.stats.spe < Data.defender.stats.spe,
    damageRoll(Attacker, Defender, MoveName, MoveName-[_,HighRollPercent]),
    HighRollPercent >= 100,
    not(sturdy(Defender, Data)).

ai_is_faster(AI, Player) :-
    AI.moves = [Move|_],
    calculate(AI, Player, Move, Data),
    Data.attacker.stats.spe >= Data.defender.stats.spe.

ai_is_slower(AI, Player) :-
    AI.moves = [Move|_],
    calculate(AI, Player, Move, Data),
    Data.attacker.stats.spe < Data.defender.stats.spe.

dead_to_crit(Defender, Attacker, MovesThatCritKill) :-
    last(Attacker.moves, Move1),
    calculate(Attacker, Defender, Move1, Data),
    MaxHP = Data.defender.stats.hp,
    maplist(highRoll(Attacker, Defender, true), Attacker.moves, Damages),
    zip_unzip(Attacker.moves, Damages, Moves),
    include([_-N]>>(N >= MaxHP), Moves, MovesThatCritKill).

% outdamaging for switchin purposes consideres percentages of current HP, hence use of damageRolls
outdamages(Pokemon, Opponent) :-
    damageRolls(Pokemon, Opponent, PokRolls),
    damageRolls(Opponent, Pokemon, OppRolls),
    predsort(moveRangeByHighest, PokRolls, [_-[_,PokHigh]|_]),
    predsort(moveRangeByHighest, OppRolls, [_-[_,OppHigh]|_]),
    PokHigh > OppHigh.

moveRangeByHighest(>, _-[_,H1], _-[_,H2]) :- H1 < H2.
moveRangeByHighest(<, _-[_,H1], _-[_,H2]) :- H1 > H2.
moveRangeByHighest(=, _-[_,H1], _-[_,H2]) :- H1 = H2.

% TODO: if multiple Pokémon have the same score, the AI sends in their Pokémon in party order
% This would mean there is only ever one option, but with ranges there might be multiple options
% since we do not know exact HP ahead of time.
post_ko_switch_in(Player, OppTeam, Switchins) :-
    maplist(switchin_pair(Player), OppTeam, Scores),
    keysort(Scores, Candidates),
    last(Candidates, Highest-_),
    include([S-_]>>(S==Highest), Candidates, SwitchinsWithScore),
    maplist([S-P,X]>>(X=P), SwitchinsWithScore, Switchins).

switchin_pair(Player, Opponent, Score-Opponent) :-
    switchin_score(Opponent, Player, Score).

%post_ko_switch_ins_after_damage(Monferno, Bibarel, ["Aqua Jet"], PostBibarelGavi, Switchins),
post_ko_switch_ins_after_damage(Winner, Loser, LoserMoves, Party, Switches) :-
    % TODO: LoserMoves is considered to have exactly one move in it atm
    LoserMoves = [Move],
    switchins_after_damage_rolls(Winner, Loser, Move, Party, Switches).

% low/high roll both normal and crit, and see who would come out.
% if all four are the same mon, we have a pretty strong guarantee it will be that one
switchins_after_damage_rolls(Winner, Loser, Move, Party, Switches) :-
    lowRoll(Loser, Winner, false, Move, Low),
    highRoll(Loser, Winner, false, Move, High),
    lowRoll(Loser, Winner, true, Move, LowCrit),
    highRoll(Loser, Winner, true, Move, HighCrit),
    calculate(Loser, Winner, Move, Data),
    DmgLow is Data.defender.originalCurHP - Low,
    LowDmgd = Winner.put(_{curHP: DmgLow}),
    post_ko_switch_in(LowDmgd, Party, [NextLow|_]),
    DmgHigh is Data.defender.originalCurHP - High,
    HighDmgd = Winner.put(_{curHP: DmgHigh}),
    post_ko_switch_in(HighDmgd, Party, [NextHigh|_]),
    DmgLowCrit is Data.defender.originalCurHP - LowCrit,
    LowCritDmgd = Winner.put(_{curHP: DmgLowCrit}),
    post_ko_switch_in(LowCritDmgd, Party, [NextLowCrit|_]),
    DmgHighCrit is Data.defender.originalCurHP - HighCrit,
    HighCritDmgd = Winner.put(_{curHP: DmgHighCrit}),
    post_ko_switch_in(HighCritDmgd, Party, [NextHighCrit|_]),
    sort([NextLow, NextHigh, NextLowCrit, NextHighCrit], Switches).

% score of Pokemon switching in when Opponent is already out
switchin_score(Pokemon, Opponent, 5) :-
    include(fast_kill_possible(Pokemon, Opponent), Pokemon.moves, [_|_]), !.
switchin_score(Pokemon, Opponent, 4) :-
    include(fast_kill_possible(Opponent, Pokemon), Opponent.moves, []),
    include(slow_kill_possible(Pokemon, Opponent), Pokemon.moves, [_|_]), !.
switchin_score(Pokemon, Opponent, 3) :-
    ai_is_faster(Pokemon, Opponent),
    outdamages(Pokemon, Opponent), !.
switchin_score(Pokemon, Opponent, 2) :-
    ai_is_slower(Pokemon, Opponent),
    outdamages(Pokemon, Opponent), !.
switchin_score(Pokemon, Opponent, 1) :-
    ai_is_faster(Pokemon, Opponent), !.
switchin_score(Pokemon, Opponent, 0) :-
    ai_is_slower(Pokemon, Opponent),
    include(fast_kill_possible(Opponent, Pokemon), Opponent.moves, []), !.
switchin_score(Pokemon, Opponent, -1) :-
    ai_is_slower(Pokemon, Opponent),
    include(fast_kill_possible(Opponent, Pokemon), Opponent.moves, [_|_]).

find_line_naive(_, [], []).
find_line_naive(Party, [Lead|Rest], [Switch|Line]) :-
    post_ko_switch_in(Lead, Party, [Switch|_]),
    find_line_naive(Party, Rest, Line).

nuzlocke_switchin(Opponent, Party, Switchins) :-
    maplist(nuzlocke_switchin_pair(Opponent), Party, Scores),
    keysort(Scores, Candidates),
    last(Candidates, Highest-_),
    include([S-_]>>(S==Highest), Candidates, SwitchinsWithScore),
    maplist([S-P,X]>>(X=P), SwitchinsWithScore, Pokemon),
    % subsort by least damage taken from highest move by opponent (noncrit)
    predsort(least_damage_taken(Opponent), Pokemon, Switchins).

least_damage_taken(Attacker, C, P1, P2) :-
    highest_damage_move(Attacker, P1, Move1),
    highRoll(Attacker, P1, false, Move1, Dmg1),
    highest_damage_move(Attacker, P2, Move2),
    highRoll(Attacker, P2, false, Move2, Dmg2),
    reify_comp(C, Dmg1, Dmg2).

reify_comp(>, X, Y) :- X > Y.
reify_comp(<, X, Y) :- X < Y.
reify_comp(=, X, Y) :- X = Y.

nuzlocke_switchin_pair(Opponent, Player, Score-Player) :-
    nuzlocke_switchin_score(Player, Opponent, Score).

% score of Pokemon switching in when Opponent is already out
nuzlocke_switchin_score(Pokemon, Opponent, 6) :-
    include(fast_kill_guaranteed(Pokemon, Opponent), Pokemon.moves, [_|_]), !.
nuzlocke_switchin_score(Pokemon, Opponent, 5) :-
    include(fast_kill_possible(Pokemon, Opponent), Pokemon.moves, [_|_]), !.
nuzlocke_switchin_score(Pokemon, Opponent, 4) :-
    include(fast_kill_possible(Opponent, Pokemon), Opponent.moves, []),
    include(slow_kill_possible(Pokemon, Opponent), Pokemon.moves, [_|_]), !.
    % TODO: and is not dead to a crit, because fast_kill doesnt check that!
nuzlocke_switchin_score(Pokemon, Opponent, 3) :-
    ai_is_faster(Pokemon, Opponent),
    outdamages(Pokemon, Opponent), !.
nuzlocke_switchin_score(Pokemon, Opponent, 2) :-
    ai_is_slower(Pokemon, Opponent),
    outdamages(Pokemon, Opponent), !.
nuzlocke_switchin_score(Pokemon, Opponent, 1) :-
    ai_is_faster(Pokemon, Opponent), !.
    % and is not immediately dead, how about that?
nuzlocke_switchin_score(Pokemon, Opponent, 0) :-
    ai_is_slower(Pokemon, Opponent),
    include(fast_kill_possible(Opponent, Pokemon), Opponent.moves, []), !.
nuzlocke_switchin_score(Pokemon, Opponent, -1) :-
    ai_is_slower(Pokemon, Opponent),
    include(fast_kill_possible(Opponent, Pokemon), Opponent.moves, [_|_]).

find_line_less_naive(_, [], []).
find_line_less_naive(Party, [Lead|Rest], [Switch|Line]) :-
    nuzlocke_switchin(Lead, Party, [Switch|_]),
    find_line_less_naive(Party, Rest, Line).

% find_line_less_naive/3 answers every opponent mon in isolation: whoever is
% already out never gets to stay in, and switching is free. Against Bug Catcher
% Rick that gives [Houndour, Skrelp, Skrelp] while Houndour in fact just sweeps
% -- Skrelp only wins the tiebreak because it takes a few percent less from a
% Pineco that nobody can OHKO anyway (Sturdy), and the switch itself is not charged for.
%
% find_line_sticky/3 keeps whoever is out for as long as it still wins the next
% matchup outright, charges a switch the free hit it takes coming in, and carries
% the remaining HP forward. The dicts it returns therefore have a curHP.
find_line_sticky(Party, OppTeam, Line) :-
    sticky_line(Party, OppTeam, none, Line).

sticky_line(_, [], _, []).
sticky_line(Party, [Opp|Rest], Current, [Out|Line]) :-
    (Current \== none, holds_up(Current, Opp) ->
        % staying in is free, so an incumbent that still holds up keeps the slot
        Out = Current
    ;
        nuzlocke_switchin(Opp, Party, Ranked),
        % every mon that comes in faces the same test the incumbent had to pass.
        % if the box has nobody safe we still have to send something, so fall
        % back on the scorer's first pick -- audit_line/3 will say where that hurts
        % survives_crit/2 is eight oracle calls and holds_up/2 is thousands, so
        % screen on the cheap test first. Sound, because holds_up checks the same
        % thing at the lowest HP of the fight, which is never above the entry HP.
        (member(Fresh, Ranked), entrant(Current, Opp, Fresh, E),
         survives_crit(E, Opp), holds_up(E, Opp) ->
            Out = E
        ;
            Ranked = [First|_],
            entrant(Current, Opp, First, Out)
        )
    ),
    % if we cannot even clearly win we have no honest HP estimate,
    % so carry on with whatever the entrant was left at
    (after_fight(Out, Opp, Survivor) -> true ; Survivor = Out),
    sticky_line(Party, Rest, Survivor, Line).

% a candidate as it arrives: the lead is already out when the fight starts,
% anything else eats a free hit on the way in
entrant(none, _, Pokemon, Pokemon).
entrant(Current, Opponent, Pokemon, Entered) :-
    Current \== none,
    switch_in(Pokemon, Opponent, Entered).

% staying in is free, switching is not: the incoming mon eats the opponent's
% best move on the way in. This is the cost find_line_less_naive never charges.
switch_in(Pokemon, Opponent, Damaged) :-
    once(highest_damage_move(Opponent, Pokemon, Move)),
    calculate(Opponent, Pokemon, Move, Data),
    highRoll(Opponent, Pokemon, false, Move, Damage),
    resolve_dmg(Opponent, Pokemon, Data, Damage, Damaged).

% winning on worst-case rolls is not enough to justify standing there: Houndour
% beats Youngster Allen's Psyduck from 22hp on paper, but only by eating 81%
% Bubble Beams. Our HP only drops as a fight goes on, so it is not enough to check
% the moment we come in either -- every turn the opponent is still alive to throw
% a crit has to survive one.
holds_up(Pokemon, Opponent) :-
    worst_case(Pokemon, Opponent, crit_safe, _).

% lines_1v1/3 enumerates every plausible move pairing on both sides, which is
% exponential in the length of the fight -- Grotle vs Hakamo-o is 1024 lines. But
% holds_up/2 and after_fight/3 only ever want the worst corner of that, so walk
% straight to it: we use our best move and lowroll it, the AI uses its most
% damaging move and highrolls it. Linear in turns instead of exponential.
% (This ignores ai_moves/3 picking priority for a revenge kill, which is a
% simplification, though not one that flatters us.)
worst_case(Pokemon, Opponent, Safety, Survivor) :-
    worst_case(Pokemon, Opponent, Safety, 0, Survivor).

worst_case(Pokemon, Opponent, Safety, Turn, Survivor) :-
    Turn < 25,     % nothing here runs that long; a stall is not a win
    % crits matter at the HP we stand at facing a living opponent, so test here
    % rather than only on the way in
    (Safety == crit_safe -> survives_crit(Pokemon, Opponent) ; true),
    once(highest_damage_move(Pokemon, Opponent, Move)),
    once(highest_damage_move(Opponent, Pokemon, OppMove)),
    move_1v1(Pokemon, Opponent, Move, OppMove, res(NewPokemon, _, NewOpponent, _)),
    (get_dict(curHP, NewOpponent, 0) ->
        Survivor = NewPokemon
    ;
        \+ get_dict(curHP, NewPokemon, 0),
        \+ stalled(Pokemon, NewPokemon, Opponent, NewOpponent),
        Next is Turn + 1,
        worst_case(NewPokemon, NewOpponent, Safety, Next, Survivor)
    ).

survives_crit(Pokemon, Opponent) :-
    lethal_crits(Opponent, Pokemon, []).

% moves of Attacker that outright kill Defender on a crit at the HP Defender has
% right now. dead_to_crit/3 asks this against max HP, which says nothing once we
% are chipped, and chipped is exactly when it matters.
lethal_crits(Attacker, Defender, Moves) :-
    findall(Move,
        (
            member(Move, Attacker.moves),
            calculate(Attacker, Defender, Move, Data),
            highRoll(Attacker, Defender, true, Move, Damage),
            Damage >= Data.defender.originalCurHP
        ),
        Moves).

% find_line_sticky prefers entrants that hold up but cannot invent one the box
% does not have, so check a line before trusting it. Reports, per slot, the
% opponent moves that kill us outright on a crit as we stand there.
audit_line(Line, OppTeam, Unsafe) :-
    findall(PName-OName-Moves,
        (
            nth1(I, Line, P),
            nth1(I, OppTeam, O),
            lethal_crits(O, P, Moves),
            Moves \== [],
            get_dict(name, P, PName),
            get_dict(name, O, OName)
        ),
        Unsafe).

% what we are left with on the same adversarial line, whether or not it was safe
after_fight(Pokemon, Opponent, Survivor) :-
    worst_case(Pokemon, Opponent, ignore, Survivor).

% find the best option to get To out, starting point is From vs Versus.
% for now we guarantee a single pivot pokemon that takes least damage from move baited by From
% and baits one of the lowest damaging moves onto To
% if there are multiple equal options, we take the first
% for now we do not consider situations in which multiple moves are equally likely to be baited
% ie: highest damaging move is bait, ties again are broken by which move is first
% This is not how the AI actually works, it is more complicated than that!
pivot(Versus, From, To, [SoFar|Rest], Via) :-
    naive_pivot(Versus, From, To, Rest, SoFar, Via).

naive_pivot(_, _, _, [], Via, Via).

naive_pivot(Versus, From, To, [P|Rest], SoFar, Via) :-
    pivot_score(Versus, From, To, SoFar, ScoreSoFar),
    pivot_score(Versus, From, To, P, NewScore),
    ( NewScore < ScoreSoFar -> 
        naive_pivot(Versus, From, To, Rest, P, Via)
    ;
        naive_pivot(Versus, From, To, Rest, SoFar, Via)
    ).

:- table pivot_score/5.
pivot_score(Versus, From, To, P, Score) :-
    highest_damage_move(Versus, From, Bait),
    highRoll(Versus, P, false, Bait, BaitHighDmg),
    highest_damage_move(Versus, P, NewBait),
    highRoll(Versus, To, false, NewBait, NewBaitHighDmg),
    Score is BaitHighDmg + NewBaitHighDmg.

fast_kill_possible(Pokemon, Opponent) :-
    member(Move, Pokemon.moves),
    fast_kill_possible(Pokemon, Opponent, Move).

player_moves(Pokemon, Opponent, Moves) :-
    findall(M, highest_damage_move(Pokemon, Opponent, M), Moves).

ai_moves(Pokemon, Opponent, Moves) :-
    priority_moves(Pokemon, Prio),
    ((fast_kill_possible(Opponent, Pokemon), Prio = [_|_]) ->
        Moves = Prio
    ;
        findall(M, highest_damage_move(Pokemon, Opponent, M), Moves)
    ).

% backtracks over all highest damage moves on a tie
highest_damage_move(Attacker, Defender, Move) :-
    damageRolls(Attacker, Defender, Data),
    member(Move-[_,High], Data),
    include({High}/[_-[Low,_]]>>(Low > High), Data, []).

priority_moves(Pokemon, Moves) :-
    Prio = ["Quick Attack", "Aqua Jet", "Mach Punch"],  % todo: and more
    include({Prio}/[X]>>(member(X,Prio)), Pokemon.moves, Moves).

% Two pokemon enter, only one leaves. No switches considered.
% Pokemon is player-controlled, Opponent is an AI
% tabled because holds_up/2 and after_fight/3 both want the same set of lines
:- table lines_1v1/3.
lines_1v1(Pokemon, Opponent, Lines) :-
    findall(Line, line_1v1(Pokemon, Opponent, Line), Lines).

line_1v1(Pokemon, Opponent, Line) :-
    % Todo: only plausible moves
    player_moves(Pokemon, Opponent, PokemonMoves),
    ai_moves(Opponent, Pokemon, OpponentMoves),
    member(Move, PokemonMoves),
    member(OppMove, OpponentMoves),
    move_1v1(Pokemon, Opponent, Move, OppMove, Res),
    Res = res(NewPokemon, _, NewOpp, _),
    ( (NewPokemon.curHP == 0 ; NewOpp.curHP == 0) ->
        Line = [Res]
    ;
        % a turn in which neither side loses HP repeats forever: immunities
        % (Golett takes nothing from Normal or Fighting), status-only movesets.
        % That is a stall, not a win, so the line simply does not exist.
        \+ stalled(Pokemon, NewPokemon, Opponent, NewOpp),
        line_1v1(NewPokemon, NewOpp, More),
        Line = [Res|More]
    ).

% only decidable once both sides carry a curHP, ie from the second turn onwards
stalled(Pokemon, NewPokemon, Opponent, NewOpponent) :-
    get_dict(curHP, Pokemon, HP),
    get_dict(curHP, NewPokemon, HP),
    get_dict(curHP, Opponent, OppHP),
    get_dict(curHP, NewOpponent, OppHP).

move_1v1(Pokemon, Opponent, Move, OppMove, Resolution) :-
    % we are still calculating safe
    calculate(Pokemon, Opponent, Move, Data),
    Data.attacker.stats.spe > Data.defender.stats.spe,   % pokemon is faster than opponent
    lowRoll(Pokemon, Opponent, false, Move, Low),
    resolve_dmg(Pokemon, Opponent, Data, Low, NewOpponent),
    ( NewOpponent.curHP == 0 -> 
        NewPokemon = Pokemon.put(_{curHP:Data.attacker.originalCurHP}),
        Resolution = res(NewPokemon, Move, NewOpponent, none)
    ;
        calculate(Opponent, Pokemon, OppMove, OppData),
        highRoll(Opponent, Pokemon, false, OppMove, High),
        resolve_dmg(Opponent, Pokemon, OppData, High, NewPokemon),
        Resolution = res(NewPokemon, Move, NewOpponent, OppMove)
    ).

move_1v1(Pokemon, Opponent, Move, OppMove, Resolution) :-
    % we are still calculating safe
    calculate(Pokemon, Opponent, Move, Data),
    Data.attacker.stats.spe < Data.defender.stats.spe,   % pokemon is slower than opponent
    calculate(Opponent, Pokemon, OppMove, OppData),
    highRoll(Opponent, Pokemon, false, OppMove, High),
    resolve_dmg(Opponent, Pokemon, OppData, High, NewPokemon),
    ( NewPokemon.curHP == 0 -> 
        NewOpponent = Opponent.put(_{curHP:Data.defender.originalCurHP}),
        Resolution = res(NewPokemon, none, NewOpponent, OppMove)
    ;
        lowRoll(Pokemon, Opponent, false, Move, Low),
        resolve_dmg(Pokemon, Opponent, Data, Low, NewOpponent),
        Resolution = res(NewPokemon, Move, NewOpponent, OppMove)
    ).

% TODO: resolve_1v1 on a speed tie

resolve_dmg(_Pokemon, Opponent, Data, Damage, NewOpponent) :-
    Min is Data.defender.originalCurHP - Damage,
    (sturdy(Opponent, Data) ->
        NewHP is max(1, Min)
    ;
        NewHP is max(0, Min)
    ),
    NewOpponent = Opponent.put(_{curHP:NewHP}).

% Does sturdy prevent OHKO?
sturdy(Defender, Data) :-
    Defender.ability = "Sturdy",
    Data.defender.stats.hp = Data.defender.originalCurHP.

% assumes winner is in first position in results
% an empty list of lines means move_1v1 failed everywhere (eg on an unhandled speed tie),
% which must not read as a win
clear_winner(Lines) :-
    Lines = [_|_],
    maplist(line_winner, Lines).

line_winner(Line) :-
    last(Line, res(_,_,Opponent,Move)),
    ( get_dict(curHP, Opponent, 0) ; Move = none ).

line_winner(Line) :-
    last(Line, res(Pokemon,Move,Opponent,_)),
    get_dict(curHP, Opponent, HP),
    HP > 0,
    calculate(Pokemon, Opponent, Move, Data),
    Data.attacker.stats.spe < Data.defender.stats.spe,   % pokemon is slower than opponent
    lowRoll(Pokemon, Opponent, false, Move, Low),
    HP =< Low.

resolve_last(Line, Pokemon, Opponent) :-
    last(Line, res(OldPokemon,Move,OldOpponent,OppMove)),
    move_1v1(OldPokemon, OldOpponent, Move, OppMove, res(Pokemon, _, Opponent, _)).

% we assume this is a clear_winner set of lines, meaning we only calculate damage taken
lines2pivots(Lines, Team, Pivots) :-
    maplist(line2pivot(Team), Lines, Dupes),
    sort(Dupes, Pivots).

line2pivot(Team, Line, Pivot) :-
    resolve_last(Line, Pokemon, _),
    post_ko_switch_in(Pokemon, Team, [Pivot|_]).

print_lines([]).
print_lines([L|T]) :-
    maplist([X,Y]>>(X=res(P,PM,O,OM),get_dict(name,P,PN),get_dict(name,O,ON),Y=res(PN,PM,ON,OM)), L, PL),
    writeln(PL),
    print_lines(T).

parse_export([P|T]) -->
    parse_export_pokemon(P),
    "\n\n",
    parse_export(T).
parse_export([P]) --> parse_export_pokemon(P), blanks, eos.

parse_export_pokemon(Pokemon) -->
    parse_name_item(N, I),
    "Ability: ", string_without("\n", Ability), "\n",
    "Level: ", integer(L), "\n",
    nonblanks(Nature), " Nature\n",
    "IVs: ", integer(HP), " HP / ", integer(Atk), " Atk / ", integer(Def), " Def / ", integer(SpA), " SpA / ", integer(SpD), " SpD / ", integer(Spe), " Spe\n",
    parse_moves(Moves),
    {string_codes(A, Ability), string_codes(Nat, Nature),
    IVs = _{atk:Atk,def:Def,hp:HP,spa:SpA,spd:SpD,spe:Spe},
    (I==none ->
        Pokemon = #{name:N, ability:A, level:L, nature:Nat, ivs:IVs, moves:Moves};
        Pokemon = #{name:N, item:I, ability:A, level:L, nature:Nat, ivs:IVs, moves:Moves})}.

parse_name_item(N, none) --> string_without("@\n", Name), {string_codes(N, Name)}, "\n".
parse_name_item(N, I) --> parse_name(N), parse_item(I).
parse_name(N) --> string(Name), " @ ", {string_codes(N, Name)}.
parse_item(I) --> string_without("\n", Item), {string_codes(I, Item)}, "\n".
parse_move(M) --> "- ", string_without("\n", Move), {string_codes(M, Move)}.
parse_moves([M]) --> parse_move(M).
parse_moves([M|T]) --> parse_move(M), "\n", parse_moves(T).

% gen8.json abbreviates IV keys and only lists the stats that are not 31,
% eg Brawly's Poliwhirl is _{hp:30, sa:30} for Hidden Power Grass and
% 89 entries carry sp:0 for deliberately slow sets.
iv_key(hp, hp).
iv_key(at, atk).
iv_key(df, def).
iv_key(sa, spa).
iv_key(sd, spd).
iv_key(sp, spe).

trainer_ivs(Trainer, IVs) :-
    Default = _{atk:31,def:31,hp:31,spa:31,spd:31,spe:31},
    (get_dict(ivs, Trainer, Given) ->
        dict_pairs(Given, _, Pairs),
        foldl(put_iv, Pairs, Default, IVs)
    ;
        IVs = Default
    ).

% fail loudly on an unknown key rather than silently dropping the stat
put_iv(Key-Value, In, Out) :-
    (iv_key(Key, Stat) ->
        put_dict(Stat, In, Value, Out)
    ;
        domain_error(trainer_iv_key, Key)
    ).

% box pokemon are the pok/4 facts with index -1, everything else comes from gen8.json.
% without this, reloading duplicates every trainer mon, and opponent/2 then fails
% silently because predsort/3 has no ordering for two mons sharing an index.
retractTrainerPokemon :-
    forall((pok(I, T, N, P), I \== -1), retract(pok(I, T, N, P))).

assertTrainerPokemon :-
    retractTrainerPokemon,
    open("gen8.json", read, Stream),
    % cant use json_read_dict because Vivillion has multiple 'Bug Maniac Jeffrey' keys...
    % same with Magikarp having multiple Fisherman Darian entries
    json_read(Stream, json(JSON)),
    forall(member(Pokemon=json(Trainers), JSON),
        forall(member(Trainer=json(T), Trainers),
            (
                member(index=I, T),
                atom_json_term(A, json(T), []),
                atom_json_dict(A, D, []),
                trainer_ivs(D, IVs),
                WithName = D.put(#{name:Pokemon, ivs:IVs}),
                assertz(pok(I, Trainer, Pokemon, WithName))
            )
        )
    ).

assertExportedPokemon :-
    phrase_from_file(parse_export(Pokemon), "export.txt"),
    retractall(pok(-1, you, _, _)),
    forall(member(P, Pokemon),
        (
            assertz(pok(-1, you, P.name, P))
        )
    ).

zip_unzip(Names,Values,Zipped) :- maplist([N,V,N-V]>>true,Names,Values,Zipped).

run :-
    Opponent = 'Youngster Calvin',
    %Opponent = 'Bug Catcher Rick',
    %Opponent = 'Youngster Allen',
    %Opponent = 'Lady Cindy',
    %Opponent = 'Team Aqua Grunt Petalburg Woods',
    %Opponent = 'Camper Gavi',
    %Opponent = 'Battle Girl Jocelyn',
    opponent(Opponent, OppTeam),
    writeln(OppTeam),
    box(Box),
    writeln(Box),
    forall(member(Opp, OppTeam), (
        forall(member(Pok, Box), (
            last(Pok.moves, Move),
            calculate(Pok, Opp, Move, Data),
            PokSpeed = Data.attacker.stats.spe,
            OppSpeed = Data.defender.stats.spe,
            damageRolls(Pok, Opp, PokDamage),
            damageRolls(Opp, Pok, OppDamage),
            format('~w (spe: ~d) VS ~w (spe: ~d)\n', [Pok.name,PokSpeed,Opp.name,OppSpeed]),
            format('~w\n', [PokDamage]),
            format('~w\n', [OppDamage]),
            include(fast_kill_guaranteed(Pok, Opp), Pok.moves, PokFastKills),
            format('Fast kills: ~w\n', [PokFastKills]),
            dead_to_crit(Pok, Opp, MovesThatCritKill),
            format('Dead to crit: ~w\n', [MovesThatCritKill])
        ))
    )).