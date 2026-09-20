:- use_module(library(http/json)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_client)).
:- use_module(library(dcg/basics)).
:- use_module(library(pio)).
:- use_module(library(tabling)).

:- dynamic pok/4.
% table the oracle itself, not calculate/4: highRoll/5 and lowRoll/5 go straight
% to calculate_http/5, so tabling one level up left every roll an uncached request
% The oracle used to be tabled on whole Pokemon dicts. Two problems: the dicts
% carry anonymous sub-dicts (ivs, boosts) whose tags are unbound variables, so
% every tabled call is a call with variables in it, and long planner runs died
% inside the answer tries with a stack error while the stacks were nearly empty;
% and the answers are multi-kilobyte JSON dicts, so the table space ran out. The
% cache below keys on a compact GROUND term built from the dict's pairs instead,
% and stores answers as plain dynamic facts.
:- dynamic oracle_cache/3.
cached(Key, Value, Goal) :-
    term_hash(Key, Hash),
    (   oracle_cache(Hash, Key, Stored)
    ->  Value = Stored
    ;   call(Goal),
        assertz(oracle_cache(Hash, Key, Value))
    ).

% everything the calc reads off a Pokemon, as ground pairs; moves are not part
% of the damage of any one move, so they are left out and the key merges across
% movesets
calc_key(Pokemon, Key) :-
    dict_pairs(Pokemon, _, Pairs),
    findall(K-F,
        (
            member(K-V, Pairs),
            K \== moves,
            (is_dict(V) -> dict_pairs(V, _, F) ; F = V)
        ),
        Key).

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
    % percentages of the defender's CURRENT HP, as the note above says; the calc
    % result no longer carries current HP, so read it off the dict. A fainted
    % defender has no meaningful percentage; treat it as 1 HP rather than divide
    % by zero (the switch-in AI is occasionally asked about a Pokemon that went
    % down the same turn as theirs did).
    current_hp(Defender, CurHP),
    (CurHP > 0 -> DefenderMaxHP = CurHP ; DefenderMaxHP = 1),
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
    damage_rolls(Attacker, Defender, Move, Crit, Rolls),
    (Rolls = [_|_] -> last(Rolls, High) ; High = Rolls).
lowRoll(Attacker, Defender, Crit, Move, Low) :-
    damage_rolls(Attacker, Defender, Move, Crit, Rolls),
    (Rolls = [Low|_] -> true ; Low = Rolls).

% The damage a move does depends on stats, boosts, types, items and abilities --
% not on how much HP either side has left. The search walks through hundreds of
% HP values that all produce the same numbers, and keying the cache on the whole
% Pokemon made every one of them a fresh request. Strip current HP out of the key
% and they collapse onto one entry.
%
% A few moves genuinely do read HP: Brine doubles under half, Reversal and Flail
% scale with the user's. Those keep their HP in the key and pay full price.
% Endeavor brings the target down to the user's HP. The calc hands back 0 for
% it, so the rule lives here. Ghost types are immune, like any Normal move.
% Retaliate doubles to 140 power on the turn after an ally fainted. The Pokemon
% the AI sends after a KO carries retaliateBoost until its first turn is over.
% This is the reason to Quick Attack Lopunny's Eject Button rather than let it
% move, and the reason the switch-in AI rates Lopunny a +5 after Kubfu.
damage_rolls(Attacker, Defender, "Retaliate", Crit, Rolls) :-
    get_dict(retaliateBoost, Attacker, true), !,
    without_hp(Attacker, A), without_hp(Defender, D),
    calc_key(A, KA), calc_key(D, KD),
    cached(calc_bp(KA, KD, "Retaliate", 140, Crit), Out, calculate_bp_raw(A, D, "Retaliate", 140, Crit, Out)),
    Rolls = Out.damage.
damage_rolls(Attacker, Defender, "Endeavor", _, Damage) :- !,
    current_hp(Attacker, Ours),
    current_hp(Defender, Theirs),
    (   calculate(Attacker, Defender, "Endeavor", Data),
        memberchk("Ghost", Data.defender.types)
    ->  Damage = 0
    ;   Damage is max(0, Theirs - Ours)
    ).
damage_rolls(Attacker, Defender, Move, Crit, Rolls) :-
    (hp_sensitive(Move) ->
        A = Attacker, D = Defender
    ;
        without_hp(Attacker, A),
        without_hp(Defender, D)
    ),
    rolls_cached(A, D, Move, Crit, Rolls).

rolls_cached(Attacker, Defender, Move, Crit, Rolls) :-
    calculate_http(Attacker, Defender, Move, Crit, Out),
    Rolls = Out.damage.

without_hp(Pokemon, Stripped) :-
    (get_dict(curHP, Pokemon, _) ->
        del_dict(curHP, Pokemon, _, Stripped)
    ;
        Stripped = Pokemon
    ).

% Getting this list wrong is silent: the cache just serves numbers computed at
% the wrong HP. Super Fang deals half the target's CURRENT hp, and leaving it out
% made Bibarel prefer it against a Geodude it had already chipped.
hp_sensitive("Super Fang").        % half the target's current HP
hp_sensitive("Nature's Madness").
hp_sensitive("Ruination").
hp_sensitive("Endeavor").          % brings the target down to the user's HP
hp_sensitive("Brine").             % doubles under half
hp_sensitive("Reversal").          % scales with the user's HP
hp_sensitive("Flail").
hp_sensitive("Eruption").
hp_sensitive("Water Spout").
hp_sensitive("Wring Out").         % scales with the target's HP
hp_sensitive("Crush Grip").
hp_sensitive("Hard Press").
hp_sensitive("Counter").           % scale with damage taken, not a stat
hp_sensitive("Mirror Coat").
hp_sensitive("Metal Burst").
hp_sensitive("Bide").
hp_sensitive("Facade").            % status, which travels with the mon

% Nothing calculate/4's callers read off the result depends on current HP --
% types, stats, species, the damage rolls -- except for the few HP-scaling moves.
% Keying the tabled oracle on HP anyway meant a new multi-kilobyte answer for
% every HP a search walked through, which is what ran the table space out of
% memory at twenty thousand turn-steps. Strip it, so the table stays the size of
% the matchup space rather than the state space.
calculate(Attacker, Defender, Move, Out) :-
    (   hp_sensitive(Move)
    ->  A = Attacker, D = Defender
    ;   without_hp(Attacker, A), without_hp(Defender, D)
    ),
    calculate_http(A, D, Move, false, Out).
calculate_with_crit(Attacker, Defender, Move, Out) :-
    calculate_http(Attacker, Defender, Move, true, Out).

calculate_http(Attacker, Defender, Move, Crit, Out) :-
    calc_key(Attacker, KA),
    calc_key(Defender, KD),
    cached(calc(KA, KD, Move, Crit), Out, calculate_http_raw(Attacker, Defender, Move, Crit, Out)).

calculate_http_raw(Attacker, Defender, Move, Crit, Out) :-
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
    speed_of(Attacker, Fast), speed_of(Defender, Slow), Fast > Slow,
    lowRoll(Attacker, Defender, false, MoveName, Low),
    current_hp(Defender, HP), Low >= HP,
    not(sturdy(Defender, _)).

fast_kill_possible(Attacker, Defender, MoveName) :-
    % Note the >= here vs > on _guaranteed
    speed_of(Attacker, Fast), speed_of(Defender, Slow), Fast >= Slow,
    highRoll(Attacker, Defender, false, MoveName, High),
    current_hp(Defender, HP), High >= HP,
    not(sturdy(Defender, _)).

% The Post-KO Switch-in AI's KO checks. Judged against current HP, which is what
% every observation in switchins.pl supports once Retaliate is given its doubled
% power after a KO (see retaliate_boost below): Lopunny after Kubfu against a
% Masquerain on 37/66 is a +5 because a 140-power Retaliate rolls 36..43. The
% basis stays switchable so that experiment can be repeated.
:- dynamic switchin_hp_basis/1.
switchin_hp_basis(current).
sw_kill_hp(Defender, HP) :-
    (switchin_hp_basis(max) -> max_hp(Defender, HP) ; current_hp(Defender, HP)).
sw_fast_kill_possible(Attacker, Defender, MoveName) :-
    speed_of(Attacker, Fast), speed_of(Defender, Slow), Fast >= Slow,
    highRoll(Attacker, Defender, false, MoveName, High),
    sw_kill_hp(Defender, HP), High >= HP,
    not(sturdy(Defender, _)).
sw_slow_kill_possible(Attacker, Defender, MoveName) :-
    speed_of(Attacker, Slow), speed_of(Defender, Fast), Slow < Fast,
    highRoll(Attacker, Defender, false, MoveName, High),
    sw_kill_hp(Defender, HP), High >= HP,
    not(sturdy(Defender, _)).

slow_kill_possible(Attacker, Defender, MoveName) :-
    speed_of(Attacker, Slow), speed_of(Defender, Fast), Slow < Fast,
    highRoll(Attacker, Defender, false, MoveName, High),
    current_hp(Defender, HP), High >= HP,
    not(sturdy(Defender, _)).

ai_is_faster(AI, Player) :-
    speed_of(AI, Theirs), speed_of(Player, Ours),
    Theirs >= Ours.

ai_is_slower(AI, Player) :-
    speed_of(AI, Theirs), speed_of(Player, Ours),
    Theirs < Ours.

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

% Score of Pokemon switching in when Opponent is already out. Straight from the
% Post-KO Switch-in AI sheet in the Run & Bun docs:
%   +5  faster and OHKOs
%   +4  slower but OHKOs and is not OHKO'd
%   +3  faster and deals more damage than it takes (in %, not raw numbers)
%   +2  slower and deals more damage than it takes
%   +1  faster
%    0  default
%   -1  slower and is OHKO'd
% ties go to the first Pokemon in party order, which post_ko_switch_in/3 gets for
% free from keysort/2 being stable over an OppTeam already sorted by index.
%
% The sheet lists Ditto and Wobbuffet/Wynaut as special cases scoring +2. It does
% not say whether that is an absolute score or a floor; read here as absolute,
% which is the literal reading. Maxie's Wobbuffet is the one that matters.
switchin_score(Pokemon, _, 2) :-
    Pokemon.name == 'Ditto', !.
switchin_score(Pokemon, Opponent, Score) :-
    memberchk(Pokemon.name, ['Wobbuffet', 'Wynaut']), !,
    (
        ai_is_slower(Pokemon, Opponent),
        include(sw_fast_kill_possible(Opponent, Pokemon), Opponent.moves, [_|_])
    ->
        Score = -1      % slower and OHKO'd, so the special case does not apply
    ;
        Score = 2
    ).
switchin_score(Pokemon, Opponent, 5) :-
    include(sw_fast_kill_possible(Pokemon, Opponent), Pokemon.moves, [_|_]), !.
switchin_score(Pokemon, Opponent, 4) :-
    include(sw_fast_kill_possible(Opponent, Pokemon), Opponent.moves, []),
    include(sw_slow_kill_possible(Pokemon, Opponent), Pokemon.moves, [_|_]), !.
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
    include(sw_fast_kill_possible(Opponent, Pokemon), Opponent.moves, []), !.
switchin_score(Pokemon, Opponent, -1) :-
    ai_is_slower(Pokemon, Opponent),
    include(sw_fast_kill_possible(Opponent, Pokemon), Opponent.moves, [_|_]).

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
    find_line_sticky(Party, OppTeam, assume(rolls(low, high), crit_safe), Line).

find_line_sticky(Party, OppTeam, Assume, Line) :-
    find_line_sticky(Party, OppTeam, Assume, Line, _).

% Parties is the state of the box as each slot is reached, so a report can ask
% "what did I still have at that point" rather than assuming everyone is fresh.
find_line_sticky(Party, OppTeam, Assume, Line, Parties) :-
    sticky_line(Party, OppTeam, Assume, none, Steps),
    maplist([step(P,_),P]>>true, Steps, Line),
    maplist([step(_,B),B]>>true, Steps, Parties).

sticky_line(_, [], _, _, []).
sticky_line([], _, _, _, []) :- !.      % box wiped, nothing left to send out
sticky_line(Party, [Opp|Rest], Assume, Current, [step(Out, Party)|Line]) :-
    next_out(Party, Opp, Assume, Current, Out, _),
    (after_fight(Out, Opp, Assume, Survivor) ->
        % the damage goes back into the box, so later slots pick from the team as
        % it actually is. Without this every switchin looks fresh however battered.
        update_party(Party, Survivor, NewParty),
        Next = Survivor
    ;
        % we do not win this one, which in a nuzlocke means the mon is gone. Its
        % replacement comes in FREE: you choose it when the previous one faints and
        % it does not eat a hit on the way in. Carrying the loser forward and then
        % charging its successor a pivot made every slot after a loss look worse
        % than it is, on top of pretending nothing had happened.
        drop_from_party(Party, Out, NewParty),
        Next = none
    ),
    sticky_line(NewParty, Rest, Assume, Next, Line).

% Who is out against this opponent, and how they got there. Shared by the line
% and the play by play so the two can never disagree about what happens.
%   How = stays                     the incumbent holds, which costs nothing
%       = lead(Name)                first mon of the fight, already out
%       = switch(From, To, Baited, Dmg) came in behind From, eating what From baited
% OutgoingAfter is From after Pursuit has had its say, or `none` if nobody left.
next_out(Party, Opp, Assume, Current, Out, How) :-
    next_out(Party, Opp, Assume, Current, arrived, Out, How).

next_out(Party, Opp, Assume, Current, OppState, Out, How) :-
    next_out(Party, Opp, Assume, Current, OppState, Out, _, How).

next_out(_, Opp, Assume, Current, _, Current, none, stays) :-
    Current \== none,
    holds_up(Current, Opp, Assume), !.
next_out(Party, Opp, Assume, Current, OppState, Out, OutgoingAfter, How) :-
    switch_out(Party, Opp, Assume, Current, OppState, Out, OutgoingAfter, How).

% always leaves, never considers staying. You cannot switch into the Pokemon that
% is already out, so it is not a candidate for its own replacement.
switch_out(Party, Opp, Assume, Current, OppState, Out, OutgoingAfter, How) :-
    (Current == none -> Available = Party ; drop_from_party(Party, Current, Available)),
    Available = [_|_],
    nuzlocke_switchin(Opp, Available, Ranked),
    % every mon that comes in faces the same test the incumbent had to pass. If
    % the box has nobody safe we still have to send something, so fall back on the
    % scorer's first pick -- audit_line/3 will say where that hurts.
    % survives_crit/2 is eight oracle calls and holds_up/3 is thousands, so screen
    % on the cheap test first. Sound, because holds_up checks the same thing at the
    % lowest HP of the fight, which is never above the entry HP.
    (member(Fresh, Ranked), entrant(Current, Opp, OppState, Fresh, Left, E),
     crit_screen(E, Opp, Assume), holds_up(E, Opp, Assume) ->
        Out = E, Chosen = Fresh, OutgoingAfter = Left
    ;
        Ranked = [First|_],
        entrant(Current, Opp, OppState, First, OutgoingAfter, Out), Chosen = First
    ),
    entry_cost(Current, Chosen, Out, OutgoingAfter, Opp, OppState, How).

entry_cost(none, Chosen, _, _, _, _, lead(Name)) :- !, get_dict(name, Chosen, Name).
% OutgoingAfter is what switch_in/6 already worked out. Recomputing it here meant
% a second oracle call and two chances to disagree -- which is how the trace ended
% up claiming Pursuit hit for 0 and left the target on 0.
entry_cost(Outgoing, Chosen, Entered, OutgoingAfter, Opponent, OppState, How) :-
    get_dict(name, Outgoing, From),
    get_dict(name, Chosen, To),
    (OppState == arrived -> When = first ; When = later),
    once(ai_moves(Opponent, Outgoing, When, Baited)),
    (memberchk("Pursuit", Baited) ->
        % Pursuit hit the one leaving, not the one arriving
        full_hp(Outgoing, Opponent, WasOn),
        get_dict(curHP, OutgoingAfter, LeftOn),
        Taken is WasOn - LeftOn,
        How = pursued(From, To, Taken, LeftOn)
    ;
        Baited = [Move|_],
        full_hp(Chosen, Opponent, Before),
        get_dict(curHP, Entered, After),
        Damage is Before - After,
        How = switch(From, To, Move, Damage)
    ).

% the HP a Pokemon has before anything happens to it this slot
full_hp(Pokemon, Opponent, HP) :-
    (get_dict(curHP, Pokemon, HP) -> true
    ; once(highest_damage_move(Opponent, Pokemon, M)),
      calculate(Opponent, Pokemon, M, Data),
      HP = Data.defender.originalCurHP).

% replace a box member by name, leaving the rest alone. Assumes box names are
% unique, which they are unless you are carrying two of the same species.
update_party([], _, []).
update_party([P|Rest], Damaged, [Out|Tail]) :-
    (get_dict(name, P, Name), get_dict(name, Damaged, Name) ->
        Out = Damaged,
        Tail = Rest
    ;
        Out = P,
        update_party(Rest, Damaged, Tail)
    ).

crit_screen(Pokemon, Opponent, assume(_, crit_safe)) :- !, survives_crit(Pokemon, Opponent).
crit_screen(_, _, _).

% Everything above asks one mon to win a matchup outright, which is why Brawly
% leaves Lopunny, Hitmontop and Scraggy lost at every relaxation level: nothing in
% the box solos them. Real play does not solo them either, it chips with one mon
% and finishes with another. chip/5 is one contributor's share -- attack for as
% long as it can do so safely, then leave -- and beat_together/4 stacks them.

% attack while it stays safe, stop the moment the next turn would not be, and hand
% back how far we got and what we have left. Always succeeds, possibly having done
% nothing at all.
chip(Pokemon, Opponent, Assume, Chipped, Survivor) :-
    chip(Pokemon, Opponent, Assume, 0, Chipped, Survivor).

chip(Pokemon, Opponent, assume(Rolls, Crits), Turn, Chipped, Survivor) :-
    (
        Turn < 25,
        crit_screen(Pokemon, Opponent, assume(Rolls, Crits)),
        once(highest_damage_move(Pokemon, Opponent, Move)),
        once(highest_damage_move(Opponent, Pokemon, OppMove)),
        move_1v1(Pokemon, Opponent, Move, OppMove, Rolls, res(NewPokemon, _, NewOpponent, _)),
        \+ get_dict(curHP, NewPokemon, 0)
    ->
        (get_dict(curHP, NewOpponent, 0) ->
            Chipped = NewOpponent,
            Survivor = NewPokemon
        ;
            Next is Turn + 1,
            chip(NewPokemon, NewOpponent, assume(Rolls, Crits), Next, Chipped, Survivor)
        )
    ;
        Chipped = Opponent,
        Survivor = Pokemon
    ).

% send in whoever takes the most off it, until it is down. Greedy rather than a
% full search: it answers "can this box gang up on that thing at all", not "what
% is the cheapest way". Each contributor after the first eats the switch-in hit.
beat_together(Party, Opponent, Assume, Contributors) :-
    gang_up(Party, Opponent, Assume, none, [], Contributors).

gang_up(_, Opponent, _, _, Acc, Contributors) :-
    get_dict(curHP, Opponent, 0), !,
    reverse(Acc, Contributors).
gang_up(Party, Opponent, Assume, Outgoing, Acc, Contributors) :-
    Party = [_|_],
    findall(Left-(P-Chipped-Survivor),
        (
            member(P, Party),
            entrant(Outgoing, Opponent, P, Entered),
            \+ get_dict(curHP, Entered, 0),
            intimidate(Entered, Opponent, Faced),
            chip(Entered, Faced, Assume, Chipped, Survivor),
            opponent_hp(Chipped, Left)
        ),
        Options),
    keysort(Options, [Best-(Winner-Chipped-Survivor)|_]),
    opponent_hp(Opponent, Before),
    Best < Before,                      % somebody actually took HP off it
    select(Winner, Party, Rest),
    % whoever just left is what the next one switches in behind, and so is what
    % baits the move it eats on the way in
    gang_up(Rest, Chipped, Assume, Survivor, [Winner|Acc], Contributors).

opponent_hp(Opponent, HP) :-
    (get_dict(curHP, Opponent, HP) -> true ; HP = inf).

% the slots a line loses that more than one mon can still take, and the slots
% that stay lost however many we throw at them
line_gang_ups(Line, OppTeam, Party, Assume, Report) :-
    findall(OName-Verdict,
        (
            nth1(I, Line, P),
            nth1(I, OppTeam, O),
            get_dict(name, O, OName),
            \+ slips_absorbed(P, O, Assume, _),          % this slot is lost solo
            (beat_together(Party, O, Assume, Contributors) ->
                maplist([X,Y]>>get_dict(name,X,Y), Contributors, Names),
                Verdict = together(Names)
            ;
                Verdict = unwinnable
            )
        ),
        Report).

% a candidate as it arrives: the lead is already out when the fight starts,
% anything else eats a free hit on the way in
% OppState is `arrived` if the opponent only just came out, which is the one turn
% it can Fake Out, or `settled` if it has been standing there a while.
entrant(Outgoing, Opponent, Pokemon, Entered) :-
    entrant(Outgoing, Opponent, settled, Pokemon, _, Entered).

% OutgoingAfter matters because of Pursuit: the mon leaving can be hit on its way
% out, so the caller has to put that damage back into the box.
entrant(none, _, _, Pokemon, none, Pokemon).
entrant(Outgoing, Opponent, OppState, Pokemon, OutgoingAfter, Entered) :-
    Outgoing \== none,
    switch_in(Outgoing, Pokemon, Opponent, OppState, OutgoingAfter, Entered).

% Staying in is free, switching is not. But what the incoming mon eats is the move
% the AI picked against whoever was still out, not the move that would be best
% against the newcomer -- the AI commits before the switch resolves, which is the
% whole basis of baiting and what pivot/5 was reaching for. Worst case over the
% moves the AI might have chosen, since ties are broken at random.
% Pursuit is the exception to all of this. It does not hit the Pokemon coming in,
% it hits the one leaving, at double power, before the switch resolves. So baiting
% it does not make a pivot cheap -- it makes it expensive for exactly the Pokemon
% you were trying to save, which is the whole point of the move.
switch_in(Outgoing, Pokemon, Opponent, OppState, OutgoingAfter, Damaged) :-
    (OppState == arrived -> When = first ; When = later),
    once(ai_moves(Opponent, Outgoing, When, Baited)),
    (memberchk("Pursuit", Baited) ->
        pursuit_on_switch(Opponent, Outgoing, OutgoingAfter),
        % the newcomer arrives untouched, but still needs an explicit curHP for
        % everything downstream that reads one
        full_hp(Pokemon, Opponent, HP),
        Damaged = Pokemon.put(_{curHP: HP})
    ;
        OutgoingAfter = Outgoing,
        switch_in(Outgoing, Pokemon, Opponent, OppState, Damaged)
    ).

% double base power, aimed at the mon on its way out
pursuit_on_switch(Opponent, Outgoing, Hit) :-
    calculate_pursuit(Opponent, Outgoing, Data),
    (Data.damage = [_|_] -> last(Data.damage, Damage) ; Damage = Data.damage),
    resolve_dmg(Opponent, Outgoing, Data, Damage, Hit).

calculate_pursuit(Attacker0, Defender0, Out) :-
    without_hp(Attacker0, Attacker), without_hp(Defender0, Defender),
    calc_key(Attacker, KA), calc_key(Defender, KD),
    cached(pursuit(KA, KD), Out, calculate_pursuit_raw(Attacker, Defender, Out)).

% a move at a base power the situation changed
calculate_bp_raw(Attacker, Defender, Move, BP, Crit, Out) :-
    Body = json([
        gen=8,
        attackingPokemon=Attacker.name,
        attackingPokemonOptions=Attacker,
        defendingPokemon=Defender.name,
        defendingPokemonOptions=Defender,
        moveName=Move,
        crit=Crit,
        moveOverrides=json([basePower=BP])
    ]),
    http_get('http://localhost:3000/calculate', Out,
             [method(get), post(json(Body)), json_object(dict)]).

calculate_pursuit_raw(Attacker, Defender, Out) :-
    Body = json([
        gen=8,
        attackingPokemon=Attacker.name,
        attackingPokemonOptions=Attacker,
        defendingPokemon=Defender.name,
        defendingPokemonOptions=Defender,
        moveName="Pursuit",
        crit=false,
        moveOverrides=json([basePower=80])
    ]),
    http_get('http://localhost:3000/calculate', Out,
             [method(get), post(json(Body)), json_object(dict)]).

switch_in(Outgoing, Pokemon, Opponent, OppState, Damaged) :-
    (OppState == arrived -> When = first ; When = later),
    ai_moves(Opponent, Outgoing, When, Baited),
    findall(Dmg-Move,
        (member(Move, Baited), highRoll(Opponent, Pokemon, false, Move, Dmg)),
        Options),
    Options = [_|_],
    keysort(Options, Sorted),
    last(Sorted, Damage-Worst),
    calculate(Opponent, Pokemon, Worst, Data),
    resolve_dmg(Opponent, Pokemon, Data, Damage, Damaged).

% no outgoing mon to bait with, so assume the worst: the AI's best move on us
switch_in(Pokemon, Opponent, Damaged) :-
    takes_hit(Pokemon, Opponent, Damaged).

% one round of the opponent's best move at its high roll
takes_hit(Pokemon, Opponent, Damaged) :-
    once(highest_damage_move(Opponent, Pokemon, Move)),
    calculate(Opponent, Pokemon, Move, Data),
    highRoll(Opponent, Pokemon, false, Move, Damage),
    resolve_dmg(Opponent, Pokemon, Data, Damage, Damaged).

% A miss on our side and a flinch on theirs have exactly the same shape: we lose
% a turn, the fight runs one round longer, and we eat one more hit for nothing.
% Call that a slip. A line that survives no slips at all is not worth much, and a
% line that survives every slip usually does not exist -- a 30% flinch move can
% in principle flinch forever. So the useful question is how many a matchup
% absorbs, which is the number runners quote when they say a line has one 1/24
% in it. slips_absorbed/3 answers it; turn_slip_chance/4 prices one turn of it.
slips_absorbed(Pokemon, Opponent, Slips) :-
    slips_absorbed(Pokemon, Opponent, assume(rolls(low, high), ignore), Slips).

slips_absorbed(Pokemon, Opponent, Assume, Slips) :-
    after_fight(Pokemon, Opponent, Assume, Survivor),
    absorb(Survivor, Opponent, 0, Slips).

absorb(Pokemon, Opponent, SoFar, Slips) :-
    (takes_hit(Pokemon, Opponent, Hit), get_dict(curHP, Hit, HP), HP > 0 ->
        Next is SoFar + 1,
        absorb(Hit, Opponent, Next, Slips)
    ;
        Slips = SoFar
    ).

% the chance that a single turn slips: our move misses, or their move flinches us.
% Accuracy and flinch chances are percentages straight out of @pkmn/dex.
turn_slip_chance(Pokemon, Opponent, OurMove, Chance) :-
    calculate(Pokemon, Opponent, OurMove, OurData),
    move_data(OurMove, OurData.defender, Ours),
    Miss is (100 - Ours.accuracy) / 100,
    once(highest_damage_move(Opponent, Pokemon, TheirMove)),
    calculate(Opponent, Pokemon, TheirMove, TheirData),
    move_data(TheirMove, TheirData.defender, Theirs),
    flinch_chance(Theirs, Flinch),
    % either one costs us the turn
    Chance is 1 - (1 - Miss) * (1 - Flinch).

% Run & Bun changes move definitions. The fork's calc data carries its power and
% type changes, but accuracy and effect chances live nowhere in it, so those are
% vanilla gen 8 assumptions unless rnb-move-overrides.json supplies them. Every
% number turn_slip_chance/4 and flinch_chance/2 produce is only as trustworthy as
% this list is empty.
unverified_moves(Pokemon, Opponent, Moves) :-
    findall(M,
        (
            member(M, Pokemon.moves),
            calculate(Pokemon, Opponent, M, Data),
            move_data(M, Data.defender, MoveData),
            MoveData.verified == false
        ),
        Moves).

flinch_chance(MoveData, Chance) :-
    (
        member(Secondary, MoveData.secondaries),
        Secondary.volatileStatus == "flinch"
    ->
        Chance is Secondary.chance / 100
    ;
        Chance = 0
    ).

% winning on worst-case rolls is not enough to justify standing there: Houndour
% beats Youngster Allen's Psyduck from 22hp on paper, but only by eating 81%
% Bubble Beams. Our HP only drops as a fight goes on, so it is not enough to check
% the moment we come in either -- every turn the opponent is still alive to throw
% a crit has to survive one.
holds_up(Pokemon, Opponent) :-
    holds_up(Pokemon, Opponent, assume(rolls(low, high), crit_safe)).

holds_up(Pokemon, Opponent, Assume) :-
    worst_case(Pokemon, Opponent, Assume, _).

% lines_1v1/3 enumerates every plausible move pairing on both sides, which is
% exponential in the length of the fight -- Grotle vs Hakamo-o is 1024 lines. But
% holds_up/2 and after_fight/3 only ever want the worst corner of that, so walk
% straight to it: we use our best move and lowroll it, the AI uses its most
% damaging move and highrolls it. Linear in turns instead of exponential.
% (This ignores ai_moves/3 picking priority for a revenge kill, which is a
% simplification, though not one that flatters us.)
worst_case(Pokemon, Opponent, Assume, Survivor) :-
    worst_case(Pokemon, Opponent, Assume, 0, Survivor).

worst_case(Pokemon, Opponent, assume(Rolls, Crits), Turn, Survivor) :-
    Turn < 25,     % nothing here runs that long; a stall is not a win
    % crits matter at the HP we stand at facing a living opponent, so test here
    % rather than only on the way in
    (Crits == crit_safe -> survives_crit(Pokemon, Opponent) ; true),
    once(highest_damage_move(Pokemon, Opponent, Move)),
    % the opponent's move comes from the AI model rather than "whatever hurts
    % most", so a guaranteed flinch is seen here too and not just in the trace.
    % Worst case is now over rolls given the AI plays as modelled, rather than
    % over rolls and every move it might conceivably pick.
    (Turn =:= 0 -> When = first ; When = later),
    ai_moves(Opponent, Pokemon, When, Candidates),
    worst_opponent_move(Candidates, Opponent, Pokemon, OppMove),
    resolve_turn(Pokemon, Opponent, Move, OppMove, Rolls, res(NewPokemon, _, NewOpponent, _)),
    (get_dict(curHP, NewOpponent, 0) ->
        Survivor = NewPokemon
    ;
        \+ get_dict(curHP, NewPokemon, 0),
        \+ stalled(Pokemon, NewPokemon, Opponent, NewOpponent),
        Next is Turn + 1,
        worst_case(NewPokemon, NewOpponent, assume(Rolls, Crits), Next, Survivor)
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
            highRoll(Attacker, Defender, true, Move, Damage),
            current_hp(Defender, HP),
            Damage >= HP
        ),
        Moves).

% the slots of a line that the assumptions simply lose, with how many lost turns
% the ones we do win can still absorb. A slot reported as lost is not a plan, it
% is a coin flip the line is quietly assuming goes our way.
line_losses(Line, OppTeam, Losses) :-
    line_losses(Line, OppTeam, assume(rolls(low, high), crit_safe), Losses).

line_losses(Line, OppTeam, Assume, Losses) :-
    findall(PName-OName-Verdict,
        (
            nth1(I, Line, P),
            nth1(I, OppTeam, O),
            get_dict(name, P, PName),
            get_dict(name, O, OName),
            (slips_absorbed(P, O, Assume, Slips) -> Verdict = slips(Slips) ; Verdict = lost)
        ),
        Losses).

% Nothing survives every guarantee at once -- against Brawly every slot is lost
% on worst-case rolls. So give them up one at a time, strictest first, and report
% which one had to go. The point is not that a level-2 line is safe; it is that
% you know exactly what you are trusting.
relaxation(0, fully_safe,      assume(rolls(low, high), crit_safe)).
relaxation(1, ignoring_crits,  assume(rolls(low, high), ignore)).
relaxation(2, fair_own_rolls,  assume(rolls(avg, high), ignore)).
relaxation(3, fair_all_rolls,  assume(rolls(avg, avg), ignore)).

% first level of the ladder that wins every slot outright
find_line_deepening(Party, OppTeam, Level-Name, Line) :-
    relaxation(Level, Name, Assume),
    once(find_line_sticky(Party, OppTeam, Assume, Line)),
    line_losses(Line, OppTeam, Assume, Losses),
    \+ member(_-_-lost, Losses),
    !.

% You take six Pokemon into a fight, not the whole box. Planning against the box
% lets a line spend twelve of them, which is not a line, it is a fantasy -- and it
% quietly turns "we lose" into "we win eventually" by feeding bodies in.
max_party(6).

% Picking the six by "best answer to each opposing mon" fills all six slots with
% counters and leaves no room for the mons a plan is actually made of. Most of the
% turns in the run7 Brawly notes are spent on Pokemon that never KO anything: they
% come in, eat a hit, take a stage off the attacker with Intimidate, and leave.
% So: cover the opponents greedily, since one counter often answers several, then
% spend whatever slots that frees on the best pivot fodder.
select_party(Box, OppTeam, Party) :-
    max_party(Max),
    cover_counters(Box, OppTeam, Counters),
    exclude(named_in(Counters), Box, Rest),
    rank_fodder(Rest, OppTeam, Fodder),
    append(Counters, Fodder, Ordered),
    take(Max, Ordered, Party).

% greedy set cover: repeatedly take whoever answers the most opponents nobody has
% answered yet. One Pokemon covering three of them costs one slot, not three.
cover_counters(Box, OppTeam, Counters) :-
    cover_counters(Box, OppTeam, [], Counters).

cover_counters(_, [], Acc, Counters) :- !, reverse(Acc, Counters).
cover_counters(Box, Uncovered, Acc, Counters) :-
    findall(N-P,
        (
            member(P, Box),
            \+ named_in(Acc, P),
            include(counters(P), Uncovered, Beaten),
            length(Beaten, Len),
            Len > 0,
            N is -Len          % negate so keysort puts the best first
        ),
        Scored),
    (Scored == [] ->
        reverse(Acc, Counters)
    ;
        keysort(Scored, [_-Best|_]),
        exclude(counters(Best), Uncovered, Still),
        cover_counters(Box, Still, [Best|Acc], Counters)
    ).

% good enough to be the answer to something: it sees a kill, or it outdamages
% without being killed first
counters(Pokemon, Opponent) :-
    nuzlocke_switchin_score(Pokemon, Opponent, Score),
    Score >= 4.

% What a Pokemon is worth beyond what it beats: being able to come in on something,
% survive long enough to do a job, and leave. Intimidate is worth a lot more than
% that, because the job is the ability itself and it can be done over and over.
rank_fodder(Candidates, OppTeam, Ranked) :-
    map_list_to_pairs(fodder_cost(OppTeam), Candidates, Scored),
    keysort(Scored, Ascending),
    pairs_values(Ascending, Ranked).

fodder_cost(OppTeam, Pokemon, Cost) :-
    aggregate_all(count, (member(O, OppTeam), survives_entry(Pokemon, O)), Entries),
    aggregate_all(count, (member(O, OppTeam), stacks_intimidate(Pokemon, O)), Stacks),
    Cost is -(Entries + 3 * Stacks).       % negative: keysort puts the best first

% can come in and still be standing a turn later, so it can do something and go
survives_entry(Pokemon, Opponent) :-
    takes_hit(Pokemon, Opponent, Once),
    get_dict(curHP, Once, HP), HP > 0,
    takes_hit(Once, Opponent, Twice),
    get_dict(curHP, Twice, HP2), HP2 > 0.

stacks_intimidate(Pokemon, Opponent) :-
    get_dict(ability, Pokemon, "Intimidate"),
    get_dict(ability, Opponent, Ability),
    \+ intimidate_immune(Ability),
    survives_entry(Pokemon, Opponent).

named_in(List, Pokemon) :-
    get_dict(name, Pokemon, Name),
    member(Other, List),
    get_dict(name, Other, Name), !.

take(0, _, []) :- !.
take(_, [], []) :- !.
take(N, [H|T], [H|Rest]) :- N1 is N - 1, take(N1, T, Rest).

dedupe_by_name([], []).
dedupe_by_name([P|Rest], [P|Out]) :-
    get_dict(name, P, Name),
    exclude([X]>>get_dict(name, X, Name), Rest, Filtered),
    dedupe_by_name(Filtered, Out).

% The strictest set of assumptions that still wins every slot. If nothing wins
% them all, the level that loses fewest, so there is always a line to report on
% rather than a bare failure. keysort/2 is stable, so among levels that lose the
% same number of slots the strictest one wins.
best_line(Party, OppTeam, Level-Name, Line) :-
    findall(Lost-(L-N-Ln),
        (
            relaxation(L, N, Assume),
            once(find_line_sticky(Party, OppTeam, Assume, Ln)),
            line_losses(Ln, OppTeam, Assume, Losses),
            include([_-_-lost]>>true, Losses, LostSlots),
            length(LostSlots, Lost)
        ),
        Options),
    keysort(Options, [_-(Level-Name-Line)|_]).

% Everything the model knows about one fight, slot by slot. Takes a box and an
% opposing team and works the whole fight out from full HP, which is right for a
% gym (the docs say gym leaders heal your party first) and for anything you walk
% into fresh.
%
% FUTURE WORK: the other half of this is reporting from a position rather than
% from the start -- "I am three mons in, Seadra is on 20, what now". That needs a
% battle state as a first class term (who is out, everyone's HP and status, what
% the opponent has already revealed and spent) rather than a box plus a team, and
% the search would resume from it instead of leading. The per-slot machinery here
% would carry over unchanged; it is the entry point and the state plumbing that
% are missing.
fight_report(Box, OppTeam, report(Level-Name, Slots)) :-
    select_party(Box, OppTeam, Party),
    best_line(Party, OppTeam, Level-Name, _),
    relaxation(Level, Name, Assume),
    once(find_line_sticky(Party, OppTeam, Assume, Line, Parties)),
    findall(Slot,
        (
            nth1(I, Line, P),
            nth1(I, OppTeam, O),
            nth1(I, Parties, Remaining),    % the box as it stands at this slot
            slot_report(P, O, Remaining, Assume, Slot)
        ),
        Slots).

% Party here is the box as it stands when this slot is reached, not as it started,
% so a together verdict names Pokemon that are still in shape to do the job.
slot_report(Pokemon, Opponent, Party, Assume, slot(OppName, Ours, Verdict, Risks)) :-
    get_dict(name, Opponent, OppName),
    get_dict(name, Pokemon, Ours),
    intimidate(Pokemon, Opponent, Faced),
    (slips_absorbed(Pokemon, Faced, Assume, Slips) ->
        Verdict = solo(Slips)
    ; beat_together(Party, Faced, Assume, Contributors) ->
        maplist([X,Y]>>get_dict(name,X,Y), Contributors, Names),
        Verdict = together(Names)
    ;
        Verdict = unwinnable
    ),
    slot_risks(Pokemon, Opponent, Risks).

slot_risks(Pokemon, Opponent, Risks) :-
    findall(R,
        (
            (lethal_crits(Opponent, Pokemon, Crits), Crits \== [],
             crit_chance(C), R = crit(C, Crits))
        ;
            (ai_moves(Opponent, Pokemon, first, Opening),
             ai_moves(Opponent, Pokemon, later, Later),
             Opening \== Later, R = opens_with(Opening))
        ;
            (unmodelled_healing(Opponent, Item), R = unmodelled(Item))
        ;
            (unverified_moves(Opponent, Pokemon, U), U \== [], R = unverified(U))
        ),
        Risks).

% The same fight told turn by turn instead of slot by slot: who comes out, what
% each side clicks, what it does, and where the fight can slip out of your hands.
% The opponent's move comes from ai_moves/4 rather than "whatever hurts most",
% so this is what the AI is actually likely to do -- where several moves tie the
% AI picks at random, so all of them are listed and the worst is the one played.
fight_trace(Party, OppTeam, Assume, Events) :-
    trace_fight(Party, OppTeam, Assume, none, 1, Events).

% OppTurn counts how long the CURRENT opponent has been out, which is not the same
% as how long our mon has. Fake Out only works on the opponent's own first turn, so
% Hitmontop gets exactly one of them however many of ours it goes through.
trace_fight(_, [], _, _, _, []).
trace_fight([], _, _, _, _, [out_of_pokemon]) :- !.
trace_fight(Party, [Opp|Rest], Assume, Current, OppTurn, Events) :-
    get_dict(name, Opp, OppName),
    (OppTurn =:= 1 -> OppState = arrived, Fresh = sent ; OppState = settled, Fresh = still_out),
    next_out(Party, Opp, Assume, Current, OppState, Out, OutgoingAfter, How),
    % A switch-in can be lethal: the free hit on the way in, or Pursuit catching
    % the one leaving. Either way that is a death, and the replacement after it is
    % free -- carrying the corpse on as the active Pokemon had it "switching out"
    % again on the next turn.
    (get_dict(curHP, Out, 0) ->
        Events = [versus(OppName, Fresh, How), died_on_entry(Out.name)|More],
        drop_from_party(Party, Out, Survivors),
        trace_fight(Survivors, [Opp|Rest], Assume, none, OppTurn, More)
    ;
        continue_fight(Party, Opp, Rest, Assume, OppTurn, OppState, Out, OutgoingAfter,
                       OppName, Fresh, How, Events)
    ).

continue_fight(Party, Opp, Rest, Assume, OppTurn, OppState, Out, OutgoingAfter,
               OppName, Fresh, How, [versus(OppName, Fresh, How)|Events]) :-
    % Pursuit hits the mon on its way out, so that damage goes back in the box
    (OutgoingAfter == none ->
        Party1 = Party
    ; get_dict(curHP, OutgoingAfter, 0) ->
        drop_from_party(Party, OutgoingAfter, Party1)   % Pursuit killed it leaving
    ;
        update_party(Party, OutgoingAfter, Party1)
    ),
    % A pivot is only worth making if whoever comes in can actually do something.
    % Without this the trace chains switches: each mon arrives, finds it would die,
    % leaves again, and the whole box files past the opponent taking entry damage
    % and never attacking. Nobody plays that, and it is not a plan.
    drop_from_party(Party1, Out, Others),
    (worthwhile_pivot(Others, Out, Opp, Assume, OppState, OppTurn) ->
        CanPivot = true
    ;
        CanPivot = false
    ),
    intimidate(Out, Opp, Opp1),
    trace_matchup(Out, Opp1, Assume, OppTurn, CanPivot, Turns, Survivor, OppAfter, Outcome, NextTurn),
    (Outcome = we_faint(_) ->
        % a nuzlocke loses the mon, and the opponent is still standing there, so
        % the next one comes in against the same thing rather than the next slot.
        % It keeps its turn counter: it does not get to Fake Out again.
        drop_from_party(Party1, Survivor, NewParty),
        trace_fight(NewParty, [OppAfter|Rest], Assume, none, NextTurn, More)
    ; Outcome = ejected(_) ->
        % it leaves under its own steam; the next one is already coming in and
        % the ejected mon rejoins the queue behind the rest
        update_party(Party1, Survivor, NewParty),
        append(Rest, [OppAfter], Queue),
        trace_fight(NewParty, Queue, Assume, Survivor, 1, More)
    ; Outcome = pivot(_) ->
        % chip and get out: the mon lives, keeps its damage, and something else
        % comes in to finish the job against the same opponent
        update_party(Party1, Survivor, NewParty),
        trace_fight(NewParty, [OppAfter|Rest], Assume, Survivor, NextTurn, More)
    ;
        update_party(Party1, Survivor, NewParty),
        trace_fight(NewParty, Rest, Assume, Survivor, 1, More)
    ),
    append(Turns, [Outcome|More], Events).

% ---- stat stages ----------------------------------------------------------
% The calc takes a boosts dict, so the oracle side of this was always free; what
% was missing was anything tracking them. Intimidate on entry is the engine the
% run7 Brawly plan runs on: pivot in and out until the thing is at -4 or -6 attack
% and then kill it with something that could never have stood there at -0.
boosts_of(Pokemon, Boosts) :-
    (get_dict(boosts, Pokemon, Boosts) ->
        true
    ;
        Boosts = _{atk:0, def:0, hp:0, spa:0, spd:0, spe:0}
    ).

apply_boost(Pokemon, Stat, Delta, Boosted) :-
    boosts_of(Pokemon, Boosts),
    get_dict(Stat, Boosts, Old),
    New is max(-6, min(6, Old + Delta)),
    put_dict(Stat, Boosts, New, Updated),
    Boosted = Pokemon.put(_{boosts: Updated}).

% Fires every time the holder comes out, including onto something that just
% fainted -- which is what makes the pivot cycle work. But plenty of things shrug
% it off, and two of them turn it against you.
intimidate(Entering, Opponent, Result) :-
    (get_dict(ability, Entering, "Intimidate") ->
        get_dict(ability, Opponent, Ability),
        intimidated(Ability, Opponent, Result)
    ;
        Result = Opponent
    ).

intimidated(Ability, Opponent, Opponent) :-
    intimidate_immune(Ability), !.
% Defiant answers any stat drop with +2 Attack, so Intimidating it is worse than
% doing nothing: the drop lands and then it comes back up two stages.
intimidated("Defiant", Opponent, Result) :- !,
    apply_boost(Opponent, atk, -1, Dropped),
    apply_boost(Dropped, atk, 2, Result).
% Competitive keeps the Attack drop but answers with +2 Special Attack
intimidated("Competitive", Opponent, Result) :- !,
    apply_boost(Opponent, atk, -1, Dropped),
    apply_boost(Dropped, spa, 2, Result).
intimidated(_, Opponent, Result) :-
    apply_boost(Opponent, atk, -1, Result).

% Gen 8 gave the flinch/confusion abilities Intimidate immunity, and Run & Bun is
% gen 8 for anything its own docs do not override. Brawly's Kubfu has Inner Focus,
% so the whole pivot-to-stack plan does nothing to it.
intimidate_immune("Inner Focus").
intimidate_immune("Own Tempo").
intimidate_immune("Oblivious").
intimidate_immune("Scrappy").
intimidate_immune("Clear Body").
intimidate_immune("White Smoke").
intimidate_immune("Full Metal Body").
intimidate_immune("Hyper Cutter").

% whatever the move itself does to stats, from the move data: Power-Up Punch's
% guaranteed +1 attack, Work Up, Charm and Baby-Doll Eyes on the way down
apply_move_boosts(Attacker, Defender, "Focus Energy", NewAttacker, Defender) :- !,
    pumped(Attacker, NewAttacker).
apply_move_boosts(Attacker, Defender, Move, NewAttacker, NewDefender) :-
    (calculate(Attacker, Defender, Move, Data),
     move_data(Move, Data.defender, MoveData) ->
        self_boosts(MoveData, SelfBoosts),
        target_boosts(MoveData, TargetBoosts),
        foldl([S-D, In, Out]>>apply_boost(In, S, D, Out), SelfBoosts, Attacker, NewAttacker),
        foldl([S-D, In, Out]>>apply_boost(In, S, D, Out), TargetBoosts, Defender, NewDefender)
    ;
        NewAttacker = Attacker, NewDefender = Defender
    ).

% Only effects that always land; a 10% chance is a risk, not a plan.
% Three places a stat change can hide, and I was reading one of them:
%   move.self.boosts        Superpower's -1 atk/-1 def, Overheat's -2 spa
%   move.boosts             the target's, unless the move targets the user
%   secondary.self.boosts   Power-Up Punch's +1 atk at 100%
self_boosts(MoveData, Pairs) :-
    findall(Stat-Delta,
        (
            (
                MoveData.self \== null,
                get_dict(boosts, MoveData.self, SelfBoosts),
                get_dict(Stat, SelfBoosts, Delta)
            ;
                % Work Up and the other setup moves target themselves
                MoveData.boosts \== null,
                self_targeting(MoveData.target),
                get_dict(Stat, MoveData.boosts, Delta)
            ;
                member(Secondary, MoveData.secondaries),
                get_dict(chance, Secondary, 100),
                get_dict(self, Secondary, Self),
                get_dict(boosts, Self, Boosts),
                get_dict(Stat, Boosts, Delta)
            )
        ),
        Pairs).

target_boosts(MoveData, Pairs) :-
    findall(Stat-Delta,
        (
            (
                MoveData.boosts \== null,
                \+ self_targeting(MoveData.target),
                get_dict(Stat, MoveData.boosts, Delta)
            ;
                member(Secondary, MoveData.secondaries),
                get_dict(chance, Secondary, 100),
                get_dict(boosts, Secondary, Boosts),
                get_dict(Stat, Boosts, Delta)
            )
        ),
        Pairs).

self_targeting("self").
self_targeting("adjacentAllyOrSelf").

% ---- Eject Button ---------------------------------------------------------
% Lopunny's item drags it out the moment it is hit, which the run7 notes use
% deliberately ("Aqua Jet Eject it") to choose what comes in next. It fires once
% and is gone, so the item is consumed on the way.
ejected(Before, After, TheirHP, Ejected) :-
    TheirHP > 0,
    get_dict(item, After, "Eject Button"),
    get_dict(curHP, Before, WasOn),
    TheirHP < WasOn,                 % only on taking damage
    Ejected = After.put(_{item: ""}).

% ---- pinch berries --------------------------------------------------------
% Mechanic Changes: "Confuse inducing berries: Restore half HP, triggering at 1/4
% HP" -- half, not the vanilla third. Brawly's Kubfu carries an Iapapa, so without
% this it is worth half a Pokemon less than it really is.
%
% They also confuse anyone whose nature dislikes the flavour. Kubfu is Jolly,
% which dislikes Dry, so its own Iapapa (Sour) never confuses it.
pinch_berry("Figy Berry", spicy).
pinch_berry("Wiki Berry", dry).
pinch_berry("Mago Berry", sweet).
pinch_berry("Aguav Berry", bitter).
pinch_berry("Iapapa Berry", sour).

% a nature dislikes the flavour of the stat it lowers
nature_dislikes("Lonely", sour).      nature_dislikes("Brave", sweet).
nature_dislikes("Adamant", dry).      nature_dislikes("Naughty", bitter).
nature_dislikes("Bold", spicy).       nature_dislikes("Relaxed", sweet).
nature_dislikes("Impish", dry).       nature_dislikes("Lax", bitter).
nature_dislikes("Timid", spicy).      nature_dislikes("Hasty", sour).
nature_dislikes("Jolly", dry).        nature_dislikes("Naive", bitter).
nature_dislikes("Modest", spicy).     nature_dislikes("Mild", sour).
nature_dislikes("Quiet", sweet).      nature_dislikes("Rash", bitter).
nature_dislikes("Calm", spicy).       nature_dislikes("Gentle", sour).
nature_dislikes("Sassy", sweet).      nature_dislikes("Careful", dry).

% A Pokemon's stats depend on its species, level, IVs, nature, item, ability and
% boosts -- never on how much HP it has left. calculate/4 was being called all
% over the place just to read a speed or a max HP, and every one of those is a
% tabled lookup that hashes an entire Pokemon dict. 8.6 million of them in a
% three-opponent search.
stats_of(Pokemon, Stats) :-
    without_hp(Pokemon, P),
    calculate_http(P, P, "Tackle", false, Data),
    Stats = Data.attacker.stats.

% The fork's calc already applies paralysis to the speed it reports (quartered,
% Run & Bun's rule), because the status travels on the dict into the request.
% Quartering again here had a paralysed Hitmontop at speed 2 instead of 9.
speed_of(Pokemon, Speed) :-
    stats_of(Pokemon, Stats),
    get_dict(spe, Stats, Speed).

max_hp(Pokemon, MaxHP) :-
    stats_of(Pokemon, Stats),
    get_dict(hp, Stats, MaxHP).

% Straight off the dict; only ask the oracle when it has not been damaged yet.
% Note the Cur: testing get_dict against an already-bound HP would fail for a
% damaged Pokemon and fall through to the max, which quietly made sturdy/2 true
% forever.
current_hp(Pokemon, HP) :-
    (get_dict(curHP, Pokemon, Cur) -> HP = Cur ; max_hp(Pokemon, HP)).

confused_by(Pokemon, Berry) :-
    pinch_berry(Berry, Flavour),
    get_dict(nature, Pokemon, Nature),
    nature_dislikes(Nature, Flavour).

% Berries that fire on their own. Pinch berries: half of max HP at a quarter HP
% or less (Mechanic Changes). Oran: 10 HP, Sitrus: a quarter of max, both at half
% HP or less. Only consumed the once. Masquerain sitting on an Oran is 10 HP that
% the Post-KO switch-in AI sees and that decides who it sends out next.
berry_proc(Pokemon, MaxHP, Healed) :-
    (
        get_dict(item, Pokemon, Berry),
        get_dict(curHP, Pokemon, HP),
        HP > 0,
        berry_heals(Berry, HP, MaxHP, Amount)
    ->
        Restored is min(MaxHP, HP + Amount),
        Healed = Pokemon.put(_{curHP: Restored, item: ""})
    ;
        Healed = Pokemon
    ).

berry_heals(Berry, HP, MaxHP, Half) :-
    pinch_berry(Berry, _),
    HP * 4 =< MaxHP,
    Half is MaxHP // 2.
berry_heals("Oran Berry", HP, MaxHP, 10) :-
    HP * 2 =< MaxHP.
berry_heals("Sitrus Berry", HP, MaxHP, Quarter) :-
    HP * 2 =< MaxHP,
    Quarter is MaxHP // 4.

% any berry berry_proc/3 knows how to fire
heal_berry(Berry) :- pinch_berry(Berry, _).
heal_berry("Oran Berry").
heal_berry("Sitrus Berry").

% ---- item removal ---------------------------------------------------------
% Pluck and Bug Bite eat the target's berry. Covet and Thief do NOT: the move
% changes sheet lists both as "no longer steal items" in Run & Bun, which matches
% the run3 note confirming Covet left a Berry Juice alone.
% Pluck and Bug Bite eat the berry off the target: the target loses it and the
% attacker gets its effect. Plucking a pinch berry therefore both denies the heal
% and takes it, which is the whole of the run7 Kubfu answer -- and Prinplup is
% Bashful, a neutral nature, so nothing confuses it on the way.
eat_berry(OurMove, Played, Attacker, Opponent, NewAttacker, After) :-
    (
        Played \== flinched, Played \== none,
        berry_eater(OurMove),
        get_dict(item, Opponent, Item),
        sub_string(Item, _, _, 0, "Berry")
    ->
        After = Opponent.put(_{item: ""}),
        plucker_heals(Attacker, Item, NewAttacker)
    ;
        After = Opponent,
        NewAttacker = Attacker
    ).

% eating it off someone else triggers it regardless of your own HP
plucker_heals(Attacker, Berry, Healed) :-
    (pinch_berry(Berry, _), get_dict(curHP, Attacker, HP) ->
        max_hp(Attacker, MaxHP),
        Restored is min(MaxHP, HP + MaxHP // 2),
        Healed = Attacker.put(_{curHP: Restored})
    ;
        Healed = Attacker
    ).

% attack and switch out in one turn: no lost tempo, unlike a normal pivot
pivot_move("U-turn").
pivot_move("U-Turn").
pivot_move("Volt Switch").
pivot_move("Flip Turn").

% would this eat a berry whose flavour the eater's nature dislikes
unpriced_confusion(Move, Played, Eater, Target) :-
    Played \== flinched,
    Played \== none,
    berry_eater(Move),
    get_dict(item, Target, Item),
    confused_by(Eater, Item).

berry_eater("Pluck").
berry_eater("Bug Bite").

% somebody on the bench who survives coming in and is then able to act
worthwhile_pivot(Bench, Current, Opp, Assume, OppState, OppTurn) :-
    member(Candidate, Bench),
    entrant(Current, Opp, OppState, Candidate, _, Entered),
    \+ get_dict(curHP, Entered, 0),
    \+ unsafe_to_stay(Entered, Opp, Assume, OppTurn), !.

drop_from_party([], _, []).
drop_from_party([P|Rest], Dead, Out) :-
    (get_dict(name, P, Name), get_dict(name, Dead, Name) ->
        Out = Rest
    ;
        Out = [P|Tail],
        drop_from_party(Rest, Dead, Tail)
    ).

% CanPivot says whether there is anyone left to pivot into. The stopping rule is
% the one chip/5 already uses -- stop the moment acting is no longer safe -- which
% is what lets the narration show a chip and pivot instead of standing there until
% something dies. Without it the trace and beat_together/4 were describing two
% different fights.
trace_matchup(Pokemon, Opponent, Assume, N, CanPivot, Turns, Survivor, OppAfter, Outcome, NextTurn) :-
    % N < 25 bounds it: two mons that can never safely attack would otherwise
    % pivot into each other forever
    (CanPivot == true, N < 25, unsafe_to_stay(Pokemon, Opponent, Assume, N) ->
        get_dict(name, Pokemon, Name),
        Turns = [], Survivor = Pokemon, OppAfter = Opponent,
        Outcome = pivot(Name), NextTurn is N + 1
    ;
        play_turn(Pokemon, Opponent, Assume, N, CanPivot, Turns, Survivor, OppAfter, Outcome, NextTurn)
    ).

% cannot act safely any more: a crit would take us, or the next turn kills us
% outright without taking the opponent down with it
unsafe_to_stay(Pokemon, Opponent, Assume, N) :-
    (\+ crit_screen(Pokemon, Opponent, Assume) ->
        true
    ;
        next_turn_kills_us(Pokemon, Opponent, Assume, N)
    ).

next_turn_kills_us(Pokemon, Opponent, Assume, N) :-
    once(highest_damage_move(Pokemon, Opponent, Ours)),
    next_turn_kills_us(Pokemon, Opponent, Assume, N, Ours).

% with the move we would actually click: a priority finish changes the answer
next_turn_kills_us(Pokemon, Opponent, Assume, N, Ours) :-
    Assume = assume(Rolls, _),
    (N =:= 1 -> When = first ; When = later),
    ai_moves_cached(Opponent, Pokemon, When, Candidates),
    worst_opponent_move(Candidates, Opponent, Pokemon, Theirs),
    resolve_turn(Pokemon, Opponent, Ours, Theirs, Rolls, res(NewPokemon, _, NewOpponent, _)),
    get_dict(curHP, NewPokemon, 0),
    \+ get_dict(curHP, NewOpponent, 0).     % unless we take it with us

play_turn(Pokemon, Opponent, Assume, N, CanPivot, Turns, Survivor, OppAfter, Outcome, NextTurn) :-
    Assume = assume(Rolls, _),
    (
        N =< 25,
        % `first` is the OPPONENT's first turn out, not ours
        (N =:= 1 -> When = first ; When = later),
        once(highest_damage_move(Pokemon, Opponent, Ours)),
        ai_moves(Opponent, Pokemon, When, Candidates),
        worst_opponent_move(Candidates, Opponent, Pokemon, Theirs),
        resolve_turn(Pokemon, Opponent, Ours, Theirs, Rolls, res(NewPok0, Played, NewOpp0, _)),
        % stat changes from the moves that actually went off
        ((Played == flinched ; Played == none) ->
            NewPok1 = NewPok0, NewOpp1 = NewOpp0
        ;
            apply_move_boosts(NewPok0, NewOpp0, Ours, NewPok1, NewOpp1)
        ),
        apply_move_boosts(NewOpp1, NewPok1, Theirs, NewOpp, NewPok)
    ->
        % Pluck takes the berry off them and eats it; then any pinch berry that
        % survived fires if the damage took its holder to a quarter or less
        eat_berry(Ours, Played, NewPok, NewOpp, NewPokA, NewOppA),
        max_hp(Opponent, OppMax), berry_proc(NewOppA, OppMax, NewOpp2),
        max_hp(Pokemon, OurMax), berry_proc(NewPokA, OurMax, NewPok2),
        get_dict(curHP, NewPok2, OurHP),
        get_dict(curHP, NewOpp2, TheirHP),
        turn_order(Pokemon, Opponent, Theirs, Order),
        Turn = turn(N, Played, TheirHP, Theirs, Candidates, OurHP, Order),
        Next is N + 1,
        (TheirHP =:= 0 ->
            Turns = [Turn], Survivor = NewPok2, OppAfter = NewOpp2,
            Outcome = faints(Opponent.name), NextTurn = Next
        ; pivot_move(Ours), Played == Ours, CanPivot == true ->
            % U-Turn attacks AND leaves in the same turn, so unlike a normal
            % pivot it costs nothing. The mon coming in still eats whatever the
            % AI committed to, which is the bait the run7 notes exploit.
            Turns = [Turn], Survivor = NewPok2, OppAfter = NewOpp2,
            Outcome = pivot(Pokemon.name), NextTurn = Next
        ; ejected(Opponent, NewOpp2, TheirHP, Ejected) ->
            Turns = [Turn], Survivor = NewPok2, OppAfter = Ejected,
            Outcome = ejected(Opponent.name), NextTurn = Next
        ; OurHP =:= 0 ->
            Turns = [Turn], Survivor = NewPok2, OppAfter = NewOpp2,
            Outcome = we_faint(Pokemon.name), NextTurn = Next
        ; stalled(Pokemon, NewPok2, Opponent, NewOpp2) ->
            Turns = [Turn], Survivor = NewPok2, OppAfter = NewOpp2,
            Outcome = stalemate, NextTurn = Next
        ;
            trace_matchup(NewPok2, NewOpp2, Assume, Next, CanPivot, More, Survivor, OppAfter, Outcome, NextTurn),
            Turns = [Turn|More]
        )
    ;
        Turns = [], Survivor = Pokemon, OppAfter = Opponent, Outcome = no_line, NextTurn = N
    ).

% One turn of a fight. A guaranteed flinch from something that moves first means
% we simply do not act: Fake Out is 100% flinch at +3 priority, so the turn it
% lands we deal no damage at all. A 30% flinch is a risk rather than a certainty
% and belongs in the slip accounting, not here.
resolve_turn(Pokemon, Opponent, Ours, Theirs, Rolls, Resolution) :-
    (guaranteed_flinch(Opponent, Pokemon, Theirs) ->
        flinched_turn(Pokemon, Opponent, Theirs, Rolls, Resolution)
    ;
        move_1v1(Pokemon, Opponent, Ours, Theirs, Rolls, Resolution)
    ).

guaranteed_flinch(Opponent, Pokemon, Move) :-
    calculate(Opponent, Pokemon, Move, Data),
    move_data(Move, Data.defender, MoveData),
    flinch_chance(MoveData, Chance),
    Chance =:= 1,            % arithmetic: 100/100 is the integer 1, not 1.0
    moves_first(Opponent, Pokemon, Move).

moves_first(Attacker, Defender, Move) :-
    calculate(Attacker, Defender, Move, Data),
    (move_data(Move, Data.defender, MoveData), MoveData.priority > 0 ->
        true
    ;
        speed_of(Attacker, Fast), speed_of(Defender, Slow), Fast >= Slow
    ).

% only their move resolves; ours never happens
flinched_turn(Pokemon, Opponent, Theirs, rolls(_, TheirRoll),
              res(NewPokemon, flinched, NewOpponent, Theirs)) :-
    calculate(Opponent, Pokemon, Theirs, OppData),
    damage_roll(TheirRoll, Opponent, Pokemon, false, Theirs, Damage),
    resolve_dmg(Opponent, Pokemon, OppData, Damage, NewPokemon),
    explicit_hp(Opponent, NewOpponent).

% Among the moves the AI might click, the one that hurts us most -- except that a
% guaranteed flinch costs us the whole turn, which is worse than any amount of
% damage on it. Shared by worst_case/5 and the trace so the two cannot disagree.
worst_opponent_move(Candidates, Opponent, Pokemon, Move) :-
    (member(M, Candidates), guaranteed_flinch(Opponent, Pokemon, M) ->
        Move = M
    ;
        worst_candidate(Candidates, Opponent, Pokemon, Move)
    ).

% ties are broken at random, so plan against the one that hurts most
worst_candidate(Candidates, Attacker, Defender, Worst) :-
    findall(D-M, (member(M, Candidates), highRoll(Attacker, Defender, false, M, D)), Scored),
    keysort(Scored, Sorted),
    last(Sorted, _-Worst).

print_play_by_play(Box, OppTeam) :-
    select_party(Box, OppTeam, Party),
    best_line(Party, OppTeam, Level-Name, _),
    relaxation(Level, Name, Assume),
    once(fight_trace(Party, OppTeam, Assume, Events)),
    format("assumptions: level ~w (~w)~n", [Level, Name]),
    forall(member(E, Events), print_event(E)).

print_event(versus(Opp, sent, How)) :- !,
    format("~n  >> they send out ~w~n", [Opp]),
    print_entry(How).
print_event(versus(Opp, still_out, How)) :-
    format("~n  (~w still out)~n", [Opp]),
    print_entry(How).
% Who acts first, and why. A speed tie resolves their way -- in game it is a coin
% flip and the AI's own scoring assumes it is faster -- which is why a Pokemon can
% take a hit on the turn it lands the kill.
turn_order(Pokemon, Opponent, TheirMove, Order) :-
    once(highest_damage_move(Pokemon, Opponent, Ours)),
    turn_order(Pokemon, Opponent, Ours, TheirMove, Order).

% who went first, from the moves that were actually played. `none` on their side
% means they never got to move (we KO'd first); on ours it means we did not.
turn_order(_, _, _, none, us_first) :- !.
turn_order(_, _, Ours, _, them_first) :- (Ours == none ; Ours == flinched), !.
turn_order(Pokemon, Opponent, Ours, Theirs, Order) :-
    move_data_priority(Pokemon, Opponent, Ours, P1),
    move_data_priority(Opponent, Pokemon, Theirs, P2),
    speed_of(Pokemon, S1),
    speed_of(Opponent, S2),
    (   P1 > P2 ->  Order = our_priority
    ;   P2 > P1 ->  Order = their_priority
    ;   S1 > S2 ->  Order = us_first
    ;   S1 =:= S2 -> Order = speed_tie
    ;   Order = them_first
    ).

move_data_priority(Attacker, Defender, Move, Priority) :-
    calculate(Attacker, Defender, Move, Data),
    move_data(Move, Data.defender, MoveData),
    get_dict(priority, MoveData, Priority).

order_note(us_first, '  [we are faster]').
order_note(them_first, '  [they are faster]').
order_note(their_priority, '  [priority]').
order_note(our_priority, '  [priority]').
order_note(speed_tie, '  [speed tie -- resolved their way]').

print_event(turn(N, Ours, TheirHP, Theirs, Candidates, OurHP, Order)) :-
    (Candidates = [_,_|_] -> format(atom(Tie), "   (or ~w)", [Candidates]) ; Tie = ''),
    order_note(Order, Note),
    % same rule as the searched line: print them in the order they happened, or a
    % Pokemon that was outsped looks like it attacked after fainting
    our_turn_line(Ours, TheirHP, OursLine),
    their_turn_line(Theirs, OurHP, Tie, Note, TheirsLine),
    ((Order == us_first ; Order == our_priority) ->
        format("     ~w. ~w~n", [N, OursLine]),
        format("        ~w~n", [TheirsLine])
    ;
        format("     ~w. ~w~n", [N, TheirsLine]),
        format("        ~w~n", [OursLine])
    ).

our_turn_line(flinched, _, 'we flinch, no move').
our_turn_line(none, _, 'we go down before we move').
our_turn_line(Move, TheirHP, Line) :-
    Move \== flinched, Move \== none,
    format(atom(Line), "we use ~w~t~30|-> them ~w hp", [Move, TheirHP]).

their_turn_line(none, OurHP, _, _, Line) :-
    format(atom(Line), "they never move~t~30|-> us ~w hp", [OurHP]).
their_turn_line(Move, OurHP, Tie, Note, Line) :-
    Move \== none,
    format(atom(Line), "they use ~w~t~30|-> us ~w hp~w~w", [Move, OurHP, Tie, Note]).
print_event(pivot(Who))    :- format("        ~w pulls out before it dies~n", [Who]).
print_event(ejected(Who))  :- format("        ~w is dragged out by its Eject Button~n", [Who]).
print_event(out_of_pokemon) :- format("~n  ** party wiped -- run over **~n").
print_event(faints(Who))   :- format("        ~w faints~n", [Who]).
print_event(died_on_entry(Who)) :- format("        !! our ~w faints on the way in~n", [Who]).
print_event(we_faint(Who)) :- format("        !! our ~w faints~n", [Who]).
print_event(stalemate)     :- format("     stalemate, neither side progresses~n").
print_event(no_line)       :- format("     no line found from here~n").

print_entry(lead(Name))  :- format("  << we send out ~w~n", [Name]).
print_entry(stays)       :- format("  << we stay in~n").
print_entry(pursued(From, To, Taken, LeftOn)) :-
    format("  << we switch ~w -> ~w, but Pursuit catches ~w for ~w (~w hp left)~n",
           [From, To, From, Taken, LeftOn]).
print_entry(switch(From, To, Baited, Dmg)) :-
    format("  << we switch ~w -> ~w, baiting ~w, taking ~w on the way in~n",
           [From, To, Baited, Dmg]).

print_fight_report(Box, OppTeam) :-
    select_party(Box, OppTeam, Party),
    maplist([X,Y]>>get_dict(name,X,Y), Party, Bringing),
    fight_report(Box, OppTeam, report(Level-Name, Slots)),
    format("bringing: ~w~n", [Bringing]),
    format("assumptions: level ~w (~w)~n~n", [Level, Name]),
    forall(member(slot(Opp, Ours, Verdict, Risks), Slots),
        (
            format("~w~t~14|~w~t~26|~w~n", [Opp, Ours, Verdict]),
            forall(member(R, Risks), format("~t~26|! ~w~n", [R]))
        )),
    aggregate_all(count, member(slot(_,_,unwinnable,_), Slots), Bad),
    (Bad == 0 -> format("~nevery slot accounted for~n") ; format("~n~w slot(s) with no answer~n", [Bad])).

% every level, so you can see where a line first appears and what it costs.
% Crits are flagged rather than required from level 1 on: audit_line/3 lists the
% slots where giving them up is actually load bearing.
deepening_report(Party, OppTeam, Report) :-
    findall(Level-Name-Outcome,
        (
            relaxation(Level, Name, Assume),
            once(find_line_sticky(Party, OppTeam, Assume, Line)),
            line_losses(Line, OppTeam, Assume, Losses),
            include([_-_-lost]>>true, Losses, Lost),
            length(Lost, N),
            (N == 0 ->
                audit_line(Line, OppTeam, Crits),
                maplist([X,Y]>>get_dict(name,X,Y), Line, Names),
                Outcome = line(Names, crit_risk(Crits))
            ;
                Outcome = lost_slots(N)
            )
        ),
        Report).

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

% what we are left with on the same line. The crit setting is a safety question,
% not an HP one, so the HP walk always ignores it.
after_fight(Pokemon, Opponent, Survivor) :-
    after_fight(Pokemon, Opponent, assume(rolls(low, high), ignore), Survivor).

after_fight(Pokemon, Opponent, assume(Rolls, _), Survivor) :-
    worst_case(Pokemon, Opponent, assume(Rolls, ignore), Survivor).

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

% Run & Bun scores every move and uses the highest scorer, breaking ties at
% random. Per the community AI doc for 1.07, the relevant part of that scoring is
%   highest damaging move        +6, and +8 on a 20% roll
%   plus a kill bonus            +6 when faster or using priority, +3 when slower
%   non-attacking moves          +6 flat
% which lands on fast kill 12/14 > slow kill 9/11 > { highest damage 6/8, status 6 }.
% So a status move ties with the best attack whenever the AI sees no kill, and it
% is as likely to pick one -- Kecleon really does just Thunder Wave. Speed ties
% count as the AI being faster, which fast_kill_possible/3 already assumes.
ai_moves(Pokemon, Opponent, Moves) :-
    ai_moves(Pokemon, Opponent, later, Moves).

% Turn is `first` on the AI mon's first turn out, which Fake Out and Stealth Rock
% care about. Everything ties at the top score is returned, because the AI breaks
% ties at random.
% NOT tabled: the key includes current HP and boosts, so a search exploring
% 10^5 positions creates 10^5 table entries and the trie space -- which counts
% against the stack limit -- runs away. Six gigabytes was not enough.
% The AI scores Retaliate at its printed 70 power even on the turn it would hit
% for 140: the run7 video has a fresh Lopunny click Headbutt into a 17 HP
% Eelektrik where the doubled Retaliate was the strictly better kill, and the
% run7 notes had already guessed "AI calculates with Retaliate at power 70
% always". The doubled hit still lands (damage_rolls/5) and the switch-in AI
% still sees it (sends_out/5); only the move choice is blind to it.
ai_moves(Pokemon0, Opponent, Turn, Moves) :-
    spent_retaliate(Pokemon0, Pokemon),
    findall(Score-M,
        (
            member(M, Pokemon.moves),
            \+ unusable(M, Turn),
            ai_score(Pokemon, Opponent, Turn, M, Score)
        ),
        Scored),
    Scored = [_|_],
    keysort(Scored, Sorted),
    last(Sorted, Top-_),
    findall(M, member(Top-M, Sorted), Moves).

% The AI doc's scoring is additive: a move's special AI stacks on top of the
% standard damaging-move scoring. Relic Song is the doc's own example, +10
% normally and +13 when it sees a slow kill.
% Not modelled: the 80/20 variance (+2 on a fifth of rolls), Moxie-family +1,
% the high-crit-plus-super-effective +1, and most of the fifty-odd per-move
% cases in the doc. The tiers and the cases below are what change predictions.
ai_score(Pokemon, Opponent, Turn, Move, Score) :-
    (   setup_attack(Move)
    ->  setup_score(Pokemon, Opponent, Base)
    ;   highest_damage_score(Pokemon, Opponent, Move, Base)
    ),
    kill_score(Pokemon, Opponent, Move, Kill),
    special_score(Pokemon, Opponent, Turn, Move, Special),
    status_score(Pokemon, Opponent, Move, Status),
    Score is Base + Kill + Special + Status.

% Attacks that raise a stat with a 100% effect are scored as SETUP by the AI, not
% as damage: "Brawly's Scraggy won't Power-up Punch if you have any killing rolls
% on it, even if PuP is a kill" (AI doc). They are also left out of the highest-
% damage ranking, which is why Scraggy's Feint Attack is a live option against
% Cufant even though Power-Up Punch would do more -- the run7 video has it click
% Feint Attack on one such turn and Power-Up Punch on the next.
setup_attack("Power-Up Punch").
setup_attack("Charge Beam").

% General setup: +6, never (-20) when the player's Pokemon can KO the AI's, and
% -5 when the AI is slower and 2HKO'd
setup_score(Pokemon, Opponent, Score) :-
    (   get_dict(moves, Opponent, Theirs),
        member(M, Theirs),
        ko_possible(Opponent, Pokemon, M)
    ->  Score = -14
    ;   speed_of(Pokemon, Ours), speed_of(Opponent, TheirSpe),
        Ours < TheirSpe,
        current_hp(Pokemon, HP),
        get_dict(moves, Opponent, Theirs),
        member(M, Theirs),
        highRoll(Opponent, Pokemon, false, M, High),
        High * 2 >= HP
    ->  Score = 1
    ;   Score = 6
    ).

% +6 for being the highest damaging move, except for the moves the AI never
% rolls damage for and so never considers highest, and the setup attacks above
highest_damage_score(Pokemon, Opponent, Move, 6) :-
    \+ never_highest(Move),
    \+ setup_attack(Move),
    ai_highest_damage_move(Pokemon, Opponent, Move), !.
highest_damage_score(_, _, _, 0).

% the AI's own damage ranking: best_moves/3, whose ties are moves with
% OVERLAPPING roll ranges (the AI rolls its damage, so Carvanha's Bite at 16 and
% Poison Fang at 18 are both live -- the run10 notes have it click Bite), with
% the setup attacks taken out of the list first
ai_highest_damage_move(Pokemon, Opponent, Move) :-
    get_dict(moves, Pokemon, Moves),
    exclude(setup_attack, Moves, Ranked),
    Ranked = [_|_],
    best_moves(Pokemon.put(_{moves: Ranked}), Opponent, Best),
    member(Move, Best).

% these are rolled differently and are never the "highest damaging move"
never_highest("Explosion").
never_highest("Self-Destruct").
never_highest("Misty Explosion").
never_highest("Final Gambit").
never_highest("Relic Song").
never_highest("Rollout").
never_highest("Meteor Beam").
never_highest("Future Sight").
never_highest(Move) :- trapping_move(Move).

trapping_move("Whirlpool").
trapping_move("Fire Spin").
trapping_move("Sand Tomb").
trapping_move("Magma Storm").
trapping_move("Infestation").
trapping_move("Wrap").
trapping_move("Bind").
trapping_move("Clamp").

% faster, or priority while slower, is +6; slower is +3
kill_score(Pokemon, Opponent, Move, Score) :-
    (fast_kill_possible(Pokemon, Opponent, Move) ->
        Score = 6
    ; slow_kill_possible(Pokemon, Opponent, Move) ->
        (has_priority(Pokemon, Opponent, Move) -> Score = 6 ; Score = 3)
    ;
        Score = 0
    ).

% priority straight out of the move data rather than a hardcoded list of three
has_priority(Pokemon, Opponent, Move) :-
    calculate(Pokemon, Opponent, Move, Data),
    move_data(Move, Data.defender, MoveData),
    MoveData.priority > 0.

% kills from the defender's current HP, speed aside
ko_possible(Attacker, Defender, Move) :-
    highRoll(Attacker, Defender, false, Move, High),
    current_hp(Defender, HP),
    High >= HP,
    \+ sturdy(Defender, _).

hp_fraction(_, Defender, _, Fraction) :-
    current_hp(Defender, HP),
    max_hp(Defender, Max),
    Fraction is HP / Max.

% a non-attacking move that can still do something scores a flat +6
status_score(Pokemon, Opponent, Move, 6) :-
    status_moves(Pokemon, Opponent, Moves),
    memberchk(Move, Moves), !.
status_score(_, _, _, 0).

% Fake Out does not merely score badly after the first turn, it fails outright, so
% the AI's bad-move check keeps it off the table. Without this the model had
% Hitmontop flinch-locking the whole box, which is exactly the thing it cannot do.
unusable("Fake Out", later).

% Rollout is a flat +7, which is why Whirlipede locks itself into it over a move
% that would score +6 for being the highest damaging one.
special_score(_, _, _, "Rollout", 7) :- !.
% Fake Out on the first turn out, unless the target shrugs off the flinch
special_score(_, Opponent, first, "Fake Out", 9) :-
    \+ memberchk(Opponent.ability, ["Shield Dust", "Inner Focus"]), !.
% Pursuit has its own AI, stacking on top of the standard damaging-move scoring
% rather than replacing it:
%   can KO the player mon         +10
%   else player mon below 20% hp  +10
%   else player mon below 40% hp  +8 on half the rolls
%   and +3 more on any of those if the AI is faster
% The 40% case is a coin flip in game. It is taken as landing here, because the
% question this model answers is what you have to survive, not what you can hope
% for -- and Pursuit is the move that punishes a line built on pivoting.
special_score(Pokemon, Opponent, _, "Pursuit", Score) :- !,
    (ko_possible(Pokemon, Opponent, "Pursuit") ->
        Base = 10
    ;
        hp_fraction(Pokemon, Opponent, "Pursuit", Fraction),
        (Fraction < 0.2 -> Base = 10
        ; Fraction < 0.4 -> Base = 8
        ; Base = 0
        )
    ),
    (ai_is_faster(Pokemon, Opponent) -> Score is Base + 3 ; Score = Base).

% a doomed slower AI reaches for priority
special_score(Pokemon, Opponent, _, Move, 11) :-
    ai_is_slower(Pokemon, Opponent),
    fast_kill_possible(Opponent, Pokemon),
    has_priority(Pokemon, Opponent, Move), !.
special_score(_, _, _, _, 0).

% by category rather than by dealing no damage, so that an attack the defender
% happens to be immune to is not mistaken for one. The AI also checks whether a
% move would do anything at all before scoring it, so a status move that cannot
% land is not on the table.
status_moves(Pokemon, Opponent, Moves) :-
    findall(M,
        (
            member(M, Pokemon.moves),
            calculate(Pokemon, Opponent, M, Data),
            get_dict(category, Data.move, "Status"),
            \+ useless(M, Data.defender)
        ),
        Moves).

% the damage calc carries no accuracy, inflicted status or secondary chances,
% so those come from @pkmn/dex through the server's /move endpoint
% No Pokemon dict carries its types -- not the box, not the trainers -- and
% this used to require the key and fail quietly without it, which meant drain
% heals and status infliction never applied to anyone. Ask the calc instead.
move_data(Move, Defender, Data) :-
    types_of(Defender, Types),
    move_http(Move, Types, Data).

types_of(Pokemon, Types) :-
    (   get_dict(types, Pokemon, Types)
    ->  true
    ;   without_hp(Pokemon, P),
        calculate_http(P, P, "Tackle", false, Data),
        Types = Data.attacker.types
    ).

:- table move_http/3.
move_http(Move, Types, Data) :-
    Body = json([gen=8, moveName=Move, defenderTypes=Types]),
    http_get('http://localhost:3000/move', Data,
             [method(get), post(json(Body)), json_object(dict)]).

% a status move the target cannot be affected by. The AI will not pick one.
useless(Move, Defender) :-
    move_data(Move, Defender, Data),
    (Data.typeImmune == true
    ;
        Data.status \== null,
        status_blocked(Data.status, Defender)
    ), !.

% a non-volatile status cannot be applied over another one, and types and
% abilities give outright immunity to particular statuses
status_blocked(_, Defender) :-
    get_dict(status, Defender, Current),
    Current \== "",
    Current \== null.
status_blocked(Status, Defender) :-
    status_immune_type(Status, Type),
    types_of(Defender, Types),
    memberchk(Type, Types).
status_blocked(Status, Defender) :-
    status_immune_ability(Status, Ability),
    Defender.ability == Ability.

status_immune_type("par", "Electric").
status_immune_type("brn", "Fire").
status_immune_type("psn", "Poison").
status_immune_type("psn", "Steel").
status_immune_type("tox", "Poison").
status_immune_type("tox", "Steel").
status_immune_type("frz", "Ice").

status_immune_ability("par", "Limber").
status_immune_ability("brn", "Water Veil").
status_immune_ability("psn", "Immunity").
status_immune_ability("tox", "Immunity").
status_immune_ability("frz", "Magma Armor").
status_immune_ability("slp", "Insomnia").
status_immune_ability("slp", "Vital Spirit").

% Backtracks over all highest damage moves on a tie.
%
% This used to rank on percentage of the defender's current HP, which meant
% recomputing float arithmetic for every move on every node of the search -- 55%
% of its runtime. Every move shares the same denominator, so the ranking is
% identical on raw damage, and raw damage does not depend on HP at all. That makes
% the whole ranking cacheable across the hundreds of HP values a search walks.
highest_damage_move(Attacker, Defender, Move) :-
    best_moves(Attacker, Defender, Moves),
    member(Move, Moves).

% hot in the search and four oracle calls deep, so it keeps a cache of its own
best_moves(Attacker, Defender, Moves) :-
    calc_key(Attacker, KA),
    calc_key(Defender, KD),
    cached(best(KA, KD), Moves, best_moves_raw(Attacker, Defender, Moves)).

best_moves_raw(Attacker, Defender, Moves) :-
    get_dict(moves, Attacker, MoveList),
    % Brine and friends read HP, so a set containing one cannot be HP-stripped
    (   \+ (member(M0, MoveList), hp_sensitive(M0))
    ->  without_hp(Attacker, A), without_hp(Defender, D)
    ;   A = Attacker, D = Defender
    ),
    findall(Move-Lo-Hi,
        (
            member(Move, MoveList),
            damage_rolls(A, D, Move, false, Rolls),
            roll_bounds(Rolls, Lo, Hi)
        ),
        Data),
    findall(Move,
        (
            member(Move-_-Hi, Data),
            \+ (member(_-Lo2-_, Data), Lo2 > Hi)
        ),
        Moves).

roll_bounds(Rolls, Lo, Hi) :-
    (Rolls = [Lo|_] -> last(Rolls, Hi) ; Lo = Rolls, Hi = Rolls).

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

% "calculating safe" is one choice of rolls among several, so name it rather than
% hardcode it. rolls(Ours, Theirs), each of low / avg / high.
move_1v1(Pokemon, Opponent, Move, OppMove, Resolution) :-
    move_1v1(Pokemon, Opponent, Move, OppMove, rolls(low, high), Resolution).

% Turn order is priority bracket first, speed second. Resolving on speed alone
% had Aqua Jet and Quick Attack landing after the hit they exist to pre-empt, and
% Sucker Punch landing after the Gust that was supposed to KO through it -- which
% is most of what "finish with a priority move" means in a nuzlocke line.
move_1v1(Pokemon, Opponent, Move, OppMove, rolls(Ours, Theirs), Resolution) :-
    acts_first(Pokemon, Opponent, Move, OppMove),
    explicit_hp(Pokemon, Pokemon0), explicit_hp(Opponent, Opponent0),
    calculate(Pokemon, Opponent, Move, Data),
    hit(Ours, Pokemon0, Opponent0, Move, Data, Pokemon1, Opponent1, Dealt),
    (   Opponent1.curHP == 0
    ->  Resolution = res(Pokemon1, Move, Opponent1, none)
    ;   ejects(Opponent1, Dealt)
    ->  % struck first while holding an Eject Button: it is pulled out before it
        % gets to move, so the switch-in eats nothing this turn
        Resolution = res(Pokemon1, Move, Opponent1, none)
    ;   calculate(Opponent, Pokemon, OppMove, OppData),
        hit(Theirs, Opponent1, Pokemon1, OppMove, OppData, Opponent2, Pokemon2, _),
        Resolution = res(Pokemon2, Move, Opponent2, OppMove)
    ).

% Ties in both bracket and speed resolve their way. In game the order is a coin
% flip and the AI's own scoring already assumes it is the faster one, so taking
% the hit first is both the conservative reading and the AI's.
move_1v1(Pokemon, Opponent, Move, OppMove, rolls(Ours, Theirs), Resolution) :-
    \+ acts_first(Pokemon, Opponent, Move, OppMove),
    explicit_hp(Pokemon, Pokemon0), explicit_hp(Opponent, Opponent0),
    calculate(Opponent, Pokemon, OppMove, OppData),
    hit(Theirs, Opponent0, Pokemon0, OppMove, OppData, Opponent1, Pokemon1, _),
    (   Pokemon1.curHP == 0
    ->  Resolution = res(Pokemon1, none, Opponent1, OppMove)
    ;   calculate(Pokemon, Opponent, Move, Data),
        hit(Ours, Pokemon1, Opponent1, Move, Data, Pokemon2, Opponent2, _),
        Resolution = res(Pokemon2, Move, Opponent2, OppMove)
    ).

acts_first(Pokemon, Opponent, Move, OppMove) :-
    move_data_priority(Pokemon, Opponent, Move, Ours),
    move_data_priority(Opponent, Pokemon, OppMove, Theirs),
    (   Ours > Theirs
    ->  true
    ;   Ours =:= Theirs,
        speed_of(Pokemon, OurSpe), speed_of(Opponent, TheirSpe),
        OurSpe > TheirSpe
    ).

explicit_hp(Pokemon, WithHP) :-
    current_hp(Pokemon, HP),
    WithHP = Pokemon.put(_{curHP: HP}).

% Focus Energy raises the user's crit stage by two; a high-crit move adds one
% more; three or more is a guaranteed crit (gen 6+ table). Anything short of that
% is a coin flip a safe line does not get to lean on, so it counts as no crit.
% Whether a second Focus Energy stacks in Run & Bun is unverified -- vanilla
% refuses it -- so a plan that uses two is flagged when printed.
crit_stage(Pokemon, Stage) :-
    (get_dict(critStage, Pokemon, Stage) -> true ; Stage = 0).

sure_crit(Attacker, Defender, Move) :-
    crit_stage(Attacker, Stage),
    (   move_data(Move, Defender, MD), get_dict(critRatio, MD, R), R >= 2
    ->  Total is Stage + 1
    ;   Total = Stage
    ),
    Total >= 3.

crit_flag(Attacker, Defender, Move, Crit) :-
    (sure_crit(Attacker, Defender, Move) -> Crit = true ; Crit = false).

pumped(Pokemon, Pumped) :-
    crit_stage(Pokemon, S),
    S2 is S + 2,
    Pumped = Pokemon.put(_{critStage: S2}).

% one side's move landing: damage, then the move's own status, then any drain.
% Dealt is the HP actually removed, which is what drain heals are based on.
hit(Roll, Attacker, Defender, Move, Data, NewAttacker, NewDefender, Dealt) :-
    calc_defender(Defender, Data, WithTypes),
    crit_flag(Attacker, WithTypes, Move, Crit),
    damage_roll(Roll, Attacker, Defender, Crit, Move, Dmg),
    resolve_dmg(Attacker, Defender, Data, Dmg, Defender1),
    current_hp(Defender, Before), current_hp(Defender1, After),
    Dealt is Before - After,
    inflict_status(Attacker, Move, Defender1, NewDefender),
    drain_heal(Move, Dealt, Attacker, NewAttacker).

% move_data/3 reads types off its second argument, which the calc result has
calc_defender(Defender, Data, WithTypes) :-
    (   get_dict(types, Defender, _)
    ->  WithTypes = Defender
    ;   get_dict(defender, Data, D), get_dict(types, D, Types),
        WithTypes = Defender.put(_{types: Types})
    ).

ejects(Pokemon, Dealt) :-
    Dealt > 0,
    get_dict(item, Pokemon, "Eject Button").

% A move's primary status (Thunder Wave, Will-O-Wisp, Toxic), applied only when it
% cannot miss for this user: a safe line does not get to lean on a 90% roll. Run &
% Bun makes Thunder Wave unmissable for Electric types, which is the case that
% matters -- Eelektrik paralysing Hitmontop is what makes it outspeed it.
% Secondary chances (Spark's 30%) are not applied; that is the conservative side.
inflict_status(Attacker, Move, Defender, Out) :-
    (   move_data(Move, Defender, MD),
        get_dict(status, MD, S), S \== null,
        \+ status_blocked(S, Defender),
        cannot_miss(Attacker, Move, MD)
    ->  Out = Defender.put(_{status: S})
    ;   Out = Defender
    ).

cannot_miss(_, _, MD) :- get_dict(alwaysHits, MD, true), !.
cannot_miss(_, _, MD) :- get_dict(accuracy, MD, A), A >= 100, !.
cannot_miss(Attacker, "Thunder Wave", _) :-
    types_of(Attacker, Types), memberchk("Electric", Types).

% Giga Drain and friends restore a fraction of the damage dealt, read off the move
% data as [Numerator, Denominator]. Opponents heal the same way (Drain Punch).
drain_heal(Move, Dealt, Attacker, Out) :-
    (   Dealt > 0,
        move_data(Move, Attacker, MD),
        get_dict(drain, MD, [Num, Den]),
        Den > 0
    ->  Heal is max(1, (Dealt * Num) // Den),
        current_hp(Attacker, HP), max_hp(Attacker, Max),
        New is min(Max, HP + Heal),
        Out = Attacker.put(_{curHP: New})
    ;   Out = Attacker
    ).

% the calc hands back all 16 rolls and the model has been keeping 2 of them
damage_roll(low, A, D, Crit, Move, Dmg) :- lowRoll(A, D, Crit, Move, Dmg).
damage_roll(high, A, D, Crit, Move, Dmg) :- highRoll(A, D, Crit, Move, Dmg).
damage_roll(avg, A, D, Crit, Move, Dmg) :-
    damage_rolls(A, D, Move, Crit, Rolls),
    (is_list(Rolls) ->
        sum_list(Rolls, Sum),
        length(Rolls, N),
        Dmg is Sum // N
    ;
        Dmg = Rolls
    ).

% TODO: resolve_1v1 on a speed tie

resolve_dmg(_Pokemon, Opponent, Data, Damage, NewOpponent) :-
    current_hp(Opponent, HP),
    Min is HP - Damage,
    (sturdy(Opponent, Data) ->
        NewHP is max(1, Min)
    ;
        NewHP is max(0, Min)
    ),
    NewOpponent = Opponent.put(_{curHP:NewHP}).

% Does Sturdy or a Focus Sash prevent the OHKO? Identical semantics -- survive on
% 1hp, but only from full HP. 141 trainer Pokemon hold a Focus Sash against 16
% with Sturdy, so checking only the ability was claiming kills that do not happen.
sturdy(Defender, _) :-
    (get_dict(ability, Defender, "Sturdy") ; get_dict(item, Defender, "Focus Sash")),
    max_hp(Defender, Max),
    current_hp(Defender, Max).

% Run & Bun sets the critical hit rate to 1/16 and the multiplier to 1.5
% (Mechanic Changes). Gen 8 vanilla would be 1/24, so anything priced off the
% vanilla rate understates the risk by half.
crit_chance(0.0625).

% what relaxation level 1 actually costs: the chance a crit takes this slot off us
crit_risk(Pokemon, Opponent, Chance) :-
    lethal_crits(Opponent, Pokemon, Moves),
    (Moves == [] -> Chance = 0 ; crit_chance(Chance)).

% Pinch berries restore half HP at 1/4 HP in Run & Bun, not the vanilla third,
% and Sitrus and Leftovers heal on their own schedule. worst_case/5 models none
% of it, so a matchup against a holder is optimistic about the kill -- Brawly's
% Kubfu carries an Iapapa, which is the fight where that hurts most.
% the pinch berries are modelled now, by berry_proc/3, so they are not listed here
% Oran and Sitrus are modelled by berry_proc/3 now; these still are not
healing_item("Leftovers").
healing_item("Berry Juice").

unmodelled_healing(Pokemon, Item) :-
    get_dict(item, Pokemon, Item),
    healing_item(Item).

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
% ===========================================================================
% Search
% ===========================================================================
% Everything above plans greedily: one mon per opposing mon, chosen by a
% heuristic, then narrated. That can never find a line whose individual steps all
% look like losses, which is most of what the run7 Brawly notes are made of --
% switch in, eat a hit, take a stage off it with Intimidate, leave.
%
% This searches instead. The opponent is a known policy rather than an adversary,
% so it is an AND-OR search, not a minimax: OR over our actions, AND over
% everything we do not control. A state is winnable when SOME action of ours has
% EVERY outcome leading to a state that is still winnable with nobody lost.
%
% Nuzlocke rules are the objective, not a filter: losing a Pokemon is failure, not
% a cost. And the party cap is a search constraint rather than a pre-filter --
% which six you bring depends on the line you intend to play, so the line picks
% them. A branch that would send out a seventh is simply not a branch.

% battle(OurActive, Bench, TheirActive, TheirRest, OppTurn, Brought)
%   Bench      our unfainted reserves, still carrying their damage
%   TheirRest  their remaining team in party order
%   OppTurn    how long TheirActive has been out (Fake Out, Pursuit thresholds)
%   Brought    names we have already committed a party slot to
% A six-Pokemon fight cannot be won in fourteen turns, so a fixed bound was not
% pruning the search, it was making the answer impossible. Scale it with the size
% of the fight instead: roughly five turns per opposing Pokemon.
search_depth(OppCount, Depth) :- Depth is 5 * OppCount + 4.
:- dynamic node_budget/1.
node_budget(6000).

% A ground fingerprint of the position, so the search can recognise that it has
% been here before. Raw dicts will not do: they carry variable tags and unordered
% keys. Bench order does not matter, so it is sorted out of the key.
:- dynamic seen_loss/3.
:- dynamic memo_probes/1.
:- dynamic memo_hits/1.
memo_probe :- (retract(memo_probes(N)) -> N1 is N+1 ; N1 = 1), assertz(memo_probes(N1)).
memo_hit :- (retract(memo_hits(N)) -> N1 is N+1 ; N1 = 1), assertz(memo_hits(N1)).
memo_stats(P, H, Keys) :-
    (memo_probes(P) -> true ; P = 0),
    (memo_hits(H) -> true ; H = 0),
    aggregate_all(count, seen_loss(_,_,_), Keys).
:- dynamic nodes_used/1.

battle_key(battle(Active, Bench, Opponent, Rest, OppTurn, _Brought), Key) :-
    mon_key(Active, KA),
    maplist(mon_key, Bench, KB0), msort(KB0, KB),
    mon_key(Opponent, KO),
    maplist(mon_key, Rest, KR),
    % Brought is derivable from who has been out, and including it split
    % positions that play identically. Out of the key.
    (OppTurn =:= 1 -> Fresh = first ; Fresh = later),
    Key = k(KA, KB, KO, KR, Fresh).

mon_key(Pokemon, m(Name, Bucket, Boosts, Item, Status, Crit)) :-
    get_dict(name, Pokemon, Name),
    hp_bucket(Pokemon, Bucket),
    boosts_of(Pokemon, B),
    dict_pairs(B, _, Boosts),
    (get_dict(item, Pokemon, Item) -> true ; Item = none),
    (get_dict(status, Pokemon, Status) -> true ; Status = none),
    crit_stage(Pokemon, Crit).

% Keying the memo on exact HP means two positions that differ by a single point
% never merge, even though no decision turns on that point. Bucketing collapses
% them. Fainted stays its own bucket, because that is a different kind of state.
%
% This is an approximation and it cuts one way: two positions in a bucket can
% differ on whether something is a KO, so a branch that would have won may get
% skipped as already-failed. That loses lines, it does not invent them -- the
% search can come back "no line" when one exists, never the reverse.
hp_buckets(4).

hp_bucket(Pokemon, Bucket) :-
    current_hp(Pokemon, HP),
    (HP =:= 0 ->
        Bucket = fainted
    ;
        max_hp(Pokemon, Max),
        hp_buckets(N),
        Bucket is 1 + (HP * N) // Max
    ).

reset_search :-
    retractall(seen_loss(_, _, _)),
    retractall(seen_macro_loss(_, _, _)),
    retractall(expand_memo(_, _, _)),
    retractall(ai_cache(_, _, _)),
    retractall(nodes_used(_)),
    retractall(exhausted),
    retractall(memo_probes(_)), retractall(memo_hits(_)),
    assertz(nodes_used(0)).

% did the last search prove there is no line, or just run out of room
search_outcome(exhausted) :- exhausted, !.
search_outcome(proven_none) :- verify_proof.

spend_node :-
    retract(nodes_used(N)),
    N1 is N + 1,
    assertz(nodes_used(N1)),
    node_budget(Max),
    (N1 =< Max ->
        true
    ;
        % out of budget is not the same answer as proven impossible, and reporting
        % one as the other is how a search starts lying to you
        (exhausted -> true ; assertz(exhausted)),
        fail
    ).

:- dynamic exhausted/0.

% Every Pokemon in the box is a candidate lead, best-scoring first. Restricting
% this to nuzlocke_switchin's top tier meant the search never looked past a
% handful of leads and then reported "proven none" having barely searched.
initial_battle(Box, [Opp|Rest], Assume, battle(Lead, Bench, Faced, Rest, 1, [Name])) :-
    ranked_leads(Box, Opp, Ranked),
    member(Lead, Ranked),
    crit_screen(Lead, Opp, Assume),
    get_dict(name, Lead, Name),
    drop_from_party(Box, Lead, Bench),
    intimidate(Lead, Opp, Faced).

ranked_leads(Box, Opponent, Ranked) :-
    findall(Score-P,
        (
            member(P, Box),
            (nuzlocke_switchin_score(P, Opponent, S) -> Score is -S ; Score = 0)
        ),
        Scored),
    keysort(Scored, Sorted),
    pairs_values(Sorted, Ranked).

% the whole fight, as a plan tree
search_line(Box, OppTeam, Assume, plan(Battle, Tree)) :-
    reset_search,
    length(OppTeam, OppCount),
    search_depth(OppCount, Max),
    retractall(depth_limit(_)),
    assertz(depth_limit(Max)),
    once((
        initial_battle(Box, OppTeam, Assume, Battle),
        winnable(Battle, Assume, 0, Tree)
    )).

:- dynamic depth_limit/1.

winnable(Battle, Assume, Depth, Tree) :-
    Battle = battle(Active, Bench, Opponent, Rest, OppTurn, Brought),
    (   opponents_down(Opponent, Rest)
    ->  Tree = won
    ;   depth_limit(Max), Depth < Max,
        Remaining is Max - Depth,
        battle_key(Battle, Key),
        memo_probe,
        % been here before with at least this much room and could not win
        (  (seen_loss(Key, Had), Had >= Remaining)
        -> memo_hit, fail
        ;  true
        ),
        spend_node,
        (   winnable_here(Battle, Assume, Depth, Active, Bench, Opponent, Rest, OppTurn, Brought, T)
        ->  Tree = T
        ;   assertz(seen_loss(Key, Remaining)), fail
        )
    ).

winnable_here(Battle, Assume, Depth, Active, Bench, Opponent, Rest, OppTurn, Brought, Tree) :-
    (   true,
        Next is Depth + 1,
        action(Active, Opponent, Bench, Brought, Assume, OppTurn, Action),
        % AND over everything we do not control: every move the AI might click
        ai_options(Active, Opponent, OppTurn, Assume, Options),
        Options = [_|_],
        findall(Option-Sub,
            (
                member(Option, Options),
                step(Battle, Assume, Action, Option, After),
                winnable(After, Assume, Next, Sub)
            ),
            Branches),
        length(Options, N), length(Branches, N),      % every outcome survived
        Tree = node(Action, Branches)
    ).

opponents_down(Opponent, Rest) :-
    get_dict(curHP, Opponent, 0),
    Rest == [].

% Our actions, attacks first so the simple line is found before the clever one.
% Only moves worth clicking: the full four would quadruple the branching with
% moves no one would pick. That does rule out setup and status for now.
action(Active, Opponent, Bench, Brought, Assume, OppTurn, Action) :-
    (   get_dict(curHP, Active, 0)
    ->  % it fainted: the only thing to do is send the next one, and that is free
        action_replace(Bench, Opponent, Brought, Action)
    ;   (   action_attack(Active, Opponent, Action)
        ;   action_switch(Bench, Active, Opponent, Brought, Assume, OppTurn, Action)
        )
    ).

action_replace(Bench, Opponent, Brought, replace(P)) :-
    (nuzlocke_switchin(Opponent, Bench, Ranked) -> true ; Ranked = Bench),
    slot_order(Ranked, Brought, Committed),
    by_strength(Committed, Ordered),
    member(P, Ordered),
    get_dict(name, P, Name),
    (memberchk(Name, Brought) ->
        true
    ;
        max_party(Max),
        exclude([X]>>(X = run(_)), Brought, Names),
        length(Names, Used),
        Used < Max
    ).

action_attack(Active, Opponent, attack(Move)) :-
    highest_damage_move(Active, Opponent, Move).

% Ordered by the switchin scorer rather than by bench order. The search
% satisfices -- it takes the first safe line it finds -- so action order IS the
% quality of the answer. Unordered, it happily burns four turns switching through
% the whole box before the mon that could have come in first.
action_switch(Bench, Current, Opponent, Brought, Assume, OppTurn, switch(P)) :-
    % A party slot is a resource, so spend the ones already committed before
    % opening a new one, and among those prefer the stronger Pokemon: base stat
    % total is a blunt instrument but a general one. switch_candidates/4 keeps
    % the two orderings nested; sorting the whole list by strength afterwards had
    % been throwing the slot order away.
    switch_candidates(Bench, Opponent, Brought, Ordered),
    member(P, Ordered),
    (OppTurn =:= 1 -> OppState = arrived ; OppState = settled),
    entrant(Current, Opponent, OppState, P, _, Entered),
    \+ get_dict(curHP, Entered, 0),
    worth_switching(Entered, Opponent, Assume, OppTurn).

% A switch earns its turn if the newcomer can do something once it is in -- OR if
% the act of bringing it in is itself the point.
%
% Intimidate is that case, and requiring the newcomer to be able to STAY was
% quietly forbidding the strongest play in the game. Cufant is Steel, so it baits
% a Fighting move; Staravia comes in on that (neutral on Normal/Flying rather than
% the Rock Slide it would otherwise eat) and drops the attacker a stage. Staravia
% then baits a Rock move, and Cufant comes back in on that at half damage. Neither
% of them can stand there for a turn. The switching back and forth IS the play,
% and each repetition takes another stage off the thing that is beating you.
worth_switching(Entered, Opponent, Assume, OppTurn) :-
    (   stacks_a_drop(Entered, Opponent)
    ->  true
    ;   \+ unsafe_to_stay(Entered, Opponent, Assume, OppTurn)
    ).

% would coming in actually lower something, ie is there a stage left to take
stacks_a_drop(Entered, Opponent) :-
    get_dict(ability, Entered, "Intimidate"),
    get_dict(ability, Opponent, Ability),
    \+ intimidate_immune(Ability),
    boosts_of(Opponent, Boosts),
    get_dict(atk, Boosts, Atk),
    Atk > -6.

% strongest first, on base stat total
by_strength(Candidates, Ordered) :-
    map_list_to_pairs(negated_bst, Candidates, Scored),
    keysort(Scored, Sorted),
    pairs_values(Sorted, Ordered).

negated_bst(Pokemon, Negated) :-
    bst(Pokemon, BST),
    Negated is -BST.

bst(Pokemon, Total) :-
    get_dict(name, Pokemon, Name),
    cached(bst(Name), Total, bst_raw(Pokemon, Total)).

bst_raw(Pokemon, Total) :-
    without_hp(Pokemon, P),
    calculate_http(P, P, "Tackle", false, Data),
    get_dict(species, Data.attacker, Species),
    get_dict(baseStats, Species, Base),
    dict_pairs(Base, _, Pairs),
    findall(V, member(_-V, Pairs), Values),
    sum_list(Values, Total).

slot_order(Ranked, Brought, Ordered) :-
    include(already_brought(Brought), Ranked, Committed),
    exclude(already_brought(Brought), Ranked, Fresh),
    append(Committed, Fresh, Ordered).

already_brought(Brought, Pokemon) :-
    get_dict(name, Pokemon, Name),
    memberchk(Name, Brought).

% every move the AI might pick, since it breaks ties at random and we have to
% survive all of them
ai_options(Active, Opponent, OppTurn, _, Options) :-
    (OppTurn =:= 1 -> When = first ; When = later),
    (ai_moves_cached(Opponent, Active, When, Options) -> true ; Options = []).

% What the AI clicks depends on both Pokemon's species, HP, boosts, items and
% status, and on whether this is its first turn out. Key on exactly that. Cleared
% with the rest of the search state.
:- dynamic ai_cache/3.
ai_moves_cached(Opponent, Active, When, Moves) :-
    exact_mon_key(Opponent, KO),
    exact_mon_key(Active, KA),
    Key = KO-KA-When,
    term_hash(Key, Hash),
    (   ai_cache(Hash, Key, Cached)
    ->  Moves = Cached
    ;   ai_moves(Opponent, Active, When, Moves),
        assertz(ai_cache(Hash, Key, Moves))
    ).

% One turn, from a battle state and a committed pair of actions, to the next
% battle state. Fails if the turn loses us a Pokemon -- in a nuzlocke that is not
% a cost to weigh, it is the branch being invalid.
step(battle(Active, Bench, Opponent, Rest, OppTurn, Brought), _Assume,
     replace(Incoming), _OppMove, After, replaced(Name)) :-
    get_dict(curHP, Active, 0),
    get_dict(name, Incoming, Name),
    full_hp(Incoming, Opponent, HP),
    Entered = Incoming.put(_{curHP: HP}),
    intimidate(Entered, Opponent, Faced),
    drop_from_party(Bench, Incoming, NewBench),
    (memberchk(Name, Brought) -> Brought2 = Brought ; Brought2 = [Name|Brought]),
    After = battle(Entered, NewBench, Faced, Rest, OppTurn, Brought2).

step(battle(Active, Bench, Opponent, Rest, OppTurn, Brought), Assume,
     switch(Incoming), OppMove, After, switched(OppMove)) :-
    % A switch costs the turn: they attack whoever comes in, except Pursuit which
    % catches the one leaving instead.
    % The AI picked OppMove against the Pokemon that was out; that is the move
    % that lands on whoever comes in. Hitting the newcomer with the worst of the
    % AI's options on every branch, as this used to, had Eelektrik taking 25 from
    % a Thunder Punch the video shows doing 10, and made the AND over branches
    % meaningless for switches.
    (OppMove == "Pursuit" ->
        pursuit_on_switch(Opponent, Active, Left),
        get_dict(curHP, Left, LeftHP), LeftHP > 0,
        full_hp(Incoming, Opponent, HP),
        Entered = Incoming.put(_{curHP: HP}),
        Opp2 = Opponent
    ;
        Left = Active,
        Assume = assume(rolls(_, Theirs), _),
        full_hp(Incoming, Opponent, HP0),
        Entered0 = Incoming.put(_{curHP: HP0}),
        % Intimidate fires the moment the newcomer lands, before the AI's move
        % is executed, so the move is already at the lower stage (video: Staravia
        % takes 19 from Mach Punch on entry, the -1 number, not the -0 one)
        intimidate(Entered0, Opponent, Opp0),
        calculate(Opp0, Incoming, OppMove, OppData),
        hit(Theirs, Opp0, Entered0, OppMove, OppData, Opp1, Entered1, _),
        apply_move_boosts(Opp1, Entered1, OppMove, Opp2, Entered2),
        max_hp(Incoming, InMax), berry_proc(Entered2, InMax, Entered)
    ),
    get_dict(curHP, Entered, EnteredHP), EnteredHP > 0,
    (OppMove == "Pursuit" -> intimidate(Entered, Opp2, Faced0) ; Faced0 = Opp2),
    spent_retaliate(Faced0, Faced),
    swap_bench(Bench, Incoming, Left, NewBench),
    get_dict(name, Incoming, Name),
    (memberchk(Name, Brought) -> Brought2 = Brought ; Brought2 = [Name|Brought]),
    NextTurn is OppTurn + 1,
    After = battle(Entered, NewBench, Faced, Rest, NextTurn, Brought2).

step(Battle, Assume, Action, Option, After) :-
    step(Battle, Assume, Action, Option, After, _).

% U-turn and friends: hit, then leave, with the replacement taking whatever the
% opponent does afterwards. If they are faster their move lands on the user
% before it goes; if it KOs the user nothing switches. Pursuit on a U-turner
% that moved first catches it at double power on the way out. If the hit KOs,
% the AI picks its replacement against the Pokemon we brought in -- "U-Turn
% makes me choose first, and the AI just chooses afterwards based on that".
step(battle(Active, Bench, Opponent, Rest, OppTurn, Brought), Assume,
     pivot_attack(Move, Incoming), OppMove, After, uturned(Played, TheirPlayed, Name)) :-
    Assume = assume(rolls(Ours, Theirs), _),
    get_dict(name, Incoming, Name),
    \+ guaranteed_flinch(Opponent, Active, OppMove),
    explicit_hp(Active, Active0), explicit_hp(Opponent, Opp0),
    full_hp(Incoming, Opponent, InHP),
    Entered0 = Incoming.put(_{curHP: InHP}),
    calculate(Active, Opponent, Move, Data),
    (   acts_first(Active, Opponent, Move, OppMove)
    ->  hit(Ours, Active0, Opp0, Move, Data, Active1, Opp1, Dealt),
        Played = Move,
        (   Opp1.curHP == 0
        ->  Opp2 = Opp1, Entered = Entered0, TheirPlayed = none, Left = Active1
        ;   ejects(Opp1, Dealt)
        ->  Opp2 = Opp1, Entered = Entered0, TheirPlayed = none, Left = Active1
        ;   OppMove == "Pursuit"
        ->  pursuit_on_switch(Opp1, Active1, Left),
            current_hp(Left, LeftHP), LeftHP > 0,
            Opp2 = Opp1, Entered = Entered0, TheirPlayed = "Pursuit"
        ;   calculate(Opponent, Incoming, OppMove, OppData),
            hit(Theirs, Opp1, Entered0, OppMove, OppData, Opp2, Entered, _),
            TheirPlayed = OppMove, Left = Active1
        )
    ;   calculate(Opponent, Active, OppMove, OppData),
        hit(Theirs, Opp0, Active0, OppMove, OppData, Opp1, ActiveHit, _),
        TheirPlayed = OppMove,
        (   ActiveHit.curHP == 0
        ->  Played = none, Left = ActiveHit, Opp2 = Opp1, Entered = none
        ;   hit(Ours, ActiveHit, Opp1, Move, Data, Left, Opp2, _),
            Played = Move, Entered = Entered0
        )
    ),
    (   Entered == none
    ->  % died before moving: an ordinary lost turn, nobody switched
        NextTurn0 is OppTurn + 1,
        After = battle(Left, Bench, Opp2, Rest, NextTurn0, Brought)
    ;   get_dict(curHP, Entered, EnteredHP), EnteredHP > 0,
        max_hp(Opponent, OppMax), berry_proc(Opp2, OppMax, Opp3),
        max_hp(Incoming, InMax), berry_proc(Entered, InMax, Entered1),
        max_hp(Active, OutMax), berry_proc(Left, OutMax, Left1),
        intimidate(Entered1, Opp3, Faced),
        swap_bench(Bench, Incoming, Left1, NewBench),
        (memberchk(Name, Brought) -> Brought2 = Brought ; Brought2 = [Name|Brought]),
        get_dict(curHP, Faced, TheirHP),
        landed(Active, Opponent, Played, Hit),
        advance_opponent(Entered1, Faced, TheirHP, Hit, Rest, OppTurn, NextOpp, NextRest, NextTurn),
        After = battle(Entered1, NewBench, NextOpp, NextRest, NextTurn, Brought2)
    ).

step(battle(Active, Bench, Opponent, Rest, OppTurn, Brought), Assume,
     attack(Move), OppMove, After, traded(Played, TheirPlayed, Ate)) :-
    Assume = assume(Rolls, _),
    resolve_turn(Active, Opponent, Move, OppMove, Rolls, res(Pok0, Played, Opp0, TheirPlayed)),
    ((Played == flinched ; Played == none) ->
        Pok1 = Pok0, Opp1 = Opp0
    ;
        apply_move_boosts(Pok0, Opp0, Move, Pok1, Opp1)
    ),
    apply_move_boosts(Opp1, Pok1, OppMove, Opp2, Pok2),
    (get_dict(item, Opp2, HadItem) -> true ; HadItem = ""),
    % Plucking a berry your nature dislikes heals you AND confuses you. Confusion
    % is not modelled, so taking the heal and ignoring the cost would hand the
    % search a free win that does not exist -- and it went straight for one. Refuse
    % the line instead: better no answer than an answer built on a gap.
    \+ unpriced_confusion(Move, Played, Active, Opp2),
    eat_berry(Move, Played, Pok2, Opp2, Pok3, Opp3),
    (get_dict(item, Opp3, ""), HadItem \== "" -> Ate = HadItem ; Ate = none),
    max_hp(Opponent, OppMax), berry_proc(Opp3, OppMax, Opp4),
    max_hp(Active, OurMax), berry_proc(Pok3, OurMax, Pok4),
    get_dict(curHP, Opp4, TheirHP),
    verify_hp_rise(Active, Pok4, Ate, Played),
    landed(Active, Opponent, Played, Hit),
    advance_opponent(Pok4, Opp4, TheirHP, Hit, Rest, OppTurn, NextOpp, NextRest, NextTurn),
    After = battle(Pok4, Bench, NextOpp, NextRest, NextTurn, Brought).

% who they have out after the turn: the same one, the next one if it fainted, or
% the next one with this one sent to the back if its Eject Button went off
% Hit says whether our move connected this turn: an Eject Button only fires on a
% hit, and it used to fire on any turn its holder was still carrying it -- so a
% Lopunny that had just KO'd our Pokemon before it moved was "ejected" and the
% switch-in AI was scored against the fainted Pokemon.
advance_opponent(Us, Opp, 0, _, Rest, _, Faced, NextRest, 1) :-
    Rest = [_|_], !,
    sends_out(Us, Rest, after_ko, Next, NextRest),
    Faced = Next.put(_{retaliateBoost: true}).
advance_opponent(_, Opp, 0, _, [], _, Opp, [], 1) :- !.
advance_opponent(Us, Opp0, _, Hit, Rest, OppTurn, Faced, NextRest, NextTurn) :-
    spent_retaliate(Opp0, Opp),
    (Hit == true, get_dict(item, Opp, "Eject Button") ->
        Ejected = Opp.put(_{item: ""}),
        (Rest = [_|_] ->
            sends_out(Us, Rest, after_eject, Faced, Rest1),
            % it goes back to its own party slot, not to the back: ties in the
            % switch-in AI break on party order, and Lopunny is second in Brawly's
            in_party_order([Ejected|Rest1], NextRest),
            NextTurn = 1
        ;
            Faced = Ejected, NextRest = [], NextTurn is OppTurn + 1
        )
    ;
        Faced = Opp, NextRest = Rest, NextTurn is OppTurn + 1
    ).

with_retaliate_boost(P, Q) :- Q = P.put(_{retaliateBoost: true}).

% trainer Pokemon carry their party slot as `index`
in_party_order(Team, Ordered) :-
    (   maplist([P]>>get_dict(index, P, _), Team)
    ->  map_list_to_pairs([P, I]>>get_dict(index, P, I), Team, Pairs),
        keysort(Pairs, Sorted), pairs_values(Sorted, Ordered)
    ;   Ordered = Team
    ).

% the doubled Retaliate lasts one turn
spent_retaliate(Opp, Stripped) :-
    (del_dict(retaliateBoost, Opp, _, Stripped) -> true ; Stripped = Opp).

% Who they send is the Post-KO Switch-in AI's choice scored against whoever WE have
% out at that moment, ties in party order. The search used to take party order
% outright, which threw away the lever the run7 notes pull hardest: pick who is
% standing there when the KO lands, and you pick who comes out next.
sends_out(Us, Rest, Context, Next, Remaining) :-
    % after a KO the AI's Retaliate is the doubled one for the check as well
    (   Context == after_ko
    ->  maplist(with_retaliate_boost, Rest, Scored)
    ;   Scored = Rest
    ),
    (post_ko_switch_in(Us, Scored, [Picked|_]) -> true ; Scored = [Picked|_]),
    get_dict(name, Picked, Name),
    member(Next, Rest), get_dict(name, Next, Name), !,
    drop_from_party(Rest, Next, Remaining).

% did a damaging move of ours connect this turn
landed(_, _, Played, false) :- (Played == none ; Played == flinched), !.
landed(Active, Opponent, Played, Hit) :-
    (highRoll(Active, Opponent, false, Played, Dmg), Dmg > 0 -> Hit = true ; Hit = false).

% the one going out joins the bench, the one coming in leaves it
swap_bench(Bench, Incoming, Leaving, NewBench) :-
    drop_from_party(Bench, Incoming, Without),
    NewBench = [Leaving|Without].


% ---------------------------------------------------------------------------
% The searched line, narrated the same way the greedy one is. The plan is a tree,
% since the AI breaks ties at random and every branch had to be survivable; what
% is printed is the branch where it picks the move that hurts most.
print_search_play_by_play(Box, OppTeam, Assume) :-
    (search_line(Box, OppTeam, Assume, plan(Battle, Tree)) ->
        Battle = battle(Lead, _, Opp, _, _, _),
        get_dict(name, Lead, LeadName),
        get_dict(name, Opp, OppName),
        format("searched line -- every branch below is survivable~n~n"),
        format("  >> they send out ~w~n  << we send out ~w~n", [OppName, LeadName]),
        narrate(Battle, Tree, Assume, 1)
    ;
        search_outcome(Outcome),
        nodes_used(Used),
        format("no line found: ~w (~w nodes)~n", [Outcome, Used])
    ).

narrate(_, won, _, _) :-
    format("        -- fight won, nobody lost --~n").
narrate(Battle, node(Action, Branches), Assume, N) :-
    Battle = battle(Active, _, Opponent, _, _, _),
    % the branch where they click the move that hurts most
    worst_branch(Branches, Opponent, Active, Option-Sub),
    step(Battle, Assume, Action, Option, After, Happened),
    narrate_step(N, Active, Opponent, Action, Happened, After, Branches),
    N1 is N + 1,
    narrate(After, Sub, Assume, N1).

worst_branch(Branches, Opponent, Active, Worst) :-
    pairs_keys(Branches, Moves),
    (worst_opponent_move(Moves, Opponent, Active, Pick), memberchk(Pick-Sub, Branches) ->
        Worst = Pick-Sub
    ;
        Branches = [Worst|_]
    ).

% `none` in either slot means that side never got to move: it was KO'd first, or
% flinched. Printing the move it had picked instead of saying so is what made
% Kubfu look like it attacked after fainting.
narrate_step(N, Active, Opponent, attack(_), traded(OurPlayed, TheirPlayed, Ate), After, Branches) :-
    Move = OurPlayed, TheirMove = TheirPlayed,
    After = battle(NewActive, _, NewOpp, NewRest, _, _),
    get_dict(name, Active, Us),
    get_dict(name, Opponent, Them),
    current_hp(NewActive, OurHP),
    opponent_shown(Opponent, NewOpp, NewRest, Them, TheirHP, Note),
    turn_order(Active, Opponent, Move, TheirMove, Order),
    order_note(Order, OrderNote),
    alternatives(Branches, Alt),
    % print them in the order they actually happened -- showing our move first
    % when they are faster makes a Pokemon look like it attacks after fainting
    ours_line(Move, Us, Them, TheirHP, Note, OursLine),
    theirs_line(TheirMove, Them, Us, OurHP, Alt, OrderNote, TheirsLine),
    ((Order == us_first ; Order == our_priority) ->
        format("     ~w. ~w~n", [N, OursLine]),
        format("        ~w~n", [TheirsLine])
    ;
        format("     ~w. ~w~n", [N, TheirsLine]),
        format("        ~w~n", [OursLine])
    ),
    % an eaten berry moves HP in the other direction and looks like a bug otherwise
    (Ate \== none, pinch_berry(Ate, _) ->
        format("        (~w eats ~w's ~w and heals)~n", [Us, Them, Ate])
    ; Ate \== none ->
        format("        (~w plucks ~w's ~w away)~n", [Us, Them, Ate])
    ;
        true
    ).
narrate_step(N, Active, Opponent, pivot_attack(_, _), uturned(Played, TheirPlayed, In), After, Branches) :-
    After = battle(NewActive, NewBench, NewOpp, NewRest, _, _),
    get_dict(name, Active, Us),
    get_dict(name, Opponent, Them),
    opponent_shown(Opponent, NewOpp, NewRest, Them, TheirHP, Note),
    alternatives(Branches, Alt),
    turn_order(Active, Opponent, Played, TheirPlayed, Order),
    order_note(Order, OrderNote),
    (   Played == none
    ->  current_hp(NewActive, OurHP),
        format("     ~w. ~w uses ~w~t~30|-> ~w ~w hp~w~w~n", [N, Them, TheirPlayed, Us, OurHP, Alt, OrderNote]),
        format("        ~w never gets to U-turn~n", [Us])
    ;   format(atom(OursLine), "~w uses ~w~t~30|-> ~w ~w hp~w, ~w comes in", [Us, Played, Them, TheirHP, Note, In]),
        (   TheirPlayed == none
        ->  format(atom(TheirsLine), "~w never moves", [Them])
        ;   (   (Order == us_first ; Order == our_priority)
            ->  Hit = In, current_hp(NewActive, HitHP)
            ;   Hit = Us, member(P, NewBench), get_dict(name, P, Us), current_hp(P, HitHP)
            ),
            format(atom(TheirsLine), "~w uses ~w~t~30|-> ~w ~w hp~w~w", [Them, TheirPlayed, Hit, HitHP, Alt, OrderNote])
        ),
        (   (Order == us_first ; Order == our_priority)
        ->  format("     ~w. ~w~n        ~w~n", [N, OursLine, TheirsLine])
        ;   format("     ~w. ~w~n        ~w~n", [N, TheirsLine, OursLine])
        )
    ).
narrate_step(_, Active, _, replace(_), replaced(Name), After, _) :-
    After = battle(NewActive, _, _, _, _, _),
    get_dict(name, Active, Out),
    current_hp(NewActive, HP),
    format("  << ~w has fainted; we send out ~w (~w hp)~n", [Out, Name, HP]).
narrate_step(_, Active, Opponent, switch(Incoming), switched(TheirMove), After, _) :-
    After = battle(NewActive, _, _, _, _, _),
    get_dict(name, Active, Out),
    get_dict(name, Incoming, In),
    get_dict(name, Opponent, Them),
    current_hp(NewActive, HP),
    (TheirMove == "Pursuit" ->
        format("  << we switch ~w -> ~w; ~w Pursuits ~w on the way out~n", [Out, In, Them, Out])
    ;
        format("  << we switch ~w -> ~w, eating ~w (~w on ~w hp)~n", [Out, In, TheirMove, In, HP])
    ).

ours_line(flinched, Us, _, _, _, Line) :-
    format(atom(Line), "~w flinches, no move", [Us]).
ours_line(none, Us, _, _, _, Line) :-
    format(atom(Line), "~w goes down before it moves", [Us]).
ours_line(Move, Us, Them, TheirHP, Note, Line) :-
    Move \== flinched, Move \== none,
    format(atom(Line), "~w uses ~w~t~30|-> ~w ~w hp~w", [Us, Move, Them, TheirHP, Note]).

theirs_line(none, Them, Us, OurHP, _, _, Line) :-
    format(atom(Line), "~w never moves~t~30|-> ~w ~w hp", [Them, Us, OurHP]).
theirs_line(Move, Them, Us, OurHP, Alt, OrderNote, Line) :-
    Move \== none,
    format(atom(Line), "~w uses ~w~t~30|-> ~w ~w hp~w~w", [Them, Move, Us, OurHP, Alt, OrderNote]).

% did that kill it, and who is out now
opponent_shown(Old, New, NewRest, _, HP, Note) :-
    get_dict(name, Old, Name),
    get_dict(name, New, NewName),
    (   Name == NewName
    ->  current_hp(New, HP), Note = ''
    ;   member(R, NewRest), get_dict(name, R, Name)
    ->  % still in their party: it was dragged out by its Eject Button, not KO'd
        current_hp(R, HP),
        format(atom(Note), "   ** Eject Button, they send out ~w **", [NewName])
    ;   HP = 0,
        format(atom(Note), "   ** faints, they send out ~w **", [NewName])
    ).

alternatives(Branches, Alt) :-
    pairs_keys(Branches, Moves),
    (Moves = [_,_|_] -> format(atom(Alt), "   (or ~w)", [Moves]) ; Alt = '').

% ---------------------------------------------------------------------------
% Segmented search
% ---------------------------------------------------------------------------
% A flat search over the whole fight does not scale: three opposing Pokemon fit,
% four do not, and pruning and depth-scaling both turned out to change nothing.
% The fight is naturally sequential, so search it one opposing Pokemon at a time.
%
% The whole-fight context survives because the battle state carries forward --
% party damage, boosts, spent items, which slots the party cap has already been
% charged for, the opponent's remaining queue -- and because segments/3 is
% nondeterministic: when a later opponent turns out to be unwinnable from where
% the previous segment left us, it backtracks and beats that one a different way.
% That is the part a greedy per-slot planner cannot do.
%
% One approximation, and it is worth being explicit about. Inside a segment every
% move the AI might pick has to be survivable, as before. Between segments we
% continue from the WORST of those branches, so a line is judged on the least
% favourable way its previous segment could have gone.
segment_depth(10).
segment_depths([3, 5, 8, 12]).

search_fight(Box, OppTeam, Assume, Fight) :-
    max_party(Max),
    length(Box, Size),
    Allowed is min(Max, Size),
    between(0, Allowed, Deaths),        % cheapest first
    retractall(death_budget(_)), assertz(death_budget(Deaths)),
    retractall(party_size(_)), assertz(party_size(Allowed)),
    search_fight_at(Box, OppTeam, Assume, Fight).

search_fight_at(Box, OppTeam, Assume, Fight) :-
    search_fight_raw(Box, OppTeam, Assume, Fight),
    verify_deaths(Fight, Assume).

search_fight_raw(Box, OppTeam, Assume, fight(Battle, Segments)) :-
    reset_search,
    initial_battle(Box, OppTeam, Assume, Battle),
    segments(Battle, Assume, Segments).

segments(Battle, _, []) :-
    Battle = battle(_, _, Opp, Rest, _, _),
    opponents_down(Opp, Rest), !.
segments(Battle, Assume, [segment(Name, Tree)|More]) :-
    Battle = battle(_, _, Opponent, _, _, _),
    get_dict(name, Opponent, Name),
    % Iterative deepening. Most opponents fall in two or three turns; allowing ten
    % from the start pays the full branching factor for depth almost nothing uses.
    % Shallow first also means the line we find is the short one.
    segment_depths(Depths),
    member(Depth, Depths),
    beat_target(Battle, Assume, Name, Depth, Tree, After),
    segments(After, Assume, More).

% done the moment the thing we were aiming at is no longer the one standing there
beat_target(Battle, _, Target, _, done, Battle) :-
    Battle = battle(_, _, Opp, _, _, _),
    get_dict(name, Opp, Name),
    Name \== Target, !.
beat_target(Battle, _, _, _, done, Battle) :-
    Battle = battle(_, _, Opp, Rest, _, _),
    opponents_down(Opp, Rest), !.
% Failure memo. beat_target/6 has to stay nondeterministic -- all_branches/8
% needs to come back for a different way -- so the if-then-else trick winnable/4
% uses is not available. Instead: run the alternatives, and if none of them
% produced a solution, record the loss on the way out. nb_setarg gives a flag that
% survives backtracking, which is what tells the two cases apart.
beat_target(Battle, Assume, Target, Depth, Tree, After) :-
    Depth > 0,
    within_death_budget(Battle),
    \+ hopeless(Battle, Depth),
    battle_key(Battle, Key),
    memo_probe,
    % hash first so SWI can index on an integer: a dynamic predicate keyed on a
    % deep compound gets almost no first-argument indexing, and at 60k+ probes
    % every lookup was a scan
    term_hash(Key-Target, Hash),
    (   seen_loss(Hash, Key-Target, Had), Had >= Depth
    ->  memo_hit, fail
    ;   true
    ),
    Found = found(no),
    (   beat_here(Battle, Assume, Target, Depth, Tree, After),
        nb_setarg(1, Found, yes)
    ;   arg(1, Found, no),
        % only a real loss if we actually finished looking. Running out of budget
        % also fails, and recording that as a loss poisons the memo with positions
        % we never explored.
        \+ exhausted,
        assertz(seen_loss(Hash, Key-Target, Depth)),
        fail
    ).

beat_here(Battle, Assume, Target, Depth, node(Action, Branches), After) :-
    spend_node,
    Battle = battle(Active, Bench, Opponent, _, OppTurn, Brought),
    action(Active, Opponent, Bench, Brought, Assume, OppTurn, Action),
    ai_options(Active, Opponent, OppTurn, Assume, Options),
    Options = [_|_],
    Next is Depth - 1,
    % Every AI option has to be survivable, and we must still be able to come back
    % and survive it a DIFFERENT way when something later fails. findall cannot do
    % that: it either over-collects (one entry per sub-line, so the arity check
    % never matches) or, wrapped in once/1, freezes each branch to its first
    % solution and makes the whole search far weaker than it looks. Explicit
    % recursion gives the conjunction over options while leaving choicepoints.
    all_branches(Options, Battle, Assume, Action, Target, Next, Branches, Ends),
    worst_end(Ends, After).

all_branches([], _, _, _, _, _, [], []).
all_branches([Option|Rest], Battle, Assume, Action, Target, Depth,
             [Option-Sub|Subs], [End|Ends]) :-
    step(Battle, Assume, Action, Option, Stepped),
    beat_target(Stepped, Assume, Target, Depth, Sub, End),
    all_branches(Rest, Battle, Assume, Action, Target, Depth, Subs, Ends).

:- dynamic death_budget/1.
:- dynamic party_size/1.

% How many of ours a line is allowed to lose. Requiring zero is the right question
% only if a deathless line exists, and for these boxes it mostly does not -- your
% own runs.txt has one clear in eleven. Searching for the CHEAPEST loss always
% returns something and degrades gracefully: "wins, costs Skrelp" is the decision
% you are actually making at the keyboard.
% Count the bodies directly. Comparing a survivor count against a party-size
% figure went wrong the moment the box was bigger than the party: Lost came out
% negative and the budget never bit.
% A Pokemon that faints is DROPPED from the party, so counting curHP == 0 among
% the survivors counts nothing and the budget never binds -- which is how a line
% that spends three of them was reported as costing one. Count instead the ones we
% committed a slot to that are no longer standing.
within_death_budget(battle(Active, Bench, _, _, _, Brought)) :-
    death_budget(Max),
    aggregate_all(count,
        (
            member(Name, Brought),
            \+ (
                member(P, [Active|Bench]),
                get_dict(name, P, Name),
                current_hp(P, HP),
                HP > 0
            )
        ),
        Lost),
    Lost =< Max.

% Cheap admissible cutoff. If the hardest hit anyone available can land, repeated
% for every turn we have left, still cannot take the thing in front of us down,
% then no arrangement of those turns wins and there is nothing to search. This is
% what stops a hopeless fight from running to the node budget and coming back
% "exhausted" -- which reads like ignorance when it is actually a loss.
hopeless(battle(Active, Bench, Opponent, _, _, Brought), Depth) :-
    current_hp(Opponent, OppHP),
    OppHP > 0,
    max_party(Max),
    exclude([X]>>(X = run(_)), Brought, BroughtNames),
    length(BroughtNames, Used),
    findall(P,
        (
            member(P, [Active|Bench]),
            current_hp(P, HP), HP > 0,
            (already_brought(Brought, P) -> true ; Used < Max)
        ),
        Available),
    best_available_hit(Available, Opponent, Best),
    Best * Depth < OppHP.

best_available_hit(Available, Opponent, Best) :-
    findall(Dmg,
        (
            member(P, Available),
            get_dict(moves, P, Moves),
            member(M, Moves),
            highRoll(P, Opponent, false, M, Dmg)
        ),
        Damages),
    (Damages == [] -> Best = 0 ; max_list(Damages, Best)).

% the branch that leaves us in the worst shape, measured over the whole party
worst_end(Ends, Worst) :-
    findall(Health-End, (member(End, Ends), party_health(End, Health)), Scored),
    keysort(Scored, [_-Worst|_]).

party_health(battle(Active, Bench, _, _, _, _), Health) :-
    findall(HP, (member(P, [Active|Bench]), current_hp(P, HP)), HPs),
    sum_list(HPs, Health).

% narration for the segmented search: walk each segment's tree from the state the
% previous one left, so the turn numbers and HP run continuously through the fight
print_searched_fight(Box, OppTeam, Assume) :-
    (search_fight(Box, OppTeam, Assume, fight(Battle, Segments)) ->
        Battle = battle(Lead, _, Opp, _, _, _),
        get_dict(name, Lead, LeadName),
        get_dict(name, Opp, OppName),
        nodes_used(Used),
        format("line found, nobody lost (~w nodes)~n~n", [Used]),
        format("  >> they send out ~w~n  << we send out ~w~n", [OppName, LeadName]),
        narrate_segments(Battle, Segments, Assume, 1)
    ;
        search_outcome(Outcome),
        nodes_used(Used),
        format("no line: ~w (~w nodes)~n", [Outcome, Used])
    ).

narrate_segments(Battle, [], _, _) :-
    actual_losses(Battle, Lost),
    (   Lost =:= 0
    ->  format("        -- fight won, nobody lost --~n")
    ;   format("        -- fight won, ~w lost --~n", [Lost])
    ).
narrate_segments(Battle, [segment(_, Tree)|Rest], Assume, N) :-
    narrate_tree(Battle, Tree, Assume, N, After, Next),
    narrate_segments(After, Rest, Assume, Next).

narrate_tree(Battle, done, _, N, Battle, N).
narrate_tree(Battle, node(Action, Branches), Assume, N, End, EndN) :-
    Battle = battle(Active, _, Opponent, _, _, _),
    worst_branch(Branches, Opponent, Active, Option-Sub),
    step(Battle, Assume, Action, Option, After, Happened),
    narrate_step(N, Active, Opponent, Action, Happened, After, Branches),
    Next is N + 1,
    narrate_tree(After, Sub, Assume, Next, End, EndN).

% ---------------------------------------------------------------------------
% Team selection by witness
% ---------------------------------------------------------------------------
% Searching "which six" and "how to play them" together is what makes the full
% fight blow up: a greedy prefix spends the party cap, and unwinding it means
% re-searching the entire play space.
%
% Split them. Beating one opposing Pokemon from full, with the whole box to hand,
% is cheap -- tens of nodes. Do that once per opponent and record WHICH Pokemon
% the winning line actually used. That set is a witness: those Pokemon suffice for
% that opponent. The union over all opponents is a squad that can answer
% everything, and if it fits in six it is a candidate worth searching properly.
%
% The isolation check is a sound filter, not just a heuristic: you arrive at an
% opponent with less than you started with, so a squad that cannot beat it fresh
% cannot beat it damaged either. (That holds while the action set has no setup
% moves, so no positive boosts can carry in from an earlier fight.)
witness_squad(Box, OppTeam, Assume, Squad, Coverage) :-
    findall(Name-Used,
        (
            member(Opponent, OppTeam),
            get_dict(name, Opponent, Name),
            (   once(search_fight(Box, [Opponent], Assume, Fight))
            ->  line_members(Fight, Assume, Used)
            ;   Used = unbeatable
            )
        ),
        Coverage),
    findall(N, (member(_-Us, Coverage), is_list(Us), member(N, Us)), AllNames),
    sort(AllNames, Needed),
    findall(P, (member(P, Box), get_dict(name, P, PN), memberchk(PN, Needed)), Squad).

% which of ours actually appear in a plan
line_members(fight(Battle, Segments), Assume, Names) :-
    Battle = battle(Lead, _, _, _, _, _),
    get_dict(name, Lead, LeadName),
    walk_members(Battle, Segments, Assume, Rest),
    sort([LeadName|Rest], Names).

walk_members(_, [], _, []).
walk_members(Battle, [segment(_, Tree)|More], Assume, Names) :-
    walk_tree_members(Battle, Tree, Assume, Here, After),
    walk_members(After, More, Assume, Later),
    append(Here, Later, Names).

walk_tree_members(Battle, done, _, [], Battle).
walk_tree_members(Battle, node(Action, Branches), Assume, Names, End) :-
    Battle = battle(Active, _, Opponent, _, _, _),
    worst_branch(Branches, Opponent, Active, Option-Sub),
    step(Battle, Assume, Action, Option, After),
    (Action = switch(In) -> get_dict(name, In, N), Names = [N|Rest] ; Names = Rest),
    walk_tree_members(After, Sub, Assume, Rest, End).

% ===========================================================================
% Invariants
% ===========================================================================
% Three results in this session rested on a broken invariant -- proofs that were
% really budget exhaustion, a line that spent three Pokemon while reporting one,
% and a heal that came from a mechanic the model does not simulate. Every one was
% caught by reading output, none by the search noticing. These check the claims at
% the point they are made, and the wrong answer was the optimistic one each time,
% so they are worth the cost.
:- dynamic invariant_violation/1.

note_violation(What) :-
    assertz(invariant_violation(What)),
    format(user_error, "!! INVARIANT VIOLATED: ~w~n", [What]).

violations(Vs) :- findall(V, invariant_violation(V), Vs).

clear_violations :- retractall(invariant_violation(_)).

% 1. a plan must cost what it claims
verify_deaths(fight(Battle, Segments), Assume) :-
    death_budget(Max),
    plan_final_state(Battle, Segments, Assume, Final),
    actual_losses(Final, Lost),
    (   Lost =< Max
    ->  true
    ;   note_violation(deaths(budget(Max), actually_lost(Lost)))
    ).

actual_losses(battle(Active, Bench, _, _, _, Brought), Lost) :-
    aggregate_all(count,
        (
            member(Name, Brought),
            \+ (
                member(P, [Active|Bench]),
                get_dict(name, P, Name),
                current_hp(P, HP),
                HP > 0
            )
        ),
        Lost).

plan_final_state(Battle, [], _, Battle).
plan_final_state(Battle, [segment(_, Tree)|Rest], Assume, Final) :-
    plan_walk(Battle, Tree, Assume, Next),
    plan_final_state(Next, Rest, Assume, Final).

plan_walk(Battle, done, _, Battle).
plan_walk(Battle, node(Action, Branches), Assume, Final) :-
    Battle = battle(Active, _, Opponent, _, _, _),
    worst_branch(Branches, Opponent, Active, Option-Sub),
    step(Battle, Assume, Action, Option, After),
    plan_walk(After, Sub, Assume, Final).

% 2. a proof is only a proof if we never ran out of room
verify_proof :-
    nodes_used(Used),
    node_budget(Max),
    (   Used >= Max
    ->  note_violation(proof_claimed_at_budget_limit(Used))
    ;   true
    ).

% 3. our HP may only rise if something that heals actually fired
verify_hp_rise(Before, After, Ate) :-
    verify_hp_rise(Before, After, Ate, none).

verify_hp_rise(Before, After, Ate, Played) :-
    current_hp(Before, Was),
    current_hp(After, Now),
    (   Now > Was,
        Ate == none,
        \+ healed_by_item(Before, After),
        \+ healed_by_drain(Played, After)
    ->  get_dict(name, Before, Name),
        note_violation(hp_rose_unexplained(Name, Was, Now))
    ;   true
    ).

% a drain move we actually played is the other modelled way HP goes up
healed_by_drain(Played, Pokemon) :-
    string(Played),
    move_data(Played, Pokemon, MD),
    get_dict(drain, MD, [_, _]).

% a pinch berry firing is the one modelled way HP goes up on its own
healed_by_item(Before, After) :-
    get_dict(item, Before, Item),
    heal_berry(Item),
    (get_dict(item, After, "") -> true ; \+ get_dict(item, After, Item)).


% ===========================================================================
% Macro planner
% ===========================================================================
% The turn-level search cannot reach the lines in the run7 notes: an Intimidate
% cycle alone is twelve switch turns, and Thunder Wave is not in its action set.
% Read the notes as a program and the vocabulary is not turns, it is matchup
% moves: solo this, pivot on that, cycle these two until it is at -6, paralyse it
% and sweep. A whole fight is a dozen of those.
%
% So plan in that vocabulary. A macro is a deterministic per-turn POLICY; expanding
% it runs the policy through step/6 with the same AND over every move the AI might
% pick, so a macro that gets someone killed on any branch fails exactly as a bad
% turn did. Nothing is trusted that the stepper did not simulate. What the planner
% gains is depth: the eighteen-turn Hitmontop plan is four edges.
%
% Between macros we continue from the branch the narration follows (plan_walk/4),
% as the segmented search did between opponents. Inside a macro every AI branch
% must survive.
% Iterative deepening on the number of macros in the WHOLE fight, not per
% opponent. Searching each opponent to its own depth and backtracking into the
% earlier ones enumerated thousands of ways to beat Lopunny, none of which left
% anything for Hitmontop. Shortest whole plan first finds the efficient line and
% bounds the thrash: a longer plan is only considered once every shorter one has
% failed, and those failures are memoised by position and macros remaining.
plan_depths(OppCount, Depths) :-
    Max is 3 * OppCount + 2,
    numlist(OppCount, Max, All),
    include([D]>>(0 is (D - OppCount) mod 2), All, Depths).
macro_turn_cap(14).

:- dynamic seen_macro_loss/3.
:- dynamic expand_memo/3.

% set planner_trace to watch the planner work its way through the opposing team
:- dynamic planner_trace/0.
planner_note(Fmt, Args) :-
    (   planner_trace
    ->  nodes_used(U),
        format(user_error, "[~w] ", [U]),
        format(user_error, Fmt, Args),
        nl(user_error)
    ;   true
    ).

% same contract as search_fight/4: cheapest death budget first, deaths verified.
% Each budget is a full iterative-deepening pass of a few minutes, and a line
% that spends three Pokemon is not one anybody plays, so the ladder stops at two.
max_plan_deaths(2).
plan_fight(Box, OppTeam, Assume, Fight) :-
    max_party(Max),
    length(Box, Size),
    max_plan_deaths(Cap),
    Allowed is min(Cap, min(Max, Size)),
    between(0, Allowed, Deaths),
    retractall(death_budget(_)), assertz(death_budget(Deaths)),
    retractall(party_size(_)), assertz(party_size(Allowed)),
    plan_fight_at(Box, OppTeam, Assume, Fight).

plan_fight_at(Box, OppTeam, Assume, Fight) :-
    plan_fight_raw(Box, OppTeam, Assume, Fight),
    verify_deaths(Fight, Assume).

plan_fight_raw(Box, OppTeam, Assume, fight(Battle, Segments)) :-
    reset_search,
    length(OppTeam, OppCount),
    plan_depths(OppCount, Depths),
    member(Depth, Depths),
    planner_note("whole fight in ~w macros", [Depth]),
    initial_battle(Box, OppTeam, Assume, Battle),
    finish_fight(Battle, Assume, Depth, 0, Segments).

% Segments come out as segment(macro(Target, What), Tree) with Tree in the
% turn-level shape, so narration, verify_deaths and line_members read a planned
% fight exactly as they read a searched one.
%
% Pivots counts the pivots in a row so far. Two is a real pattern -- bait a move
% with one Pokemon, bring the answer in on it -- four is the planner burning
% party slots because nothing else worked, and it never leads anywhere.
finish_fight(Battle, _, _, _, []) :-
    Battle = battle(_, _, Opp, Rest, _, _),
    opponents_down(Opp, Rest), !.
finish_fight(Battle, Assume, Remaining, Pivots, [segment(macro(Target, What), Tree)|More]) :-
    Remaining > 0,
    within_death_budget(Battle),
    Battle = battle(Active, _, Opponent, _, _, _),
    get_dict(name, Opponent, Target),
    battle_key(Battle, Key),
    memo_probe,
    term_hash(Key-Pivots, Hash),
    (   seen_macro_loss(Hash, Key-Pivots, Had), Had >= Remaining
    ->  memo_hit, fail
    ;   true
    ),
    Found = found(no),
    (   macro_action(Battle, Assume, Macro),
        (   (Macro = pivot(_) ; Macro = uturn(_, _))
        ->  Pivots < 2, Pivots1 is Pivots + 1
        ;   Pivots1 = 0
        ),
        macro_desc(Macro, What),
        get_dict(name, Active, ActName),
        (   expand(Macro, Target, Battle, Assume, Tree, End)
        ->  End = battle(EA, _, EO, _, _, _), current_hp(EA, EHP), current_hp(EO, OHP),
            planner_note("~*c~w vs ~w: ~w -> ~w ~w hp, ~w ~w hp",
                         [Pivots, 0' , ActName, Target, What, EA.name, EHP, EO.name, OHP])
        ;   planner_note("~w vs ~w: ~w fails to expand", [ActName, Target, What]), fail
        ),
        Next is Remaining - 1,
        finish_fight(End, Assume, Next, Pivots1, More),
        nb_setarg(1, Found, yes)
    ;   arg(1, Found, no),
        \+ exhausted,
        assertz(seen_macro_loss(Hash, Key-Pivots, Remaining)),
        fail
    ).

% ---- the macros ------------------------------------------------------------
% Ordered simple to clever, since the planner takes the first that works.
%   sweep(M)       stay in and use M; finish with a priority move when it KOs
%   status(M)      one turn of a status move that cannot miss and would land
%   cycle(P,Q,K)   Intimidate P and partner Q switch back and forth until the
%                  opponent's Attack is at stage K
%   setup(M)       one turn of Focus Energy or a self-boosting move
%   uturn(M,P)     hit with a pivot move and bring P in on the same turn
%   pivot(P)       one switch, on the same terms the turn-level search allowed
%   sack(M)        stay in and use M until this Pokemon drops (needs death budget)
%   replace(P)     the free send-out after one of ours fainted
macro_action(battle(Active, Bench, Opp, _, OppTurn, Brought), Assume, Macro) :-
    (   get_dict(curHP, Active, 0)
    ->  action_replace(Bench, Opp, Brought, replace(P)),
        Macro = replace(P)
    ;   (   % a sweep with the hardest-hitting move, and one with any move that
            % eats the target's berry: Prinplup's Pluck ties Bubble Beam against
            % Kubfu and takes its Iapapa for half of Prinplup's HP on the way
            sweep_moves(Active, Opp, Moves),
            member(Move, Moves),
            Macro = sweep(Move)
        ;   status_macro(Active, Opp, Macro)
        ;   setup_macro(Active, Opp, Macro)
        ;   cycle_macro(Active, Bench, Opp, Brought, Macro)
        ;   uturn_macro(Active, Bench, Opp, Brought, Macro)
        ;   action_switch(Bench, Active, Opp, Brought, Assume, OppTurn, switch(P)),
            Macro = pivot(P)
        ;   % last resort, and only when the death budget has room: keep hitting
            % until this one drops. "Endeavor Scraggy when low as a sac" is a real
            % line, and without it a death budget above zero could never be spent
            death_budget(Allowed), Allowed > 0,
            sack_moves(Active, Opp, Moves),
            member(Move, Moves),
            Macro = sack(Move)
        )
    ).

% the hardest hit, plus Endeavor when it would take more off than that
sack_moves(Active, Opp, Moves) :-
    once(highest_damage_move(Active, Opp, Best)),
    (   sweepable(Best), hurts(Active, Opp, Best) -> Moves0 = [Best] ; Moves0 = [] ),
    (   get_dict(moves, Active, All), memberchk("Endeavor", All),
        highRoll(Active, Opp, false, "Endeavor", E), highRoll(Active, Opp, false, Best, B), E > B
    ->  Moves = ["Endeavor"|Moves0]
    ;   Moves = Moves0
    ).

% a self-targeting move that leaves the user better off: Focus Energy, or any
% move whose boosts land on the user (Work Up, Bulk Up), while there is room
setup_macro(Active, Opp, setup(Move)) :-
    get_dict(moves, Active, Moves),
    member(Move, Moves),
    (   Move == "Focus Energy"
    ->  crit_stage(Active, S), S < 3
    ;   calculate(Active, Opp, Move, Data),
        move_data(Move, Data.defender, MD),
        get_dict(category, MD, "Status"),
        self_boosts(MD, Pairs), Pairs = [_|_],
        boosts_of(Active, B),
        member(Stat-Delta, Pairs), Delta > 0,
        get_dict(Stat, B, Cur), Cur < 6
    ).

% attack and leave in one move, bringing P in on whatever they do next
uturn_macro(Active, Bench, Opp, Brought, uturn(Move, P)) :-
    get_dict(moves, Active, Moves),
    member(Move, Moves),
    pivot_move(Move),
    hurts(Active, Opp, Move),
    switch_candidates(Bench, Opp, Brought, Ordered),
    member(P, Ordered),
    current_hp(P, HP), HP > 0.

% The hardest-hitting move(s); any damaging move that eats the target's berry;
% and, when they outspeed us, any priority move -- Quick Attack on Lopunny is not
% about the damage, it is about hitting its Eject Button before Retaliate lands.
sweep_moves(Active, Opp, Moves) :-
    best_moves(Active, Opp, Best),
    get_dict(moves, Active, All),
    findall(M,
        (
            member(M, All),
            berry_eater(M),
            get_dict(item, Opp, Item),
            sub_string(Item, _, _, 0, "Berry")
        ),
        Eaters),
    (   speed_of(Active, Ours), speed_of(Opp, Theirs), Ours =< Theirs
    ->  findall(M, (member(M, All), has_priority(Active, Opp, M)), Fast)
    ;   Fast = []
    ),
    % Endeavor's damage is the HP gap, so it is a sweep candidate whenever it hurts
    (memberchk("Endeavor", All) -> Gap = ["Endeavor"] ; Gap = []),
    append([Best, Eaters, Fast, Gap], Both),
    list_to_set(Both, Distinct),
    include(sweepable, Distinct, Usable),
    include(hurts(Active, Opp), Usable, Moves).

hurts(Active, Opp, Move) :-
    highRoll(Active, Opp, false, Move, Dmg),
    Dmg > 0.

% Fake Out only works on the turn its user comes in, which no sweep is, and a
% pivot move forces the switch, so it is only ever an uturn macro
sweepable(Move) :-
    Move \== "Fake Out",
    \+ pivot_move(Move),
    \+ phazing_move(Move).

% these drag the target out for a random replacement, which nothing here models,
% so they are not offered as plain damage
phazing_move("Circle Throw").
phazing_move("Dragon Tail").
phazing_move("Roar").
phazing_move("Whirlwind").

status_macro(Active, Opp, status(Move)) :-
    get_dict(moves, Active, Moves),
    member(Move, Moves),
    move_data(Move, Opp, MD),
    get_dict(status, MD, S), S \== null,
    \+ useless(Move, Opp),
    cannot_miss(Active, Move, MD).

% the Intimidate holder is either already out or on the bench; the partner is
% whoever is out otherwise, or any bench Pokemon the party cap still allows
cycle_macro(Active, Bench, Opp, Brought, cycle(P, Q, K)) :-
    get_dict(ability, Opp, Ability),
    \+ intimidate_immune(Ability),
    boosts_of(Opp, Boosts),
    get_dict(atk, Boosts, Atk),
    Atk > -6,
    (   get_dict(ability, Active, "Intimidate")
    ->  P = Active,
        switch_candidates(Bench, Opp, Brought, Partners),
        member(Q, Partners),
        current_hp(Q, QHP), QHP > 0
    ;   switch_candidates(Bench, Opp, Brought, Holders),
        member(P, Holders),
        get_dict(ability, P, "Intimidate"),
        Q = Active
    ),
    member(K, [-6, -3]),
    K < Atk.

% Bench, in the order the search prefers: already-committed party slots first,
% then base stat total, and only those the party cap still allows.
switch_candidates(Bench, Opp, Brought, Ordered) :-
    (nuzlocke_switchin(Opp, Bench, Ranked) -> true ; Ranked = Bench),
    include(already_brought(Brought), Ranked, Committed0),
    exclude(already_brought(Brought), Ranked, Fresh0),
    by_strength(Committed0, Committed),
    by_strength(Fresh0, Fresh1),
    include(cap_allows(Brought), Fresh1, Fresh),
    append(Committed, Fresh, Ordered).

cap_allows(Brought, P) :-
    get_dict(name, P, Name),
    (   memberchk(Name, Brought)
    ->  true
    ;   max_party(Max),
        exclude([X]>>(X = run(_)), Brought, Names),
        length(Names, Used),
        Used < Max
    ).

% what the plan says at the macro level, names only
macro_desc(sweep(M), sweep(M)).
macro_desc(sack(M), sack(M)).
macro_desc(setup(M), setup(M)).
macro_desc(uturn(M, P), uturn(M, N)) :- get_dict(name, P, N).
macro_desc(status(M), status(M)).
macro_desc(pivot(P), pivot(N)) :- get_dict(name, P, N).
macro_desc(replace(P), replace(N)) :- get_dict(name, P, N).
macro_desc(cycle(P, Q, K), cycle(PN, QN, K)) :- get_dict(name, P, PN), get_dict(name, Q, QN).

% ---- policies: the concrete action a macro takes from a given position ------
macro_policy(sweep(Chosen), battle(Active, _, Opp, _, _, _), attack(Move)) :-
    sweep_move(Active, Opp, Chosen, Move).
macro_policy(sack(Chosen), battle(Active, _, Opp, _, _, _), attack(Move)) :-
    sweep_move(Active, Opp, Chosen, Move).
macro_policy(status(Move), _, attack(Move)).
macro_policy(setup(Move), _, attack(Move)).
macro_policy(uturn(Move, P), battle(_, Bench, _, _, _, _), pivot_attack(Move, In)) :-
    get_dict(name, P, Name),
    member(In, Bench),
    get_dict(name, In, Name).
macro_policy(pivot(P), _, switch(P)).
macro_policy(replace(P), _, replace(P)).
macro_policy(cycle(P, Q, _), battle(Active, Bench, _, _, _, _), switch(In)) :-
    get_dict(name, Active, Out),
    get_dict(name, P, PN), get_dict(name, Q, QN),
    (Out == PN -> Wanted = QN ; Wanted = PN),
    % the macro's own dicts are snapshots; the bench carries the current HP
    member(In, Bench),
    get_dict(name, In, Wanted).

% finish with a priority move when it KOs from here, otherwise the sweep's own
% move. "Brine + Aqua Jet" is one sweep.
sweep_move(Active, Opp, Chosen, Move) :-
    (   get_dict(moves, Active, Moves),
        member(Move, Moves),
        has_priority(Active, Opp, Move),
        calculate(Active, Opp, Move, Data), calc_defender(Opp, Data, OppT),
        crit_flag(Active, OppT, Move, Crit),
        lowRoll(Active, Opp, Crit, Move, Low),
        current_hp(Opp, HP),
        Low >= HP
    ->  true
    ;   Move = Chosen
    ).

% ---- when a macro is over -------------------------------------------------
% Any macro ends when our Pokemon fainted (the planner then has to replace it and
% the death budget decides whether that was acceptable) or when the target is no
% longer the one standing there.
macro_done(Macro, _, _, battle(Active, _, _, _, _, _), _) :-
    Macro \= replace(_),
    get_dict(curHP, Active, 0), !.
macro_done(_, _, _, battle(_, _, Opp, Rest, _, _), _) :-
    opponents_down(Opp, Rest), !.
macro_done(_, _, Target, battle(_, _, Opp, _, _, _), _) :-
    get_dict(name, Opp, Name),
    Name \== Target, !.
% A sweep stops while leaving is still an option: if the next turn kills us it
% hands the position back to the planner, which can pivot. Sweeping into a death
% is not a plan step; spending a Pokemon on purpose would be its own macro.
macro_done(sweep(Chosen), Assume, _, battle(Active, _, Opp, _, OppTurn, _), _) :-
    (   \+ crit_screen(Active, Opp, Assume)
    ->  true
    ;   sweep_move(Active, Opp, Chosen, Move),
        next_turn_kills_us(Active, Opp, Assume, OppTurn, Move)
    ), !.
macro_done(status(_), _, _, _, N) :- N >= 1.
macro_done(setup(_), _, _, _, N) :- N >= 1.
macro_done(uturn(_, _), _, _, _, N) :- N >= 1.
macro_done(pivot(_), _, _, _, N) :- N >= 1.
macro_done(replace(_), _, _, _, N) :- N >= 1.
macro_done(cycle(_, _, K), _, _, battle(_, _, Opp, _, _, _), _) :-
    boosts_of(Opp, Boosts),
    get_dict(atk, Boosts, Atk),
    Atk =< K, !.
macro_done(cycle(P, Q, _), _, _, battle(Active, Bench, _, _, _, _), _) :-
    % the partner is gone: nothing left to cycle with
    get_dict(name, Active, Out),
    get_dict(name, P, PN), get_dict(name, Q, QN),
    (Out == PN -> Wanted = QN ; Wanted = PN),
    \+ (member(B, Bench), get_dict(name, B, Wanted), current_hp(B, HP), HP > 0).

% ---- expansion --------------------------------------------------------------
% Run the policy turn by turn, AND over the AI's options, into a turn-level tree.
% Fails if any branch hits a refused mechanic or a switch that gets the leaver
% killed, exactly as step/6 does for the turn-level search.
expand(Macro, Target, Battle, Assume, Tree, End) :-
    retractall(expand_memo(_, _, _)),
    expand_tree(Macro, Target, Battle, Assume, 0, Tree),
    Tree \== done,                     % a macro that does nothing is not a plan step
    plan_walk(Battle, Tree, Assume, End).

expand_tree(Macro, Target, Battle, Assume, N, Tree) :-
    Battle = battle(Active, _, Opp, _, OppTurn, _),
    (   macro_done(Macro, Assume, Target, Battle, N)
    ->  Tree = done
    ;   macro_turn_cap(Cap), N < Cap,
        macro_policy(Macro, Battle, Action),
        ai_options(Active, Opp, OppTurn, Assume, Options),
        Options = [_|_],
        spend_node,
        N1 is N + 1,
        expand_branches(Options, Macro, Target, Battle, Assume, Action, N1, Branches),
        Tree = node(Action, Branches)
    ).

expand_branches([], _, _, _, _, _, _, []).
expand_branches([Option|Rest], Macro, Target, Battle, Assume, Action, N, [Option-Sub|Subs]) :-
    step(Battle, Assume, Action, Option, Stepped),
    expand_sub(Macro, Target, Stepped, Assume, N, Sub),
    expand_branches(Rest, Macro, Target, Battle, Assume, Action, N, Subs).

% two AI options that land in the same exact position get the same subtree; a
% cycle whose every turn has two AI options would otherwise be 2^12 leaves
expand_sub(Macro, Target, Battle, Assume, N, Sub) :-
    exact_key(Battle, Key),
    term_hash(Key-N, Hash),
    (   expand_memo(Hash, Key-N, Found)
    ->  Sub = Found
    ;   expand_tree(Macro, Target, Battle, Assume, N, Sub),
        assertz(expand_memo(Hash, Key-N, Sub))
    ).

exact_key(battle(Active, Bench, Opponent, Rest, OppTurn, _), k(KA, KB, KO, KR, OppTurn)) :-
    exact_mon_key(Active, KA),
    maplist(exact_mon_key, Bench, KB0), msort(KB0, KB),
    exact_mon_key(Opponent, KO),
    maplist(exact_mon_key, Rest, KR).

exact_mon_key(Pokemon, m(Name, HP, Boosts, Item, Status, Crit)) :-
    get_dict(name, Pokemon, Name),
    current_hp(Pokemon, HP),
    boosts_of(Pokemon, B),
    dict_pairs(B, _, Boosts),
    (get_dict(item, Pokemon, Item) -> true ; Item = none),
    (get_dict(status, Pokemon, Status) -> true ; Status = none),
    crit_stage(Pokemon, Crit).

% ---- report -----------------------------------------------------------------
print_planned_fight(Box, OppTeam, Assume) :-
    (   plan_fight(Box, OppTeam, Assume, fight(Battle, Segments))
    ->  Battle = battle(Lead, _, Opp, _, _, _),
        get_dict(name, Lead, LeadName),
        get_dict(name, Opp, OppName),
        death_budget(Deaths),
        nodes_used(Used),
        format("line found, costs ~w (~w turn-steps)~n~n", [Deaths, Used]),
        print_macro_plan(Segments),
        nl,
        format("  >> they send out ~w~n  << we send out ~w~n", [OppName, LeadName]),
        narrate_segments(Battle, Segments, Assume, 1)
    ;   search_outcome(Outcome),
        nodes_used(Used),
        format("no line: ~w (~w turn-steps)~n", [Outcome, Used])
    ).

print_macro_plan(Segments) :-
    format("  plan:~n"),
    forall(member(segment(macro(Target, What), _), Segments),
           format("    vs ~w: ~w~n", [Target, What])),
    aggregate_all(count, member(segment(macro(_, setup("Focus Energy")), _), Segments), FE),
    (   FE >= 2
    ->  format("  !! assumes a second Focus Energy stacks; vanilla refuses it and Run & Bun's docs do not say~n")
    ;   true
    ).

% The relaxation ladder over the planner: safest assumption that has a line.
% Death budget is the inner loop of plan_fight/4, so a level's answer is already
% the cheapest line at that level.
print_planned_ladder(Box, OppTeam) :-
    (   relaxation(Level, Name, Assume),
        format("~n=== level ~w: ~w ===~n", [Level, Name]),
        plan_fight(Box, OppTeam, Assume, Fight)
    ->  Fight = fight(Battle, Segments),
        Battle = battle(Lead, _, Opp, _, _, _),
        death_budget(Deaths), nodes_used(Used),
        format("line found at level ~w (~w), costs ~w (~w turn-steps)~n~n", [Level, Name, Deaths, Used]),
        print_macro_plan(Segments), nl,
        format("  >> they send out ~w~n  << we send out ~w~n", [Opp.name, Lead.name]),
        narrate_segments(Battle, Segments, Assume, 1)
    ;   format("no line at any level~n")
    ).

% ---------------------------------------------------------------------------
% Beam planner
% ---------------------------------------------------------------------------
% The exhaustive planner grows about sevenfold per two macros, and a six-Pokemon
% fight is fifteen macros. It cannot get there. This variant gives up
% completeness for reach: for each opposing Pokemon in turn it collects up to
% beam_candidates/1 ways of beating it within segment_macros/1 macros, keeps the
% beam_width/1 best by how much party is left, commits, and moves on. It
% backtracks only within the beam. A "no line" from this is not a proof of
% anything; a line from it is verified turn by turn like any other.
:- dynamic beam_width/1, beam_candidates/1.
beam_width(2).
beam_candidates(20).
segment_macros(4).

plan_fight_beam(Box, OppTeam, Assume, fight(Battle, Segments)) :-
    reset_search,
    initial_battle(Box, OppTeam, Assume, Battle),
    beam_segments(Battle, Assume, Segments).

beam_segments(Battle, _, []) :-
    Battle = battle(_, _, Opp, Rest, _, _),
    opponents_down(Opp, Rest), !.
beam_segments(Battle, Assume, Segments) :-
    Battle = battle(_, _, Opponent, _, _, _),
    get_dict(name, Opponent, Target),
    segment_macros(K),
    beam_candidates(N),
    % scores negated so keysort puts the best first and, being stable, keeps
    % the simpler macro ahead of the fancier one on a tie
    findall(Neg-(Here-After),
        limit(N, (beat_one(Battle, Assume, Target, K, 0, Here, After), end_score(After, Score), Neg is -Score)),
        Found),
    Found = [_|_],
    keysort(Found, Best),
    beam_width(W),
    take(W, Best, Kept),
    length(Found, NF),
    findall(Sc-Ws, (member(Sc-(Segs-_), Kept), findall(W, member(segment(macro(_, W), _), Segs), Ws)), Summary),
    planner_note("beam: ~w ways past ~w, keeping ~w", [NF, Target, Summary]),
    member(_-(Here-After), Kept),
    beam_segments(After, Assume, More),
    append(Here, More, Segments).

% one opposing Pokemon, from here, in at most Depth macros
beat_one(Battle, _, Target, _, _, [], Battle) :-
    Battle = battle(_, _, Opp, _, _, _),
    get_dict(name, Opp, Name),
    Name \== Target, !.
beat_one(Battle, _, _, _, _, [], Battle) :-
    Battle = battle(_, _, Opp, Rest, _, _),
    opponents_down(Opp, Rest), !.
beat_one(Battle, Assume, Target, Depth, Pivots, [segment(macro(Target, What), Tree)|More], After) :-
    Depth > 0,
    within_death_budget(Battle),
    macro_action(Battle, Assume, Macro),
    (   (Macro = pivot(_) ; Macro = uturn(_, _))
    ->  Pivots < 2, Pivots1 is Pivots + 1
    ;   Pivots1 = 0
    ),
    macro_desc(Macro, What),
    expand(Macro, Target, Battle, Assume, Tree, End),
    Next is Depth - 1,
    beat_one(End, Assume, Target, Next, Pivots1, More, After).

% what a way of beating one opponent leaves us: HP across the party, a death
% costing far more than any amount of damage, and every party slot opened
% costing a little, since slots are what runs out at the end of a long fight
end_score(After, Score) :-
    party_health(After, Health),
    actual_losses(After, Lost),
    After = battle(_, _, _, _, _, Brought),
    length(Brought, Slots),
    Score is Health - 1000 * Lost - 20 * Slots.

print_beam_fight(Box, OppTeam, Assume, Deaths) :-
    retractall(death_budget(_)), assertz(death_budget(Deaths)),
    retractall(party_size(_)), assertz(party_size(6)),
    (   plan_fight_beam(Box, OppTeam, Assume, fight(Battle, Segments))
    ->  verify_deaths(fight(Battle, Segments), Assume),
        Battle = battle(Lead, _, Opp, _, _, _),
        plan_final_state(Battle, Segments, Assume, Final),
        actual_losses(Final, Lost),
        nodes_used(Used),
        format("beam line found, loses ~w (~w turn-steps)~n~n", [Lost, Used]),
        print_macro_plan(Segments), nl,
        format("  >> they send out ~w~n  << we send out ~w~n", [Opp.name, Lead.name]),
        narrate_segments(Battle, Segments, Assume, 1)
    ;   nodes_used(Used),
        format("beam: no line (~w turn-steps) -- not a proof, the beam is narrow~n", [Used])
    ).
