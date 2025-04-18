:- use_module(library(http/json)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_client)).
:- use_module(library(dcg/basics)).
:- use_module(library(pio)).
:- use_module(library(tabling)).

:- dynamic pok/4.
:- table calculate/4.

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
    member(Pokemon, List),
    Pokemon.name = Name.

damageRolls(Attacker, Defender, Data) :-
    maplist(damageRoll(Attacker, Defender), Attacker.moves, Data).
    
damageRoll(Attacker, Defender, Move, Range) :-
    calculate(Attacker, Defender, Move, Data),
    %writeln(Data.damage),
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

% backtracks over all highest damage moves on a tie
highest_damage_move(Attacker, Defender, Move) :-
    damageRolls(Attacker, Defender, Data),
    member(Move-[_,High], Data),
    include({High}/[_-[Low,_]]>>(Low > High), Data, []).

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
    LowRollPercent >= 100.

fast_kill_possible(Attacker, Defender, MoveName) :-
    calculate(Attacker, Defender, MoveName, Data),
    % Note the >= here vs > on _guaranteed
    Data.attacker.stats.spe >= Data.defender.stats.spe,
    damageRoll(Attacker, Defender, MoveName, MoveName-[_,HighRollPercent]),
    HighRollPercent >= 100.

slow_kill_possible(Attacker, Defender, MoveName) :-
    calculate(Attacker, Defender, MoveName, Data),
    Data.attacker.stats.spe < Data.defender.stats.spe,
    damageRoll(Attacker, Defender, MoveName, MoveName-[_,HighRollPercent]),
    HighRollPercent >= 100.

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
nuzlocke_switchin_score(Pokemon, Opponent, 3) :-
    ai_is_faster(Pokemon, Opponent),
    outdamages(Pokemon, Opponent), !.
nuzlocke_switchin_score(Pokemon, Opponent, 2) :-
    ai_is_slower(Pokemon, Opponent),
    outdamages(Pokemon, Opponent), !.
nuzlocke_switchin_score(Pokemon, Opponent, 1) :-
    ai_is_faster(Pokemon, Opponent), !.
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

assertTrainerPokemon :-
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
                WithName = D.put(#{name:Pokemon, ivs:_{atk:31,def:31,hp:31,spa:31,spd:31,spe:31}}),
                assertz(pok(I, Trainer, Pokemon, WithName))
            )
        )
    ).

assertExportedPokemon :-
    phrase_from_file(parse_export(Pokemon), "export.txt"),
    retractall(pok(-1, 'You', _, _)),
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