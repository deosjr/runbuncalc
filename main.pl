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
            PokSpeed = Data.attacker.rawStats.spe,
            OppSpeed = Data.defender.rawStats.spe,
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

damageRolls(Attacker, Defender, Data) :-
    maplist(damageRoll(Attacker, Defender), Attacker.moves, Data).
    
damageRoll(Attacker, Defender, Move, Range) :-
    calculate(Attacker, Defender, Move, Data),
    %writeln(Data.damage),
    % nature is included in rawStats
    DefenderMaxHP = Data.defender.rawStats.hp,
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

assertTrainerPokemon :-
    open("gen8.json", read, Stream),
    % cant use json_read_dict because Vivillion has multiple 'Bug Maniac Jeffrey' keys...
    json_read(Stream, json(JSON)),
    forall(member(Pokemon=json(Trainers), JSON),
        forall(member(Trainer=json(T), Trainers),
            (
                member(index=I, T),
                atom_json_term(A, json(T), []),
                atom_json_dict(A, D, []),
                WithName = D.put(#{name:Pokemon, ivs:[31,31,31,31,31,31]}),
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

% AI sees speed ties as them being faster than the player
% TODO: this will impact fast_kill tests for Player Attackers on speed ties!
fast_kill_guaranteed(Attacker, Defender, MoveName) :-
    calculate(Attacker, Defender, MoveName, Data),
    Data.attacker.rawStats.spe > Data.defender.rawStats.spe,
    damageRoll(Attacker, Defender, MoveName, MoveName-[LowRollPercent|_]),
    LowRollPercent >= 100.

fast_kill_possible(Attacker, Defender, MoveName) :-
    calculate(Attacker, Defender, MoveName, Data),
    % Note the >= here vs > on _guaranteed
    Data.attacker.rawStats.spe >= Data.defender.rawStats.spe,
    damageRoll(Attacker, Defender, MoveName, MoveName-[_,HighRollPercent]),
    HighRollPercent >= 100.

slow_kill_possible(Attacker, Defender, MoveName) :-
    calculate(Attacker, Defender, MoveName, Data),
    Data.attacker.rawStats.spe < Data.defender.rawStats.spe,
    damageRoll(Attacker, Defender, MoveName, MoveName-[_,HighRollPercent]),
    HighRollPercent >= 100.

ai_is_faster(AI, Player) :-
    AI.moves = [Move|_],
    calculate(AI, Player, Move, Data),
    Data.attacker.rawStats.spe >= Data.defender.rawStats.spe.

ai_is_slower(AI, Player) :-
    AI.moves = [Move|_],
    calculate(AI, Player, Move, Data),
    Data.attacker.rawStats.spe < Data.defender.rawStats.spe.

dead_to_crit(Defender, Attacker, MovesThatCritKill) :-
    last(Attacker.moves, Move1),
    calculate(Attacker, Defender, Move1, Data),
    MaxHP = Data.defender.rawStats.hp,
    maplist(highRoll(Attacker, Defender, true), Attacker.moves, Damages),
    zip_unzip(Attacker.moves, Damages, Moves),
    include([_-N]>>(N >= MaxHP), Moves, MovesThatCritKill).

outdamages(Pokemon, Opponent) :-
    damageRolls(Pokemon, Opponent, PokRolls),
    damageRolls(Opponent, Opponent, OppRolls),
    predsort(moveRangeByHighest, PokRolls, [_-[_,PokHigh]]),
    predsort(moveRangeByHighest, OppRolls, [_-[_,OppHigh]]),
    PokHigh > OppHigh.

moveRangeByHighest(>, _-[_,H1], _-[_,H2]) :- H1 > H2.
moveRangeByHighest(<, _-[_,H1], _-[_,H2]) :- H1 < H2.
moveRangeByHighest(=, _-[_,H1], _-[_,H2]) :- H1 = H2.

post_ko_switch_in(Player, OppTeam, Switchins) :-
    maplist(switchin_pair(Player), OppTeam, Scores),
    keysort(Scores, RevCandidates),
    reverse(RevCandidates, Candidates),
    Candidates = [_-Highest|_],
    include([_-S]>>(S==Highest), Candidates, SwitchinsWithScore),
    maplist([P-S,X]>>(X=P), SwitchinsWithScore, Switchins).

switchin_pair(Player, Opponent, Opponent-Score) :-
    switchin_score(Opponent, Player, Score).

% score of Pokemon switching in when Opponent is already out
switchin_score(Pokemon, Opponent, 5) :-
    include(fast_kill_possible(Pokemon, Opponent), Pokemon.moves, [_|_]).
switchin_score(Pokemon, Opponent, 4) :-
    include(fast_kill_possible(Opponent, Pokemon), Opponent.moves, []),
    include(slow_kill_possible(Pokemon, Opponent), Pokemon.moves, [_|_]).
switchin_score(Pokemon, Opponent, 3) :-
    ai_is_faster(Pokemon, Opponent),
    outdamages(Pokemon, Opponent).
switchin_score(Pokemon, Opponent, 2) :-
    ai_is_slower(Pokemon, Opponent),
    outdamages(Pokemon, Opponent).
switchin_score(Pokemon, Opponent, 1) :-
    ai_is_faster(Pokemon, Opponent).
switchin_score(Pokemon, Opponent, 0) :-
    ai_is_slower(Pokemon, Opponent),
    include(fast_kill_possible(Opponent, Pokemon), Opponent.moves, []).
switchin_score(Pokemon, Opponent, -1) :-
    ai_is_slower(Pokemon, Opponent),
    include(fast_kill_possible(Opponent, Pokemon), Opponent.moves, [_|_]).

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
    (I==none ->
        Pokemon = #{name:N, ability:A, level:L, nature:Nat, ivs:[HP, Atk, Def, SpA, SpD, Spe], moves:Moves};
        Pokemon = #{name:N, item:I, ability:A, level:L, nature:Nat, ivs:[HP, Atk, Def, SpA, SpD, Spe], moves:Moves})}.

parse_name_item(N, none) --> string_without("@\n", Name), {string_codes(N, Name)}, "\n".
parse_name_item(N, I) --> parse_name(N), parse_item(I).
parse_name(N) --> string(Name), " @ ", {string_codes(N, Name)}.
parse_item(I) --> string_without("\n", Item), {string_codes(I, Item)}, "\n".
parse_move(M) --> "- ", string_without("\n", Move), {string_codes(M, Move)}.
parse_moves([M]) --> parse_move(M).
parse_moves([M|T]) --> parse_move(M), "\n", parse_moves(T).

zip_unzip(Names,Values,Zipped) :- maplist([N,V,N-V]>>true,Names,Values,Zipped).