:- use_module(library(http/json)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_client)).
:- use_module(library(dcg/basics)).
:- use_module(library(pio)).
:- use_module(library(tabling)).

:- dynamic pok/4.
:- table calculate/4.

run :-
    assertTrainerPokemon,
    assertExportedPokemon,
    %Opponent = 'Youngster Calvin',
    %Opponent = 'Bug Catcher Rick',
    %Opponent = 'Youngster Allen',
    %Opponent = 'Team Aqua Grunt Petalburg Woods',
    %Opponent = 'Camper Gavi',
    Opponent = 'Battle Girl Jocelyn',
    findall(P-T, pok(_, Opponent, P, T), OppTeam),
    writeln(OppTeam),
    findall(P-T, pok(-1, you, P, T), Box),
    writeln(Box),
    forall(member(OppName-Opp, OppTeam), (
        forall(member(PokName-Pok, Box), (
            dict_to_json(Pok, AtkOptions),
            list_to_json(Opp, DefOptions),
            last(Pok.moves, Move),
            calculate(PokName, AtkOptions, OppName, DefOptions, Move, Data),
            PokSpeed = Data.attacker.rawStats.spe,
            OppSpeed = Data.defender.rawStats.spe,
            damageRolls_dict(PokName-Pok, OppName-Opp, PokDamage),
            damageRolls_list(OppName-Opp, PokName-Pok, OppDamage),
            format('~w (spe: ~d) VS ~w (spe: ~d)\n', [PokName,PokSpeed,OppName,OppSpeed]),
            format('~w\n', [PokDamage]),
            format('~w\n', [OppDamage])
        ))
    )).

damageRolls_dict(Name-Pokemon, OppName-OppPokemon, Data) :-
    dict_to_json(Pokemon, AtkOptions),
    list_to_json(OppPokemon, DefOptions),
    maplist(damageRoll(Name-AtkOptions, OppName-DefOptions), Pokemon.moves, Data).
    
damageRolls_list(Name-Pokemon, OppName-OppPokemon, Data) :-
    list_to_json(Pokemon, AtkOptions),
    dict_to_json(OppPokemon, DefOptions),
    maplist(damageRoll(Name-AtkOptions, OppName-DefOptions), Pokemon.moves, Data).

damageRoll(Name-AtkOptions, OppName-DefOptions, Move, Range) :-
    calculate(Name, AtkOptions, OppName, DefOptions, Move, Data),
    %writeln(Data.damage),
    % nature is included in rawStats
    DefenderMaxHP = Data.defender.rawStats.hp,
    (Data.damage = [Min|_] ->
        MinPercentage is Min / DefenderMaxHP * 100,
        last(Data.damage, Max),
        MaxPercentage is Max / DefenderMaxHP * 100,
        Range = Move-[MinPercentage, MaxPercentage]
        %format('~1f - ~1f\n', [MinPercentage,MaxPercentage])
    ; 
        Percentage is Data.damage / DefenderMaxHP * 100,
        Range = Move-[Percentage, Percentage]
    ).

assertTrainerPokemon :-
    open("gen8.json", read, Stream),
    json_read(Stream, json(JSON)),
    forall(member(Pokemon=json(Trainers), JSON),
        forall(member(Trainer=json(T), Trainers),
            (
                member(index=I, T),
                assertz(pok(I, Trainer, Pokemon, T))
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

calculate(Attacker, AttackerOptions, Defender, DefenderOptions, Move, Out) :-
    Data = json([
        gen=8,
        attackingPokemon=Attacker,
        attackingPokemonOptions=AttackerOptions,
        defendingPokemon=Defender,
        defendingPokemonOptions=DefenderOptions,
        moveName=Move
    ]),
    http_get('http://localhost:3000/calculate', Out, [method(get), post(json(Data)), json_object(dict)]).

dict_to_json(Dict, Out) :-
    [HP, Atk, Def, SpA, SpD, Spe] = Dict.ivs,
    Out = json([level=Dict.level, nature=Dict.nature, ivs=json([hp=HP, atk=Atk, def=Def, spa=SpA, spd=SpD, spe=Spe])]).

list_to_json(List, Out) :-
    member(level=L, List),
    member(nature=N, List),
    Out = json([level=L, nature=N, ivs=json([hp=31,atk=31,def=31,spa=31,spd=31,spe=31])]).

parse_export([P|T]) -->
    parse_export_pokemon(P),
    "\n\n",
    parse_export(T).
parse_export([P]) --> parse_export_pokemon(P), blanks, eos.

parse_export_pokemon(#{name:N, item:I, ability:A, level:L, nature:Nat, ivs:[HP, Atk, Def, SpA, SpD, Spe], moves:Moves}) -->
    parse_name_item(N, I),
    "Ability: ", string_without("\n", Ability), "\n",
    "Level: ", integer(L), "\n",
    nonblanks(Nature), " Nature\n",
    "IVs: ", integer(HP), " HP / ", integer(Atk), " Atk / ", integer(Def), " Def / ", integer(SpA), " SpA / ", integer(SpD), " SpD / ", integer(Spe), " Spe\n",
    parse_moves(Moves),
    {string_codes(A, Ability), string_codes(Nat, Nature)}.

parse_name_item(N, none) --> string_without("@\n", Name), {string_codes(N, Name)}, "\n".
parse_name_item(N, I) --> parse_name(N), parse_item(I).
parse_name(N) --> string(Name), " @ ", {string_codes(N, Name)}.
parse_item(I) --> string_without("\n", Item), {string_codes(I, Item)}, "\n".
parse_move(M) --> "- ", string_without("\n", Move), {string_codes(M, Move)}.
parse_moves([M]) --> parse_move(M).
parse_moves([M|T]) --> parse_move(M), "\n", parse_moves(T).