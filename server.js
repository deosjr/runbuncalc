const express = require("express");
const calc = require("./calc");
const {Dex} = require("@pkmn/dex");
const app = express();
app.listen(3000, () => {
	console.log("Server running on port 3000");
});

// parse application/json
app.use(express.json())

app.get("/calculate",(req, res, next) => {
	const gen = calc.Generations.get((typeof req.body.gen === 'undefined') ? 9 : req.body.gen);
	const crit = (typeof req.body.crit === 'undefined') ? false : req.body.crit === "true";
	let error = "";
	if(typeof req.body.attackingPokemon === 'undefined')
		error += "attackingPokemon must exist and have a valid pokemon name\n";
	if(typeof req.body.defendingPokemon === 'undefined')
		error += "defendingPokemon must exist and have a valid pokemon name\n";
	if(error)
		throw new Error(error)
	const result = calc.calculate(
		gen,
		new calc.Pokemon(gen, req.body.attackingPokemon, req.body.attackingPokemonOptions),
		new calc.Pokemon(gen, req.body.defendingPokemon, req.body.defendingPokemonOptions),
		// moveOverrides lets the caller bend the move data, which Pursuit needs:
		// it hits at double power when its target is switching out
		new calc.Move(gen, req.body.moveName, {
			isCrit: crit,
			overrides: (typeof req.body.moveOverrides === 'undefined') ? undefined : req.body.moveOverrides,
		}),
		new calc.Field((typeof req.body.field === 'undefined') ? undefined : req.body.field)
	);
	res.json(result);
})

// The damage calc does not carry accuracy, inflicted status or secondary effect
// chances, because it does not need them to compute damage. @pkmn/dex does, and
// the AI model needs all three: to know a status move is useless against a target
// that cannot take the status, and to know what can go wrong on a turn.
//
// But @pkmn/dex is VANILLA gen 8 and Run & Bun changes move definitions. The
// fork's own calc data is authoritative for type, category and power, so that is
// what we use -- Covet is Fairy here, not Normal, and type immunity must be
// computed from that. Accuracy and secondary chances have no home in the calc
// data at all, so they come from vanilla unless rnb-move-overrides.json says
// otherwise, and the response says which, so nothing silently trusts a guess.
const rnbMoves = require("./rnb-move-overrides.json");
const overrides = rnbMoves.moves || {};
// moves whose behaviour Run & Bun changed in ways no stat field expresses
const functional = rnbMoves.functional || {};

app.get("/move", (req, res) => {
	const genNum = (typeof req.body.gen === 'undefined') ? 8 : req.body.gen;
	const dex = Dex.forGen(genNum);
	const vanilla = dex.moves.get(req.body.moveName);
	// the fork's data, which carries the Run & Bun changes
	const rnb = calc.Generations.get(genNum).moves.get(
		req.body.moveName.toLowerCase().replace(/[^a-z0-9]/g, '')
	);
	if ((!vanilla || !vanilla.exists) && !rnb)
		throw new Error("unknown move: " + req.body.moveName);

	const type = rnb ? rnb.type : vanilla.type;
	const category = rnb ? rnb.category : vanilla.category;
	const basePower = rnb ? rnb.basePower : vanilla.basePower;
	// a move the hack has already altered is one whose accuracy and effect
	// chances are most likely altered too
	const modified = !!(vanilla && vanilla.exists && rnb) && (
		vanilla.type !== type || vanilla.category !== category || vanilla.basePower !== basePower
	);

	const override = overrides[req.body.moveName] || {};
	const accuracy = (typeof override.accuracy !== 'undefined')
		? override.accuracy
		: (vanilla && vanilla.accuracy === true ? 100 : (vanilla ? vanilla.accuracy : 100));
	const secondaries = override.secondaries || (vanilla ? vanilla.secondaries : null) || [];

	// damageTaken uses the PS encoding, where 3 means immune. A status move whose
	// type the target is immune to just fails: Thunder Wave does nothing to Golett.
	const defenderTypes = req.body.defenderTypes || [];
	const typeImmune = defenderTypes.some(t => {
		const defType = dex.types.get(t);
		return defType && defType.damageTaken[type] === 3;
	});

	res.json({
		name: req.body.moveName,
		category: category,
		basePower: basePower,
		accuracy: accuracy,
		alwaysHits: vanilla ? vanilla.accuracy === true : false,
		priority: vanilla ? vanilla.priority : 0,
		type: type,
		typeImmune: typeImmune,
		status: (vanilla && vanilla.status) || null,
		// boosts apply to the move's target, which for Work Up and friends is the
		// user itself -- so `target` decides who they land on
		boosts: (vanilla && vanilla.boosts) || null,
		target: (vanilla && vanilla.target) || null,
		// self is a top level field, not a secondary: Superpower's -1 atk/-1 def
		// and Overheat's -2 spa live here and always happen
		self: (vanilla && vanilla.self) || null,
		// [numerator, denominator] of damage dealt restored to the user (Giga Drain 1/2)
		drain: (vanilla && vanilla.drain) || null,
		// 2 for high-crit moves (Karate Chop, Slash); crit stages are a client concern
		critRatio: (vanilla && vanilla.critRatio) || 1,
		secondaries: secondaries.map(s => Object.assign({}, s, {
			chance: (typeof s.chance === 'undefined') ? 100 : s.chance,
			status: s.status || null,
			volatileStatus: s.volatileStatus || null,
		})),
		// the move changes sheet is complete, so a move it does not list is vanilla
		// and trustworthy. Only an unmodelled functional change makes the numbers
		// an incomplete picture.
		modified: modified || !!overrides[req.body.moveName],
		functionalChange: functional[req.body.moveName] || null,
		verified: !functional[req.body.moveName],
	});
})

app.use(express.static('dist'))
