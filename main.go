package main

import (
	"fmt"
	"io"
	"net/http"
	"strings"
)

func main() {
	s := `{"gen":8, "attackingPokemon": "Bulbasaur", "defendingPokemon":"Charmander", "moveName":"Absorb",
	"attackingPokemonOptions": {"ivs":{"spa":0}}, "crit":"true"}`
	req, err := http.NewRequest("GET", "http://localhost:3000/calculate", strings.NewReader(s))
	req.Header.Set("Content-Type", "application/json")
	out, err := http.DefaultClient.Do(req)
	b, err := io.ReadAll(out.Body)
	fmt.Println(string(b), err)
}
