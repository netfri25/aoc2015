app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.12.0/Lb8EgiejTUzbggO2HVVuPJFkwvvsfW6LojkLR20kTVE.tar.br" }

import pf.Stdout
import pf.Task
import pf.File


raceTime : U32
raceTime = 2503

main =
    content = File.readUtf8! "./input.txt"
    input = content |> Str.split "\n" |> List.keepOks parseLine
    Stdout.line! (Inspect.toStr input)
    part1Sol = part1 input
    Stdout.line! (Str.concat "part 1: " (Num.toStr part1Sol))
    part2Sol = part2 input
    Stdout.line! (Str.concat "part 2: " (Num.toStr part2Sol))

Reindeer : {
    name : Str,
    speed : U32,
    stamina : U32,
    rest : U32
}

part1 : List Reindeer -> U32
part1 = \reindeers -> List.map reindeers (\reindeer -> distance reindeer raceTime) |> List.max |> Result.withDefault 0

part2 : List Reindeer -> U32
part2 = \reindeers ->
    reindeers
        |> List.map (\reindeer -> { original : reindeer, state : Flying reindeer.stamina, pos : 0, score : 0 })
        |> simulate raceTime

parseLine : Str -> Result Reindeer [InvalidInput Str]
parseLine = \line ->
    words = line |> Str.split " "
    when words is
        [name, speed, stamina, rest] -> Ok {
            name : name,
            speed : Result.withDefault (Str.toU32 speed) 0,
            stamina : Result.withDefault (Str.toU32 stamina) 0,
            rest : Result.withDefault (Str.toU32 rest) 0
        }
        _ -> Err (InvalidInput line)

distance : Reindeer, U32 -> U32
distance = \{ speed, stamina, rest }, time ->
    roundTime = stamina + rest
    totalRounds = time // roundTime
    leftoverTime = time % roundTime
    leftoverRoundTime = Num.min leftoverTime stamina
    leftoverRoundTime * speed + totalRounds * speed * stamina

State : [Cooldown U32, Flying U32]

SimulatedReindeer : {
    original : Reindeer,
    state : State,
    pos : U32,
    score : U32
}

# returns the score of the winner
simulate : List SimulatedReindeer, U32 -> U32
simulate = \reindeers, time ->
    if time == 0 then
        reindeers
            |> List.map (\reindeer -> reindeer.score)
            |> List.max
            |> Result.withDefault 12
    else
        stepped = List.map reindeers step
        firstPos = stepped
            |> List.map (\x -> x.pos)
            |> List.max
            |> Result.withDefault 0
        stepped
            |> List.map (\x -> if x.pos == firstPos then { x & score : x.score + 1 } else x)
            |> simulate (time - 1)

step : SimulatedReindeer -> SimulatedReindeer
step = \reindeer ->
    when reindeer.state is
        Cooldown time ->
            if time == 1 then
                { reindeer & state : Flying reindeer.original.stamina }
            else
                { reindeer & state : Cooldown (time - 1) }
        Flying time ->
            if time == 1 then
                { reindeer & state : Cooldown reindeer.original.rest, pos : reindeer.pos + reindeer.original.speed }
            else
                { reindeer & state : Flying (time - 1), pos : reindeer.pos + reindeer.original.speed }
