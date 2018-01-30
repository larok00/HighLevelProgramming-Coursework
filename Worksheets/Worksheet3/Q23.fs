/// The two players
type Player = PlayerOne | PlayerTwo

/// The point score in for a player in a game
type PlayerPoints = Zero | Fifteen | Thirty | Forty 

/// The score of a game
type TennisGameScore = 
    | Points of PlayerPoints * PlayerPoints 
    | Advantage of Player 
    | Deuce 

type 'a OrWin = Win of Player | Game of 'a

type MaybeWonTennisGameScore = TennisGameScore OrWin
/// Compute the next score in a game 
let nextPointScore a = 
    match a with 
    | Zero -> Fifteen
    | Fifteen -> Thirty
    | Thirty -> Forty
    | Forty -> failwith "should not happen!"

/// Check if we've reached deuce
let normalize score = 
    match score with 
    | Points(Forty,Forty) -> Game Deuce
    | _ -> Game score

/// Score a point in a game
let scorePoint score point =
    match score, point with 
    | Advantage player1, player2 when  player1 = player2 -> Win player1
    | Advantage _, _ -> Deuce |> Game
    | Deuce, player -> Advantage player |> Game
    | Points(Forty, _), PlayerOne -> Win PlayerOne
    | Points(_, Forty), PlayerTwo -> Win PlayerTwo
    | Points(a, b), PlayerOne -> normalize (Points (nextPointScore a, b))
    | Points(a, b), PlayerTwo -> normalize (Points (a, nextPointScore b))

/// Score a whole game
/// points after the game is won are ignored
let scoreGame (points: Player List) = 
    let scoreGamePoint score point =
        match score with
        | Win p, results -> Win p, results
        | Game score, results -> 
            let nextScore = scorePoint score point
            nextScore, (nextScore :: results)
    let startOfGame = (Zero,Zero) |> Points |> Game
    List.fold scoreGamePoint (startOfGame,[]) points

/// Generate a set of points from a string of 1s and 2s
let genPoints x = 
    x |> Seq.toList 
    |> List.collect (function 
        | '1' -> [PlayerOne] 
        | '2' -> [PlayerTwo] 
        | _ -> []
        )
