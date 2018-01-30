type Player = PlayerOne | PlayerTwo

type 'a OrWin = Win of Player | Game of 'a

type PlayerPoints = Love | Fifteen | Thirty | Forty
type TennisGameScore = Points of PlayerPoints * PlayerPoints | Advantage of Player | Deuce

type MaybeWonTennisGameScore = TennisGameScore OrWin