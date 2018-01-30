type Player = PlayerOne | PlayerTwo

type 'a OrWin = Win of Player | Game of 'a

type MaybeWonTennisGameScore = TennisGameScore OrWin