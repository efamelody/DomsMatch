{-
   DomsMatch: code to play a dominoes match between two players.

   The top level function is domsMatch - it takes five arguments:
       games - the number of games to play
       target - the target score to reach
       player1, player2 - two DomsPlayer functions, representing the two players
       seed - an integer to seed the random number generator
   The function returns a pair showing how many games were won by each player.

   The functions of type DomsPlayer must take four arguments:
       The current Hand
       The current Board
       The Player (which will be one of P1 or P2)
       The current Scores
   The function returns a tuple containing the Domino to play and End to play it on.

   Stub with types provided by Emma Norling (October 2023).

   Author: Nur Izfarwiza Binti Mohd Talib
   Date: 20 November 2023
   Description:
   Brief Summary of The File Contents:
   1. Scoring Functions:
      scoreBoard: Computes the score for a given board and a boolean indicating if it's the last domino.
      isDouble: Checks if a given domino is a double.
      calculateScore: Assigns scores according to specific conditions.
      Game Logic Functions:
  2. Game Logic Functio
      blocked: Checks if there is nothing to play in the hand.
      canPlay: Checks if a domino can be played on the board.
      playDom: Plays a domino at a specified end if possible.

  3. Player Strategies:
      simplePlayer: Chooses the first valid move from the hand that can be played on the board.
      smartPlayer: Determines the next move based on the current state, considering a special case.
      regularMoveStrategy: Filters possible moves and chooses the best move based on scoring strategy.

  4. Scoring Strategy Functions:
      findBestMove: Recursively searches through possible moves to find the move resulting in the highest score.
      listDomino: Generates a list of possible domino moves.
      playDominoAtBestEnd: Determines the end of the board to play the domino based on scoring strategy.
      findHighestScoreDomino: Recursively searches through a list of dominoes to find the highest-scoring domino


  5. Helper Functions:
      updatePipCountLeft, updatePipCountRight: Updates the pip count for the left/right side of the board.
      isMultThreeFive: Checks if a move is a multiple of 3 or 5.
      calculateTotalPip: Calculates the new score for the player after playing a given move

  6. Utility Functions:
      swapDomino: Swaps the elements of a tuple, flipping a domino to be played.

 -}
module DomsMatch where

    import Data.List
    import Data.Ord (comparing)
    import System.Random
    import Debug.Trace


    -- types used in this module
    type Domino = (Int, Int) -- a single domino
    {- Board data type: either an empty board (InitState) or the current state as represented by
        * the left-most domino (such that in the tuple (x,y), x represents the left-most pips)
        * the right-most domino (such that in the tuple (x,y), y represents the right-most pips)
        * the history of moves in the round so far
     -}
    data Board = InitState | State Domino Domino History deriving (Eq, Show)
    {- History should contain the *full* list of dominos played so far, from leftmost to
       rightmost, together with which player played that move and when they played it
     -}
    type History = [(Domino, Player, MoveNum)]
    data Player = P1 | P2 deriving (Eq, Show)
    data End = L | R deriving (Eq, Show)
    type Scores = (Int, Int) -- P1’s score, P2’s score
    type MoveNum = Int
    type Hand = [Domino]
    {- DomsPlayer is a function that given a Hand, Board, Player and Scores will decide
       which domino to play where. The Player information can be used to "remember" which
       moves in the History of the Board were played by self and which by opponent
     -}
    type DomsPlayer = Hand -> Board -> Player -> Scores -> (Domino, End)

    {- domSet: a full set of dominoes, unshuffled -}
    domSet = [ (l,r) | l <- [0..6], r <- [0..l] ]

    {- shuffleDoms: returns a shuffled set of dominoes, given a number generator
       It works by generating a random list of numbers, zipping this list together
       with the ordered set of dominos, sorting the resulting pairs based on the random
       numbers that were generated, then outputting the dominos from the resulting list.
     -}
    shuffleDoms :: StdGen -> [Domino]
    shuffleDoms gen = [ d | (r,d) <- sort (zip (randoms gen :: [Int]) domSet)]

    {- domsMatch: play a match of n games between two players,
        given a seed for the random number generator
       input: number of games to play, number of dominos in hand at start of each game,
              target score for each game, functions to determine the next move for each
              of the players, seed for random number generator
       output: a pair of integers, indicating the number of games won by each player
     -}
    domsMatch :: Int -> Int -> Int -> DomsPlayer -> DomsPlayer -> Int -> (Int, Int)
    domsMatch games handSize target p1 p2 seed
        = domsGames games p1 p2 (mkStdGen seed) (0, 0)
          where
          domsGames 0 _  _  _   wins               = wins
          domsGames n p1 p2 gen (p1_wins, p2_wins)
            = domsGames (n-1) p1 p2 gen2 updatedScore
              where
              updatedScore
                | playGame handSize target p1 p2 (if odd n then P1 else P2) gen1 == P1 = (p1_wins+1,p2_wins)
                | otherwise                                            = (p1_wins, p2_wins+1)
              (gen1, gen2) = split gen
              {- Note: the line above is how you split a single generator to get two generators.
                 Each generator will produce a different set of pseudo-random numbers, but a given
                 seed will always produce the same sets of random numbers.
               -}

    {- playGame: play a single game (where winner is determined by a player reaching
          target exactly) between two players
       input: functions to determine the next move for each of the players, player to have
              first go, random number generator 
       output: the winning player
     -}
    playGame :: Int -> Int -> DomsPlayer -> DomsPlayer -> Player -> StdGen -> Player
    playGame handSize target p1 p2 firstPlayer gen
        = playGame' p1 p2 firstPlayer gen (0, 0)
          where
          playGame' p1 p2 firstPlayer gen (s1, s2)
            | s1 == target = P1
            | s2 == target = P2
            | otherwise
                = let
                      newScores = playDomsRound handSize target p1 p2 firstPlayer currentG (s1, s2)
                      (currentG, nextG) = split gen
                  in
                  playGame' p1 p2 (if firstPlayer == P1 then P2 else P1) nextG newScores

    {- playDomsRound: given the starting hand size, two dominos players, the player to go first,
        the score at the start of the round, and the random number generator, returns the score at
        the end of the round.
        To complete a round, turns are played until either one player reaches the target or both
        players are blocked.
     -}
    playDomsRound :: Int -> Int -> DomsPlayer -> DomsPlayer -> Player -> StdGen -> (Int, Int) -> (Int, Int)
    playDomsRound handSize target p1 p2 first gen scores
        = playDomsRound' p1 p2 first (hand1, hand2, InitState, scores)
          where
          -- shuffle the dominoes and generate the initial hands
          shuffled = shuffleDoms gen
          hand1 = take handSize shuffled
          hand2 = take handSize (drop handSize shuffled)
          {- playDomsRound' recursively alternates between each player, keeping track of the game state
             (each player's hand, the board, the scores) until both players are blocked -}
          playDomsRound' p1 p2 turn gameState@(hand1, hand2, board, (score1,score2))
            | (score1 == target) || (score2 == target) || (p1_blocked && p2_blocked) = (score1,score2)
            | turn == P1 && p1_blocked = playDomsRound' p1 p2 P2 gameState
            | turn == P2 && p2_blocked = playDomsRound' p1 p2 P1 gameState
            | turn == P1               = playDomsRound' p1 p2 P2 newGameState
            | otherwise                = playDomsRound' p1 p2 P1 newGameState
              where
              p1_blocked = blocked hand1 board
              p2_blocked = blocked hand2 board
              (domino, end)          -- get next move from appropriate player
                  | turn == P1 = p1 hand1 board turn (score1, score2)
                  | turn == P2 = p2 hand2 board turn (score1, score2)
                                     -- attempt to play this move
              maybeBoard             -- try to play domino at end as returned by the player
                  | turn == P1 && not (domInHand domino hand1) = Nothing -- can't play a domino you don't have!
                  | turn == P2 && not (domInHand domino hand2) = Nothing
                  | otherwise = playDom turn domino board end
              newGameState           -- if successful update board state (exit with error otherwise)
                 | maybeBoard == Nothing = error ("Player " ++ show turn ++ " attempted to play an invalid move.")
                 | otherwise             = (newHand1, newHand2, newBoard,
                                              (limitScore score1 newScore1, limitScore score2 newScore2))
              (newHand1, newHand2)   -- remove the domino that was just played
                 | turn == P1 = (hand1\\[domino], hand2)
                 | turn == P2 = (hand1, hand2\\[domino])
              score = scoreBoard newBoard (newHand1 == [] || newHand2 == [])
              (newScore1, newScore2) -- work out updated scores
                 | turn == P1 = (score1+score,score2)
                 | otherwise  = (score1,score2+score)
              limitScore old new     -- make sure new score doesn't exceed target
                 | new > target = old
                 | otherwise    = new
              Just newBoard = maybeBoard -- extract the new board from the Maybe type

    {- domInHand: check if a particular domino is contained within a hand -}
    domInHand :: Domino -> Hand -> Bool
    domInHand (l,r) hand = [ 1 | (dl, dr) <- hand, (dl == l && dr == r) || (dr == l && dl == r) ] /= []

    {-
      scoreBoard: Calculates the score of the current board state.
      Takes a Board, a flag indicating if it's the last domino, and returns an Int representing the score.
      Input:
        - Board: Current state of the game board.
        - Bool: indicating if it's the last domino.
      Output:
        - Int: The calculated score.
    -}
    scoreBoard :: Board -> Bool -> Int
    scoreBoard InitState _ = 0 -- Initial state has a score of 0
    scoreBoard (State dom1 dom2 _) isLastDomino
      | isDouble dom2 && isDouble dom1 && isLastDomino = calculateScore (uncurry (+) dom1 + fst dom2 + snd dom2) + 1
      | isDouble dom2 && isDouble dom1 = calculateScore (uncurry (+) dom1 + fst dom2 + snd dom2) -- if both side doubles
      | isDouble dom2 && isLastDomino = calculateScore (uncurry (+) dom2 + fst dom1) + 1 -- Right is double and last
      | isDouble dom2 = calculateScore (fst dom1 + snd dom2)
      | isDouble dom1 && isLastDomino = calculateScore (uncurry (+) dom1 + snd dom2) + 1 -- Left domino is double and last
      | isDouble dom1 = calculateScore (fst dom1 + snd dom2 + snd dom1)
      | isLastDomino = calculateScore (fst dom1 + snd dom2) + 1
      | otherwise = calculateScore (fst dom1 + snd dom2)

    {- check if the remaining hand contains exactly one domino-}
    isLastDomino :: Hand -> Bool
    isLastDomino [] = True
    isLastDomino hands = length hands == 1

    {-Check if the domino just played is a double-}
    isDouble :: Domino -> Bool
    isDouble (x, y) = x == y

    {-Assigns scores according to the conditions-}
    calculateScore :: Int -> Int
    calculateScore n
      | n == 3 = 1
      | n == 5 = 1
      | n == 6 = 2
      | n == 9 = 3
      | n == 10 = 2
      | n == 12 = 4
      | n == 15 = 8
      | n == 18 = 6
      | n == 20 = 4
      | otherwise = 0

    {-
      blocked: Checks if there are no valid moves left in the hand.
      Takes a Hand, a Board, uses canPlay function and returns a Bool.
      Input:
        - Hand: The current hand.
        - Board: The current state of the game board.
      Output:
        - Bool: True if there are no valid moves left, otherwise False.
    -}
    blocked :: Hand -> Board -> Bool
    blocked [] _ = True
    blocked hand board = not (any (\domino -> canPlay domino board L || canPlay domino board R) hand) 

    {-To check if a domino can be played on the board. Takes Domino and Board-}
    canPlay :: Domino -> Board -> End -> Bool
    canPlay _ InitState _ = True
    canPlay dominoInHand (State domBoardL domBoardR _) end
      | end == L && (fst dominoInHand == fst domBoardL || snd dominoInHand == fst domBoardL) = True
      | end == R && (fst dominoInHand == snd domBoardR || snd dominoInHand == snd domBoardR) = True
      | otherwise = False

    {-
      playDom: Attempts to play a domino on the board at a specified end.
      Takes a Player, a Domino, a Board, an End, and returns a Maybe Board.

      Input:
        - Player: The current player (P1 or P2).
        - Domino: The domino to be played.
        - Board: The current state of the game board.
        - End: The end of the board where the domino should be played.

      Output:
        - Maybe Board: Just the updated board if the move is valid, otherwise Nothing.
          - The board is represented as a State containing the left and right dominos, and a history of moves.
    -}
    playDom :: Player -> Domino -> Board -> End -> Maybe Board
    playDom player domino InitState _ = Just (State domino domino [(domino, player, 1)]) -- Any domino can be played on an empty board
    playDom player domino (State left right history) end
      | end == L && domino `fitsLeft` left = Just $ State domino right updatedHistory
      | end ==L && dominoFlipped `fitsLeft` left = Just $ State dominoFlipped right updatedHistory -- adds the domino in the right place in the Board
      | end == R && domino `fitsRight` right = Just $ State left domino updatedHistory
      | end ==R && dominoFlipped `fitsRight` right = Just $ State  left dominoFlipped updatedHistory
      | otherwise = Nothing
      where
        updatedHistory = (domino, player, length history + 1) : history
        dominoFlipped = (snd domino, fst domino) -- Flips the domino to check if it fits

    {- Check if a domino fits on the left end of the board-}
    fitsLeft :: Domino -> Domino -> Bool
    fitsLeft (domL, domR) (boardL, _) = domL == boardL || domR == boardL

    {- Check if a domino fits on the right end of the board -}
    fitsRight :: Domino -> Domino -> Bool
    fitsRight (domL, domR) (_, boardR) = domL == boardR || domR == boardR

    {-|
      simplePlayer: A simple implementation of a Dominoes player that selects the first valid move.
      Takes a Hand, a Board, a Player, and Scores, and returns a tuple containing the Domino to play and the End to play it on.

      Input:
        - Hand: The current player's hand.
        - Board: The current state of the game board.
        - Player: The current player (P1 or P2).
        - Scores: The current scores of the players.

      Output:
        - Tuple: Contains the selected Domino to play and the End to play it on.
          - If there are no valid moves, an error is thrown with the message "No valid moves!".
    -}
    simplePlayer :: DomsPlayer
    simplePlayer [] _ _ _ = error "No valid moves!"
    simplePlayer hand board _ _ = firstValidMove
      where
        validMoves = [(domino, end) | domino <- hand, end <- [L, R], canPlay domino board end]
        firstValidMove = head $ take 1 validMoves

    {-Determines the next move based on the current state, takes the hand and see the state of the board.
    If it is a InitState and the player has (5,4), the player would straight away play that domino
    Else, it would do the regularMoveStrategy-}
    smartPlayer :: DomsPlayer
    smartPlayer hand board player scores =
      let doubleFiveFourPresent = any (== (5, 4)) hand
      in case doubleFiveFourPresent of
        True | board == InitState -> ((5,4), L)
        _ -> regularMoveStrategy hand board

    {-filters the possiblemoves that is a Multiple of Three or Five
    If there is no possible moves, No moves would be made  -}
    regularMoveStrategy :: Hand -> Board  -> (Domino,End)
    regularMoveStrategy hand board =
      case board of
        InitState ->
          let possibleMovesInit = [(domino, end) | domino <- hand, end <- [L, R]]
              dominoesInit = [domino | (domino, end) <- possibleMovesInit]
          in case possibleMovesInit of
            [] -> error "No valid moves (initState) no possibleMoves!"  -- You might want to handle this differently
            _  -> let (_, end) = head possibleMovesInit
                      bestMove = findBestMove dominoesInit board
                  in playDominoAtBestEnd bestMove board
        _ ->
          let possibleMoves = [(domino, end) |
                                 domino <- hand,
                                 canPlay domino board L || canPlay domino board R,
                                 end <- [L, R]]
              dominoes = [domino | (domino, end) <- possibleMoves]
          in case possibleMoves of
            [] -> error "No valid moves, no possible Moves!"
            _  -> let (_, end) = head possibleMoves  -- Extract end from the first valid move
                      bestMove = findBestMove dominoes  board
                  in playDominoAtBestEnd bestMove board

    {-Recursively searches through the possible moves to find the move that results in the highest score-}
    findBestMove :: [Domino]  ->  Board -> Domino
    findBestMove possibleMoves board =
      let validMoves = filter (\domino ->isMultThreeFive board L domino||isMultThreeFive board R domino ||isMultThreeFive board L (swapDomino domino)||isMultThreeFive board R (swapDomino domino) ) possibleMoves
      in case validMoves of
        [] -> head possibleMoves
        _ -> fst (findHighestScoreDomino allDominoes (length allDominoes) board)
          where allDominoes = listDomino validMoves board

    {-Takes a list of domioes and returns the best domino and end to play on the board in a list including the flipped side of the domino-}
    listDomino :: [Domino] -> Board  -> [(Domino, End)]
    listDomino [] _  = []  -- Base case: empty list results in an empty list of tuples
    listDomino (domino:rest) board =
      if canPlay domino board L || canPlay domino board R || canPlay (swapDomino domino) board L || canPlay (swapDomino domino) board R
        then playDominoAtBestEnd domino board : playDominoAtBestEnd (swapDomino domino) board : listDomino rest board
        else listDomino rest board


    {-Checks whether a given move is a multiple of 3 or 5 on the current board and the player's hand-}
    isMultThreeFive :: Board -> End -> Domino -> Bool
    isMultThreeFive board end dominoToPlay =
      let newScore = calculateTotalPip dominoToPlay board end
      in newScore `mod` 3 == 0 || newScore `mod` 5 == 0

    {-Calculates the new score for the player after playing a given move by calculating the total pip when a domino is played on a board-}
    calculateTotalPip :: Domino -> Board -> End -> Int
    calculateTotalPip dominoToPlay board end
      | end == L = updatePipCountLeft board dominoToPlay
      | end == R = updatePipCountRight board dominoToPlay
      | otherwise = error "Invalid end"

    {-This function updates the pip count for the left side of the board when a domino -}
    updatePipCountLeft :: Board -> Domino -> Int
    updatePipCountLeft InitState dominoToPlay = uncurry (+) dominoToPlay
    updatePipCountLeft (State domBoardL domBoardR _) dominoToPlay =
      case (isDouble dominoToPlay, isDouble domBoardR) of
        (True, True) -> uncurry (+) dominoToPlay + fst domBoardR + snd domBoardR
        (False, True) -> uncurry (+) domBoardR + fst dominoToPlay
        (True, False) -> uncurry (+) dominoToPlay + snd domBoardR
        (False, False) -> fst dominoToPlay + snd domBoardR

    {-Updates the pip count for the right side of the board when a domino is played-}
    updatePipCountRight :: Board -> Domino -> Int
    updatePipCountRight InitState dominoToPlay = uncurry (+) dominoToPlay
    updatePipCountRight (State domBoardL domBoardR _) dominoToPlay =
      case (isDouble dominoToPlay, isDouble domBoardL) of
        (True, True) -> uncurry (+) dominoToPlay + uncurry (+) domBoardL
        (False, True) -> uncurry (+) dominoToPlay + fst domBoardL
        (True, False) -> uncurry (+) domBoardL + snd dominoToPlay
        (False, False) -> fst domBoardL + snd dominoToPlay

    {- Recursively searches through a list of dominoes to find the domino that results in the highest score
    the best end is determined from playDomino -}
    findHighestScoreDomino :: [(Domino,End)] -> Int -> Board -> (Domino, End)
    findHighestScoreDomino [] _ _ = error "No Dominoes"
    findHighestScoreDomino ((domino,end):rest) recursionDepth board
      | recursionDepth == 1 = (domino, end)
      | recursionDepth <= 0 = error "recursion depth cannot be negative"
      | currentScore >= bestRestScore = (domino,end)
      | otherwise = bestRest
      where
        currentScore = calculateTotalPip domino board end
        bestRest = findHighestScoreDomino  rest (recursionDepth - 1) board
        bestRestScore = calculateTotalPip (fst bestRest) board (snd bestRest)


    {-Determines the end of the board to play the domino on based the scoring strategy and to see which one is the highest-}
    playDominoAtBestEnd :: Domino -> Board -> (Domino, End)
    playDominoAtBestEnd dominoToPlay board =
      --Calculates a score of playing a domino at a given end
      let leftEndScore = calculateTotalPip dominoToPlay board L
          rightEndScore = calculateTotalPip dominoToPlay board R
          leftEndSwapScore = calculateTotalPip (swapDomino dominoToPlay) board L
          rightEndSwapScore = calculateTotalPip (swapDomino dominoToPlay) board R
      in
        -- Choose an end based on the highest score
        if leftEndScore >= rightEndScore && leftEndScore >= leftEndSwapScore && leftEndScore >= rightEndSwapScore && canPlay' dominoToPlay board L && isMultThreeFive board L dominoToPlay
          then (dominoToPlay, L)
          else if rightEndScore  >= leftEndScore  && rightEndScore >= leftEndSwapScore && rightEndScore >= rightEndSwapScore   && canPlay' dominoToPlay board R && isMultThreeFive board R dominoToPlay
            then (dominoToPlay, R)
            else if leftEndSwapScore  >= leftEndScore  && leftEndSwapScore >= rightEndScore && leftEndSwapScore >= rightEndSwapScore   && canPlay' (swapDomino dominoToPlay) board L && isMultThreeFive board L (swapDomino dominoToPlay)
              then (swapDomino dominoToPlay,L)
              else if rightEndSwapScore  >= leftEndScore  && rightEndSwapScore >= rightEndScore && rightEndSwapScore >= leftEndSwapScore   && canPlay' (swapDomino dominoToPlay) board R && isMultThreeFive board R (swapDomino dominoToPlay)
                then (swapDomino dominoToPlay ,R)
                --default move
                else if canPlay' dominoToPlay board L
                  then (dominoToPlay, L)
                  else if canPlay' dominoToPlay board R
                    then (dominoToPlay, R)
                    else if canPlay' (swapDomino dominoToPlay) board L
                      then (swapDomino dominoToPlay, L)
                      else if canPlay' (swapDomino dominoToPlay) board R
                        then (swapDomino dominoToPlay, R)
                        else error "No valid Move!"

    {-More specific whether a domino can be played on the board at the specified end
    Doesn't handle swapDomino like canPlay-}
    canPlay' :: Domino -> Board -> End -> Bool
    canPlay' _ InitState _ = True
    canPlay' dominoInHand (State domBoardL domBoardR _) end
      | end == L &&  snd dominoInHand == fst domBoardL = True
      | end == R && fst dominoInHand == snd domBoardR  = True
      | otherwise = False

    {- Helper function to swap the elements of a tuple and flips a domino to be played-}
    swapDomino :: Domino -> Domino
    swapDomino (x, y) = (y, x)

