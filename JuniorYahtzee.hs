---------------------------
--  --
-- Albert Szmigielski    --
-- MARCH 2012             --
--             --
---------------------------

import IO 
import Random



---------MAIN----------------------------------------------------------
main:: IO()
main = do playGame sbGlobal
          putStrLn "Thank you for playing. Press enter to exit."
          a <- getLine
          putStrLn "Good Bye"
----------------------------------------------------------------------



--- DATA STRUCTURES :-------------------------------------------------------------------

data Colour = Red
            | Yellow
            | Purple
            | Green
            | White
            | Black
            | Null
              deriving (Eq, Show)

data ScorePair = ScorePair {
      colour      :: Colour
    , score       :: Int
    } deriving (Show)


data ScoreBoard = ScoreBoard {
      redScore	  :: ScorePair  
    , greenScore  :: ScorePair
    , yellowScore :: ScorePair
    , purpleScore :: ScorePair
    , whiteScore  :: ScorePair
    , blackScore   :: ScorePair
    } deriving (Show)


--- INITIAL VALUES -----------------------------------------------------------------------------

spR = ScorePair Red 0
spG = ScorePair Green 0
spY = ScorePair Yellow 0
spP = ScorePair Purple 0
spW = ScorePair White 0
spB = ScorePair Black 0

scoreBoard = ScoreBoard spR spG spY spP spW spB
sbGlobal = ScoreBoard   (ScorePair Red (-1) ) (ScorePair Green (-1)) (ScorePair Yellow (-1)) (ScorePair Purple (-1)) (ScorePair White (-1)) (ScorePair Black (-1))
sb = ScoreBoard      spR spG spY spP spW spB

--------------TEST AND DEBUG VALUES---------------------------------------------
rnd = [Red, Red, Red]
rnd1 = [Red, Red, Red, Green, Green, Purple, White, Black ]
sb1 = scoreRoll sb rnd1
sb2 = scoreRoll sb1 rnd1
--------------------------------------------------------------------------------

-------------- HELPER FUNCTIONS ----------------------------------------
valueToColour :: Int -> Colour 
valueToColour c = case c of
                  1 -> Red     
                  2 -> Yellow   
                  3 -> Purple   
                  4 -> Green    
                  5 -> White    
                  6 -> Black   


stringToColour:: String -> Colour 
stringToColour c = case c of
                  "red"     -> Red  
                  "Red"     -> Red  
                  "r"       -> Red   
                  "R"       -> Red   
                  "yellow"  -> Yellow  
                  "Yellow"  -> Yellow  
                  "Y"       -> Yellow  
                  "y"       -> Yellow   
                  "Purple"  -> Purple   
                  "purple"  -> Purple 
                  "P"       -> Purple 
                  "p"       -> Purple 
                  "green"   -> Green  
                  "Green"   -> Green 
                  "G"       -> Green 
                  "g"       -> Green   
                  "White"   -> White 
                  "white"   -> White
                  "W"       -> White
                  "w"       -> White   
                  "black"   -> Black 
-----------------------------------------------------------------------------------------------------------------------------------
printScore :: ScoreBoard -> IO ()
printScore sBoard = do
             putStr (show(colour(redScore sBoard))  ++ ":    ")
             if (score(redScore sBoard)) == (-1) then do putStrLn "not scored yet" else putStrLn (show(score(redScore sBoard)))
             
             putStr (show(colour(greenScore sBoard))  ++ ":  " )                     -- ++ show(score(greenScore sBoard)))
             if (score(greenScore sBoard)) == (-1) then do putStrLn "not scored yet" else putStrLn (show(score(greenScore sBoard)))
             putStr (show(colour(yellowScore sBoard))  ++ ": " )                     --  ++ show(score(yellowScore sBoard)))
             if (score(yellowScore sBoard)) == (-1) then do putStrLn "not scored yet" else putStrLn (show(score(yellowScore sBoard)))
             putStr (show(colour(purpleScore sBoard))  ++ ": " )                     -- ++ show(score(purpleScore sBoard)))
             if (score(purpleScore sBoard)) == (-1) then do putStrLn "not scored yet" else putStrLn (show(score(purpleScore sBoard)))
             putStr (show(colour(whiteScore sBoard))  ++ ":  " )                     -- ++ show(score(whiteScore sBoard)))
             if (score(whiteScore sBoard)) == (-1) then do putStrLn "not scored yet" else putStrLn (show(score(whiteScore sBoard)))

------------------------------------------------------------------------------------------------------------------------------------


playGame :: ScoreBoard -> IO()
playGame sb = do putStrLn " WELCOME TO JUNIOR YAHTZEE! LET'S PLAY!"
                 turn sbGlobal 1
----------------------------------------------------------------------------------------------------------------
turn:: ScoreBoard -> Int -> IO()
turn sbG numTurn  = if numTurn==6
         then do do putStrLn "Here is your final score:" 
                    printScore sbG
                    putStrLn ("---------- \nTotal:  " ++ show(tscore sbG)) 
         else if numTurn /= 1 then do putStrLn "\n Here is your score after this turn"
                                      printScore sbG
                                      putStrLn ("---------- \nTotal:  " ++ show(tscore sbG))
                                      putStrLn " "
                                      putStrLn (" ***************** TURN " ++ show(numTurn) ++ " *****************")
                                      (roll sb 1 [] 0 Null Null [] numTurn sbG False)
              else do putStrLn (" ***************** TURN " ++ show(numTurn) ++ " *****************")
                      roll sb 1 [] 0 Null Null [] numTurn sbG False
                 
                 
 
--ROLL------------------------------------------------------------------------------------------------------------------

roll :: ScoreBoard -> Int -> [Colour] -> Int  -> Colour -> Colour -> [Colour] -> Int -> ScoreBoard -> Bool -> IO()
roll sb nr rndm cs clr_prev clr_new keep turn_num sbGlobal sNow = 
  if sNow == True then turn sbGlobal (turn_num-1)
  else 
  case nr of
       1 ->  do putStrLn "**** ROLL 1 **** "  
                rList (5-cs) [] sb nr cs clr_prev clr_new keep turn_num sbGlobal sNow
       2 ->  do --printScore sb
                putStrLn "**** ROLL 2 **** "  
                rList (5-cs) [] sb nr cs clr_prev clr_new keep turn_num sbGlobal sNow
       3 ->  do --printScore sb
                putStrLn "**** ROLL 3 **** "
                rList (5-cs) [] sb nr cs clr_prev clr_new keep turn_num sbGlobal sNow
       4 ->  do putStrLn ("scoring colour " ++ show(clr_prev))
                --printScore sb
                turn (scoreIt cs clr_prev sbGlobal sb rndm) (turn_num+1)
       _ ->  putStrLn ("number of rolls out of range")

------------------------------------------------------------------------------------------------------------------------
rList :: Int -> [Colour] -> ScoreBoard  -> Int -> Int -> Colour -> Colour -> [Colour] -> Int -> ScoreBoard -> Bool -> IO() 
rList n xs sb nr cs clr_prev clr_new keep turn_num sbGlobal sNow = if n == 0  
             then processRandoms sb xs nr cs clr_prev clr_new keep turn_num sbGlobal sNow
             else do
               rnum <- randomRIO (1::Int, 6)
               rList (n-1) (valueToColour(rnum):xs) sb nr cs clr_prev clr_new keep turn_num sbGlobal sNow

---------------------------------------------------------------------------------------------------------------------------
processRandoms :: ScoreBoard -> [Colour] -> Int -> Int -> Colour -> Colour -> [Colour] -> Int -> ScoreBoard -> Bool -> IO() 
processRandoms sb xs nr cs clr_prev clr_new keep turn_num sbGlobal sNow =
      if cs==5 then do putStr "Since your score was 5 for your chosen color \n I have scored it for you. \n On to the next turn..."
                       turn (scoreIt ( score(redScore sb) + score(blackScore sb) ) clr_prev sbGlobal sb xs) (turn_num+1)
      else if nr ==2  then do putStrLn ("Ok im going to put aside any " ++ show clr_prev  ++ " and Black dice you had and re-roll the rest")
                              putStr "These are the dice you kept: "
                              pc(keep) 
                              putStr ("Here is the result of roll number " ++ show(nr) ++ ": ")
                              pc(xs) 
                              putStrLn ("What Colour are you going for? or type S to score a colour now: ")
                              getInput sb xs nr cs clr_prev clr_new keep turn_num sbGlobal sNow
           else if nr== 3 then do putStrLn ("Ok im going to put aside any " ++ show clr_prev  ++ " and Black dice you had and re-roll the rest")
                                  putStr "These are the dice you kept: "
                                  pc(keep)
                                  putStr ("Here is the result of roll number " ++ show(nr) ++ ": ")
                                  pc(xs) -- (show ((xs)))
                                  putStrLn ("That was the 3rd roll, time to score now. Select a colour to Score ")
                                  getInput sb xs nr cs clr_prev clr_new keep turn_num sbGlobal False
                else if sNow == True then do putStr "ok, you want to score, which colour? :"
                                             getInput sb xs nr cs clr_prev clr_new keep turn_num sbGlobal True
                     else do putStr ("Here is the result of roll number " ++ show(nr) ++ ": ")
                             pc(xs) -- (show ((xs)))
                             putStrLn ("What Colour are you going for? or type S to score a colour now: ")
                             getInput sb xs nr cs clr_prev clr_new keep turn_num sbGlobal sNow





---------------------------------------------------------------------------------------------------------------------------
getInput:: ScoreBoard -> [Colour] -> Int -> Int -> Colour -> Colour -> [Colour] -> Int -> ScoreBoard -> Bool -> IO() 
getInput sb rndm nr cs clr_prev clr_new keep turn_num sbGlobal sNow = do
        answer <- getLine
        if      ((answer) == "Red")    || (answer == "red")    || ((answer) == "R")  || ((answer) == "r")
                then do putStrLn "You selected: Red"
                        if (score (redScore sbGlobal) == (-1)) then
                           if sNow == True then turn (scoreIt ( score(redScore sb) + score(blackScore sb) ) Red sbGlobal sb rndm) (turn_num+1) 
                           else
                                roll   (ScoreBoard (ScorePair Red (score(redScore (scoreRoll sb rndm)))) spG spY spP spW ( ScorePair Black (score(blackScore (scoreRoll sb rndm)))))  
                                (nr+1) rndm ((score(redScore (scoreRoll sb rndm)) + score(blackScore (scoreRoll sb rndm)))) (stringToColour(answer)) clr_new 
                                (keepn (score(redScore (scoreRoll sb rndm))) Red (score(blackScore (scoreRoll sb rndm))) []) turn_num sbGlobal sNow
                        else do putStrLn ("You have already scored " ++ show(stringToColour(answer)) ++ ", pick another colour: ")
                                getInput sb rndm nr cs clr_prev clr_new keep turn_num sbGlobal sNow

        else if ((answer) == "Green")  || (answer == "green")  || ((answer) == "G")  || ((answer) == "g") 
                then do putStrLn "You selected: Green"
                        if (score (greenScore sbGlobal) == (-1)) then
                           if sNow == True then turn (scoreIt ( score(greenScore sb) + score(blackScore sb) ) Green sbGlobal sb rndm) (turn_num+1) 
                           else 
                                roll   (ScoreBoard spR (ScorePair Green (score(greenScore (scoreRoll sb rndm))))  spY spP spW ( ScorePair Black (score(blackScore (scoreRoll sb rndm)))))  
                                (nr+1) rndm ((score(greenScore (scoreRoll sb rndm)) + score(blackScore (scoreRoll sb rndm)))) (stringToColour(answer)) clr_new 
                                (keepn (score(greenScore (scoreRoll sb rndm))) Green (score(blackScore (scoreRoll sb rndm))) []) turn_num sbGlobal sNow
                        else do putStrLn ("You have already scored " ++ show(stringToColour(answer)) ++ ", pick another colour: ")
                                getInput sb rndm nr cs clr_prev clr_new keep turn_num sbGlobal sNow

        else if ((answer) == "Yellow") || (answer == "yellow") || ((answer) == "Y")  || ((answer) == "y") 
                then do putStrLn "You selected: Yellow"
                        if (score (yellowScore sbGlobal) == (-1)) then
                           if sNow == True then turn (scoreIt ( score(yellowScore sb) + score(blackScore sb) ) Yellow sbGlobal sb rndm) (turn_num+1) 
                           else 
                                roll   (ScoreBoard spR spG (ScorePair Yellow (score(yellowScore (scoreRoll sb rndm)))) spP spW ( ScorePair Black (score(blackScore (scoreRoll sb rndm)))))  
                                (nr+1) rndm ((score(yellowScore (scoreRoll sb rndm)) + score(blackScore (scoreRoll sb rndm)))) (stringToColour(answer)) clr_new 
                                (keepn (score(yellowScore (scoreRoll sb rndm))) Yellow (score(blackScore (scoreRoll sb rndm))) []) turn_num sbGlobal sNow
                        else do putStrLn ("You have already scored " ++ show(stringToColour(answer)) ++ ", pick another colour: ")
                                getInput sb rndm nr cs clr_prev clr_new keep turn_num sbGlobal sNow

        else if ((answer) == "Purple") || (answer == "purple") || ((answer) == "P")  || ((answer) == "p") 
                then do putStrLn "You selected: Purple"
                        if (score (purpleScore sbGlobal) == (-1)) then
                           if sNow == True then turn (scoreIt ( score(purpleScore sb) + score(blackScore sb) ) Purple sbGlobal sb rndm) (turn_num+1) 
                           else 
                                roll   (ScoreBoard spR spG spY (ScorePair Purple (score(purpleScore (scoreRoll sb rndm)))) spW ( ScorePair Black (score(blackScore (scoreRoll sb rndm)))))  
                                (nr+1) rndm ((score(purpleScore (scoreRoll sb rndm)) + score(blackScore (scoreRoll sb rndm)))) (stringToColour(answer)) clr_new 
                                (keepn (score(purpleScore (scoreRoll sb rndm))) Purple (score(blackScore (scoreRoll sb rndm))) []) turn_num sbGlobal sNow
                        else do putStrLn ("You have already scored " ++ show(stringToColour(answer)) ++ ", pick another colour: ")
                                getInput sb rndm nr cs clr_prev clr_new keep turn_num sbGlobal sNow

        else if ((answer) == "White" ) || (answer == "white" ) || ((answer) == "W")  || ((answer) == "w") 
                then do putStrLn "You selected: White"
                        if (score (whiteScore sbGlobal) == (-1)) then
                           if sNow == True then turn (scoreIt ( score(whiteScore sb) + score(blackScore sb) ) White sbGlobal sb rndm) (turn_num+1) 
                        else 
                            roll   (ScoreBoard spR spG spY spP (ScorePair White (score(whiteScore (scoreRoll sb rndm))))   ( ScorePair Black (score(blackScore (scoreRoll sb rndm)))))  
                            (nr+1) rndm ((score(whiteScore (scoreRoll sb rndm)) + score(blackScore (scoreRoll sb rndm)))) (stringToColour(answer)) clr_new 
                            (keepn (score(whiteScore (scoreRoll sb rndm))) White (score(blackScore (scoreRoll sb rndm))) []) turn_num sbGlobal sNow
                        else do putStrLn ("You have already scored " ++ show(stringToColour(answer)) ++ ", pick another colour: ")
                                getInput sb rndm nr cs clr_prev clr_new keep turn_num sbGlobal sNow

        else if ((answer) == "Score" ) || (answer == "score" ) || ((answer) == "S")  || ((answer) == "s") 
                then do putStrLn "Score - ok what colour?: "
                        getInput (scoreRoll sb rndm) rndm nr cs clr_prev clr_new keep turn_num sbGlobal True   -- (scoreIt (stringToColour answer) sbGlobal sb rndm)
        else do putStrLn "Not a colour, try again: " 
                getInput sb rndm nr cs clr_prev clr_new keep turn_num sbGlobal sNow


-----------------------------------------------------------------------------------
---   HELPER FUNCTIONS                                                          ---
-----------------------------------------------------------------------------------


pc:: [Colour] -> IO()
pc     [] = putStr ""
pc (x:[]) = putStrLn ((show (x))++ " ")            
pc (x:xs) = do
            putStr ((show (x)) ++ " ")
            (pc xs)
---------------------------------------------------------------------------------------------------------------	
scoreIt:: Int -> Colour -> ScoreBoard -> ScoreBoard -> [Colour] ->ScoreBoard
scoreIt cs clr sbGlobal sb rndm = 
      --do putStrLn "Scoring...  " 
         case clr of
             Red    -> ScoreBoard  ( ScorePair Red cs ) (greenScore sbGlobal) (yellowScore  sbGlobal) (purpleScore  sbGlobal) (whiteScore  sbGlobal) spB
             Green  -> ScoreBoard  ( redScore  sbGlobal ) (ScorePair Green cs ) (yellowScore  sbGlobal) (purpleScore  sbGlobal) (whiteScore  sbGlobal) spB
             Yellow -> ScoreBoard  ( redScore  sbGlobal ) (greenScore  sbGlobal ) (ScorePair Yellow cs) (purpleScore  sbGlobal) (whiteScore  sbGlobal) spB
             Purple -> ScoreBoard  ( redScore  sbGlobal ) (greenScore  sbGlobal ) (yellowScore  sbGlobal) (ScorePair Purple cs ) (whiteScore  sbGlobal) spB
             White  -> ScoreBoard  ( redScore  sbGlobal ) (greenScore  sbGlobal ) (yellowScore  sbGlobal) (purpleScore  sbGlobal) (ScorePair White cs) spB

             _  -> sbGlobal


---------------------------------------------------------------------------------------------------------------- 

keepn:: Int -> Colour -> Int -> [Colour] -> [Colour]
keepn nclr clr blk xs = if blk==0 && nclr==0 then []
                       else if nclr == 0 then Black:(keepn (nclr) clr (blk-1) xs)  
		       else clr:(keepn (nclr-1) clr (blk) xs)  

----------------------------------------------------------------------------------------------------------------
tscore :: ScoreBoard -> Int
tscore sbg = (if score(redScore sbg) == (-1) then 0 else score(redScore sbg))
             + (if score (greenScore sbg)  == (-1) then 0 else score (greenScore sbg) )
             + (if score (yellowScore sbg) == (-1) then 0 else score (yellowScore sbg) )
             + (if score (purpleScore sbg) == (-1) then 0 else score (purpleScore sbg) )
             + (if score (whiteScore sbg)  == (-1) then 0 else score (whiteScore sbg) )


---------------------------------------------------------------------------------------------------------------------------
----  scoreRoll - keeps track of dice colours after a roll----

scoreRoll:: ScoreBoard -> [Colour] -> ScoreBoard
scoreRoll sbRoll rndm = if null rndm then sbRoll
      else case (head(rndm)) of
             Red    -> scoreRoll (ScoreBoard  (ScorePair Red ((score(redScore sbRoll))+1)) (greenScore sbRoll) (yellowScore sbRoll) (purpleScore sbRoll) (whiteScore sbRoll) (blackScore sbRoll))  (tail(rndm))
             Green  -> scoreRoll (ScoreBoard  (redScore sbRoll) (ScorePair Green ((score(greenScore sbRoll))+1))  (yellowScore sbRoll) (purpleScore sbRoll) (whiteScore sbRoll) (blackScore sbRoll)) (tail(rndm))
             Yellow -> scoreRoll (ScoreBoard  (redScore sbRoll) (greenScore sbRoll) (ScorePair Yellow ((score(yellowScore sbRoll))+1)) (purpleScore sbRoll) (whiteScore sbRoll) (blackScore sbRoll))  (tail(rndm))
             Purple -> scoreRoll (ScoreBoard  (redScore sbRoll) (greenScore sbRoll) (yellowScore sbRoll) (ScorePair Purple ((score(purpleScore sbRoll))+1)) (whiteScore sbRoll) (blackScore sbRoll))  (tail(rndm))
             White  -> scoreRoll (ScoreBoard  (redScore sbRoll) (greenScore sbRoll) (yellowScore sbRoll) (purpleScore sbRoll) (ScorePair White ((score(whiteScore sbRoll))+1)) (blackScore sbRoll))  (tail(rndm))
             Black  -> scoreRoll (ScoreBoard  (redScore sbRoll) (greenScore sbRoll) (yellowScore sbRoll) (purpleScore sbRoll) (whiteScore sbRoll) (ScorePair Black ((score(blackScore sbRoll))+1))) (tail(rndm))
             Null   -> sbRoll

