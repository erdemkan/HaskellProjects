isWinner :: String -> [Char] -> Int -> Int -> IO Bool
isWinner secretWord guessedWords count temp = do
    let newTemp = if (secretWord !! count) `elem` guessedWords then 
                    temp + 1
                  else 
                    temp
    if temp == length secretWord then
        return True
    else
        if count < length secretWord then
            isWinner secretWord guessedWords (count + 1) newTemp
        else
            return False



guessedPartFinder :: String -> String -> Int -> String -> IO()
guessedPartFinder secretWord guessedPart count guessedWords = do
  let newGuessedPart =  if (secretWord !! count) `elem` guessedWords then 
                          guessedPart ++ [secretWord !! count]
                        else  
                          guessedPart ++ "_"
  
  if count == length secretWord - 1
    then putStrLn ("secret word: " ++ newGuessedPart)
    else guessedPartFinder secretWord newGuessedPart (count + 1) guessedWords


printGuessedPart :: String -> String -> IO()
printGuessedPart secretWord guessedWords = do
  let guessedPart = ""
  let count = 0
  guessedPartFinder secretWord guessedPart count guessedWords

  
 
guess :: String -> Int -> String -> Int -> IO()
guess secretWord guessCount guessedWords mistakeNum = do
    
  printGuessedPart secretWord guessedWords

  putStrLn ("You made " ++ show mistakeNum ++ " mistakes so far!!")
  putStrLn "Make a guess: "

  a <- getLine
  let b = head a

  if b `elem` secretWord then
    putStrLn "Correct Guess"
  else putStrLn "Wrong Guess"

  putStrLn ""

  let newMistakeNum =  if b `elem` secretWord then 
                          mistakeNum
                        else  
                          mistakeNum + 1
  res <- isWinner secretWord (guessedWords ++ a) 0 0                        
  if res then do
    putStrLn "CONGRATS YOU WON"
    putStrLn ("secret word was: " ++ secretWord)
  else      
    if mistakeNum < 4 then  
        guess secretWord (guessCount + 1) (guessedWords ++ a) newMistakeNum
    else putStrLn "You made 5 mistakes, THE GAME IS OVER"

main :: IO()
main = do
  
  putStrLn "----------------------"
  putStrLn "Welcome To The HANGMAN"
  putStrLn "----------------------"

  let guessedWords = ""
  let secretWord = "haskell"
  
  guess secretWord 0 guessedWords 0
  

