module LGPT.TUI where

{-
This file is the main entry point to your coursework.

You can create or modify any files in src/ as much as you like. The 
code that is included here is a good starting point, but you don't need to 
keep it if you don't want to.
-}
import Text.Megaparsec
import Text.Megaparsec.Char
import LGPT.Helpers (Parser, prompt, runStart)   
import LGPT.Numbers (parseLonghand, printLonghand)
import Data.Time
import Data.Time.Calendar
--import GHC.Stack (getCurrentCCS)

import Control.Monad
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.IO.Class

import Data.Map (Map)
import qualified Data.Map as Map

--------------------------------------------------------------------------------
{- | Our program. It runs a loop which:
      1. Reads a line of input
      2. Parses it into a structured Request
      3. Does something based on that request (normally printing something out).
-}
-- Initialises memory using the Var type.
runREPL :: IO ()
runREPL = evalStateT runREPL' Var {
    rememberMap = Map.empty,
    thatNumber = Nothing
  }

-- We want the output to be a Monad that can handle both IO and State (for memory).
-- However, runREPL only outputs IO (). So we have this alternate runREPL' which uses state, then the original runREPL just evaluates the state.
runREPL' :: (MonadState Var m, MonadIO m) => m ()
runREPL' = forever $ do
  liftIO $ putStr prompt
  req <- liftIO getLine
  respondTo (readRequest req)


--------------------------------------------------------------------------------
-- Parsing and responding to requests:


-- | Our request type, the result of parsing a string. We have a constructor for each kind of request, which contains any relevant data!
data Request = Unknown | Hello | DayToday | DayTomorrow | TimeSince Day | Expression Expr | WriteMemory String String | ReadMemory String | ClearMemory String
  deriving (Eq, Ord, Show)

-- | Our expression type, recursively defined to unambiguously represent an arithmatic expression. ThatNumber represents whatever number is stored in memory.
data Expr =
    Plus Expr Expr
    | Minus Expr Expr
    | Times Expr Expr
    | Number Int
    | ThatNumber
    deriving (Eq, Ord, Show)

-- | Our variables type, used to name variables stored in the state memory.
-- We wrap the different arguments in Var using record syntax, and then use a single var instance to evaluate the state.
data Var = Var {
  rememberMap :: Map String String,
  thatNumber :: Maybe Int
}


{- | Read a request.
    This runs the parse function from Megaparsec, and
    converts any failed parses into an Unknown request.
-}
readRequest :: String -> Request
readRequest str = case parse parseRequest "<stdin>" str of
  Left  err -> Unknown
  Right req -> req
  
{- | Parses the user's input.
    We build a parser for each possible input, and then use choice to take the first one that matches the user's input.
    Each parser ends by lifting the request it is associated with.
-}
parseRequest :: Parser Request
parseRequest = choice [
    (string "Hello" <|> string "Hi" <|> string "Hey") >> parseRequestEnd >> pure Hello,
    string "What day is it tomorrow" >> parseRequestEnd >> pure DayTomorrow,
    (string "What day is it today" <|> string "What day is it") >> parseRequestEnd >> pure DayToday,
    string "How long ago was " >> parseDate >>= \d -> parseRequestEnd >> pure (TimeSince d),
    string "What is " >> parseExpr >>= \e -> parseRequestEnd >> pure (Expression e),
    do
      string "Remember that "
      name <- manyTill anySingle (string " is ")
      thing <- manyTill anySingle parseRequestEnd
      pure (WriteMemory name thing),
    string "Tell me about " >> manyTill anySingle parseRequestEnd >>= \name -> pure (ReadMemory name),
    string "Forget about " >> manyTill anySingle parseRequestEnd >>= \name -> pure (ClearMemory name)
    ]

-- caps the input  by making sure its the end of the input. Flexible with punctuation.
parseRequestEnd :: Parser ()
parseRequestEnd = (string "." <|> string "?" <|> string "!" <|> string "") >> eof


-- | Respond to a request. Pattern matches on the Request, and sends an appropriate response.
--   Uses a monad which has the computational power of both IO and state so that it can send outputs as well as access memory.
respondTo :: (MonadState Var m, MonadIO m) => Request -> m ()
respondTo Unknown = liftIO $ putStrLn "I don't understand that."

respondTo Hello = liftIO $ putStrLn "Hi there!"

-- Makes use of the Data.Time library to get the current time according to the user's device. Converts to approriate data to print.
respondTo DayToday = liftIO $ do
  t <- getCurrentTime
  let day = utctDay t
  let dayOfMonth = (\(x,y,z) -> z) . toGregorian $ day
  let monthOfYear = (\(x,y,z) -> y) . toGregorian $ day
  putStrLn $ "Today is " ++ (show . dayOfWeek $ day) ++ " the " ++ show dayOfMonth ++ getOrdinalIndicator dayOfMonth ++" of " ++ showMonth monthOfYear ++ "."

-- See above. Does the same but just looks at the next day.
respondTo DayTomorrow = liftIO $ do
  t <- getCurrentTime
  let day = succ . utctDay $ t
  let dayOfMonth = (\(x,y,z) -> z) . toGregorian $ day
  let monthOfYear = (\(x,y,z) -> y) . toGregorian $ day
  putStrLn $ "Tomorrow is " ++ (show . dayOfWeek $ day) ++ " the " ++ show dayOfMonth ++ getOrdinalIndicator dayOfMonth ++" of " ++ showMonth monthOfYear ++ "."

-- As above, uses Data.Time to get current time. After this it calculates the number of days ago it was compared to the input, then gives an appropriate response.
respondTo (TimeSince d) = liftIO $ do
  t <- getCurrentTime
  let diff = diffDays (utctDay t) d
  putStrLn $ if diff < -1 then show d ++ " is " ++ show (-diff) ++ " days in the future."
    else if diff == -1 then show d ++ " is tomorrow!"
    else if diff == 0 then show d ++ " is today!"
    else if diff == 1 then show d ++ " was yesterday!"
    else show d ++ " was " ++ show diff ++ " days ago."

-- Evaluates the input expression and prints the output. Can fail if "that" variable is used before anything has been evaluated.
-- If it doesn't, sets "that" to the result of the expression.
respondTo (Expression e) = do
  maybeRes <- evalExpr e
  case maybeRes of
    Nothing -> liftIO $ putStrLn "I haven't evaluated anything yet."
    Just n -> do
      modify $ \var -> var {thatNumber = Just n}
      liftIO $ putStrLn $ "The answer is " ++ printLonghand n ++ "."

-- Links "name" to "thing" in memory using a Map from String to String. 
-- modify lets us apply a function to the current stateful value in state memory.
-- We take the var in memory and change its rememberMap by inserting (name, thing).
respondTo (WriteMemory name thing) = do
  modify $ \var -> var {rememberMap = Map.insert name thing (rememberMap var)}
  liftIO $ putStrLn "Okay."

-- Accesses memory to output the information stored about name.
-- gets let us access the current stateful value and apply a function to it. We use this to get the rememberMap element of var.
-- We then use rememberMap to lookup the name given, and give the appropriate response depending on if it exists in the map.
respondTo (ReadMemory name) = do
  map <- gets rememberMap
  case Map.lookup name map of
    Nothing -> liftIO $ putStrLn $ "Sorry, I don't know anything about " ++ name ++ "."
    Just thing -> liftIO $ putStrLn $ "Sure - " ++ name ++ " is " ++ thing ++ "."

-- Removes the element with key "name" from the map.
-- Uses modify, which lets us apply a function to the current stateful value.
-- We take the var in memory and change its rememberMap by deleting the relevant key value pair using Map.delete.
respondTo (ClearMemory name) = do
  modify $ \var -> var {rememberMap = Map.delete name (rememberMap var)}
  liftIO $ putStrLn "Okay."

-- helper function for time responses. Returns the ordinal indicator (1st, 2nd, etc.)
getOrdinalIndicator :: Int -> String
getOrdinalIndicator 1 = "st"
getOrdinalIndicator 2 = "nd"
getOrdinalIndicator 3 = "rd"
getOrdinalIndicator x
  | x <=20 = "th"
  | x <=30 = getOrdinalIndicator (x-20)
  | otherwise = getOrdinalIndicator (x-10)

-- helper function for time responses. Returns the string representation of a month.
showMonth :: MonthOfYear -> String
showMonth x = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]!!(x-1)



-- | Parse a date in the format YYYY-MM-DD and return it as a Day using Data.Time
parseDate :: Parser Day
parseDate = do
  y1 <- digitChar
  y2 <- digitChar
  y3 <- digitChar
  y4 <- digitChar
  char '-'
  m1 <- digitChar
  m2 <- digitChar
  char '-'
  d1 <- digitChar
  d2 <- digitChar
  let year = read [y1, y2, y3, y4]
  let month = read [m1, m2]
  let day = read [d1,d2]
  pure $ fromGregorian year month day

-- | Parse an expression written in longhand from left to right.
-- An expression should always start with a number. After that it is (an operator then another number) some number of times >= 0.
parseExpr :: Parser Expr
parseExpr = do
  n1 <- parseNumber
  parseOp n1 <|> pure n1

-- We can parse a number as either "that", which stands for the number stored in memory, or a longhand written number.
-- We use the Expr data type to represent both of these in code.
parseNumber :: Parser Expr
parseNumber = (string "that" >> pure ThatNumber)
  <|> (Number <$> parseLonghand)

-- After an initial number is parsed, the expression is (an operator then another number) some number of times >= 0.
-- We use the Expr data type to represent operators and numbers.
parseOp :: Expr -> Parser Expr
parseOp n1 = do
  op <- (string " plus " >> pure Plus)
      <|> (string " minus " >> pure Minus)
      <|> (string " times " >> pure Times)
  n2 <- parseNumber
  let n3 = op n1 n2 -- Since we will calculate left to right, after reading (number operator number) we want to do that operation first, then look for other operators.
  parseOp n3 <|> pure n3

-- To evaluate an expression we determine which of its constructors it is via pattern matching.
-- If it is one of the operators, we must first evaluate the sub expressions, then apply the function to the results.
evalExpr :: (MonadState Var m, MonadIO m) => Expr -> m (Maybe Int)
evalExpr expr = case expr of
  Plus a b -> op a (+) b
  Minus a b -> op a (-) b
  Times a b -> op a (*) b
  Number x -> pure $ Just x
  ThatNumber -> gets thatNumber
  where
    op e1 f e2 = do
      x <- evalExpr e1
      y <- evalExpr e2
      pure (f <$> x <*> y) -- the results x and y are lifted into the Maybe monad, and f is not. Hence we use fmap <$> and then <*>