
import System.IO
import qualified Text.Read as R
import qualified Data.Map as Map
import qualified Data.List as L

-- Constants
kSHELL_PROMPT :: String 
kSHELL_PROMPT = "> "

kSHELL_OUTPUT_PREFIX :: String
kSHELL_OUTPUT_PREFIX = "Output: "

kSHELL_QUIT_MESSAGE :: String
kSHELL_QUIT_MESSAGE = "Quitting..."

kSHELL_INVALID_COMMAND_MESSAGE :: String
kSHELL_INVALID_COMMAND_MESSAGE = "* ERROR, Invalid command! use help for list of valid commands"

kSHELL_INVALID_ARGUMENTS_MESSAGE :: String
kSHELL_INVALID_ARGUMENTS_MESSAGE = "* ERROR, Invalid arguments! "

-- Helper Functions

type CommandHandler = ([String] -> REPLResult)

data REPLResult = 
  REPLValidResult { _message :: String }
  | REPLInvalidArgumentsResult {message :: String}
  | REPLInvalidCommandResult
  | REPLQuitResult deriving (Show)

handlerMap :: Map.Map String CommandHandler
handlerMap = 
  Map.fromList [
    ("help"   , handlerHelp  ),
    ("author" , handlerAuthor),
    ("quit"   , handlerQuit  ),
    ("add"    , handlerAdd   ),
    ("mult"   , handlerMultiply),
    ("div"    , handlerDivide)
  ]

(??) :: Show a => [a] -> Int -> Maybe a
(??) list index = 
  if index < length list
    then Just $ list !! index
    else Nothing

resultToMessage :: REPLResult -> String
resultToMessage result = 
  case result of 
    REPLValidResult m -> 
      kSHELL_OUTPUT_PREFIX ++ m
    REPLInvalidArgumentsResult m ->
      kSHELL_INVALID_ARGUMENTS_MESSAGE ++ m
    REPLInvalidCommandResult ->
      kSHELL_INVALID_COMMAND_MESSAGE
    REPLQuitResult ->
      kSHELL_QUIT_MESSAGE


maybeify :: (a -> b) -> a -> (Maybe a -> b)
maybeify func def = 
  \ma -> 
    case ma of 
      Just ja -> func ja
      Nothing -> func def

getHandlerByName :: String -> CommandHandler
getHandlerByName hname = 
  let 
    maybeHandler = Map.lookup hname handlerMap
  in
    case maybeHandler of 
      Just handler -> handler
      Nothing      -> handlerInvalid


evaluateString :: String -> REPLResult
evaluateString commandString = 
  let 
    arguments = words commandString
    command = arguments ?? 0
  in
    ((maybeify getHandlerByName "") command) arguments

--- Command Handlers

handlerHelp :: CommandHandler
handlerHelp _ = 
  let 
    baseHelpMessage = "The valid commands are: \n"
    allCommands = Map.keys handlerMap 
    commandToRow = (\k -> "- " ++ k)
    commandsMessage = L.intercalate "\n" $ map commandToRow allCommands
  in
    REPLValidResult $ baseHelpMessage ++ commandsMessage

handlerAuthor :: CommandHandler
handlerAuthor _ = REPLValidResult "John Doe"

_handlerNumBinaryOperator :: (Float -> Float -> Float) -> String -> CommandHandler
_handlerNumBinaryOperator func command =
  \arguments -> 
    let 
      lmaybe    = (arguments ?? 1) >>= R.readMaybe :: Maybe Float
      rmaybe    = (arguments ?? 2) >>= R.readMaybe :: Maybe Float
      endMarker = (arguments ?? 3) -- hack to check if list has more than 2 args
    in
      case (lmaybe, rmaybe, endMarker) of
        (Just lnum, Just rnum, Nothing) -> REPLValidResult $ show $ func lnum rnum
        _                               -> REPLInvalidArgumentsResult $ command ++ " <num> <num>"

handlerAdd :: [String] -> REPLResult
handlerAdd = _handlerNumBinaryOperator (+) "add"
  
handlerMultiply :: CommandHandler
handlerMultiply = _handlerNumBinaryOperator (*) "add"

handlerDivide :: CommandHandler
handlerDivide = _handlerNumBinaryOperator (/) "div"

handlerQuit :: CommandHandler
handlerQuit _ = REPLQuitResult

handlerInvalid :: CommandHandler
handlerInvalid _ = REPLInvalidCommandResult

--- REPL Base

shellRead :: IO String
shellRead = do
  putStr kSHELL_PROMPT
  hFlush stdout
  getLine

shellEvaluate :: String -> IO REPLResult
shellEvaluate command = 
  return $ evaluateString command

shellPrint :: REPLResult -> IO REPLResult
shellPrint result = do
  putStrLn $ resultToMessage result
  return result
  
shellLoop :: REPLResult -> IO ()
shellLoop result = 
  case result of 
    REPLQuitResult -> return ()
    _              -> main

main :: IO () 
main = do
      shellRead 
  >>= shellEvaluate
  >>= shellPrint
  >>= shellLoop

