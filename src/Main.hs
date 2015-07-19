{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite as Sqlite
import           Database.Persist.TH
import           System.IO
import           Text.Read
import           Data.List.Split
import           Data.Time
import           Control.Monad
import           Data.List as List

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Chinese
    value String Unique
German
    value String Unique
Vocab
    vocab ChineseId
    translation GermanId
    created UTCTime default=CURRENT_TIME
    deriving Show
|]


translate = 1
ask = 2
insertNew = 3
database = "vocab.db"


commandsWithHelp =
  [ (("m", const mainMenu), "Return to the menu")
  , (("ti", translateOne), "Do an inplace translation of the word")
  , (("t", const translationLoop), "switch to the translation mode")
  , (("ask", const askLoop), "switch to the ask mode")
  , (("aski", const askOne), "ask one vocab in place")
  , (("?", const helpMenu), "Show this help menu")
  , (("e", const enterLoop), "Enter new vocabs")
  , (("ei", enterOne), "Enter one new vocab in place")
  ]


commands = map fst commandsWithHelp


mainMenu :: IO ()
mainMenu = do
  let
    message =
      unlines
        [ "Choose a Mode"
        , "1) Translate"
        , "2) Ask"
        , "3) Insert new"
        ]

  doItAgain message (continue . read)

  where
    continue resp
      | resp == translate =
        translationLoop
      | resp == ask =
        askLoop
      | resp == insertNew =
        enterLoop


parseInput :: String -> (String -> IO ()) -> String -> IO ()
parseInput message action (':':stuff) =
  case splitOn " " stuff of
    (x:xs) ->
      maybe
        (parseFailed action "unrecognized command")
        (\cmd -> cmd (unwords xs) >> doItAgain message action)
        (lookup x commands)
    _ -> parseFailed action "unrecognized command"
parseInput _ action line = action line


parseFailed :: (String -> IO ()) -> String -> IO ()
parseFailed action message = do
  let
    output = unlines
      [ "Unrecognized Input"
      , message
      , "Try again"
      ]
  doItAgain output action


doItAgain :: String -> (String -> IO ()) -> IO ()
doItAgain message action = putStrLn message >> getLine >>= parseInput message action


translateOne :: String -> IO ()
translateOne resp = fmap (map entityVal) $ runSqlite database $ do
  selector <- selectWhere resp
  selectList [VocabOriginal ==. selector] [] =<< selector


translationLoop :: IO ()
translationLoop  = doItAgain "Enter a word to translate" action
  where
    action resp = translateOne resp >> translationLoop


helpMenu :: IO ()
helpMenu =
  forM_ commandsWithHelp $ \((shorthand, _), message) ->
    putStrLn $ ':' : shorthand ++ "   " ++ message


askOne :: IO ()
askOne = do
  mvocab <- runSqlite database (selectFirst [] [LimitTo 1])
  case mvocab of
    Just evocab -> do
      let vocab = entityVal evocab
      value <- chineseValue <$> vocabValue vocab
      translation <- germanValue <$> vocabTranslation vocab
      let message = "What does " ++ value  ++ " mean?"
      doItAgain message $ \answer ->
        putStrLn ((if translation == answer then "correct"  else "sadly incorrect") ++ "  " ++ translation)
    Nothing -> error ""


askLoop :: IO ()
askLoop = askOne >> askLoop


addVocab :: String -> [String] -> IO ()
addVocab voc trans = void $ runSqlite database $ do
  time <- getCurrentTime
  cid <- Sqlite.insert $ Chinese voc
  gids <- forM trans $ Sqlite.insert . German
  void $ forM gids $ \germ -> Sqlite.insert cid germ time


enterOne :: String -> IO ()
enterOne new =
  case words new of
    (vocab:"-":trans) -> addVocab vocab trans
    _ -> doItAgain "you need to provide at least one translation" enterOne

enterLoop :: IO ()
enterLoop = do
  doItAgain "Enter a new word-translation pair to store" enterOne
  enterLoop


deleteOne :: String -> IO ()
deleteOne word =
  case words word of
    (xa:"-":xs) -> void $ runSqlite database $ do
      allT <- catMaybes <$> forM xs selectBy
      deleteWhere [VocabTranslation <-. allT]
    [x] -> void $ runSqlite database $
      Sqlite.selectBy x >>=
        maybe
          (return ())
          (deleteWhere . (:[]) (VocabValue ==.))



main :: IO ()
main = do
  runSqlite "vocab.db" $ runMigration migrateAll
  mainMenu
