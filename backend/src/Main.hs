module Main where

import Options.Applicative

import qualified Pixit
import qualified Game.WSGame.Engine as Engine

data Options = Options
  { fnLanguage :: String
  , address  :: String
  , port :: Int
  }

options :: Parser Options
options = Options
  <$> strArgument
    ( metavar "WORDS.TXT"
    <> help "wordlist"
    )
  <*> strOption
    ( long "address"
    <> short 'a'
    <> metavar "ADDRESS"
    <> value "0.0.0.0"
    <> help "address to listen on"
    )
  <*> option auto
    ( long "port"
    <> short 'p'
    <> metavar "PORT"
    <> value 8093
    <> help "port to listen on"
    )

parseOptions :: IO Options
parseOptions = execParser $
  info (options <**> helper)
    ( fullDesc
    <> header "Pixit backend"
    <> progDesc "Run a backend for Pixit."
    )

main :: IO ()
main = do
  Options{..} <- parseOptions
  initialState <- Pixit.mkInitialState fnLanguage

  putStrLn $ "starting the backend at " ++ address ++ ":" ++ show port
  Engine.runGame address port initialState () Pixit.game
