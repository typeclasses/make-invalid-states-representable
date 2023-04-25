import Relude

import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Validation (Validation (..), validation)
import Options.Applicative qualified as Options
import Path (parseAbsFile, Path, Abs, File)

---

data Configuration = Configuration
  { inputs :: Set (Input (Path Abs File)),
    verbosity :: Verbosity
  }
  deriving stock (Eq, Ord, Show)

data Verbosity = Quiet | NormalOutput | Verbose
  deriving stock (Eq, Ord, Enum, Bounded, Show)

data Input filepath
  = StandardInput
  | FileInput filepath
  deriving stock (Eq, Ord, Show)

data Options = Options
  { inputOptions :: Set (Input Text),
    verbosityOptions :: Set Verbosity
  }
  deriving stock (Eq, Ord, Show)

---

parser :: Options.Parser Options
parser = do
  inputOptions <- inputParser
  verbosityOptions <- verbosityParser
  pure Options {..}

inputParser :: Options.Parser (Set (Input Text))
inputParser =
  fmap (foldl' Set.union Set.empty) $
    sequenceA
      [ Set.fromList <$> many inputFileParser,
        (fromList . toList) <$> standardInputParser
      ]

inputFileParser :: Options.Parser (Input Text)
inputFileParser =
  FileInput
    <$> Options.strOption @Text
      ( Options.long "input-file"
          <> Options.help
            "Absolute file path to read from \
            \(may be given more than once)"
      )

standardInputParser :: Options.Parser (Maybe (Input filepath))
standardInputParser =
  optional $
    Options.flag'
      StandardInput
      ( Options.long "standard-input"
          <> Options.help "Read from the standard input stream"
      )

verbosityParser :: Options.Parser (Set Verbosity)
verbosityParser =
  fromList
    <$> optionals
      [ Options.flag' Quiet $
          Options.long "quiet"
            <> Options.help "Do not print anything to stderr",
        Options.flag' Verbose $
          Options.long "verbose"
            <> Options.help "Print lots of extra info to stderr"
      ]

optionals :: Alternative f => Ord a => [f a] -> f [a]
optionals = fmap catMaybes . sequenceA . fmap optional

---

newtype Error = Error {errorMessage :: Text}
  deriving newtype (Eq, Ord, Show, IsString)

newtype Errors = Errors {errorSequence :: Seq Error}
  deriving newtype (Eq, Ord, Show, Semigroup)

oneError :: Error -> Validation Errors a
oneError e = Failure (Errors (Seq.singleton e))

---

configurationFromOptions ::
  Options ->
  Validation Errors Configuration
configurationFromOptions Options {..} = do
  inputs <- traverseSet inputFromOption inputOptions
  verbosity <- verbosityFromOptions verbosityOptions
  pure Configuration {..}

traverseSet :: (Applicative f, Ord b) => (a -> f b) -> Set a -> f (Set b)
traverseSet f = fmap fromList . traverse f . toList

inputFromOption ::
  Input Text ->
  Validation Errors (Input (Path Abs File))
inputFromOption = \case
  StandardInput -> Success StandardInput
  FileInput x -> case parseAbsFile (toString x) of
    Left e -> oneError (Error ("Not an absolute file path: " <> x))
    Right fp -> Success (FileInput fp)

verbosityFromOptions :: Set Verbosity -> Validation Errors Verbosity
verbosityFromOptions verbosityOptions =
  case Set.toAscList verbosityOptions of
    [] -> Success NormalOutput
    [x] -> Success x
    xs -> oneError (Error ("Too many verbosity options: " <> show xs))

main :: IO ()
main = do
  options <-
    Options.execParser $
      Options.info (parser <**> Options.helper) mempty
  configurationFromOptions options
    & validation printErrors printConfiguration

printConfiguration :: Configuration -> IO ()
printConfiguration = putOkayLine . show

printErrors :: Errors -> IO ()
printErrors = traverse_ (putErrorLine . errorMessage) . errorSequence

parserDemo :: [String] -> IO ()
parserDemo args = do
  case parseArgs args of
    Options.Success options ->
      configurationFromOptions options
        & validation printErrors printConfiguration
    x -> putErrorLine (show x)

parseArgs :: [String] -> Options.ParserResult Options
parseArgs =
  Options.execParserPure
    Options.defaultPrefs
    (Options.info parser mempty)

putOkayLine :: MonadIO m => Text -> m ()
putOkayLine x = putTextLn ("✅ " <> x)

putErrorLine :: MonadIO m => Text -> m ()
putErrorLine x = putTextLn ("🔥 " <> x)
