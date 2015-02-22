module Builder where

data Foo  = Foo  deriving (Show)
data Bar  = Bar  deriving (Show)
data Quux = Quux deriving (Show)
data Gorp = Gorp deriving (Show)

data Config = Config
                { a :: Foo
                , b :: Bar
                , c :: Quux
                , d :: Maybe Gorp
                } deriving (Show)


-- Option 1: Use the Config ctor to build Config
--
-- Pros:
--   Statically checked
--   Easy as pie
--   Error messages are reasonable
-- Cons:
--   Have to create another function to reorder parameters
--   Have to create another function to overwrite values
funcCtor :: Foo -> Bar -> Quux -> Maybe Gorp -> Config
funcCtor = Config




-- Option 2: Phantom types
--
-- Pros:
--   Statically typed
--   Easy to rotate parameters
--   Easy to overwrite parameters
-- Cons:
--   O(n) phantom types where n is the number of necessary fields
--   O(f) functions where f is the number of fields
--   Each function requires a type signature
--   Intermediate type with conversions
--   Ugly error messages
data ConfigBuilder f b q = ConfigBuilder
                             { ma :: Maybe Foo
                             , mb :: Maybe Bar
                             , mc :: Maybe Quux
                             , md :: Maybe Gorp
                             } deriving (Show)

defaultConfig :: ConfigBuilder () () ()
defaultConfig = ConfigBuilder Nothing Nothing Nothing Nothing

withFoo :: Foo -> ConfigBuilder f b q -> ConfigBuilder Foo b q
withFoo f cb = cb{ma = Just f}

withBar :: Bar -> ConfigBuilder f b q -> ConfigBuilder f Bar q
withBar r cb = cb{mb = Just r}

withQuux :: Quux -> ConfigBuilder f b q -> ConfigBuilder f b Quux
withQuux q cb = cb{mc = Just q}

withGorp :: Gorp -> ConfigBuilder f b q -> ConfigBuilder f b q
withGorp g cb = cb{md = Just g}


buildConfig :: (ConfigBuilder () () () -> ConfigBuilder Foo Bar Quux) -> Config
buildConfig f =
    let ConfigBuilder
          { ma = Just foo
          , mb = Just bar
          , mc = Just quux
          , md = gorp
          } = f defaultConfig
    in Config{a = foo, b = bar, c = quux, d = gorp}
