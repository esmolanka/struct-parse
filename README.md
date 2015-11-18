# struct-parse

Error-reporting parser for unityped data structures (e.g. JSON). Aimed
to be convenient for parsing resource files, configs and other pieces
written by human.

Some examples of parsing and error-reporting:

```haskell

-- We'll use example.toml from Tom's Preston-Werner https://github.com/toml-lang/toml
ghci> :t parseFromFile
parseFromFile :: FilePath -> Data.TomlObject.TomlParser a -> IO (Either String a)

-- So let's read it, run parser and print results (or errors)
ghci> let example p = either putStrLn print =<< parseFromFile "example.toml" p


ghci> :browse Data.TomlObject
...
bool :: StructParser AnnotatedTomlObject Bool
int :: StructParser AnnotatedTomlObject GHC.Int.Int64
double :: StructParser AnnotatedTomlObject Double
string :: StructParser AnnotatedTomlObject Data.Text.Internal.Text
index :: Int -> StructParser AnnotatedTomlObject AnnotatedTomlObject
elems :: StructParser AnnotatedTomlObject [AnnotatedTomlObject]
key :: String -> StructParser AnnotatedTomlObject AnnotatedTomlObject
fields :: StructParser AnnotatedTomlObject (HashMap Text AnnotatedTomlObject)
context :: String -> StructParser a a
this :: StructParser a a
traversing :: (Traversable t) => StructParser a b -> StructParser (t a) (t b)
...

ghci> example $ key "database" >>> key "ports" >>> elems >>> traversing int
[8001,8001,8002]

ghci> example $ key "database" >>> key "ports" >>> elems >>> traversing bool
Failure:
in .database(Object): in .ports(Array): all of
{ at [0](Integer): expected Bool, got Integer
, at [1](Integer): expected Bool, got Integer
, at [2](Integer): expected Bool, got Integer
}

ghci> example $ key "servers" >>> fields >>> traversing (key "ip" >>> string)
fromList [("alpha","10.0.0.1"),("beta","10.0.0.2")]

ghci> :{
ghci| example $ (,)
ghci|   <$> (key "owner" >>> key "name" >>> string)
ghci|   <*> (key "products" >>> elems >>> traversing (key "name" >>> string))
ghci| :}
("Tom Preston-Werner",["Hammer","Nail"])

```
