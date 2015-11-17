# struct-parse

Error-reporting parser for unityped data structures (e.g. JSON). Aimed
to be convenient for parsing resource files, configs and other pieces
written by human.

Some examples of parsing and error-reporting:

```haskell
-- Function test reads and parses "example.toml"

Test> test $ key "database" >>> key "ports" >>> elems >>> traversing int
[8001,8001,8002]

Test> test $ key "database" >>> key "ports" >>> elems >>> traversing bool
Failure:
in .database(Object): in .ports(Array): all of
{ at [0](Integer): expected Bool, got Integer
, at [1](Integer): expected Bool, got Integer
, at [2](Integer): expected Bool, got Integer
}

Test> test $ key "servers" >>> fields >>> traversing (key "ip" >>> string)
fromList [("alpha","10.0.0.1"),("beta","10.0.0.2")]

```
