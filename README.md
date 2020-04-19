# json-parser
A JSON parser written in Haskell from scratch using no dependencies. Done mostly as an educational exercise to learn about [parser combinators](https://en.wikipedia.org/wiki/Parser_combinator).

### Usage

Compiling:
```shell
$ ghc json_parser.hs
```

Running:
```shell
$ echo '[{ "foo": "bar", "num": 5 }, null, true]' | json_parser
Just (JArray [JObject [("foo",JString "bar"),("num",JNumber 5)],JNull,JBool True])
$ echo '{Invalidjson' | json_parser
Nothing
```
