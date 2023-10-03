SHELL := /bin/bash

json_parser: json_parser.hs
	ghc json_parser.hs

test: json_parser
	diff \
		<(echo '[{ "foo": "bar", "num": -5 }    , null,true  ]' | ./json_parser) \
		<(echo -n "Just (JArray [JObject [(\"foo\",JString \"bar\"),(\"num\",JNumber (-5))],JNull,JBool True])") \
		&& echo "All good"
