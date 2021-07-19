# applicative_parser
*parser combinators applicative TypeScript deno module*

The applicative approach to parsing has benefits compared to the monadic approach,
it separates the structure of the parser from the implementation. This allows different
implementations to be used (for example with or without backtracking, or error recovery)
without changing the use of the application of the parser. It also allows other algorithms
over the parser applications such as pretty-printing or symbol extraction.

These features cannot be implemented with a monadic parser because the second argument of
monadic-bind is a function, and hence opaque.

Key features:

- no external dependencies
- able to extract all valid symbols from parser
- allows pretty printing
- supports multiple evaluation strategies (backtracking vs non-backtracking etc)

Based on "Notes on Computing", "Applicative Parsing" in ML :

<https://jobjo.github.io/2019/05/19/applicative-parsing.html>

Using existential encoding from "Existential Quantification in TypeScript"
to encode the GADT used in "Applicative Parsing"

<https://unsafe-perform.io/posts/2020-02-21-existential-quantification-in-typescript>

Here is an example parser for floating point numbers:

```ts
const digit = OneOf('0123456789');
const float1 = seqMap((a, b) => [...a, b], many1(digit), OneOf('.'));
const float2 = seqMap((a, b) => [...a, ...b], float1, many1(digit));
const float3 = seqMap((a, b, c) => [...a, b, ...c], choice(float2, many1(digit)), OneOf('e'), many1(digit));
export const float: Parser<unknown,number> = FMap(a => parseFloat(a.join('')), choice(float3, float2, float1));
}
```
This parser can be compiled into a function:
```ts
const floatParser = parse(float);
```
`parse` is just one possible compiler, as the static parser definition (in this case float) does
not depend on the implementation of `parse`, you can replace `parse` with a different implementation
of the parser (say with no backtracking, or better error reporting) _without_ chaning the definiton
of `float` at all.

The supplied `parse` compiler produces a parser with the following type:
```ts
type Parse<A,B> = (_: {cs: string, pos: number, args: A}) => {result: B, cs: string, pos: number}|null;
```
So the parser can be applied to an input string as follows:
```ts
const parsed = floatParser({cs: '123.456e2', pos: 0});
```
The output `parsed` would then be:
```ts
{cs: '123.456e2', pos: 9, result: 12345.6}
```
`cs` is the complete input string, `pos` is the final position of the parser, and these can be used as the input to further parsers, alhough it would be better to build all the logic into a single parser as any needed logic can be
expressed using the parser combinators. A `null` will be returned if the parser failed.

Below is the result of `show(float)` but with the functions in the maps replaced with `?`, to show that the 
whole structure of the parser is static, and transparent (unlike with the monadic approach). The helper functions like
`choice` and `seqMap` are all represented by the combinators in the initial algebra.
```ts
FMap(?, (Either(FMap(?, (FMap(?, (Product(FMap(?, (Either(FMap(?, (FMap(?, (Product(FMap(?, (FMap(?, (FMap(?, (Product(FMap(?, (FMap(?, (Product(FMap(?, (OneOf('0123456789')), Fix(Either(FMap(?, (Product(FMap(?, (OneOf('0123456789')), Fail(''))), Return()))))), FMap(?, (Product(FMap(?, (OneOf('.')), Return())))))), FMap(?, (Product(FMap(?, (FMap(?, (Product(FMap(?, (OneOf('0123456789')), Fix(Either(FMap(?, (Product(FMap(?, (OneOf('0123456789')), Fail(''))), Return()))))), Return()))))), Either(FMap(?, (Product(FMap(?, (OneOf('0123456789')), Fix(Either(FMap(?, (Product(FMap(?, (OneOf('0123456789')), Fail(''))), Return())))), Fail('')))), FMap(?, (Product(FMap(?, (OneOf('e')), FMap(?, (Product(FMap(?, (FMap(?, (Product(FMap(?, (OneOf('0123456789')), Fix(Either(FMap(?, (Product(FMap(?, (OneOf('0123456789')), Fail(''))), Return()))))), Return()))))))), Either(FMap(?, (FMap(?, (Product(FMap(?, (FMap(?, (FMap(?, (Product(FMap(?, (FMap(?, (Product(FMap(?, (OneOf('0123456789')), Fix(Either(FMap(?, (Product(FMap(?, (OneOf('0123456789')), Fail(''))), Return()))))), FMap(?, (Product(FMap(?, (OneOf('.')), Return())))))), FMap(?, (Product(FMap(?, (FMap(?, (Product(FMap(?, (OneOf('0123456789')), Fix(Either(FMap(?, (Product(FMap(?, (OneOf('0123456789')), Fail(''))), Return()))))), Return()))))), Either(FMap(?, (FMap(?, (Product(FMap(?, (FMap(?, (Product(FMap(?, (OneOf('0123456789')), Fix(Either(FMap(?, (Product(FMap(?, (OneOf('0123456789')), Fail(''))), Return()))))), FMap(?, (Product(FMap(?, (OneOf('.')), Return()))))), Fail('')))))
```