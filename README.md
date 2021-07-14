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

export function float(): Parser<number> {
    const float1 = seqMap((a, b) => a.concat([b]), many1(digit), OneOf('.'));
    const float2 = seqMap((a, b) => a.concat(b), float1, many1(digit));
    const float3 = seqMap((a, b, c) => a.concat([b], c), choice(float2, many1(digit)), OneOf('e'), many1(digit));
    return FMap(a => parseFloat(a.join('')), choice(float3, float2, float1));
}
```
