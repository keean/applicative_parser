# applicative_parser
*Typescript applicative parser combinators (deno module)*

Applicative approach to parsing has benefits compared to the monaic approach.
This cant be done because the second argument of monadic-bind is a function,
and hence opaque:

- able to extract all valid symbols from parser
- allows pretty printing
- supports multiple evaluation strategies (backtracking vs non-backtracking etc).

Based on "Notes on Computing", "Applicative Parsing" in ML :

<https://jobjo.github.io/2019/05/19/applicative-parsing.html>

Using existential encoding from "Existential Quantification in TypeScript"
to encode the GADT used in "Applicative Parsing"

<https://unsafe-perform.io/posts/2020-02-21-existential-quantification-in-typescript>

Here is an example parser for floating point numbers:

```ts
const digit = OneOf('0123456789');

function float(): Parser<number> {
    const float1 = FMap(([a,b]) => a.concat(List.of(b)), seq(many1(digit), OneOf('.')));
    const float2 = FMap(([a,b]) => a.concat(b), seq(float1, many1(digit)));
    const float3 = FMap(([a,b,c]) => a.concat(List.of(b), c), seq(
        choice(float2, many1(digit)), OneOf('e'), many1(digit)
    ));
    return FMap(a => parseFloat(a.join('')), choice(float3, float2, float1));
}
```


