/**
 * Applicative Parser
 * @module parser
 */

type Left<A> = A extends [infer B, infer C] ? B : never;
type Right<A> = A extends [infer B, infer C] ? C : never;

type Fail<A,B> = {tag: 'fail', exists: <R>(cont: (_: {fail: string}) => R) => R};
type Empty<A,B> = {tag: 'empty', exists: <R>(cont: () => R) => R};
type OneOf<A,B> = {tag: 'oneOf', exists: <R>(cont: (_: {oneOf: string}) => R) => R};
type Return<A,B> = {tag: 'return', exists: <R>(cont: (_: {result: B}) => R) => R};
type Forget<A,B> = {tag: 'forget', exists: <R>(cont: (_: {forget: Parser<A,B>}) => R) => R}
type FMap<A,B> = {tag: 'map', exists: <R>(cont: <C>(_: {map: (_r:C,_a:A) => B, parser: Parser<A,C>}) => R) => R};
type Product<A,B> = {tag: 'product', exists: <R>(cont: (_: {left: Parser<A,Left<B>>, right: Parser<A,Right<B>>}) => R) => R};
type Either<A,B> = {tag: 'either', exists: <R>(cont: (_: {left: Parser<A,B>, right: Parser<A,B>}) => R) => R};
type Fix<A,B> = {tag: 'fix', exists: <R>(cont: (_: {f: (_:Parser<A,B>) => Parser<A,B>}) => R) => R};
type Raw<A,B> = {tag: 'raw', exists: <R>(cont: (_: {f: Parse<A,B>}) => R) => R};
type ArgMap<A,B> = {tag: 'argmap', exists: <R>(cont: <C>(_: {map: (_a:A, _r:Left<B>) => C, left: Parser<A,Left<B>>, right: Parser<C,Right<B>>}) => R) => R};

/**
 * `Parser<A>` is the type of a parser combinator that returns a value with generic type `A`.
 */
export type Parser<A,B> =
    | Fail<A,B>
    | Empty<A,B>
    | OneOf<A,B>
    | Return<A,B>
    | Forget<A,B>
    | FMap<A,B>
    | Product<A,B> 
    | Either<A,B>
    | Fix<A,B> 
    | Raw<A,B>
    | ArgMap<A,B>
    ;

// Primitive parsers

/**
 * `Fail` parser consumes no input and always fails, with error message `fail`.
 */
export function Fail<A,B>(fail: string): Parser<A,B> {
    return {tag: 'fail', exists: cont => cont({fail})};
}

/**
 * `Empty` parser matches end-of-file or fails.
 */
export function Empty<A,B>(): Parser<A, B> {
    return {tag: 'empty', exists: cont => cont()};
}

/**
 * `OneOf` parser consumes one character if it is in the `oneOf` string argument or fails.
 */
export function OneOf<A,B>(oneOf: string): Parser<A,string> {
    return {tag: 'oneOf', exists: cont => cont({oneOf})};
}

/**
 * `Return` is constant parser that evalueates to `result` and cannot fail.
 */
export function Return<A,B>(result: B): Parser<A,B> {
    return {tag: 'return', exists: cont => cont({result})};
}

/**
 * `Forget` runs the `forget` parser and returns its result without consuming any input.
 * This is used to implement lookahead.
 */
export function Forget<A,B>(forget: Parser<A,B>): Parser<A,B> {
    return {tag: 'forget', exists: cont => cont({forget})};
}

// Composition: applicative

/**
 * `FMap` applies the `map` function to the result of running the `parser` argument.
 * 
 * This is where the constuction of any output data structures occurs, for example
 * building an abstract syntax tree node from the results of a parser.
 */
export function FMap<A,B,C>(map: (_r:C,_a:A) => B, parser: Parser<A,C>): Parser<A,B> {
    return {tag: 'map', exists: cont => cont({map, parser})};
}

/**
 * `Product` runs the `left` parser, and then if it succeeds, runs the `right` parser.
 * The `left` and `right` parsers can be different types.
 * 
 * This allows the results of two parsers to be combined together into a single result.
 */
export function Product<A,B,C>(left: Parser<A,B>, right: Parser<A,C>): Parser<A,[B,C]> {
    return {tag: 'product', exists: cont => cont({left, right})};
}

// Composition - alternative

/**
 * `Either` runs the `left` parser, and then if it fails, runs the `right` parser.
 * The `left` and `right` parsers must be the same type.
 */
export function Either<A,B>(left: Parser<A,B>, right: Parser<A,B>): Parser<A,B> {
    return {tag: 'either', exists: cont => cont({left, right})};
}

// Recursion

/**
 * `Fix` finds the fixed-point of the `f` parser argument.
 * Used to implement dynamic recursion.
 */
export function Fix<A,B>(f: (_:Parser<A,B>) => Parser<A,B>): Parser<A,B> {
    return {tag: 'fix', exists: cont => cont({f})};
}

function Raw<A,B>(f: Parse<A,B>): Parser<A,B> {
    return {tag: 'raw', exists: cont => cont({f})};
}

// Inherited (Parent) Attributes

/**
 * `ArgMap` applies `map` to the attributes passed from the parent node in the 
 * parser tree structure.
 */
export function ArgMap<A,B,C,D>(map: (_a:A, _r:D) => C, left: Parser<A,D>, right: Parser<C,B>): Parser<A,[D,B]> {
    return {tag: 'argmap', exists: cont => cont({map, left, right})};
}

//----------------------------------------------------------------------------

/**
 * `show` pretty prints the `parser` argument.
 */
export function show<A,B>(parser: Parser<A,B>): string {
    switch (parser.tag) {
        case 'fail' :
            return parser.exists(p => `Fail('${p.fail}')`);
        case 'empty':
            return 'Empty()';
        case 'return':
            return parser.exists(p => `Return(${p.result})`);
        case 'forget':
            return parser.exists(p => `Forget(${show(p.forget)})`);
        case 'oneOf':
            return parser.exists(p => `OneOf('${p.oneOf}')`);
        case 'map':
            return parser.exists(p => `FMap(${p.map.toString().replace(/\s+/g, ' ')}, (${show(p.parser)})`);
        case 'product':
            return parser.exists(p => `Product(${show(p.left)}, ${show(p.right)})`);
        case 'either':
            return parser.exists(p => `Either(${show(p.left)}, ${show(p.right)})`);
        case 'fix':
            return parser.exists(p => `Fix(${show(p.f(Fail<A,B>('')))})`);
        case 'raw':
            return '';
        case 'argmap':
            return parser.exists(p => `ArgMap(${p.map.toString().replace(/\s+/g, ' ')}, (${show(p.left)}), (${show(p.right)})`);
    }
}

//----------------------------------------------------------------------------

/**
 * `symbols` extracts all the valid symbold from the `parser` argument.
 */
export function symbols<A,B>(parser: Parser<A,B>): Set<string> {
    switch (parser.tag) {
        case 'fail':
        case 'empty':
        case 'return':
            return new Set();
        case 'forget':
            return parser.exists(p => symbols(p.forget));
        case 'oneOf':
            return parser.exists(p => new Set(Array.from(p.oneOf)));
        case 'map':
            return parser.exists(p => symbols(p.parser));
        case 'product':
            return parser.exists(p => new Set([...symbols(p.left), ...symbols(p.right)]));
        case 'either':
            return parser.exists(p => new Set([...symbols(p.left), ...symbols(p.right)]));
        case 'fix':
            return parser.exists(p => symbols<A,B>(p.f(Fix(_ => Fail('')))));
        case 'raw':
            return new Set();
        case 'argmap':
            return parser.exists(p => new Set([...symbols(p.left), ...symbols(p.right)]));
    }
}
   
//----------------------------------------------------------------------------

function constant<A,B>(x:A): (_:B) => A {
    return _ => x;
}

/**
 * `Parse` is the type returned by the `parse` function, which takes an input string `cs`,
 * a position `pos`, and the inherited attributes `args` passed to the root of the parser,
 * and returns the input string, the updated position, and the result of the 
 * parser. 
 */
export type Parse<A,B> = (_: {cs: string, pos: number, args: A}) => {result: B, cs: string, pos: number}|null;

export function parse<A,B,P extends Parser<A,B>>(parser: P extends Fail<A,B> ? Parser<A,B> : never): Parse<A,B>;
export function parse<A,B,P extends Parser<A,B>>(parser: P extends Empty<A,B> ? Parser<A,B> : never): Parse<A,undefined>;
export function parse<A,B,P extends Parser<A,B>>(parser: P extends Return<A,B> ? Parser<A,B> : never): Parse<A,B>;
export function parse<A,B,P extends Parser<A,B>>(parser: P extends OneOf<A,B> ? Parser<A,B> : never): Parse<A,string>;
export function parse<A,B,P extends Parser<A,B>>(parser: P extends Forget<A,B> ? Parser<A,B> : never): Parse<A,B>;
export function parse<A,B,P extends Parser<A,B>>(parser: P extends FMap<A,B> ? Parser<A,B> : never): Parse<A,B>;
export function parse<A,B,P extends Parser<A,B>>(parser: P extends Product<A,B> ? Parser<A,B> : never): Parse<A,[Left<B>,Right<B>]>;
export function parse<A,B,P extends Parser<A,B>>(parser: P extends Either<A,B> ? Parser<A,B> : never): Parse<A,B>;
export function parse<A,B,P extends Parser<A,B>>(parser: P extends Fix<A,B> ? Parser<A,B> : never): Parse<A,B>;
export function parse<A,B,P extends Parser<A,B>>(parser: P extends Raw<A,B> ? Parser<A,B> : never): Parse<A,B>;
export function parse<A,B,P extends Parser<A,B>>(parser: P extends ArgMap<A,B> ? Parser<A,B> : never): Parse<A,B>;
/**
 * `parse` takes a statis parser-combinator Abstract Syntax Tree as its only argument, and compiles it
 * to a parser from input string and position, to parse result, input string, and position.
 * see `Parse` type for the details of the type of the resulting parser. 
 */
export function parse<A,B>(parser: Parser<A,B>): Parse<A,B|undefined|string|[Left<B>,Right<B>]> {
    switch (parser.tag) {
        case 'fail':
            return parser.exists(() => constant(null));
        case 'empty':
            return parser.exists(() => ({cs, pos}) => (pos >= cs.length) ? {result: undefined, cs, pos} : null);
        case 'return':
            return parser.exists(p => ({cs, pos}) => ({cs, pos, result: p.result}));
        case 'oneOf':
            return parser.exists(p => ({cs, pos}) => {
                const result = cs.charAt(pos);
                return (result && p.oneOf.indexOf(result) >= 0) ? ({result, cs, pos: pos + 1}) : null;
            });
        case 'forget':
            return parser.exists(p => {
                const ep = parse(p.forget);
                return state => {
                    const r = ep(state);
                    return (r !== null) ? {cs: state.cs, pos: state.pos, result: r.result} : null;
                };
            });
        case 'map':
            return parser.exists(p => {
                const ep = parse(p.parser);
                return state => {
                    const r = ep(state);
                    return (r !== null) ? {...r, result: p.map(r.result, state.args)} : null;
                };
            });
        case 'product':
            return parser.exists(p => {
                const ep = parse(p.left);
                const eq = parse(p.right);
                return state => {
                    const left = ep(state) 
                    if (left !== null) {
                        const right = eq({cs: state.cs, pos: left.pos, args: state.args});
                        if (right !== null) {
                            return {...right, result: [left.result, right.result]};
                        }
                    }
                    return null;
                };
            });
        case 'either':
            return parser.exists(p => {
                const ep = parse(p.left);
                const eq = parse(p.right);
                return state => {
                    const left = ep(state);
                    return (left === null) ? eq(state) : left;
                };
            });
        case 'fix':
            return parser.exists(p => {
                const k: Parse<A,B> = parse(p.f(Raw(state => k(state))));
                return k;
            });
        case 'raw':
            return parser.exists(p => {
                return p.f;
            });
        case 'argmap':
            return parser.exists(p => {
                const ep = parse(p.left);
                const eq = parse(p.right);
                return state => {
                    const left = ep(state);
                    if (left !== null) {
                        const right = eq({cs: left.cs, pos: left.pos, args: p.map(state.args, left.result)});
                        if (right !== null) {
                            return {cs: right.cs, pos: right.pos, result: [left.result, right.result]};
                        }
                    } 
                    return null;
                };
            });
    }
}

//----------------------------------------------------------------------------
// Useful parsers

/**
 * `opt` applies parser `p` and succeeds with default value `x` if it fails
 */
export function opt<A,B>(p: Parser<A,B>, x: B): Parser<A,B> {
    return Either(p, Return(x));
}

/**
 * `choice` tries each parser in turn, returning the first success, or failing if none succeed.
 */
export function choice<A,B>(...ps: Array<Parser<A,B>>): Parser<A,B> {
    return ps.reduceRight((acc, x) => Either(x, acc), Fail(''));
}

// Infix operators, not implementable in typescript, use prefix forms:

// f <$> p = FMap(f, p)
// p <|> q = Either(p, q)
// pf <*> px = apply(pf, px)
// p *> q = first(p, q)
// p <* q = second(p, q)

/**
 * `apply` applies a higher-order parser to another parser.
 */
// deno-lint-ignore no-explicit-any
export function apply<A,B,C extends (_:B) => any>(fa: Parser<A,C>, xa: Parser<A,B>): Parser<A,C extends (_:B) => infer D ? D : never> {
    return FMap(([f, x]) => f(x), Product(fa, xa));
}

/**
 * `first` applies both parsers, but only returns the result of the first.
 */
export function first<A,B,C>(fx: Parser<A,B>, fy: Parser<A,C>): Parser<A,B> {
    return apply(FMap((x:B) => (_:C) => x, fx), fy);
}

/**
 * `second` applies both parsers, but only returns the result of the second.
 */
export function second<A,B,C>(fx: Parser<A,B>, fy: Parser<A,C>): Parser<A,C> {
    return apply(FMap((_:B) => (y:C) => y, fx), fy);
}

//const listCons = <A>(t: A) => (ts: List<A>) => ts.unshift(t)

const arrayCons = <A>(t: A) => (ts: A[]) => [t, ...ts]

/**
 * `many` applies parser `p` zero or more times, returning an immutable list,
 * this cannot fail.
 */
export function many<A,B>(p: Parser<A,B>) {
    return Fix<A,B[]>(many => 
        Either(apply(FMap(arrayCons, p), many), Return<A,B[]>([])),
    );
}

/**
 * `many1` applies parser `p` one or more times, returning an immutable list,
 * this will fail if it does not succeed at least once.
 */
export function many1<A,B>(p: Parser<A,B>) {
    return apply(FMap(arrayCons, p), many(p));
}

/**
 * `between` applies parser `ps` then parser `p` then parser `pe`, but only returns the
 * result of parser `p`.
 */
export function between<A,B,C,D>(ps: Parser<A,B>, pe: Parser<A,C>, p: Parser<A,D>): Parser<A,D> {
    return second(ps, first(p, pe));
}

/**
 * `spaces` succeeds if there are one or more spaces, otherwise fails
 */
// deno-lint-ignore no-explicit-any
export const spaces: Parser<any, string[]> = many1(OneOf('\n\r\t '));

/**
 * `optSpaces` accepts zero or more spaces, always succeeds.
 */
// deno-lint-ignore no-explicit-any
export const optSpaces: Parser<any, string[]> = many(OneOf('\n\r\t '));

/**
 * `string` matches string `s` or fails.
 */
// deno-lint-ignore no-explicit-any
export function string(s: string): Parser<any ,string> {
    // deno-lint-ignore no-explicit-any
    return FMap(cs => cs.join(''), Array.from(s).reduceRight((cs, c) => apply(FMap(arrayCons, OneOf(c)), cs), Return<any, string[]>([])));
}

/**
 * `string` matches string `token` and consumes any trailing spaces, or fails.
 */
export function token<A,B>(tok: Parser<A,B>): Parser<A,B> {
    return first(tok, optSpaces);
}

// deno-lint-ignore no-explicit-any
type UnwrapParser<T> = T extends Parser<any, infer U> ? U : T;

type UnwrapParsers<T extends unknown[]> = T extends [infer Head, ...infer Tail]
    ? [UnwrapParser<Head>, ...UnwrapParsers<Tail>] : [];

// deno-lint-ignore no-explicit-any
type UnwrapArg<T> = T extends Parser<infer U, any>[] ? U : unknown;

//type RemapParsers<T extends unknown[]> = Parser<any, UnwrapParsers<T>>;

//function tupleCons<T>(t: T): <TS extends unknown[]>(ts: TS) => readonly [T, ...TS] {
//    return ts => [t, ...ts] as const;
//}

const tupleCons: <T>(t: T) => <TS extends unknown[]>(ts: TS) => readonly [T, ...TS] = t => ts => [t, ...ts] as const;

/**
 * `seq` applies each parser in sequence and returns a tuple of their results if they all succeed,
 * or fails if any fail.
 */
// deno-lint-ignore no-explicit-any
export function seq<T extends Parser<any,any>[]>(...parsers: T): Parser<UnwrapArg<T>, UnwrapParsers<T>> {
    return parsers.reduceRight((ps, p) => apply(FMap(tupleCons, p), ps), Return([]));
}

/**
 * `seqMap` accepts a map function as the first argument, and then the remaining arguments are parsers.
 * If all the parsers succeed, the map function is called with the result of each parser as an
 * argument to the map function in the same order as the parsers.
 * 
 * This is used to implement the common pattern of calling a map function to construct an abstract
 * syntax tree node with the results of several parsers that must all succeed, where each parser can
 * return a different type.
 */
// deno-lint-ignore no-explicit-any
export function seqMap<A,T extends Parser<any, any>[]>(map: (..._: [...UnwrapParsers<T>, UnwrapArg<T>]) => A, ...parsers: T): Parser<UnwrapArg<T>, A> {
    return FMap((x, y) => map(...x, y), seq(...parsers));
}

//----------------------------------------------------------------------------
// Example parsers

// integer

const digit = OneOf('0123456789');

/**
 * `integer` parses an integer number.
 */
export const integer: Parser<unknown,number> = FMap(cs => parseInt(cs.join('')), many1(digit));

// float
/*
function float(): Parser<number> {
    const aux = (m1: <A>(p: Parser<A>)
        => Parser<List<A>>, s: string, m2: <A>(p: Parser<A>) 
            => Parser<List<A>>) => FMap(
                ([a,b,c]) => parseFloat(a.concat(List.of(b), c).join('')),
                seq(
                    m1(digit), 
                    OneOf(s), 
                    m2(digit)
                )
            );

    return choice(
        aux(many1, 'e', many1),
        aux(many1, '.', many),
        aux(many, '.', many1)
    );
}
*/

// float

const float1 = seqMap((a, b) => [...a, b], many1(digit) as Parser<number, string[]>, OneOf('.'));
const float2 = seqMap((a, b) => [...a, ...b], float1, many1(digit));
const float3 = seqMap((a, b, c) => [...a, b, ...c], choice(float2, many1(digit)), OneOf('e'), many1(digit));
/**
 * `float` parses a floating point number.
 */
export const float = FMap(a => parseFloat(a.join('')), choice(float3, float2, float1));

// S-Expression

/**
 * Type of `Atom` for example s-expression parser.
 */
export type Atom =
    | {tag: 'int', int: number}
    | {tag: 'float', float: number}
    | {tag: 'string', string: string}
    | {tag: 'symbol', symbol: string}
    ;

/**
 * Type of `SExp` for example s-expression parser.
 */
export type SExp =
    | {tag: 'atom', atom: Atom}
    | {tag: 'list', list: SExp[]}
        
const MkInt = (int: number): Atom => ({tag: 'int', int});
const MkFloat = (float: number): Atom => ({tag: 'float', float});
const MkString = (string: string): Atom => ({tag: 'string', string});
const MkSymbol = (symbol: string): Atom => ({tag: 'symbol', symbol});
const MkAtom = (atom: Atom): SExp => ({tag: 'atom', atom});
const MkList = (list: SExp[]): SExp => ({tag: 'list', list}); 
const leftParen = OneOf('(');
const rightParen = OneOf(')');
const quote = OneOf('"');
const space = OneOf('\n\r\t ');
const specialChar = '()\n\r\t\" ';
const isRegular = [...Array.from(Array(256).keys(), x => String.fromCharCode(x))].filter(c => specialChar.indexOf(c) < 0).join('');
const regularChar = OneOf(isRegular);
const regularString = FMap(x => x.join(''), many1(regularChar));
const quotedString = FMap(x => x.join(''), between(quote, quote, many(choice(regularChar, space, leftParen, rightParen))));
const endNum = Forget(choice(Empty<unknown, string>(), space, rightParen)); 
const atom = choice(
    FMap(MkInt, first(integer, endNum)),
    FMap(MkFloat, first(float, endNum)),
    FMap(MkString, quotedString),
    FMap(MkSymbol, regularString),
);
const expr = (sexpr: Parser<unknown,SExp>) => Either(
    FMap(MkList, between(leftParen, rightParen, many(sexpr))),
    FMap(MkAtom, atom)
);
/**
 * `sexpr` parses an s-expression which can consist of an atom, integer,
 * float, or string, and lists of these types enclosed in parentheses.
 * See `SExp` and `Atom` for result structure.
 */
export const sexpr: Parser<unknown,SExp> = Fix(sexp => between(many(space), many(space), expr(sexp)));
