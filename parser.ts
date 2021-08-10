/**
 * Applicative Parser
 * @module parser
 */

//// deno-lint-ignore no-explicit-any
type Top = unknown;

type Fst<A> = A extends [infer B, infer C] ? B : never;
type Snd<A> = A extends [infer B, infer C] ? C : never;



type Try<A,B> = {tag: 'try', exists: <R>(cont: (_: {parser: Parser<A,B>}) => R) => R};
type Fail<A,B> = {tag: 'fail', exists: <R>(cont: (_: {fail: string}) => R) => R};
type Empty<A,B> = {tag: 'empty', exists: <R>(cont: () => R) => R};
type OneOf<A,B> = {tag: 'oneOf', exists: <R>(cont: (_: {oneOf: string}) => R) => R};
type Return<A,B> = {tag: 'return', exists: <R>(cont: (_: {result: (_:A) => B}) => R) => R};
type Forget<A,B> = {tag: 'forget', exists: <R>(cont: (_: {forget: Parser<A,B>}) => R) => R};
type RMap<A,B> = {tag: 'rmap', exists: <R>(cont: <C>(_: {map: (_:C) => B, parser: Parser<A,C>}) => R) => R};
type Cartesian<A,B> = {tag: 'cartesian', exists: <R>(cont: (_: {first: Parser<Fst<A>,Fst<B>>, second: Parser<Snd<A>,Snd<B>>}) => R) => R};
type Either<A,B> = {tag: 'either', exists: <R>(cont: (_: {left: Parser<A,B>, right: Parser<A,B>}) => R) => R};
type Fix<A,B> = {tag: 'fix', exists: <R>(cont: (_: {f: (_:Parser<A,B>) => Parser<A,B>}) => R) => R};
type Raw<A,B> = {tag: 'raw', exists: <R>(cont: (_: {f: Parse<A,B>}) => R) => R};
type LMap<A,B> = {tag: 'lmap', exists: <R>(cont: <C>(_: {map: (_a:A) => C, parser: Parser<C,B>}) => R) => R};
type Compose<A,B> = {tag: 'compose', exists: <R>(cont: <C>(_: {left: Parser<A,C>, right: Parser<C,B>}) => R) => R};
type First<A,B> = {tag: 'first', exists: <R>(cont: (_: {parser: Parser<Fst<A>, Fst<B>>}) => R) => R};
type Second<A,B> = {tag: 'second', exists: <R>(cont: (_: {parser: Parser<Snd<A>, Snd<B>>}) => R) => R};

/**
 * `Parser<A>` is the type of a parser combinator that returns a value with generic type `A`.
 */
export type Parser<A,B> =
    | Try<A,B>
    | Fail<A,B>
    | Empty<A,B>
    | OneOf<A,B>
    | Return<A,B>
    | Forget<A,B>
    | Either<A,B>
    | LMap<A,B>
    | RMap<A,B>
    | First<A,B>
    | Second<A,B>
    | Cartesian<A,B>
    | Compose<A,B>
    | Fix<A,B>
    | Raw<A,B>
    ;

// Primitive parsers

/**
 * `Try` backtracks the parser if it fails, so another option can be tried.
 */
export function Try<A,B>(parser: Parser<A,B>): Parser<A,B> {
    return {tag: 'try', exists: cont => cont({parser})};
}

/**
 * `Fail` parser consumes no input and always fails, with error message `fail`.
 */
export function Fail<A,B=undefined>(fail: string): Parser<A,B> {
    return {tag: 'fail', exists: cont => cont({fail})};
}

/**
 * `Empty` parser matches end-of-file or fails.
 */
export function Empty<A,B=null>(): Parser<A,B> {
    return {tag: 'empty', exists: cont => cont()};
}

/**
 * `OneOf` parser consumes one character if it is in the `oneOf` string argument or fails.
 */
export function OneOf<A,B=string>(oneOf: string): Parser<A,string> {
    return {tag: 'oneOf', exists: cont => cont({oneOf})};
}

/**
 * `Return` is constant parser that evalueates to `result` and cannot fail.
 */
export function Return<A,B>(result: (_:A) => B): Parser<A,B> {
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
 * `RMap` applies the `map` function to the result of running the `parser` argument.
 * 
 * This is where the constuction of any output data structures occurs, for example
 * building an abstract syntax tree node from the results of a parser.
 */
export function RMap<A,B,C>(map: (_:C) => B, parser: Parser<A,C>): Parser<A,B> {
    return {tag: 'rmap', exists: cont => cont({map, parser})};
}

/**
 * `Cartesian` runs the `first` parser, and then if it succeeds, runs the `second` parser.
 * The `first` and `second` parsers can be different types.
 * 
 * This allows the results of two parsers to be combined together into a single result.
 */
export function Cartesian<A,B,C,D>(first: Parser<A,C>, second: Parser<B,D>): Parser<[A,B], [C,D]> {
    return {tag: 'cartesian', exists: cont => cont({first, second})};
}

// Composition - alternative

/**
 * `Either` runs the `first` parser, and then if it fails, runs the `second` parser.
 * The `first` and `second` parsers must be the same type.
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
 * `LMap` applies `map` to the attributes passed from the parent node in the 
 * parser tree structure.
 */
export function LMap<A,B,C>(map: (_:A) => C, parser: Parser<C,B>): Parser<A,B> {
    return {tag: 'lmap', exists: cont => cont({map, parser})};
}

export function Compose<A,B,C>(left: Parser<A,C>, right: Parser<C,B>): Parser<A,B> {
    return {tag: 'compose', exists: cont => cont({left, right})};
}

export function First<A,B,C>(parser: Parser<A,B>): Parser<[A,C],[B,C]> {
    return {tag: 'first', exists: cont => cont({parser})};
}

export function Second<A,B,C>(parser: Parser<A,B>): Parser<[C,A],[C,B]> {
    return {tag: 'second', exists: cont => cont({parser})};
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
        case 'try':
            return parser.exists(p => `Try(${show(p.parser)})`);
        case 'forget':
            return parser.exists(p => `Forget(${show(p.forget)})`);
        case 'oneOf':
            return parser.exists(p => `OneOf('${p.oneOf}')`);
        case 'rmap':
            return parser.exists(p => `RMap(${p.map.toString().replace(/\s+/g, ' ')}, (${show(p.parser)})`);
        case 'cartesian':
            return parser.exists(p => `Cartesian(${show(p.first)}, ${show(p.second)})`);
        case 'compose':
            return parser.exists(p => `Compose(${show(p.left)}, ${show(p.right)})`);
        case 'either':
            return parser.exists(p => `Either(${show(p.left)}, ${show(p.right)})`);
        case 'fix':
            return parser.exists(p => `Fix(${show(p.f(Fail<A,B>('')))})`);
        case 'raw':
            return '';
        case 'lmap':
            return parser.exists(p => `LMap(${p.map.toString().replace(/\s+/g, ' ')}, (${show(p.parser)})`);
        case 'first':
            return parser.exists(p => `First(${show(p.parser)})`);
        case 'second':
            return parser.exists(p => `Second(${show(p.parser)})`);
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
        case 'try':
            return parser.exists(p => symbols(p.parser));
        case 'forget':
            return parser.exists(p => symbols(p.forget));
        case 'oneOf':
            return parser.exists(p => new Set(Array.from(p.oneOf)));
        case 'rmap':
            return parser.exists(p => symbols(p.parser));
        case 'cartesian':
            return parser.exists(p => new Set([...symbols(p.first), ...symbols(p.second)]));
        case 'compose':
            return parser.exists(p => new Set([...symbols(p.left), ...symbols(p.right)]));
        case 'either':
            return parser.exists(p => new Set([...symbols(p.left), ...symbols(p.right)]));
        case 'fix':
            return parser.exists(p => symbols<A,B>(p.f(Fix(_ => Fail('')))));
        case 'raw':
            return new Set();
        case 'lmap':
            return parser.exists(p => symbols(p.parser));
        case 'first':
            return parser.exists(p => symbols(p.parser));
        case 'second':
            return parser.exists(p => symbols(p.parser));
    }
}
   
//----------------------------------------------------------------------------

// deno-lint-ignore no-explicit-any
export function constant<A>(x:A): (_:any) => A {
    return _ => x;
}

export type Result<A> = {result?: A, cs: string, pos: number, errors: Error[]};
export function Result<A>(result: A|undefined, cs: string, pos: number, errors: Error[]): Result<A> {
    return {result, cs, pos, errors};
}

export type Error = {error: string, pos: number};
export function Error(error: string, pos: number): Error {
    return {error, pos};
}

/**
 * `Parse` is the type returned by the `parse` function, which takes an input string `cs`,
 * a position `pos`, and the inherited attributes `attr` passed to the root of the parser,
 * and returns the input string, the updated position, and the result of the 
 * parser. 
 */
export type Parse<A,B> = (_: {cs: string, pos: number, attr: A}) => Result<B>;

export function parse<A,B,P extends Parser<A,B>>(parser: P extends Try<A,B> ? Parser<A,B> : never): Parse<A,B>;
export function parse<A,B,P extends Parser<A,B>>(parser: P extends Fail<A,B> ? Parser<A,B> : never): Parse<A,B>;
export function parse<A,B,P extends Parser<A,B>>(parser: P extends Empty<A,B> ? Parser<A,B> : never): Parse<A,null>;
export function parse<A,B,P extends Parser<A,B>>(parser: P extends Return<A,B> ? Parser<A,B> : never): Parse<A,B>;
export function parse<A,B,P extends Parser<A,B>>(parser: P extends OneOf<A,B> ? Parser<A,B> : never): Parse<A,string>;
export function parse<A,B,P extends Parser<A,B>>(parser: P extends Forget<A,B> ? Parser<A,B> : never): Parse<A,B>;
export function parse<A,B,P extends Parser<A,B>>(parser: P extends RMap<A,B> ? Parser<A,B> : never): Parse<A,B>;
export function parse<A,B,P extends Parser<A,B>>(parser: P extends Cartesian<A,B> ? Parser<A,B> : never): Parse<[Fst<A>,Snd<A>],[Fst<B>,Snd<B>]>;
export function parse<A,B,P extends Parser<A,B>>(parser: P extends Compose<A,B> ? Parser<A,B> : never): Parse<A,B>;
export function parse<A,B,P extends Parser<A,B>>(parser: P extends Either<A,B> ? Parser<A,B> : never): Parse<A,B>;
export function parse<A,B,P extends Parser<A,B>>(parser: P extends Fix<A,B> ? Parser<A,B> : never): Parse<A,B>;
export function parse<A,B,P extends Parser<A,B>>(parser: P extends Raw<A,B> ? Parser<A,B> : never): Parse<A,B>;
export function parse<A,B,P extends Parser<A,B>>(parser: P extends LMap<A,B> ? Parser<A,B> : never): Parse<A,B>;
export function parse<A,B,P extends Parser<A,B>>(parser: P extends First<A,B> ? Parser<A,B> : never): Parse<[Fst<A>,Snd<A>],[Fst<B>, Snd<B>]>;
export function parse<A,B,P extends Parser<A,B>>(parser: P extends Second<A,B> ? Parser<A,B> : never): Parse<[Fst<A>,Snd<A>],[Fst<B>, Snd<B>]>;
/**
 * `parse` takes a statis parser-combinator Abstract Syntax Tree as its only argument, and compiles it
 * to a parser from input string and position, to parse result, input string, and position.
 * see `Parse` type for the details of the type of the resulting parser. 
 */
export function parse<A,B>(parser: Parser<A,B>): Parse<A,B|undefined|null|string|[Fst<B>,Snd<B>]> {
    switch (parser.tag) {
        case 'fail':
            return parser.exists(p => ({cs, pos}) => 
                Result(undefined, cs, pos, [Error(p.fail, pos)]));
        case 'empty':
            return parser.exists(() => ({cs, pos}) => (pos >= cs.length)
                ? Result(null, cs, pos, []) 
                : Result(undefined, cs, pos, [Error('expected end-of-file', pos)]));
        case 'return':
            return parser.exists(p => ({cs, pos, attr}) => 
                Result(p.result(attr), cs, pos, []));
        case 'oneOf':
            return parser.exists(p => ({cs, pos})=> {
                const result = cs.charAt(pos);
                return (result && p.oneOf.indexOf(result) >= 0) 
                    ? Result(result, cs, pos + 1, []) 
                    : Result(undefined, cs, pos, [Error(`expected one of "${p.oneOf}"`, pos)]);
            });
        case 'try':
            return parser.exists(p => {
                const ep = parse(p.parser);
                return state => {
                    const r = ep(state);
                    return (r.result === undefined) ? Result(r.result, state.cs, state.pos, []) : r;
                };
            });
        case 'forget':
            return parser.exists(p => {
                const ep = parse(p.forget);
                return state => {
                    const r = ep(state);
                    return Result(r.result, state.cs, state.pos, []); 
                };
            });
        case 'rmap':
            return parser.exists(p => {
                const ep = parse(p.parser);
                return state => {
                    const r = ep(state);
                    return Result((r.result !== undefined) ? p.map(r.result) : undefined, r.cs, r.pos, r.errors);
                };
            });
        case 'cartesian':
            return parser.exists(p => {
                const ep = parse(p.first);
                const eq = parse(p.second);
                return state => {
                    if (!Array.isArray(state.attr) || state.attr.length < 2) {
                        throw "this should never happen";
                    }
                    const first = ep({...state, attr: state.attr[0]})
                    if (first.result === undefined) {
                        return Result(undefined, first.cs, first.pos, first.errors);
                    }
                    const second = eq({...first, attr: state.attr[1]});
                    return Result((second.result !== undefined) ? [first.result, second.result] : undefined, second.cs, second.pos, first.errors.concat(second.errors));
                };
            });
        case 'compose':
            return parser.exists(p => {
                const ep = parse(p.left);
                const eq = parse(p.right);
                return state => {
                    const left = ep(state)
                    if (left.result === undefined) {
                        return Result(undefined, left.cs, left.pos, left.errors);
                    }
                    const right = eq({...left, attr: left.result});
                    return Result(right.result, right.cs, right.pos, left.errors.concat(right.errors));
                };
            });
        case 'either':
            return parser.exists(p => {
                const ep = parse(p.left);
                const eq = parse(p.right);
                return state => {
                    const left = ep(state);
                    if (left.result !== undefined || left.pos > state.pos) {
                        return left;
                    }
                    const right = eq(state);
                    return (right.result === undefined)
                        ? Result(right.result, right.cs, right.pos, left.errors.concat(right.errors))
                        : right;
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
        case 'lmap':
            return parser.exists(p => {
                const ep = parse(p.parser);
                return ({cs, pos, attr}) => ep({cs, pos, attr: p.map(attr)});
            });
        case 'first':
            return parser.exists(p => {
                const ep = parse(p.parser);
                return ({cs, pos, attr}) => {
                    if (!(Array.isArray(attr) && attr.length > 1)) {
                        throw 'this should never happen';
                    } 
                    const r = ep({cs, pos, attr: attr[0]});
                    return Result((r.result !== undefined) ? [r.result, attr[1]] : undefined, r.cs, r.pos, r.errors);
                };
            });
        case 'second':
            return parser.exists(p => {
                const ep = parse(p.parser);
                return ({cs, pos, attr}) => {
                    if (!(Array.isArray(attr) && attr.length > 1)) {
                        throw 'this should never happen';
                    }
                    const r = ep({cs, pos, attr: attr[1]});
                    return Result((r.result !== undefined) ? [attr[0], r.result]: undefined, r.cs, r.pos, r.errors);
                };
            });
    }
}

//----------------------------------------------------------------------------
// Useful functions

export function tuple<A extends unknown[]>(...x: A): A {
    return x;
}

export function maybe<A>(x?:A): A|null {
    return x ?? null as A|null;
}

export function singleton<A>(x:A): A[] {
    return [x];
}

export function id<A>(x:A): A {
    return x;
}

export function fst<A,B>([x,_] : [A,B]): A {
    return x;
}

export function snd<A,B>([_,x] : [A,B]): B {
    return x;
}

//----------------------------------------------------------------------------
// Useful parsers

/**
 * `opt` applies parser `p` and succeeds with default value `x` if it fails
 */
export function opt<A,B>(p: Parser<A,B>, x: B): Parser<A,B> {
    return trap(p, Return(_ => x));
}

export function trap<A,B>(left: Parser<A,B>, right: Parser<A,B>): Parser<A,B> {
    return Either(Try(left), right);
}

export function branch<A,B>(...ps: Array<Parser<A,B>>): Parser<A,B> {
    return ps.reduceRight((acc, x) => Either(x, acc), Fail(''));
}

/**
 * `choice` tries each parser in turn, returning the first success, or failing if none succeed.
 */
export function choice<A,B>(...ps: Array<Parser<A,B>>): Parser<A,B> {
    return ps.reduceRight((acc, x) => trap(x, acc), Fail(''));
}

// Infix operators, not implementable in typescript, use prefix forms:

// f <$> p = RMap(f, p)
// p <|> q = Either(p, q)
// pf <*> px = apply(pf, px)
// p *> q = first(p, q)
// p <* q = second(p, q)

/**
 * `apply` applies a higher-order parser to another parser.
 */
// deno-lint-ignore no-explicit-any
export function apply<A,B,C extends (_:B) => any>(fa: Parser<A,C>, xa: Parser<A,B>): Parser<A,C extends (_:B) => infer D ? D : never> {
    return LMap(x => tuple(x,x), RMap(([f, x]) => f(x), Cartesian(fa, xa)));
}

export function product<A,B,C,D>(f: Parser<A,B>, g: Parser<C,D>): Parser<A&C, [B,D]> { 
    return LMap(x => tuple(x,x), Cartesian(f, g));
}

/**
 * `first` applies both parsers, but only returns the result of the first.
 */
export function first<A,B,C,D>(fx: Parser<A,B>, fy: Parser<C,D>): Parser<A&C,B> {
    return RMap(([x,_]:[B,D]) => x, product(fx, fy));
}

/**
 * `second` applies both parsers, but only returns the result of the second.
 */
export function second<A,B,C,D>(fx: Parser<A, B>, fy: Parser<C,D>): Parser<A&C,D> {
    return RMap(([_,y]:[B,D]) => y, product(fx, fy));
}

//const listCons = <A>(t: A) => (ts: List<A>) => ts.unshift(t)

const arrayCons = <A>(t: A) => (ts: A[]) => [t, ...ts]

/**
 * `many` applies parser `p` zero or more times, returning an immutable list,
 * this cannot fail.
 */
export function many<A,B>(p: Parser<A,B>) {
    return Fix<A,B[]>(many => 
        Either(Try(apply(RMap(arrayCons, p), many)), Return<A,B[]>(_ => [])),
    );
}

/**
 * `many1` applies parser `p` one or more times, returning an immutable list,
 * this will fail if it does not succeed at least once.
 */
export function many1<A,B>(p: Parser<A,B>) {
    return apply(RMap(arrayCons, p), many(p));
}

/**
 * `between` applies parser `ps` then parser `p` then parser `pe`, but only returns the
 * result of parser `p`.
 */
export function between<A,B,C,D>(ps: Parser<A,B>, pe: Parser<A,C>, p: Parser<A,D>): Parser<A,D> {
    return second(ps, first(p, pe));
}

export function parens<A,B>(p:Parser<A,B>): Parser<A,B> {
    return between(token(OneOf('(')), token(OneOf(')')), p);
}

/**
 * `spaces` succeeds if there are one or more spaces, otherwise fails
 */
export function spaces<A>(): Parser<A, string[]> {
    return many1(OneOf('\n\r\t '));
}

/**
 * `optSpaces` accepts zero or more spaces, always succeeds.
 */
export function optSpaces<A>(): Parser<A, string[]> {
    return many(OneOf('\n\r\t '));
}

/**
 * `string` matches string `s` or fails.
 */
export function string<A>(s: string): Parser<A, string> {
    return RMap(cs => cs.join(''), Array.from(s).reduceRight((cs, c) => apply(RMap(arrayCons, OneOf(c)), cs), Return<A, string[]>(_ => [])));
}

export function except<A>(x: string): Parser<A, string> {
    return OneOf([...Array.from(Array(65536).keys(), y => String.fromCharCode(y))].filter(c => x.indexOf(c) < 0).join(''));
}

export function quotedString<A>(): Parser<A, string> {
    return token(between(string('"'), string('"'), except('"')));
}

/**
 * `string` matches string `token` and consumes any trailing spaces, or fails.
 */
export function token<A,B>(tok: Parser<A,B>): Parser<A,B> {
    return first(tok, optSpaces());
}

export function strtok<A>(x: string): Parser<A,string> {
    return first(string<A>(x), optSpaces<A>());
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
    return parsers.reduceRight((ps, p) => apply(RMap(tupleCons, p), ps), Return(_ => []));
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
export function seqMap<A,T extends Parser<any, any>[]>(map: (..._: [...UnwrapParsers<T>]) => A, ...parsers: T): Parser<UnwrapArg<T>, A> {
    return RMap(x => map(...x), seq(...parsers));
}

//----------------------------------------------------------------------------
// Example parsers

// integer

const digit = OneOf('0123456789');

/**
 * `integer` parses an integer number.
 */
export const integer: Parser<unknown,number> = RMap(cs => parseInt(cs.join('')), many1(digit));

// float
/*
function float(): Parser<number> {
    const aux = (m1: <A>(p: Parser<A>)
        => Parser<List<A>>, s: string, m2: <A>(p: Parser<A>) 
            => Parser<List<A>>) => RMap(
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
export const float = RMap(a => parseFloat(a.join('')), choice(float3, float2, float1));

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
const regularString = RMap(x => x.join(''), many1(regularChar));
const squotedString = RMap(x => x.join(''), between(quote, quote, many(choice(regularChar, space, leftParen, rightParen))));
const endNum = Forget(choice(Empty<unknown, string>(), space, rightParen)); 
const atom = choice(
    RMap(MkInt, first(integer, endNum)),
    RMap(MkFloat, first(float, endNum)),
    RMap(MkString, squotedString),
    RMap(MkSymbol, regularString),
);
const expr = (sexpr: Parser<unknown,SExp>) => trap(
    RMap(MkList, between(leftParen, rightParen, many(sexpr))),
    RMap(MkAtom, atom)
);
/**
 * `sexpr` parses an s-expression which can consist of an atom, integer,
 * float, or string, and lists of these types enclosed in parentheses.
 * See `SExp` and `Atom` for result structure.
 */
export const sexpr: Parser<unknown,SExp> = Fix(sexp => between(many(space), many(space), expr(sexp)));
