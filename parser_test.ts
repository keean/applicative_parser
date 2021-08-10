import {assertEquals} from './test_deps.ts';
import {Error, tuple, string, parse, Fail, Empty, OneOf, Return, RMap, product, many, many1, between, symbols, show, token, integer, float, sexpr, LMap, First, constant, Result, trap} from './parser.ts';

const empty = {cs: '', pos: 0, attr: {}};
const text = {cs: 'a b ab aa bb c', pos: 0, attr: {}};

Deno.test('fail parser', () => {
    assertEquals(
        parse(Fail('failure message'))(text),
        Result(undefined, 'a b ab aa bb c', 0, [Error("failure message", 0)])
    );
});

Deno.test('empty parser', () => {
    assertEquals(
        parse(Empty())(empty),
        Result(null, '', 0, [])
    );
    assertEquals(
        parse(Empty())(text),
        Result(undefined, 'a b ab aa bb c', 0, [Error('expected end-of-file', 0)])
    );
});

Deno.test('one-of parser', () => {
    assertEquals(
        parse(OneOf('a'))(text),
        Result('a', 'a b ab aa bb c', 1, [])
    );
    assertEquals(
        parse(OneOf('b'))(text),
        Result(undefined, 'a b ab aa bb c', 0, [Error('expected one of "b"',0)])
    );
});

Deno.test('return parser', () => {
    assertEquals(
        parse(Return(_ => 'a'))(text),
        Result('a', 'a b ab aa bb c', 0, [])
    );
});

Deno.test('fmap parser', () => {
    assertEquals(
        parse(RMap(() => ({}), OneOf('a')))(text),
        Result({}, 'a b ab aa bb c', 1, [])
    );
    assertEquals(
        parse(RMap(() => ({}), OneOf('b')))(text),
        Result(undefined, 'a b ab aa bb c', 0, [Error('expected one of "b"', 0)])
    );
    assertEquals(
        parse(RMap(c => c.length, OneOf('a')))(text),
        Result(1, 'a b ab aa bb c', 1, [])
    )         
    assertEquals(
        parse(RMap(s => s.toUpperCase(), RMap(c => c + c, OneOf('a'))))(text),
        Result('AA', 'a b ab aa bb c', 1, [])
    );
});

Deno.test('product parser', () => {
    assertEquals(
        parse(product(token(string('a')), token(string('b'))))(text),
        Result(['a', 'b'], 'a b ab aa bb c', 4, [])
    );
    assertEquals(
        parse(product(token(string('b')), token(string('b'))))(text),
        Result(undefined, 'a b ab aa bb c', 0, [Error('expected one of "b"', 0)])
    );
    assertEquals(
        parse(product(token(string('a')), token(string('a'))))(text),
        Result(undefined, 'a b ab aa bb c', 2, [Error('expected one of "a"', 2)])
    );
    assertEquals(
        parse(product(token(string('b')), token(string('a'))))(text),
        Result(undefined, 'a b ab aa bb c', 0, [Error('expected one of "b"', 0)])
    );
});

Deno.test('either parser', () => {
    assertEquals(
        parse(trap(OneOf('a'), OneOf('a')))(text),
        Result('a', 'a b ab aa bb c', 1, [])
    );
    assertEquals(
        parse(trap(OneOf('a'), OneOf('b')))(text),
        Result('a', 'a b ab aa bb c', 1, [])
    );
    assertEquals(
        parse(trap(OneOf('b'), OneOf('a')))(text),
        Result('a', 'a b ab aa bb c', 1, [])
    );
    assertEquals(
        parse(trap(OneOf('b'), OneOf('b')))(text),
        Result(undefined, 'a b ab aa bb c', 0, [Error('expected one of "b"', 0)])
    );
});

Deno.test('many parser', () => {
    assertEquals(
        parse(many(trap(token(string('a')), token(string('b')))))(text),
        Result(['a', 'b', 'a', 'b', 'a', 'a', 'b', 'b'], 'a b ab aa bb c', 13, [])
    );
    assertEquals(
        parse(many(token(string('c'))))(text),
        Result([], 'a b ab aa bb c', 0, [])
    );
    assertEquals(
        parse(many1(trap(token(string('a')), token(string('b')))))(text),
        Result(['a', 'b', 'a', 'b', 'a', 'a', 'b', 'b'], 'a b ab aa bb c', 13, [])
    );
    assertEquals(
        parse(many1(token(string('c'))))(text),
        Result(undefined, 'a b ab aa bb c', 0, [Error('expected one of "c"', 0)])
    );
});

Deno.test('between parser', () => {
    assertEquals(
        parse(between(OneOf('('), OneOf(')'), many(OneOf('a'))))({cs: '(aa)', pos: 0, attr:{}}),
        Result(['a', 'a'], '(aa)', 4, [])
    );
})

Deno.test('integer parser', () => {
    assertEquals(
        parse(integer)({cs: '123456abc', pos: 0, attr:{}}),
        Result(123456, '123456abc', 6, [])
    );
});

Deno.test('float parser', () => {
    assertEquals(
        parse(float)({cs: '123.', pos: 0, attr:{}}),
        Result(123, '123.', 4, [])
    );
    assertEquals(
        parse(float)({cs: '123.456', pos: 0, attr:{}}),
        Result(123.456, '123.456', 7, [])
    );
    assertEquals(
        parse(float)({cs: '123.456e2', pos: 0, attr:{}}),
        Result(12345.6, '123.456e2', 9, [])
    )
    assertEquals(
        parse(float)({cs: '123e2', pos: 0, attr:{}}),
        Result(12300, '123e2', 5, [])
    );
    assertEquals(
        parse(float)({cs: '123.e2', pos: 0, attr:{}}),
        Result(123, '123.e2', 4, [])
    );
    assertEquals(
        parse(float)({cs: '123.', pos: 0, attr:{}}),
        Result(123, '123.', 4, [])
    );
    assertEquals(
        parse(float)({cs: '.123', pos: 0, attr:{}}),
        Result(undefined, '.123', 0, [Error('', 0)])
    );
    assertEquals(
        parse(float)({cs: '.456e2', pos: 0, attr:{}}),
        Result(undefined, '.456e2', 0, [Error('', 0)])
    );
});


Deno.test('s-expr parser', () => {
    assertEquals(
        parse(sexpr)({cs: 'one 2 3.0', pos: 0, attr:{}}),
        Result({
            tag: "atom",
            atom: {
                symbol: "one",
                tag: "symbol",
            },
        }, 'one 2 3.0', 4, [])
    );
    assertEquals(
        parse(sexpr)({cs: '(one (2 two) 3.0)', pos: 0, attr:{}}),
        Result({
            tag: 'list',
            list: [{
                tag: "atom",
                atom: {
                    symbol: "one",
                    tag: "symbol",
                },
            },{
                tag: "list",
                list: [{
                    tag: "atom",
                    atom: {
                        int: 2,
                        tag: "int",
                    },
                },{
                    tag: "atom",
                    atom: {
                        symbol: "two",
                        tag: "symbol",
                    },
                }],
            },{
                tag: "atom",
                atom: {
                    float: 3,
                    tag: "float",
                },
            }]
        }, '(one (2 two) 3.0)', 17, [])
    );
});

Deno.test('symbol extraction', () => { 
    assertEquals(
        symbols(many1(trap(OneOf('a'), OneOf('b')))),
        new Set(['a', 'b'])
    );
});

Deno.test('show parser', () => { 
    assertEquals(
        show(many1(trap(OneOf('a'), OneOf('b')))),
        "LMap(x => tuple(x, x), (RMap(([f, x]) => f(x), (Cartesian(RMap((t) => (ts) => [t, ...ts], (Either(Try(OneOf('a')), OneOf('b'))), Fix(Either(Try(LMap(x => tuple(x, x), (RMap(([f, x]) => f(x), (Cartesian(RMap((t) => (ts) => [t, ...ts], (Either(Try(OneOf('a')), OneOf('b'))), Fail(''))))), Return(_ => []))))))"
    );
});

Deno.test('AppMap', () => {
    assertEquals(
        parse(LMap((x:number) => tuple(x,x), RMap(([m,n]) => n+m, First(Return(constant(1)))))) ({cs: '', pos: 0, attr: 1}),
        Result(2, '', 0, [])
    );
});
