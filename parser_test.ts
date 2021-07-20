import {assertEquals} from './test_deps.ts';
import {string, parse, Fail, Empty, OneOf, Return, FMap, Product, Either, many, many1, between, symbols, show, token, integer, float, sexpr, ArgMap} from './parser.ts';

const empty = {cs: '', pos: 0, args: {}};
const text = {cs: 'a b ab aa bb c', pos: 0, args: {}};

Deno.test('fail parser', () => {
    assertEquals(
        parse(Fail('failure message'))(text),
        null
    );
});

Deno.test('empty parser', () => {
    assertEquals(
        parse(Empty())(empty),
        {cs: '', pos: 0, result: undefined}
    );
    assertEquals(
        parse(Empty())(text),
        null
    );
});

Deno.test('one-of parser', () => {
    assertEquals(
        parse(OneOf('a'))(text),
        {cs: 'a b ab aa bb c', pos: 1, result: 'a'}
    );
    assertEquals(
        parse(OneOf('b'))(text),
        null
    );
});

Deno.test('return parser', () => {
    assertEquals(
        parse(Return('a'))(text),
        {cs: 'a b ab aa bb c', pos: 0, result: 'a'}
    );
});

Deno.test('fmap parser', () => {
    assertEquals(
        parse(FMap(() => ({}), OneOf('a')))(text),
        {cs: 'a b ab aa bb c', pos: 1, result: {}}
    );
    assertEquals(
        parse(FMap(() => ({}), OneOf('b')))(text),
        null
    );
    assertEquals(
        parse(FMap(c => c.length, OneOf('a')))(text),
        {cs: 'a b ab aa bb c', pos: 1, result: 1}
    )         
    assertEquals(
        parse(FMap(s => s.toUpperCase(), FMap(c => c + c, OneOf('a'))))(text),
        {cs: 'a b ab aa bb c', pos: 1, result: 'AA'}
    );
});

Deno.test('product parser', () => {
    assertEquals(
        parse(Product(token(string('a')), token(string('b'))))(text),
        {cs: 'a b ab aa bb c', pos: 4, result: ['a', 'b']}
    );
    assertEquals(
        parse(Product(token(string('b')), token(string('b'))))(text),
        null
    );
    assertEquals(
        parse(Product(token(string('a')), token(string('a'))))(text),
        null
    );
    assertEquals(
        parse(Product(token(string('b')), token(string('a'))))(text),
        null
    );
});

Deno.test('either parser', () => {
    assertEquals(
        parse(Either(OneOf('a'), OneOf('a')))(text),
        {cs: 'a b ab aa bb c', pos: 1, result: 'a'}
    );
    assertEquals(
        parse(Either(OneOf('a'), OneOf('b')))(text),
        {cs: 'a b ab aa bb c', pos: 1, result: 'a'}
    );
    assertEquals(
        parse(Either(OneOf('b'), OneOf('a')))(text),
        {cs: 'a b ab aa bb c', pos: 1, result: 'a'}
    );
    assertEquals(
        parse(Either(OneOf('b'), OneOf('b')))(text),
        null
    );
});

Deno.test('many parser', () => {
    assertEquals(
        parse(many(Either(token(string('a')), token(string('b')))))(text),
        {cs: 'a b ab aa bb c', pos: 13, result: ['a', 'b', 'a', 'b', 'a', 'a', 'b', 'b']}
    );
    assertEquals(
        parse(many(token(string('c'))))(text),
        {cs: 'a b ab aa bb c', pos: 0, result: []}
    );
    assertEquals(
        parse(many1(Either(token(string('a')), token(string('b')))))(text),
        {cs: 'a b ab aa bb c', pos: 13, result: ['a', 'b', 'a', 'b', 'a', 'a', 'b', 'b']}
    );
    assertEquals(
        parse(many1(token(string('c'))))(text),
        null
    );
});

Deno.test('between parser', () => {
    assertEquals(
        parse(between(OneOf('('), OneOf(')'), many(OneOf('a'))))({cs: '(aa)', pos: 0, args:{}}),
        {cs: '(aa)', pos: 4, result: ['a', 'a']}
    );
})

Deno.test('integer parser', () => {
    assertEquals(
        parse(integer)({cs: '123456abc', pos: 0, args:{}}),
        {cs: '123456abc', pos: 6, result: 123456}
    );
});

Deno.test('float parser', () => {
    assertEquals(
        parse(float)({cs: '123.', pos: 0, args:{}}),
        {cs: '123.', pos: 4, result: 123}
    );
    assertEquals(
        parse(float)({cs: '123.456', pos: 0, args:{}}),
        {cs: '123.456', pos: 7, result: 123.456}
    );
    assertEquals(
        parse(float)({cs: '123.456e2', pos: 0, args:{}}),
        {cs: '123.456e2', pos: 9, result: 12345.6}
    )
    assertEquals(
        parse(float)({cs: '123e2', pos: 0, args:{}}),
        {cs: '123e2', pos: 5, result: 12300}
    );
    assertEquals(
        parse(float)({cs: '123.e2', pos: 0, args:{}}),
        {cs: '123.e2', pos: 4, result: 123}
    );
    assertEquals(
        parse(float)({cs: '123', pos: 0, args:{}}),
        null
    );
    assertEquals(
        parse(float)({cs: '.123', pos: 0, args:{}}),
        null
    );
    assertEquals(
        parse(float)({cs: '.456e2', pos: 0, args:{}}),
        null
    );
});


Deno.test('s-expr parser', () => {
    assertEquals(
        parse(sexpr)({cs: 'one 2 3.0', pos: 0, args:{}}),
        {cs: 'one 2 3.0', pos: 4, result: {
            tag: "atom",
            atom: {
                symbol: "one",
                tag: "symbol",
            },
        }}
    );
    assertEquals(
        parse(sexpr)({cs: '(one (2 two) 3.0)', pos: 0, args:{}}),
        {cs: '(one (2 two) 3.0)', pos: 17, result: {
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
        }}
    );
});

Deno.test('symbol extraction', () => { 
    assertEquals(
        symbols(many1(Either(OneOf('a'), OneOf('b')))),
        new Set(['a', 'b'])
    );
});

Deno.test('show parser', () => { 
    assertEquals(
        show(many1(Either(OneOf('a'), OneOf('b')))),
        "FMap(([f, x]) => f(x), (Product(FMap((t) => (ts) => [t, ...ts], (Either(OneOf('a'), OneOf('b'))), Fix(Either(FMap(([f, x]) => f(x), (Product(FMap((t) => (ts) => [t, ...ts], (Either(OneOf('a'), OneOf('b'))), Fail(''))), Return()))))"
    );
});

Deno.test('AppMap', () => {
    assertEquals(
        parse(ArgMap(({n}:{n:number}) => ({n:n+1}), Return(0), FMap((m:number, {n}:{n:number}) => n+m, Return(1)))) ({cs: '', pos: 0, args: {n:1}}),
        {cs: '', pos:0, result: [0,3]}
    );
});