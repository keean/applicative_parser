import {assertEquals} from './test_deps.ts';
import {parse, Fail, Empty, OneOf, Return, FMap, Product, Either, many, many1, between, symbols, show, token, integer, float, sexpr} from './parser.ts';

const empty = {cs: '', pos: 0};
const text = {cs: 'a b ab aa bb c', pos: 0};

Deno.test('fail parser', () => {
    assertEquals(
        parse(Fail('failure message'))(text),
        null
    );
});

Deno.test('empty parser', () => {
    assertEquals(
        parse(Empty())(empty),
        {...empty, result: undefined}
    );
    assertEquals(
        parse(Empty())(text),
        null
    );
});

Deno.test('one-of parser', () => {
    assertEquals(
        parse(OneOf('a'))(text),
        {...text, pos: 1, result: 'a'}
    );
    assertEquals(
        parse(OneOf('b'))(text),
        null
    );
});

Deno.test('return parser', () => {
    assertEquals(
        parse(Return('a'))(text),
        {...text, result: 'a'}
    );
});

Deno.test('fmap parser', () => {
    assertEquals(
        parse(FMap(() => ({}), OneOf('a')))(text),
        {...text, pos: 1, result: {}}
    );
    assertEquals(
        parse(FMap(() => ({}), OneOf('b')))(text),
        null
    );
    assertEquals(
        parse(FMap(c => c.length, OneOf('a')))(text),
        {...text, pos: 1, result: 1}
    )         
    assertEquals(
        parse(FMap(s => s.toUpperCase(), FMap(c => c + c, OneOf('a'))))(text),
        {...text, pos: 1, result: 'AA'}
    );
});

Deno.test('product parser', () => {
    assertEquals(
        parse(Product(token('a'), token('b')))(text),
        {...text, pos: 4, result: ['a', 'b']}
    );
    assertEquals(
        parse(Product(token('b'), token('b')))(text),
        null
    );
    assertEquals(
        parse(Product(token('a'), token('a')))(text),
        null
    );
    assertEquals(
        parse(Product(token('b'), token('a')))(text),
        null
    );
});

Deno.test('either parser', () => {
    assertEquals(
        parse(Either(OneOf('a'), OneOf('a')))(text),
        {...text, pos: 1, result: 'a'}
    );
    assertEquals(
        parse(Either(OneOf('a'), OneOf('b')))(text),
        {...text, pos: 1, result: 'a'}
    );
    assertEquals(
        parse(Either(OneOf('b'), OneOf('a')))(text),
        {...text, pos: 1, result: 'a'}
    );
    assertEquals(
        parse(Either(OneOf('b'), OneOf('b')))(text),
        null
    );
});

Deno.test('many parser', () => {
    assertEquals(
        parse(many(Either(token('a'), token('b'))))(text),
        {...text, pos: 13, result: ['a', 'b', 'a', 'b', 'a', 'a', 'b', 'b']}
    );
    assertEquals(
        parse(many(token('c')))(text),
        {...text, pos: 0, result: []}
    );
    assertEquals(
        parse(many1(Either(token('a'), token('b'))))(text),
        {...text, pos: 13, result: ['a', 'b', 'a', 'b', 'a', 'a', 'b', 'b']}
    );
    assertEquals(
        parse(many1(token('c')))(text),
        null
    );
});

Deno.test('between parser', () => {
    assertEquals(
        parse(between(OneOf('('), OneOf(')'), many(OneOf('a'))))({cs: '(aa)', pos: 0}),
        {cs: '(aa)', pos: 4, result: ['a', 'a']}
    );
})

Deno.test('integer parser', () => {
    assertEquals(
        parse(integer())({cs: '123456abc', pos: 0}),
        {cs: '123456abc', pos: 6, result: 123456}
    );
});

Deno.test('float parser', () => {
    assertEquals(
        parse(float())({cs: '123.', pos: 0}),
        {cs: '123.', pos: 4, result: 123}
    );
    assertEquals(
        parse(float())({cs: '123.456', pos: 0}),
        {cs: '123.456', pos: 7, result: 123.456}
    );
    assertEquals(
        parse(float())({cs: '123.456e2', pos: 0}),
        {cs: '123.456e2', pos: 9, result: 12345.6}
    )
    assertEquals(
        parse(float())({cs: '123e2', pos: 0}),
        {cs: '123e2', pos: 5, result: 12300}
    );
    assertEquals(
        parse(float())({cs: '123.e2', pos: 0}),
        {cs: '123.e2', pos: 4, result: 123}
    );
    assertEquals(
        parse(float())({cs: '123', pos: 0}),
        null
    );
    assertEquals(
        parse(float())({cs: '.123', pos: 0}),
        null
    );
    assertEquals(
        parse(float())({cs: '.456e2', pos: 0}),
        null
    );
});


Deno.test('s-expr parser', () => {
    assertEquals(
        parse(sexpr())({cs: 'one 2 3.0', pos: 0}),
        {cs: 'one 2 3.0', pos: 4, result: {
            tag: "atom",
            atom: {
                symbol: "one",
                tag: "symbol",
            },
        }}
    );
    assertEquals(
        parse(sexpr())({cs: '(one (2 two) 3.0)', pos: 0}),
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
        "map [([f, x]) => f(x))] (product (map [(t) => (ts) => [t, ...ts])] (either (oneOf [a]) (oneOf [b]))) (fix (either (map [([f, x]) => f(x))] (product (map [(t) => (ts) => [t, ...ts])] (either (oneOf [a]) (oneOf [b]))) ())) (return ))))"
    );
});
