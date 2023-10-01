-module(e2h_tests).

-include_lib("stdlib/include/assert.hrl").

-include("include/html.hrl").

-export([escape_test/0, simple_render_test/0, html_macros_render_test/0]).

escape_test() ->
    Unescaped1 = <<"<script test=\"sus()\">alert(1)</script>">>,

    Escaped1 = e2h:escape(Unescaped1),
    Expected1 = <<"&lt;script test=&quot;sus()&quot;&gt;alert(1)&lt;/script&gt;">>,
    ?assertEqual(Expected1, Escaped1),

    Unescaped2 = <<"console.log('hello'); &lt;">>,

    Escaped2 = e2h:escape(Unescaped2),
    Expected2 = <<"console.log(&#x27;hello&#x27;); &amp;lt;">>,
    ?assertEqual(Expected2, Escaped2).

simple_render_test() ->
    Elements = [
        {<<"div">>, [{<<"class">>, <<"test">>}], [
            {h1, [], [<<"Hello, World!">>]},
            {img, [{src, <<"https://example.com/image.png">>}]}
        ]}
    ],

    Rendered1 = e2h:render(Elements),
    Expected1 =
        <<"<div class=\"test\"><h1>Hello, World!</h1><img src=\"https://example.com/image.png\" /></div>">>,
    ?assertEqual(Expected1, Rendered1),

    Rendered2 = e2h:render_html(Elements),
    Expected2 = <<"<!DOCTYPE html>\n", Expected1/binary>>,
    ?assertEqual(Expected2, Rendered2).

html_macros_render_test() ->
    Elements = [
        ?eDiv([
            ?eH1([<<"Hello, World!">>]),
            ?eSpan([
                ?eP([<<"Long Story...">>]),
                ?eA(<<"https://bun.rip">>, [<<"Visit My Website!">>])
            ]),
            <<"End of the content!">>
        ])
    ],

    Rendered1 = e2h:render(Elements),
    Expected1 =
        <<"<div><h1>Hello, World!</h1><span><p>Long Story...</p><a href=\"https://bun.rip\">Visit My Website!</a></span>End of the content!</div>">>,
    ?assertEqual(Expected1, Rendered1).
