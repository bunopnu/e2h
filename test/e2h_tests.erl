-module(e2h_tests).

-include_lib("stdlib/include/assert.hrl").

-export([escape_test/0, simple_render_test/0]).

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
            {<<"h1">>, [], [<<"Hello, World!">>]}
        ]}
    ],

    Rendered = e2h:render(Elements),
    Expected = <<"<!DOCTYPE html>\n<div class=\"test\"><h1>Hello, World!</h1></div>">>,
    ?assertEqual(Expected, Rendered).
