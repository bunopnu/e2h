%%%-----------------------------------------------------------------------------
%%% @doc HTML generator for Erlang ecosystem.
%%%
%%% `e2h' is an Erlang module designed to generate HTML within the Erlang
%%% ecosystem, allowing Erlang developers to efficiently create HTML documents
%%% and elements from structured data.
%%%
%%% @author bunopnu
%%% @end
%%%-----------------------------------------------------------------------------
-module(e2h).

-export([render_html/1, render_fragment/1, escape/1]).
-export_type([attributes/0, elements/0]).

%%%=============================================================================
%%% Public types
%%%=============================================================================

-type attributes() :: [{binary(), binary()} | binary()].
%% Represents a list of HTML attribute-value pairs.
%%
%% == Example ==
%%
%% ```
%% Attributes = [{<<"type">>, <<"password">>}, {<<"id">>, <<"my-element">>}, <<"disabled">>].
%% % Represents attributes like: type="password" id="my-element" disabled
%% '''
%%

-type elements() :: [{binary(), attributes(), elements()} | {binary(), attributes()} | binary()].
%% Represents structured HTML elements or raw content.
%%
%% == Example ==
%%
%% ```
%% Elements = [
%%  {<<"div">>, [{<<"class">>, <<"container">>}], [
%%    {<<"p">>, [], [<<"This is a paragraph.">>]},
%%    {<<"a">>, [{<<"href">>, <<"#">>}], [<<"Click me">>]},
%%    {<<"img">>, [{<<"src">>, <<"https://example.com/image.png">>}]}
%%  ]}
%% ].
%% '''
%%

%%%=============================================================================
%%% Public functions
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Similar to {@link render_fragment/1}, with a DOCTYPE declaration.
%% @end
%%------------------------------------------------------------------------------
-spec render_html(elements()) -> binary().
render_html(Elements) when is_list(Elements) ->
    encode_elements(Elements, <<"<!DOCTYPE html>\n">>).


%%------------------------------------------------------------------------------
%% @doc Renders a list of structure into a binary representation of an
%% HTML document.
%%
%% == Parameters ==
%% `Elements' - A list of structure, conforming to the {@link elements()} type.
%%
%% == Returns ==
%% A binary representation of the HTML document.
%%
%% @end
%%------------------------------------------------------------------------------
-spec render_fragment(elements()) -> binary().
render_fragment(Elements) when is_list(Elements) ->
    encode_elements(Elements, <<>>).


%%------------------------------------------------------------------------------
%% @doc Escapes dangerous HTML characters within a binary data input.
%%
%% == Parameters ==
%% `Data' - The binary data to be escaped.
%%
%% == Returns ==
%% The escaped binary data.
%%
%% == Example ==
%%
%% ```
%% Unescaped = <<"Suspicious <script>alert(1)</script> document!">>,
%% Escaped = e2h:escape(Unescaped).
%% % <<"Suspicious &lt;script&gt;alert(1)&lt;/script&gt; document!">>
%% '''
%%
%% @end
%%------------------------------------------------------------------------------
-spec escape(binary()) -> binary().
escape(Data) when is_binary(Data) ->
    escape(Data, <<>>).


%%%=============================================================================
%%% Private functions
%%%=============================================================================


-spec escape(binary(), binary()) -> binary().
escape(<<$<, Tail/binary>>, Acc) ->
    escape(Tail, <<Acc/binary, "&lt;">>);
escape(<<$>, Tail/binary>>, Acc) ->
    escape(Tail, <<Acc/binary, "&gt;">>);
escape(<<$&, Tail/binary>>, Acc) ->
    escape(Tail, <<Acc/binary, "&amp;">>);
escape(<<$", Tail/binary>>, Acc) ->
    escape(Tail, <<Acc/binary, "&quot;">>);
escape(<<$', Tail/binary>>, Acc) ->
    escape(Tail, <<Acc/binary, "&#x27;">>);
escape(<<Head, Tail/binary>>, Acc) ->
    escape(Tail, <<Acc/binary, Head>>);
escape(<<>>, Acc) ->
    Acc.


-spec encode_attributes(attributes()) -> binary().
encode_attributes(Attributes) when is_list(Attributes) ->
    encode_attributes(Attributes, <<>>).


-spec encode_attributes(attributes(), binary()) -> binary().
encode_attributes([{Key, Value} | Tail], Acc) when is_binary(Key), is_binary(Value) ->
    encode_attributes(Tail, <<Acc/binary, $\s, Key/binary, "=\"", Value/binary, $">>);
encode_attributes([Key | Tail], Acc) when is_binary(Key) ->
    encode_attributes(Tail, <<Acc/binary, $\s, Key/binary>>);
encode_attributes([], Acc) ->
    Acc.


-spec encode_elements(elements()) -> binary().
encode_elements(Elements) when is_list(Elements) ->
    encode_elements(Elements, <<>>).


-spec encode_elements(elements(), binary()) -> binary().
encode_elements([Raw | Tail], Acc) when is_binary(Raw) ->
    encode_elements(Tail, <<Acc/binary, Raw/binary>>);
encode_elements([{Tag, Attributes, Value} | Tail], Acc) when is_binary(Tag) ->
    EncodedAttributes = encode_attributes(Attributes),
    EncodedValue = encode_elements(Value),

    encode_elements(
      Tail,
      <<Acc/binary, $<, Tag/binary, EncodedAttributes/binary, $>, EncodedValue/binary, "</",
        Tag/binary, $>>>);
encode_elements([{Tag, Attributes} | Tail], Acc) when is_binary(Tag) ->
    EncodedAttributes = encode_attributes(Attributes),
    encode_elements(Tail, <<Acc/binary, $<, Tag/binary, EncodedAttributes/binary, " />">>);
encode_elements([], Acc) ->
    Acc.
