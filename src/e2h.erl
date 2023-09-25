-module(e2h).

-export([encode_attributes/1, encode_elements/1]).

-type attributes() :: [{binary(), binary()}].
-type elements() :: [{binary(), attributes(), elements()} | binary()].

-spec encode_attributes(attributes()) -> iodata().
encode_attributes(Attributes) when is_list(Attributes) ->
    encode_attributes(Attributes, []).

-spec encode_attributes(attributes(), list()) -> iodata().
encode_attributes([], Acc) ->
    lists:reverse(Acc);
encode_attributes([{Key, Value} | Tail], Acc) ->
    encode_attributes(Tail, ["\"", Value, "=\"", Key, " " | Acc]).

-spec encode_elements(elements()) -> iodata().
encode_elements(Elements) ->
    encode_elements(Elements, []).

-spec encode_elements(elements(), list()) -> iodata().
encode_elements([], Acc) ->
    lists:reverse(Acc);
encode_elements([Raw | Tail], Acc) when is_binary(Raw) ->
    encode_elements(Tail, [Raw | Acc]);
encode_elements([{Tag, Attributes, Value} | Tail], Acc) ->
    EncodedValue = encode_elements(Value),
    EncodedAttributes = encode_attributes(Attributes),
    encode_elements(Tail, [">", Tag, "</", EncodedValue, ">", EncodedAttributes, Tag, "<" | Acc]).
