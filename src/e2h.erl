-module(e2h).

-export([encode_attributes/1, encode_elements/1]).

-type attributes() :: [{binary(), binary()}].
-type elements() :: [{binary(), attributes(), elements()} | binary()].

-spec encode_attributes(attributes()) -> binary().
encode_attributes(Attributes) when is_list(Attributes) ->
    encode_attributes(Attributes, <<>>).

-spec encode_attributes(attributes(), binary()) -> binary().
encode_attributes([{Key, Value} | Tail], Acc) when is_binary(Key), is_binary(Value) ->
    EncodedAttribute = <<" ", Key/binary, "=\"", Value/binary, "\"">>,
    encode_attributes(Tail, <<Acc/binary, EncodedAttribute/binary>>);
encode_attributes([], Acc) ->
    Acc.

-spec encode_elements(elements()) -> binary().
encode_elements(Elements) when is_list(Elements) ->
    encode_elements(Elements, <<>>).

-spec encode_elements(elements(), binary()) -> binary().
encode_elements([Raw | Tail], Acc) when is_binary(Raw) ->
    encode_elements(Tail, <<Acc/binary, Raw/binary>>);
encode_elements([{Tag, Attributes, Value} | Tail], Acc) when is_binary(Tag) ->
    EncodedValue = encode_elements(Value),
    EncodedAttributes = encode_attributes(Attributes),
    EncodedElement =
        <<"<", Tag/binary, EncodedAttributes/binary, ">", EncodedValue/binary, "</", Tag/binary,
            ">">>,
    encode_elements(Tail, <<Acc/binary, EncodedElement/binary>>);
encode_elements([], Acc) ->
    Acc.
