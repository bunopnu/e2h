# e2h

`e2h` is an Erlang module designed to generate HTML content within the Erlang. It provides functions for encoding HTML elements and attributes, as well as escaping suspicious characters within HTML content. This module is suitable for developers who need to programmatically generate HTML documents or elements in their Erlang applications.

<div>

<img src='https://github.com/bunopnu/fresh/actions/workflows/test.yml/badge.svg' alt='Test Status' /> 
<img src='https://coveralls.io/repos/github/bunopnu/e2h/badge.svg' alt='Coverage Status' />
<img src='https://img.shields.io/hexpm/v/e2h.svg' alt='Hex' />

</div>

## Installation

Package can be installed by adding `e2h` to your list of dependencies:

### Rebar3

```erlang
{deps, [{e2h, "0.3.0"}]}.
```

### Mix

```elixir
defp deps do
  [{:e2h, "~> 0.3.0"}]
end
```

## Example

```erlang
UserStatus = <<"busy">>,
UserProfileImage = <<"https://example.com/image.jpeg">>,
UserName = <<"joe">>,
UserJob = <<"data scientist">>,

Document = [
  {'div', [{class, <<"user">>}, {status, UserStatus}], [
    {img, [{href, UserProfileImage}]},
    {'div', [], [
      {h1, [], [UserName]},
      {p, [], [UserJob]}
    ]}
  ]}
],

e2h:render(Document).
% (output is manually formatted for readme)
%
% <div class="user" status="busy">
%   <img href="https://example.com/image.jpeg" />
%   <div>
%     <h1>joe</h1>
%     <p>data scientist</p>
%   </div>
% </div>
```

## Documentation

Please consult the [HexDocs](https://hexdocs.pm/e2h) for documentation.

## License

`e2h` is licensed under the MIT license.
