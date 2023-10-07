# e2h

`e2h` is an Erlang module designed to generate HTML content within the Erlang. It provides functions for encoding HTML elements and attributes, as well as escaping suspicious characters within HTML content. This module is suitable for developers who need to programmatically generate HTML documents or elements in their Erlang applications.

[![Test Status](https://github.com/bunopnu/fresh/actions/workflows/test.yml/badge.svg)](https://github.com/bunopnu/fresh/actions/workflows/test.yml)
[![Coverage Status](https://coveralls.io/repos/github/bunopnu/e2h/badge.svg?branch=main)](https://coveralls.io/github/bunopnu/e2h)
[![Hex Version](https://img.shields.io/hexpm/v/e2h.svg)](https://hex.pm/packages/e2h)

## Installation

Package can be installed by adding `e2h` to your list of dependencies:

### With Rebar3

```erlang
{deps, [{e2h, "0.3.2"}]}.
```

### With Mix

```elixir
defp deps do
  [{:e2h, "~> 0.3.2"}]
end
```

## Usage Example

Here's a simple example of how to use `e2h` to generate an HTML document in Erlang:

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

e2h:render_fragment(Document).
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

For documentation, please refer to the official [HexDocs](https://hexdocs.pm/e2h) page.

## License

`e2h` is licensed under the MIT license.
