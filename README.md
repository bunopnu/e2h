# e2h

`e2h` (Erlang to HTML) is an Erlang module designed to generate HTML content within the Erlang.
It provides functions for encoding HTML elements and attributes, as well as escaping dangerous characters within HTML content.

This module is suitable for developers who need to programmatically generate HTML documents or elements in their Erlang applications.

[![Test Status](https://github.com/bunopnu/e2h/actions/workflows/test.yml/badge.svg)](https://github.com/bunopnu/e2h/actions/workflows/test.yml)
[![Coverage Status](https://coveralls.io/repos/github/bunopnu/e2h/badge.svg?branch=main)](https://coveralls.io/github/bunopnu/e2h)
[![Hex Version](https://img.shields.io/hexpm/v/e2h.svg)](https://hex.pm/packages/e2h)

## Installation

Package can be installed by adding `e2h` to your list of dependencies:

```erlang
{deps, [{e2h, "0.4.2"}]}.
```

## Usage Example

Here's a simple example of how to use `e2h` to generate an HTML document in Erlang:

```erlang
UserStatus = <<"busy">>,
UserProfileImage = <<"https://example.com/image.jpeg">>,
UserName = <<"bob">>,
UserJob = <<"data scientist">>,

Document = [
  {<<"div">>, [{<<"class">>, <<"user">>}, {<<"status">>, UserStatus}], [
    {<<"img">>, [{<<"href">>, UserProfileImage}]},
    {<<"div">>, [], [
      {<<"h1">>, [], [UserName]},
      {<<"p">>, [], [UserJob]}
    ]}
  ]}
],

e2h:render_fragment(Document).
% (output is manually formatted for readme)
%
% <div class="user" status="busy">
%   <img href="https://example.com/image.jpeg" />
%   <div>
%     <h1>bob</h1>
%     <p>data scientist</p>
%   </div>
% </div>
```

## Documentation

For documentation, please refer to the official [HexDocs](https://hexdocs.pm/e2h) page.

## License

`e2h` is licensed under the MIT license.
