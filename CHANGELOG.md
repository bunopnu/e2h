# Changelog

This changelog documents all noteworthy changes in the project. The format adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html) and is inspired by [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## Next

## v0.4.0 - 7th October 2023

### Added

- New macro `eHTML/2` for simplifying the creation of an HTML document structure.

### Changed

- Renamed the `render/1` function to `render_fragment/1` for clarity.

## v0.3.2 - 2nd October 2023

### Added

- Support for [boolean attributes](https://html.spec.whatwg.org/multipage/common-microsyntaxes.html#boolean-attributes).

## v0.3.1 - 2nd October 2023

### Added

- Added macros for commonly used HTML tags.

## v0.3.0 - 27th September 2023

### Changed

- Renamed the old `render/1` function to `render_html/1` for clarity in naming.
- Introduced a new `render/1` function that returns HTML without the DOCTYPE declaration for flexibility in usage.

## v0.2.0 - 26th September 2023

### Added

- Allow passing atom for attributes and tags.

## v0.1.1 - 26th September 2023

### Added

- Support for self-closing tags.

## v0.1.0 - 25th September 2023

- Initial release of Fresh.
