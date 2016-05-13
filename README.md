# Manifold

Soon to be an FRP library inspired by Clojure's core.async, js-csp, and
observables.

## Planning

### API

- Channel
  - channel - create a channel
  - put - pushes a value into the channel
  - take - takes the next value from the channel
  - close - closes a channel
- Property
  - a channel with a current value
- Buffer
  - fixed - when full, puts will block
  - dropping - when full, puts are dropped
  - rolling - when full, oldest value is dropped
- Sequence
  - onto - puts values from an array into a channel
  - into - creates a channel containing a single array composed of values from another channel
  - reduce - reduces values from a channel into a channel containing a single value
  - collection - creates a channel containing the values of an array that is closed upon completion
- Flow
  - pipe - puts values from a source channel into a target channel
  - mult - puts values from a source channel to multiple target channels
  - split - creates a channel of filtered values and a channel of the remaining values
  - merge - creates a channel composed of values from all source channels
  - unique - creates a channel that drops consecutive duplicates

## Installation

Coming soon!

## Usage

Coming soon!
