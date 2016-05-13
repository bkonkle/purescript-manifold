# Manifold

An FRP-like state management library built on [Signal][signal]. Inspired by [Elm][elm], [Signal.Loop][signal-loop], [RxState][rx-state], and [Redux][redux].

Manifold uses a Signal of actions to trigger updates to the application state, and provides a Signal of state that your application can react to. Side effects are handled through async effects that yield lists of actions, which are then piped into the action channel.

## Installation

    $ bower install --save purescript-manifold

## Usage

Coming soon!

## Todo

* Tests!
* Middleware?

[signal]: https://github.com/bodil/purescript-signal
[signal-loop]: https://github.com/paf31/purescript-signal-loop
[elm]: http://package.elm-lang.org/packages/elm-lang/core/2.1.0/Signal
[rx-state]: https://github.com/jasonzoladz/purescript-rx-state
[redux]: http://redux.js.org/
