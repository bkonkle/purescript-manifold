# Manifold

An FRP-like state management library built on [Signal](signal) and [Aff](aff). Inspired by [Elm](elm), [Pux](pux), and [RxState](rx-state).

Manifold uses a Signal of actions to trigger updates to the application state, and provides a Signal of state that your application can react to. Side effects are handled through async effects that yield lists of actions, which are then piped into the action channel.

## Installation

    $ bower install --save purescript-manifold

## Usage

Coming soon!

## Todo

* Tests!
* Middleware?

[signal]: https://github.com/bodil/purescript-signal
[aff]: https://github.com/slamdata/purescript-aff
[elm]: http://elm-lang.org/guide/reactivity#signals
[pux]: https://github.com/alexmingoia/purescript-pux
[rx-state]: https://github.com/jasonzoladz/purescript-rx-state
