# Manifold

An FRP-like state management library built on [Signal][signal] and [Aff][aff]. Inspired by [Elm][elm], [Pux][pux], [RxState][rx-state], and [Redux][redux].

Manifold uses a Channel of actions to trigger updates to the application state, and provides a Signal of state that your application can react to. Side effects are handled through async effects that yield lists of actions, which are then piped into the action channel.

## Installation

    $ bower install --save purescript-manifold

## Usage

### Actions

To begin, you'll want to define some actions:

```purescript
data Action = SetName String | SetEmail String | ToggleBusy
```

This defines three type constructors: `SetName`, `SetEmail`, and `ToggleBusy`. `SetName` and `SetEmail` both take one argument - a string. `ToggleBusy` is a constructor that takes no arguments. You can think of these like "Action Creators" from the world of [Redux][redux].

### State

Next, you'll want to define your state shape. I recommend using `newtype` for this, so that it's easy to use generic deriving for things like rendering it as a string for debugging, or comparing state for equality.

```purescript
newtype State = State { name :: Maybe String, email :: Maybe String, busy :: Boolean }
```

This defines a constructor - State - that takes a record with fields for a name, email, and busy status. `Maybe String` is used to indicate that there may be a missing value there. Since the `Boolean` type declared for the "busy" field isn't wrapped in a `Maybe`, we know that this value is required.

### Updates

Next, you'll want to write an Update function to handle your Actions. You can think of this like a "Reducer" from the world of [Redux][redux]. Use pattern matching to match Actions and pull out the arguments passed to the constructor.

```purescript
update :: Update Action State
update (SetName name) (State state) = State $ state { name = Just name }
update (SetEmail email) (State state) = State $ state { email = Just email }
update ToggleBusy (State state) = State $ state { busy = not state.busy }
```

### Store

With this, you're ready to create and run your Store! Take a look at the type signature for `Manifold.runStore`. It takes your Update function and an initial State, and returns a synchronous side effect (`Eff`) that yields the Store. You can use this inside a monadic `do` expression to obtain the Store. This is a record which includes `stateSignal` - a Signal representing the State over time - and `actionChannel` - a Channel to `send` Actions to.

```purescript
store <- runStore update initialState
```

With this expression, your Store is now initialized, and will send Actions that come in through the `actionChannel` to your Update function. The resulting state is sent to the `stateSignal`, which you can use to react to state updates.

```purescript
store.stateSignal ~> Console.log
```

### Side Effects

To handle side effects, Manifold provides `effectChannel` - a Channel for `Aff` asynchronous effects that yield lists of actions. Upon completion of the effect, the yielded actions are sent to the `actionChannel` to update the state.

### Rendering

*Coming soon:* `Manifold.Render` will provide helpers to use Manifold with "view layers" like [React][react], [Halogen][halogen], etc. JSX will be supported, along with other ways of expressing virtual DOM trees.

### Composition

A real application is never this simple. *Coming a little less soon:* I'll document how to compose many Action/State/Update groups together into a larger application!

### FRP

*Coming after that:* How to take advantage of the FRP-like properties of Signals!

## Todo

* Manifold.Render helpers to "connect" Manifold to view layers
* Composition examples
* FRP helpers and examples
* More comprehensive documentation and examples
* Middleware

[signal]: https://github.com/bodil/purescript-signal
[aff]: https://github.com/slamdata/purescript-aff
[pux]: https://github.com/alexmingoia/purescript-pux
[elm]: http://package.elm-lang.org/packages/elm-lang/core/2.1.0/Signal
[rx-state]: https://github.com/jasonzoladz/purescript-rx-state
[redux]: http://redux.js.org/
[halogen]: https://github.com/slamdata/purescript-halogen
[react]: https://facebook.github.io/react/
