module Cli.OptionsParser.BuilderState exposing (AnyOptions, NoBeginningOptions, NoMoreOptions)

{-| A BuilderState is used to ensure that no ambiguous OptionsParsers are built.
For example, if you were to build an OptionsParser that had optional positional
arguments after required positional arguments, it would be amgiguous.

```bash
greet <greeting1> [name1][name2] <greeting2> [farewell]

greet Hi Hello Goodbye
```

Should `"Goodbye"` be set as `[name1]` or `[farewell]`? You could certainly come
up with some rules, but they're not obvious, and you'd have to think really hard!
So we just completely eliminate those confusing corner cases by making it impossible
to express!

The `BuilderState` guarantees that nothing will come after rest args (i.e. `[args]...`,
or 0 or more args that you get as a `List` of values).
And it also guarantees that Optional Positional Arguments will come after everything
but rest args.

If you're interested in the low-level details of how this Elm type trick is done,
take a look at
[this article on Phantom Types](https://medium.com/@ckoster22/advanced-types-in-elm-phantom-types-808044c5946d).

@docs AnyOptions, NoBeginningOptions, NoMoreOptions

-}


{-| A state where you can add any options (beginning, middle, or terminal)
-}
type AnyOptions
    = AnyOptions


{-| A state where you can add anything but beginning options (i.e. middle or terminal)
-}
type NoBeginningOptions
    = NoBeginningOptions


{-| A state where you can no longer add any options
-}
type NoMoreOptions
    = NoMoreOptions
