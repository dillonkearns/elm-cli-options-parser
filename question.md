Hello all! I'm about to publish my package for parsing command line options in a declarative & type-safe way with Elm (similar to elm-decoder-pipeline). Here's an example: https://github.com/dillonkearns/elm-cli-options-parser/blob/master/examples/src/ElmTest.elm

I'm pretty happy overall, but there are a couple of things I'm trying to polish, I'd love to hear suggestions!

One issue: I wish I didn't have to say `Command.withoutRestArgs`. The reason I need it now is because I need to ensure that `Command.withRestArgs` is called after all of the `Option.positionalArg`s. I rely on the current count of positional arguments in order to know where the rest args start so I can drop the args before them. Here's the relevant code for calculating the offsets:

https://github.com/dillonkearns/elm-cli-options-parser/blob/efdbf4c203f347be8c3acc930691a2d9bbb71f0d/src/Cli/Option.elm#L83-L84
https://github.com/dillonkearns/elm-cli-options-parser/blob/efdbf4c203f347be8c3acc930691a2d9bbb71f0d/src/Cli/Command.elm#L244-L246

Any thoughts on that issue or any other feedback on the package would be much appreciated!
