Reflex Platform [![CircleCI](https://circleci.com/gh/reflex-frp/reflex-platform.svg?style=svg)](https://circleci.com/gh/reflex-frp/reflex-platform)
===============

The Reflex Platform is a collection of libraries and tools that are useful for developing and deploying [Reflex](https://github.com/reflex-frp/reflex)-based applications.

To get started with Reflex development, follow the instructions below.

Try Reflex lets you set up an environment from which you can use [Reflex](https://github.com/ryantrinkle/reflex) with GHC or [GHCJS](https://github.com/ghcjs/ghcjs).

To use Reflex Platform as a build/development system for your own projects, refer to `HACKING.md`.

Important Notes
---------------

 ### OS Compatibility

If you're using one of these platforms, please take a look at notes before you begin:

* [macOS](notes/macOS.md)
* [NixOS](notes/NixOS.md)
* [Arch Linux](notes/ArchLinux.md)
* [Linux Mint](notes/LinuxMint.md)

If you encounter any problems that may be specific to your platform, please submit an issue or pull request so that we can add a note for future users.

 ### Memory Requirements

GHCJS uses a lot of memory during compilation. 16GB of memory is recommended, with 8GB being pretty close to bare minimum.


Setup
-----
This process will install the [Nix package manager](https://nixos.org/nix/). If you prefer to install Nix yourself, you may do so any time prior to step 2.

1. Clone this repository:

    ```bash
    git clone https://github.com/reflex-frp/reflex-platform
    ```

1. Navigate into the `reflex-platform` folder and run the `try-reflex` command. This will install Nix, if you don't have it already, and use it to wrangle all the dependencies you'll need and drop you in an environment from which you can use Reflex. Be warned, this might take a little while the first time:

    ```bash
    cd reflex-platform
    ./try-reflex
    ```

1. From this nix-shell, you can compile any haskell source files you like.
   Replace `your-source-file.hs` with the name of the file you'd like to compile.  For the most part, ghcjs supports the same options as ghc:

   * GHC
     ```bash
     ghc --make your-source-file.hs
     ./your-source-file
     ```
     Compilation will produce a `your-source-file` native executable via [WebkitGtk](https://github.com/WebKit/webkit). Simply run it to launch your app.

   * GHCJS
     ```bash
     ghcjs --make your-source-file.hs
     ```
     Compilation will produce a `your-source-file.jsexe` folder containing an `index.html` file. Open that in your browser to run your app.

**Don't use** `cabal install` to install libraries while inside the try-reflex shell - the resulting libraries may not be found properly by ghc or ghcjs.  Using Cabal to configure, build, test, and run a particular package, however, should work just fine.

`try-reflex` and `ghcjs --make` are not recommended for real-world projects — just as a quick and easy way to install Nix and experiment with `reflex-dom`. If you need to use additional Haskell libraries (e.g. from Hackage), we recommend using the tools described in [project-development.md](docs/project-development.md) instead.

Tutorial
--------
In this example, we'll be following [Luite Stegemann's lead](http://weblog.luite.com/wordpress/?p=127) and building a simple functional reactive calculator to be used in a web browser.

 ### DOM Basics

Reflex's companion library, Reflex-DOM, contains a number of functions used to build and interact with the Document Object Model. Let's start by getting a basic app up and running.


<!--
#ifdef SNIPPET_0
-->
```haskell

> {-# LANGUAGE OverloadedStrings #-}
> import Reflex.Dom

> main = mainWidget $ el "div" $ text "Welcome to Reflex"

```
<!--
#endif
-->


Save this file as `source.hs` and compile it by running `ghcjs source.hs`. If you've entered everything correctly, this will produce a folder named `source.jsexe` in the same directory as `source.hs`. Navigate to this folder in your file manager and open `index.html` using your browser. The browser should show a page with the text "Welcome to Reflex".

Most Reflex apps will start the same way: a call to `mainWidget` with a starting `Widget`. A `Widget` is some DOM wrapped up for easy use with Reflex. In our example, we are building the argument to `mainWidget`, (in other words, our starting `Widget`) on the same line.

`el` has the type signature:

```haskell
el :: MonadWidget t m => Text -> m a -> m a
```

The first argument to `el` is a `Text`, which will become the tag of the html element produced. The second argument is a `Widget`, which will become the child of the element being produced.

 > #### Sidebar: Interpreting the MonadWidget type
 > FRP-enabled datatypes in Reflex take an argument `t`, which identifies the FRP subsystem being used.  This ensures that wires don't get crossed if a single program uses Reflex in multiple different contexts.  You can think of `t` as identifying a particular "timeline" of the FRP system.
 > Because most simple programs will only deal with a single timeline, we won't revisit the `t` parameters in this tutorial.  As long as you make sure your `Event`, `Behavior`, and `Dynamic` values all get their `t` argument, it'll work itself out.

In our example, `el "div" $ text "Welcome to Reflex"`, the first argument to `el` was `"div"`, indicating that we are going to produce a div element.

The second argument to `el` was `text "Welcome to Reflex"`. The type signature of `text` is:

```haskell
text :: MonadWidget t m => Text -> m ()
```

`text` takes a `Text` and produces a `Widget`. The `Text` becomes a text DOM node in the parent element of the `text`. Of course, instead of a `Text`, we could have used `el` here as well to continue building arbitrarily complex DOM. For instance, if we wanted to make a unordered list:

<!--
#ifdef SNIPPET_1
-->
```haskell

> {-# LANGUAGE OverloadedStrings #-}
> import Reflex.Dom

> main = mainWidget $ el "div" $ do
>  el "p" $ text "Reflex is:"
>  el "ul" $ do
>    el "li" $ text "Efficient"
>    el "li" $ text "Higher-order"
>    el "li" $ text "Glitch-free"

```
<!--
#endif
-->


 ### Dynamics and Events
Of course, we want to do more than just view a static webpage. Let's start by getting some user input and printing it.

<!--
#ifdef SNIPPET_2
-->
```haskell

> {-# LANGUAGE OverloadedStrings #-}
> import Reflex.Dom

> main = mainWidget $ el "div" $ do
>   t <- textInput def
>   dynText $ _textInput_value t

```
<!--
#endif
-->


Running this in your browser, you'll see that it produces a `div` containing an `input` element. When you type into the `input` element, the text you enter appears inside the div as well.

`textInput` is a function with the following type:

```haskell
textInput :: MonadWidget t m => TextInputConfig t -> m (TextInput t)
```

It takes a `TextInputConfig` (given a default value in our example), and produces a `Widget` whose result is a `TextInput`. In `Reflex.Dom.Widget.Input` we can see that a `TextInput` exposes the following functionality:

```haskell
data TextInput t
   = TextInput { _textInput_value :: Dynamic t Text
               , _textInput_input :: Event t Text
               , _textInput_keypress :: Event t Int
               , _textInput_keydown :: Event t Int
               , _textInput_keyup :: Event t Int
               , _textInput_hasFocus :: Dynamic t Bool
               , _textInput_builderElement :: InputElement EventResult GhcjsDomSpace t
               }
```

Here we are using `_textInput_value` to access the `Dynamic Text` value of the `TextInput`. Conveniently, `dynText` takes a `Dynamic Text` and displays it. It is the dynamic version of `text`.

We can also access `Event`s related to the `TextInput`. For example, consider the following code:

<!--
#ifdef SNIPPET_3
-->
```haskell

> {-# LANGUAGE OverloadedStrings #-}
> import Data.Text  (pack)
> import Reflex
> import Reflex.Dom

> main = mainWidget $ el "div" $ do
>   t <- textInput def
>   text "Last key pressed: "
>   let keypressEvent = fmap (pack . show) $ _textInput_keypress t
>   keypressDyn <- holdDyn "None" keypressEvent
>   dynText keypressDyn

```
<!--
#endif
-->

Here, we are creating a `TextInput` as we were before. The function `_textInput_keypress` gives us an `Event Int` representing the key code of the pressed key. We are using `fmap` here to apply `pack . show` to the `Int`, so the type of `keypressEvent` is `Event Text`. Whenever a key is pressed inside the `TextInput`, the `keypressEvent` will fire.
`holdDyn` allows us to take create a `Dynamic` out of an `Event`. We must provide an initial value for the `Dynamic`. This will be the value of the `Dynamic` until the associated `Event` fires. The type of `holdDyn` is:

```haskell
holdDyn :: MonadHold t m => a -> Event t a -> m (Dynamic t a)
```

We won't go into the details of `MonadHold` here, but the rest of the type signature should be fairly clear: `holdDyn` takes an initial value, an `Event` containing a value of the same type as the initial, and returns a `Dynamic` containing a value of the same type.

When you run this application, you'll see a textbox and the string "Last key pressed: None" on the screen. Recall that "None" is the initial value we gave `holdDyn`.

 ### A Number Input
A calculator was promised, I know. We'll start building the calculator by creating an input for numbers.

<!--
#ifdef SNIPPET_4
-->
```haskell

> {-# LANGUAGE OverloadedStrings #-}
> import Reflex
> import Reflex.Dom
> import Data.Map (Map)
> import qualified Data.Map as Map

> main = mainWidget $ el "div" $ do
>   t <- textInput $ def & textInputConfig_inputType .~ "number"
>                        & textInputConfig_initialValue .~ "0"
>   dynText $ _textInput_value t

```
<!--
#endif
-->

The code above overrides some of the default values of the `TextInputConfig`. We provide a `Text` value for the `textInputConfig_inputType`, specifying the html input element's `type` attribute. We're using `"number"` here.

Next, we override the default initial value of the `TextInput`. We gave it `"0"`. Even though we're making an html `input` element with the attribute `type=number`, the result is still a `Text`. We'll convert this later.

Let's do more than just take the input value and print it out. First, let's make sure the input is actually a number:

<!--
#ifdef SNIPPET_5
-->
```haskell

> {-# LANGUAGE OverloadedStrings #-}
> import Reflex.Dom
> import Data.Map (Map)
> import qualified Data.Map as Map
> import Data.Text (pack, unpack)
> import Text.Read (readMaybe)

> main = mainWidget $ el "div" $ do
>   x <- numberInput
>   let numberString = fmap (pack . show) x
>   dynText numberString

> numberInput :: MonadWidget t m => m (Dynamic t (Maybe Double))
> numberInput = do
>   n <- textInput $ def & textInputConfig_inputType .~ "number"
>                        & textInputConfig_initialValue .~ "0"
>   return . fmap (readMaybe . unpack) $ _textInput_value n

```
<!--
#endif
-->


We've defined a function `numberInput` that both handles the creation of the `TextInput` and reads its value. Recall that `_textInput_value` gives us a `Dynamic Text`. The final line of code in `numberInput` uses `fmap` to apply the function `readMaybe . unpack` to the `Dynamic` value of the `TextInput`. This produces a `Dynamic (Maybe Double)`. Our `main` function uses `fmap` to map over the `Dynamic (Maybe Double)` produced by `numberInput` and `pack . show` the value it contains. We store the new `Dynamic Text` in `numberString` and feed that into `dynText` to actually display the `Text`

Running the app at this point should produce an input and some text showing the `Maybe Double`. Typing in a number should produce output like `Just 12.0` and typing in other text should produce the output `Nothing`.

 ### Adding
Now that we have `numberInput` we can put together a couple inputs to make a basic calculator.

<!--
#ifdef SNIPPET_6
-->
```haskell

> {-# LANGUAGE OverloadedStrings #-}
> import Reflex
> import Reflex.Dom
> import Data.Map (Map)
> import qualified Data.Map as Map
> import Data.Text (pack, unpack)
> import Text.Read (readMaybe)
> import Control.Applicative ((<*>), (<$>))
>
> main = mainWidget $ el "div" $ do
>   nx <- numberInput
>   text " + "
>   ny <- numberInput
>   text " = "
>   let result = zipDynWith (\x y -> (+) <$> x <*> y) nx ny
>       resultString = fmap (pack . show) result
>   dynText resultString

> numberInput :: MonadWidget t m => m (Dynamic t (Maybe Double))
> numberInput = do
>   n <- textInput $ def & textInputConfig_inputType .~ "number"
>                        & textInputConfig_initialValue .~ "0"
>   return . fmap (readMaybe . unpack) $ _textInput_value n

```
<!--
#endif
-->

`numberInput` hasn't changed here. Our `main` function now creates two inputs. `zipDynWith` is used to produce the actual sum of the values of the inputs. The type signature of `zipDynWith` is:

```haskell
    Reflex t => (a -> b -> c) -> Dynamic t a -> Dynamic t b -> Dynamic t c
```

You can see that it takes a function that combines two pure values and produces some other pure value, and two `Dynamic`s, and produces a `Dynamic`.

In our case, `zipDynWith` is combining the results of our two `numberInput`s (with a little help from `Control.Applicative`) into a sum.

We use `fmap` again to apply `pack . show` to `result` (a `Dynamic (Maybe Double)`) resulting in a `Dynamic Text`. This `resultText` is then displayed using `dynText`.

 ### Supporting Multiple Operations
Next, we'll add support for other operations. We're going to add a dropdown so that the user can select the operation to apply. The function `dropdown` has the type:

```haskell
dropdown :: (MonadWidget t m, Ord k) => k -> Dynamic t (Map k Text) -> DropdownConfig t k -> m (Dropdown t k)
```

The first argument is the initial value of the `Dropdown`. The second argument is a `Dynamic (Map k Text)` that represents the options in the dropdown. The `Text` values of the `Map` are the strings that will be displayed to the user. If the initial key is not in the `Map`, it is added and given a `Text` value of `""`. The final argument is a `DropdownConfig`.

Our supported operations will be:

```haskell
data Op = Plus | Minus | Times | Divide deriving (Eq, Ord)

ops = Map.fromList [(Plus, "+"), (Minus, "-"), (Times, "*"), (Divide, "/")]
```

We'll use this as an argument to `dropdown`:

```haskell
d <- dropdown Times (constDyn ops) def
```

We are using `constDyn` again here to turn our `Map` of operations into a `Dynamic`. Using `def`, we provide the default `DropdownConfig`. The result, `d`, will be a `Dropdown`. We can retrieve the `Dynamic` selection of a `Dropdown` by using `_dropdown_value`.

<!--
#ifdef SNIPPET_7
-->
```haskell

> {-# LANGUAGE OverloadedStrings #-}
> import Reflex
> import Reflex.Dom
> import Data.Map (Map)
> import qualified Data.Map as Map
> import Data.Text (pack, unpack, Text)
> import Text.Read (readMaybe)
> import Control.Applicative ((<*>), (<$>))
>
> main = mainWidget $ el "div" $ do
>   nx <- numberInput
>   d <- dropdown Times (constDyn ops) def
>   ny <- numberInput
>   let values = zipDynWith (,) nx ny
>       result = zipDynWith (\o (x,y) -> runOp o <$> x <*> y) (_dropdown_value d) values
>       resultText = fmap (pack . show) result
>   text " = "
>   dynText resultText
>
> numberInput :: MonadWidget t m => m (Dynamic t (Maybe Double))
> numberInput = do
>   n <- textInput $ def & textInputConfig_inputType .~ "number"
>                        & textInputConfig_initialValue .~ "0"
>   return . fmap (readMaybe . unpack) $ _textInput_value n
>
> data Op = Plus | Minus | Times | Divide deriving (Eq, Ord)
>
> ops :: Map Op Text
> ops = Map.fromList [(Plus, "+"), (Minus, "-"), (Times, "*"), (Divide, "/")]
>
> runOp :: Fractional a => Op -> a -> a -> a
> runOp s = case s of
>             Plus -> (+)
>             Minus -> (-)
>             Times -> (*)
>             Divide -> (/)

```
<!--
#endif
-->

This is our complete program. We've added an uninteresting function `runOp`
that takes an `Op` and returns an operation. The keys of the `Map` we used
to create the `Dropdown` had the type `Op`. When we retrieve the value of
`Dropdown`, we'll use `runOp` to turn the `Dropdown` selection into the
function we need to apply to our numbers.

After creating the two `numberInput`s, we combine them using `zipDynWith` applying `(,)`, making a tuple of type `Dynamic (Maybe Double, Maybe Double)` and binding it to `values`.

Next, we call `zipDynWith` again, combining the `_dropdown_value` and `values`. Now, instead of applying `(+)` to our `Double` values, we use `runOp` to select an operation based on the `Dynamic` value of our `Dropdown`.

Running the app at this point will give us our two number inputs with a dropdown of operations sandwiched between them. Multiplication should be pre-selected when the page loads.

 ### Dynamic Element Attributes
Let's spare a thought for the user of our calculator and add a little UI styling. Our number input currently looks like this:

```haskell
numberInput :: MonadWidget t m => m (Dynamic t (Maybe Double))
numberInput = do
  n <- textInput $ def & textInputConfig_inputType .~ "number"
                       & textInputConfig_initialValue .~ "0"
  return . fmap (readMaybe . unpack) $ _textInput_value n
```

Let's give it some html attributes to work with:

```haskell
numberInput :: MonadWidget t m => m (Dynamic t (Maybe Double))
numberInput = do
  let attrs = constDyn ("style" =: "border-color: blue")
  n <- textInput $ def & textInputConfig_inputType .~ "number"
                       & textInputConfig_initialValue .~ "0"
                       & textInputConfig_attributes .~ attrs
  return . fmap (readMaybe . unpack) $ _textInput_value n
```

Here, we've created a `Dynamic (Map Text Text)`. This `Map` represents the html attributes of our inputs. Because we're using `constDyn` again, this `Dynamic` will never change. If you load this in the browser, you'll see that the inputs now have a blue border.

Unchanging attributes are useful and quite common, but attributes will often need to change. Instead of just making the `TextInput` blue, let's change it's color based on whether the input successfully parses to a `Double`:

```haskell
{-# LANGUAGE RecursiveDo #-}
...
numberInput :: (MonadWidget t m) => m (Dynamic t (Maybe Double))
numberInput = do
  let errorState = "style" =: "border-color: red"
      validState = "style" =: "border-color: green"
  rec n <- textInput $ def & textInputConfig_inputType .~ "number"
                           & textInputConfig_initialValue .~ "0"
                           & textInputConfig_attributes .~ attrs
      let result = fmap (readMaybe . unpack) $ _textInput_value n
          attrs  = fmap (maybe errorState (const validState)) result
  return result
```

Note that we need to add a language pragma here to enable the `RecursiveDo` language extension. We've defined two `Map`s of attributes for our inputs: one to represent bad input and one to represent valid input. Next, you'll see that the code for actually making the number input is now inside of a `rec` block. This is because the attributes we apply depend on the value of the input.

In the first line of the `rec`, we have supplied the argument `attrs`, of type `Dynamic (Map Text Text)`. The `Dynamic` value of the input is bound to `result`. The code for parsing this value has not changed.

After we bind `result`, we use `fmap` again to apply a switching function to `result`. The switching function checks whether the value was successfully parsed. If it was, we get the `Map` of attributes representing the valid state, otherwise we get the `Map` representing the error state. The result is a `Dynamic (Map Text Text)`, which is the type `textInputConfig_attributes` expects to receive.

The complete program now looks like this:

<!--
#ifdef SNIPPET_8
-->
```haskell

> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE RecursiveDo       #-}
> import Reflex
> import Reflex.Dom
> import Data.Map (Map)
> import qualified Data.Map as Map
> import Data.Text (pack, unpack, Text)
> import Text.Read (readMaybe)
> import Control.Applicative ((<*>), (<$>))
>
> main = mainWidget $ el "div" $ do
>   nx <- numberInput
>   d <- dropdown Times (constDyn ops) def
>   ny <- numberInput
>   let values = zipDynWith (,) nx ny
>       result = zipDynWith (\o (x,y) -> runOp o <$> x <*> y) (_dropdown_value d) values
>       resultText = fmap (pack . show) result
>   text " = "
>   dynText resultText
>
> numberInput :: (MonadWidget t m) => m (Dynamic t (Maybe Double))
> numberInput = do
>   let errorState = "style" =: "border-color: red"
>       validState = "style" =: "border-color: green"
>   rec n <- textInput $ def & textInputConfig_inputType .~ "number"
>                            & textInputConfig_initialValue .~ "0"
>                            & textInputConfig_attributes .~ attrs
>       let result = fmap (readMaybe . unpack) $ _textInput_value n
>           attrs  = fmap (maybe errorState (const validState)) result
>   return result
>
> data Op = Plus | Minus | Times | Divide deriving (Eq, Ord)
>
> ops :: Map Op Text
> ops = Map.fromList [(Plus, "+"), (Minus, "-"), (Times, "*"), (Divide, "/")]
>
> runOp :: Fractional a => Op -> a -> a -> a
> runOp s = case s of
>             Plus -> (+)
>             Minus -> (-)
>             Times -> (*)
>             Divide -> (/)

```
<!--
#endif
-->

The input border colors will now change depending on their value.
