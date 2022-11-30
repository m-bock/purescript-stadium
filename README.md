Under construction... <img
src="https://media.tenor.com/MRCIli40TYoAAAAi/under-construction90s-90s.gif" width="30">

# purescript-stadium
Type safe state machines 

## Usage
### Imports
We'll need those imports for the following samples: 
```hs
module Test.Readme where

import Prelude

import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Data.Variant as V
import Prim.RowList (Cons, Nil)
import Stadium (type (>>))
import Stadium as SD
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
```
### A simple conventional state machine using ADT's
Let's first look at an example state machine implementation which is done
without any extra library.
It consists of type for the state, a type for a Message and a reducer
function that is capable to produce new states.
```hs
data State'1 = On | Off

data Msg'1 = SwitchOn | SwitchOff

reducer'1 :: Msg'1 -> State'1 -> State'1
reducer'1 msg state = case msg of
  SwitchOn -> case state of
    Off -> On
    _ -> state
  SwitchOff -> case state of
    On -> Off
    _ -> state
```
We could use it like so:
```hs
state'1 :: State'1
state'1 = Off
  # reducer'1 SwitchOn
  # reducer'1 SwitchOff
  # reducer'1 SwitchOff
  # reducer'1 SwitchOn
```
What's the problem with this approach? If you look at the reducer function
you'll notice that it's quite easy to introduce mistakes. For instance the
`SwitchOn` handler could mistakenly return the `Off` state. The types are not
strict enough to prevent this. It may look silly in this simple example but
state machines can become quite large. And then it will be quite useful to
have types that describe the protocol of a state machine more accurately. 

### A state machine with Variant types
Before we move to the first example, let's quickly redefine the same machine
with powerful alternative called [variant
types](https://github.com/natefaubion/purescript-variant). I recommend to get
familiar with this library by reading its README. We'll see later that we
don't necessarily need to use Variant types in our state type. However,
they're inevitably a part of our API and using them is a good way get a
better understanding of this library.
```hs
type State'2 = Variant
  ( on :: Unit
  , off :: Unit
  )

type Msg'2 = Variant
  ( switchOn :: Unit
  , switchOff :: Unit
  )

reducer'2 :: Msg'2 -> State'2 -> State'2
reducer'2 msg state =
  msg #
    ( V.case_ # V.onMatch
        { switchOn: \_ ->
            state #
              ( V.default state #
                  V.onMatch
                    { off: \_ -> V.inj (Proxy :: _ "on") unit
                    }
              )

        , switchOff: \_ ->
            state #
              ( V.default state #
                  V.onMatch
                    { on: \_ -> V.inj (Proxy :: _ "off") unit
                    }
              )
        }
    )
```
Above we defined the types using Variants. The `Unit` type is used when a
case does not hold any data. The reducer function looks a bit complex. Right
now, it has the same behavior as the one in the ADT example.
First we do an exhaustive pattern match on the incoming message using the
`case_` function. Inside each arm we do a non exhaustive pattern matche on
the incoming state providing a default value in case there's no match. The
injection syntax for variant types is a bit wordy, too. This will be simpler once
PureScript has "visible type application", which as of the time of writing is
being worked on.
To be complete, here's a usage example of this implementation, too:
```hs
state'2 :: State'2
state'2 = V.inj (Proxy :: _ "off") unit
  # reducer'2 (V.inj (Proxy :: _ "switchOn") unit)
  # reducer'2 (V.inj (Proxy :: _ "switchOff") unit)
  # reducer'2 (V.inj (Proxy :: _ "switchOff") unit)
  # reducer'2 (V.inj (Proxy :: _ "switchOn") unit)
```
### Increase type safety by using `stadium` 

```hs
type State'3 = State'2

type Msg'3 = Msg'2

type Protocol'3 :: SD.Protocol
type Protocol'3 = SD.Protocol_
  ( switchOff :: "on" >> "off"
  , switchOn :: "off" >> "on"
  )

reducer'3 :: Msg'3 -> State'3 -> Maybe State'3
reducer'3 = SD.mkReducer
  (Proxy :: _ Protocol'3)
  { switchOn: \_ ->
      V.case_ #
        V.onMatch
          { off: \_ -> V.inj (Proxy :: _ "on") unit
          }
  , switchOff: \_ ->
      V.case_ #
        V.onMatch
          { on: \_ -> V.inj (Proxy :: _ "off") unit
          }
  }
```
