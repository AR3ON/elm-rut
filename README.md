# elm-rut

`elm-rut` based on korutx/elm-rut for Elm 0.18

```elm
import Rut exposing ( format, computeDv, isValidRut, Formatter(..) )


{-| Format a rut String.
-}
( format Cleared "11111111-1" ) == Just "111111111"
( format Simple "11111111-1" ) == Just "11111111-1"
( format ThousandsSeparated "11111111-1" ) == Just "11.111.111-1"

{-| Compute the verifier digit
-}
( computeDv "39872491" ) == "7"


{-| Test rut validity
-}
( isValidRut 6 "7588158-4" ) == True
```