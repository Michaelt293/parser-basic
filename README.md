# parser-basic

In this project, a basic parser is implemented with the following definition -

```haskell
newtype Parser a = Parser { parse :: String -> Maybe (a, String) }
```

For this datatype, `Functor`, `Applicative`, `Monad` and `Alternative` instances are provided. From this base, a coinage metal isotope parser is developed.

The application may be run as follows -

```
$ stack exec parser-basic-exe -- Au197
Right (CoinageMetal Au 197)

$ stack exec parser-basic-exe -- ohNo197
Left ParseFail

$ stack exec parser-basic-exe -- Au197extra-input
Left (RemainingInput "extra-input")
```