# owoify-hs

Turning your worst nightmare into a Haskell package.

[![Haskell CI](https://github.com/deadshot465/owoify-hs/actions/workflows/haskell.yml/badge.svg)](https://github.com/deadshot465/owoify-hs/actions/workflows/haskell.yml)

This is a Haskell port of [mohan-cao's owoify-js](https://github.com/mohan-cao/owoify-js), which will help you turn any string into nonsensical babyspeak similar to LeafySweet's infamous Chrome extension.

Just like my other Owoify ports, three levels of owoness are available. Each level has been implemented using a discriminated union, so it should work like enums people are used to seeing in other imperative languages.

1. **owo (default)**: The most vanilla one.
2. **uwu**: The moderate one.
3. **uvu**: Litewawwy unweadabwal.
Please refer to the original [owoify-js repository](https://github.com/mohan-cao/owoify-js) for more information.

Functional programming is fun, so of course I'll port it to FP languages. 😜

Also I love Haskell. ❤️

## Reason for development

Because I love Haskell and currently Hackage doesn't have any owoify package yet. Also PureScript is influenced by Haskell and they have nearly the same syntax, so it doesn't make sense to have a PureScript port while not having a Haskell port.

The difference is that owoify-hs makes use of Haskell's lazy evaluation. `Data.Text.Lazy` is used instead of `Data.Text`. This *possibly* solved stack overflow problem with purescript-owoify when owoifying very long texts (i.e. chapter 1-20 of Tolstoy's *War and Peace*). The reason for using `Data.Text.Lazy.Text` instead of the good old `String` is because `Data.Text.Lazy.Text` and the eager version `Data.Text.Text` are meant for handling Unicode texts.

Having said that, you would need to do `Data.Text.Lazy.pack <text>` before passing the text to the `owoify` function.

## Install instructions

Add `owoify-hs` to your `package.yaml` in your Stack project:

```yaml
dependencies:
- owoify-hs == 1.0.0
```

If you are using Cabal, add the package the usual way.

```bash
cabal install owoify-hs
```

## Usage

owoify-hs is implemented as a single function. You will need to provide a source string and a owoify level. Due to random number generator and regular expressions, the result will be inside `IO` monad, so you will also need to handle that.

```haskell
module MyAwesomeModule where

import Prelude

import Data.Owoify.Owoify (OwoifyLevel(..), owoify)
import Data.Text.Lazy (pack, unpack)

main :: IO ()
main = do
  owoResult <- owoify (pack "This is the string to owo! Kinda cute, isn't it?") Owo
  uvuResult <- owoify (pack "This is the string to owo! Kinda cute, isn't it?") Uvu
  -- `print` won't print out Unicode emojis correctly.
  putStrLn $ unpack owoResult
  putStrLn $ unpack uvuResult

-- Possible outputs:
-- This is teh stwing two owo! Kinda cute, isn't it?
-- fwis is teh stwing two owowouvu Kinda cuteowo isn't it?
```

## Disclaimer

As usual, I'm writing this package for both practicing and bots' needs. Performance is **NOT** guaranteed.

That being said, `Data.Text.Lazy` is used, so it should be able to handle both short texts and long texts without any problem.

## See also

- [owoify-js](https://github.com/mohan-cao/owoify-js) - The original owoify-js repository.
- [Owoify.Net](https://www.nuget.org/packages/Owoify.Net/1.0.1) - The C# port of Owoify written by me.
- [Owoify++](https://github.com/deadshot465/OwoifyCpp) - The C++ header-only port of Owoify written by me.
- [owoify_rs](https://crates.io/crates/owoify_rs) - The Rust port of Owoify written by me.
- [owoify-py](https://pypi.org/project/owoify-py/) - The Python port of Owoify written by me.
- [owoify_dart](https://pub.dev/packages/owoify_dart) - The Dart port of Owoify written by me.
- [owoify_rb](https://rubygems.org/gems/owoify_rb) - The Ruby port of Owoify written by me.
- [owoify-go](https://github.com/deadshot465/owoify-go) - The Go port of Owoify written by me.
- [owoifySwift](https://github.com/deadshot465/OwoifySwift) - The Swift port of Owoify written by me.
- [owoifyKt](https://github.com/deadshot465/owoifyKt) - The Kotlin port of Owoify written by me.
- [owoify_ex](https://github.com/deadshot465/owoify_ex) - The Elixir port of Owoify written by me.
- [owoify_cr](https://github.com/deadshot465/owoify_cr) - The Crystal port of Owoify written by me.
- [owoifynim](https://github.com/deadshot465/owoifynim) - The Nim port of Owoify written by me.
- [owoify-clj](https://clojars.org/net.clojars.deadshot465/owoify-clj) - The Clojure port of Owoify written by me.
- [purescript-owoify](https://github.com/deadshot465/purescript-owoify) - The PureScript port of Owoify written by me.

## License

MIT License

Copyright (c) 2021 Chehui Chou

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
