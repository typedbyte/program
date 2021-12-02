# program

[![Hackage](https://img.shields.io/hackage/v/program.svg?logo=haskell&label=program)](https://hackage.haskell.org/package/program)

[RIO]: https://hackage.haskell.org/package/rio
[ReaderT]: https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/
[Handle Pattern]: https://jaspervdj.be/posts/2018-03-08-handle-pattern.html
[record-of-functions]: https://discourse.haskell.org/t/records-of-functions-and-implicit-parameters/747
[managed]: https://hackage.haskell.org/package/managed
[Hackage]: https://hackage.haskell.org/package/program

## Overview

`program` is a library for writing programs with environments and managed resources, written in Haskell. It aims to be simple, has minimal dependencies and combines features of various existing approaches for threading an environment through an application (e.g., [RIO][], [ReaderT][], [Handle Pattern][], [record-of-functions][]) and for managing resources without nested bracket-functions (e.g., [managed][]). The library and its documentation can be found on [Hackage][].

## Example

The following simple example copies `count` characters from one file to another, where `count` is demanded from the environment `e` by the function `copy` (via `pull`) and supplied by `main`. Note that we do not need to close file handles, because resources are managed automatically.

```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Example where

-- base
import Control.Monad
import Control.Monad.IO.Class
import System.IO

-- program
import Control.Program (Program, Has, manage, pull, runProgram)

copy :: e `Has` Int => FilePath -> FilePath -> Program e ()
copy from to = do
  fromHandle <- manage (withFile from ReadMode)
  toHandle   <- manage (withFile to WriteMode)
  count      <- pull
  liftIO . replicateM_ count $
    hGetChar fromHandle >>= hPutChar toHandle

main :: IO ()
main = do
  runProgram
    ( 10 :: Int )
    ( copy "/tmp/source" "/tmp/target" )
```

In larger applications, the environment would contain many more complex values (all demanded by `Has`) and can also be used to manage mutable state. See the documentation on [Hackage][] for more details.

## Advantages

* Easy to understand (e.g., no unlifting, no type-level wizardry, hardly any language extensions).
* No need for extra dependencies. All we need is `base`.
* Mocking is easy by supplying different environments via `runProgram`.
* No fight with the type inference (i.e., down-to-earth types, hardly any typeclasses).
* The environment can easily simulate beloved effects like `Reader` and `State`.
* Easy integration of other effects by putting records-of-functions into the environment.
* Clear error message because involved types are not overly generic.