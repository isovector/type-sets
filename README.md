# type-sets

[![Build Status](https://api.travis-ci.org/isovector/type-sets.svg?branch=master)](https://travis-ci.org/isovector/type-sets)
[![Hackage](https://img.shields.io/hackage/v/type-sets.svg?logo=haskell&label=type-sets)](https://hackage.haskell.org/package/type-sets)


## Dedication

> Obstacles don't have to stop you. If you run into a wall, don't turn around
> and give up. Figure out how to climb it, go through it, or work around it.

> -- Michael Jordan, on complexity analysis in Haskell


## Overview

How much do you hate programming at the type level, but only being able to use
lists? A million? *Two* million? Some mathematicians suspect that there may be
even larger degrees of hate.

Enter `type-sets`. They're sets... at the type-level! Check this:

```haskell
{-# OPTIONS_GHC -fplugin=Type.Compare.Plugin #-}

import Type.Set

type MySet = Insert Bool (Insert String (Insert (Maybe Int) 'Empty))

test1 :: Proxy (Member Bool MySet) -> Proxy 'True
test1 = id  -- Bool is a member :)

test2 :: Proxy (Member Char MySet) -> Proxy 'False
test2 = id  -- Char is not a member :(
```

See the `Type.Set` module for more operations.

But wait, there's more! There's also a proof-of-concept `Type.Set.Variant` for
describing open-sums indexed by type-sets. PRs to make it better would be
greatly appreciated!

One love.


## Acknowledgments

Huge shout-outs to [Boris Rozinov][oofp] for all of his help on this library,
including the original idea, help with the implementation, infinite amounts of
coffee, and a couch to sleep on while it was being made.

[oofp]: https://github.com/oofp

