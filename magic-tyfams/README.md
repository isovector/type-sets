# magic-tyfams

[![Hackage](https://img.shields.io/hackage/v/magic-tyfams.svg?logo=haskell&label=magic-tyfams)](https://hackage.haskell.org/package/magic-tyfams)

## Dedication

> G.O.B.: Two drowned white doves from "Flowers to Doves," a rabbit from "Doves
>   to Rabbit," also drowned. These were mice...
>
> Tobias: From "Rabbit to Mice?"
>
> G.O.B.: No. That can't be done. No, these were a part of something I call
>   *mice-cellaneous*: "Mouse in Purse." "Mouse in Drink." Here's a mouse, now
>   it's gone...
>
> --Arrested Development


## Overview

`magic-tyfams` writes the annoying parts of GHC plugins so you don't have to. It
provides a convenient interface for implementing magical type families.

For example, consider the following definition in `mypackage` of an always-stuck
type family:

```haskell
module Some.Module where

type family CmpType (a :: k) (b :: k) :: Ordering
```

We can generate a plugin that will solve this:

```haskell
module Some.Plugin where

plugin :: Plugin
plugin = magicTyFamPlugin "mypackage"
                          "Some.Module"
                          "CmpType" $
  withStuckSemantics $ \[_k, a, b] ->
    pure $ promoteOrdering $ nonDetCmpType a b


promoteOrdering :: Ordering -> Type
promoteOrdering = flip mkTyConApp [] . \case
   LT -> promotedLTDataCon
   EQ -> promotedEQDataCon
   GT -> promotedGTDataCon
```

Enabling this plugin via `-fplugin=Some.Plugin` will now automagically solve
`CmpType`! Nice!

```haskell
{-# OPTIONS_GHC -fplugin=Some.Plugin #-}

import Proxy

-- compiles just fine!
test :: Proxy (CmpType Int Int) -> Proxy 'EQ
test = id
```


## Acknowledgments

Thanks to [Christiaan Baaij][chistiaanb] and [Matt Pickering][mpickering] for
their indispensable help convincing GHC to do the right thing here.

[chistiaanb]: https://christiaanb.github.io/
[mpickering]: http://mpickering.github.io/

