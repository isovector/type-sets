# cmptype

[![Hackage](https://img.shields.io/hackage/v/cmptype.svg?logo=haskell&label=cmptype)](https://hackage.haskell.org/package/cmptype)

## Dedication

> Comparison is an act of violence against the self.
>
> --Iyanla Vanzant


## Overview

`cmptype` provides a magical type family that lets you compare types:

```haskell
type family CmpType (a :: k) (b :: k) :: Ordering
```

When you turn on the `-fplugin=Type.Compare.Plugin` flag, it will let you
compare arbitrary types. Why would you want such a thing? Probably because you
want to write a [type-level container that isn't a fucking list][type-sets]!

[type-sets]: https://github.com/isovector/type-sets


## Acknowledgments

Big thanks to [Boris Rozinov][oofp] for the initial idea, and for making as much
coffee as I could drink while I wrote it. Also thanks to [Christiaan
Baaij][chistiaanb] and [Matt Pickering][mpickering] for their indispensable help
convincing GHC to do the right thing here.

[oofp]: https://github.com/oofp
[chistiaanb]: https://christiaanb.github.io/
[mpickering]: http://mpickering.github.io/

