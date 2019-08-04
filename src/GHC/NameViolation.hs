module GHC.NameViolation where

import qualified Name as N
import FastString
import Unique
import SrcLoc
import Module
import TyCoRep
import Unsafe.Coerce


violateName :: N.Name -> Name
violateName = unsafeCoerce


showName :: Name -> String
showName (Name (External m) occ _ _)    =
  showModule m ++ "." ++ unpackFS (occNameFS occ)
showName (Name (WiredIn m _ _) occ _ _) =
  showModule m ++ "." ++ unpackFS (occNameFS occ)
showName (Name Internal occ _ _) =
  unpackFS (occNameFS occ)
showName (Name System occ _ _) =
  unpackFS (occNameFS occ)



showModule :: Module -> String
showModule = moduleStableString


data OccName = OccName
  { occNameSpace  :: !N.NameSpace
  , occNameFS     :: !FastString
  }


data Name = Name
  { n_sort :: NameSort     -- What sort of name it is
  , n_occ  :: !OccName     -- Its occurrence name
  , n_uniq :: {-# UNPACK #-} !Unique
  , n_loc  :: !SrcSpan      -- Definition site
  }


data NameSort
  = External Module

  | WiredIn Module TyThing N.BuiltInSyntax
        -- A variant of External, for wired-in things

  | Internal            -- A user-defined Id or TyVar
                        -- defined in the module being compiled

  | System              -- A system-defined Id or TyVar.  Typically the
                        -- OccName is very uninformative (like 's')
