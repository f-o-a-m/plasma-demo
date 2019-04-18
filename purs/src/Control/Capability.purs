-- | TODO (sectore / safareli): Extract this module into a library, its currently copied from
-- | https://github.com/f-o-a-m/foam.tcr/blob/master/purs/src/Control/Capability.purs

module Control.Capability where

-- | kind and type representing read write capabilities
foreign import kind Cap
foreign import data Cap :: Cap

type RW = (read :: Cap, write :: Cap)

type R' r = (read :: Cap| r)
type R = R' ()

type W' r = (write :: Cap| r)
type W = W' ()
