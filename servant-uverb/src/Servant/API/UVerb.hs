-- | An alternative to 'Verb' for end-points that respond with a resource value of any of an
-- open union of types, and specific status codes for each type in this union.  (`UVerb` is
-- short for `UnionVerb`)
--
-- This can be used for returning (rather than throwing) exceptions in a server as in, say
-- @'[Report, WaiError]@; or responding with either a 303 forward with a location header, or
-- 201 created with a different body type, depending on the circumstances.  (All of this can
-- be done with vanilla servant-server by throwing exceptions, but it can't be represented in
-- the API types.)
module Servant.API.UVerb
  ( UVerb,
    Union,
    HasStatus,
    StatusOf,
    statusOf,
    Statuses,
    WithStatus (..),
    module Servant.API.UVerb.OpenUnion,
  )
where

import Data.Functor.Identity (Identity)
import Data.SOP.NS (NS)
import Data.Typeable (Proxy (Proxy))
import qualified GHC.Generics as GHC
import GHC.TypeLits (KnownNat, Nat)
import Network.HTTP.Types.Status
import Servant.API.UVerb.OpenUnion

class KnownStatus (StatusOf a) => HasStatus (a :: *) where
  type StatusOf (a :: *) :: Nat

statusOf :: forall a proxy. HasStatus a => proxy a -> Status
statusOf = const (statusVal (Proxy :: Proxy (StatusOf a)))

type family Statuses (as :: [*]) :: [Nat]

type instance Statuses '[] = '[]

type instance Statuses (a ': as) = StatusOf a ': Statuses as

newtype WithStatus (k :: Nat) a = WithStatus a
  deriving (Eq, Show, GHC.Generic)

instance KnownStatus n => HasStatus (WithStatus n a) where
  type StatusOf (WithStatus n a) = n

-- FUTUREWORK:
-- @type Verb method statusCode contentTypes a = UVerb method contentTypes [WithStatus statusCode a]@
-- no, wait, this is not the same.  this would mean people would have to use 'respond' instead
-- of 'pure' or 'return'.
data UVerb (method :: StdMethod) (contentTypes :: [*]) (as :: [*])

type Union = NS Identity

-- this just went into master on servant: https://github.com/haskell-servant/servant/pull/1310
-- "Servant.API.Status"

class KnownNat n => KnownStatus n where
  statusVal :: proxy n -> Status

instance KnownStatus 100 where
  statusVal _ = status100

instance KnownStatus 101 where
  statusVal _ = status101

instance KnownStatus 200 where
  statusVal _ = status200

instance KnownStatus 201 where
  statusVal _ = status201

instance KnownStatus 202 where
  statusVal _ = status202

instance KnownStatus 203 where
  statusVal _ = status203

instance KnownStatus 204 where
  statusVal _ = status204

instance KnownStatus 205 where
  statusVal _ = status205

instance KnownStatus 206 where
  statusVal _ = status206

instance KnownStatus 300 where
  statusVal _ = status300

instance KnownStatus 301 where
  statusVal _ = status301

instance KnownStatus 302 where
  statusVal _ = status302

instance KnownStatus 303 where
  statusVal _ = status303

instance KnownStatus 304 where
  statusVal _ = status304

instance KnownStatus 305 where
  statusVal _ = status305

instance KnownStatus 307 where
  statusVal _ = status307

instance KnownStatus 308 where
  statusVal _ = status308

instance KnownStatus 400 where
  statusVal _ = status400

instance KnownStatus 401 where
  statusVal _ = status401

instance KnownStatus 402 where
  statusVal _ = status402

instance KnownStatus 403 where
  statusVal _ = status403

instance KnownStatus 404 where
  statusVal _ = status404

instance KnownStatus 405 where
  statusVal _ = status405

instance KnownStatus 406 where
  statusVal _ = status406

instance KnownStatus 407 where
  statusVal _ = status407

instance KnownStatus 408 where
  statusVal _ = status408

instance KnownStatus 409 where
  statusVal _ = status409

instance KnownStatus 410 where
  statusVal _ = status410

instance KnownStatus 411 where
  statusVal _ = status411

instance KnownStatus 412 where
  statusVal _ = status412

instance KnownStatus 413 where
  statusVal _ = status413

instance KnownStatus 414 where
  statusVal _ = status414

instance KnownStatus 415 where
  statusVal _ = status415

instance KnownStatus 416 where
  statusVal _ = status416

instance KnownStatus 417 where
  statusVal _ = status417

instance KnownStatus 418 where
  statusVal _ = status418

instance KnownStatus 422 where
  statusVal _ = status422

instance KnownStatus 426 where
  statusVal _ = status426

instance KnownStatus 428 where
  statusVal _ = status428

instance KnownStatus 429 where
  statusVal _ = status429

instance KnownStatus 431 where
  statusVal _ = status431

instance KnownStatus 500 where
  statusVal _ = status500

instance KnownStatus 501 where
  statusVal _ = status501

instance KnownStatus 502 where
  statusVal _ = status502

instance KnownStatus 503 where
  statusVal _ = status503

instance KnownStatus 504 where
  statusVal _ = status504

instance KnownStatus 505 where
  statusVal _ = status505

instance KnownStatus 511 where
  statusVal _ = status511
