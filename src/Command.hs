{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Command where


import Common
import Control.Arrow
import Control.Lens
import Data.Default.Class
import Data.Monoid
import Data.List
import Data.Proxy
import GHC.TypeLits

class Registered l where
  command :: Proxy l -> String

instance Registered "btc" where
  command _ = "BTC"

instance Registered "eth" where
  command _ = "ETH"

instance Registered "bch" where
  command _ = "BCH"

instance Registered "ltc" where
  command _ = "LTC"

instance Registered "xrp" where
  command _ = "XRP"

instance Registered "xem" where
  command _ = "XEM"

instance Registered "xlm" where
  command _ = "XLM"

instance Registered "xym" where
  command _ = "XYM"

instance Registered "mona" where
  command _ = "MONA"

instance Registered "btc_jpy" where
  command _ = "BTC_JPY"

instance Registered "eth_jpy" where
  command _ = "ETH_JPY"

instance Registered "bch_jpy" where
  command _ = "BCH_JPY"

instance Registered "ltc_jpy" where
  command _ = "LTC_JPY"

instance Registered "xrp_jpy" where
  command _ = "XRP_JPY"

instance Registered "all" where
  command _ = "All"

data ALL = ALL deriving (Show, Eq)

all_ :: Lens' ALL ALL 
all_ = lens (const ALL) (const id)

instance Default ALL where
  def = ALL

data Command' s a = forall l. (KnownSymbol l, Registered l) => Relate (Lens' s a) (Proxy l)

lookupRegisteredSS :: Eq s => [Command' s s] -> s -> Last [(String, String)]
lookupRegisteredSS = lookup_
  where
    lookup_ :: Eq s => [Command' s s] -> s -> Last [(String, String)]
    lookup_ [] _ = Last Nothing
    lookup_ ((Relate l p) : xs) ans = if (ans ^. l) == ans then Last (Just [(symbolVal &&& command) p]) else lookup_ xs ans

lookupFromRegisteredSS :: [Command' s s] -> String -> s -> Last s
lookupFromRegisteredSS = lookup_
  where
    lookup_ :: [Command' s s] -> String -> s -> Last s
    lookup_ [] _ _ = Last Nothing
    lookup_ ((Relate l p) : xs) s ans = if symbolVal p == s then Last (Just $ ans ^. l) else lookup_ xs s ans

isRegistered_ :: [Command' s a] -> String -> Bool
isRegistered_ [] _ = False
isRegistered_ ((Relate l p) : xs) s = symbolVal p == s || isRegistered_ xs s



class CommandObj s a | s -> a where
  symbols :: [Command' s a]
  lookupRegistered :: s -> Last [(String, String)]
  lookupFromRegisteredA :: String -> s -> Last a

instance CommandObj Coin Coin where
  symbols = 
    [
      Relate btc_ (Proxy :: Proxy "btc"), 
      Relate eth_ (Proxy :: Proxy "eth"),
      Relate bch_ (Proxy :: Proxy "bch"),
      Relate ltc_ (Proxy :: Proxy "ltc"),
      Relate xrp_ (Proxy :: Proxy "xrp"),
      Relate xem_ (Proxy :: Proxy "xem"),
      Relate xlm_ (Proxy :: Proxy "xlm"),
      Relate xym_ (Proxy :: Proxy "xym"),
      Relate mona_ (Proxy :: Proxy "mona"),
      Relate btc_jpy_ (Proxy :: Proxy "btc_jpy"),
      Relate eth_jpy_ (Proxy :: Proxy "eth_jpy"),
      Relate bch_jpy_ (Proxy :: Proxy "bch_jpy"),
      Relate ltc_jpy_ (Proxy :: Proxy "ltc_jpy"),
      Relate xrp_jpy_ (Proxy :: Proxy "xrp_jpy")
    ]
    
  lookupRegistered = lookupRegisteredSS symbols
  lookupFromRegisteredA = lookupFromRegisteredSS symbols


instance CommandObj ALL ALL where
  symbols = 
    [
      Relate all_ (Proxy :: Proxy "all")
    ]
    
  lookupRegistered = lookupRegisteredSS symbols
  lookupFromRegisteredA = lookupFromRegisteredSS symbols

data Descriptor s = 
  Descriptor 
  {
    _tag :: String,
    _elem :: s 
  } deriving Eq

makeLenses ''Descriptor

commandPrint :: String -> String -> String
commandPrint a b = a <> ": " <> b

instance (Show s, CommandObj s a) => Show (Descriptor [s]) where
  show (Descriptor t xs) = t <> enum xs  
    where
      enum xs = intercalate " or " (uncurry commandPrint  . (maybe ("", "") head . getLast . lookupRegistered) <$> xs) 

class Description d where
  descript :: d -> Descriptor d 

instance Description [Coin] where
  descript = Descriptor "Coin - " 