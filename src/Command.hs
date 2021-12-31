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

module Command where


import Common
import Control.Arrow
import Control.Lens
import Data.Default.Class
import Data.Monoid
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
      Relate eth_ (Proxy :: Proxy "eth")
    ]
    
  lookupRegistered = lookupRegisteredSS symbols
  lookupFromRegisteredA = lookupFromRegisteredSS symbols