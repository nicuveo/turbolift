{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}



module Control.Monad.TurboLift (Below, hoist) where



import           Control.Monad.Trans
import           Data.Proxy
import           Data.Type.Equality



type Below b m = Below' b m (b == m)

hoist :: forall b m a. b `Below` m => b a -> m a
hoist = hoist' (Proxy :: Proxy (b == m))



class Below' b m (eq :: Bool) where
  hoist' :: Proxy eq -> b a -> m a

instance b ~ m => Below' b m 'True where
  hoist' _ = id

instance (MonadTrans t, Monad m, b `Below` m) => Below' b (t m) 'False where
  hoist' _ = lift . hoist
