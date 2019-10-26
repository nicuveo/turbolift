{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}



module Control.Monad.TurboLift (Below, hoist) where



import           Control.Monad.Trans
import           Data.Proxy
import           Data.Type.Equality



type Below b m = Below' b m (b == m)

hoist :: forall b m a. Below b m => b a -> m a
hoist = hoistImpl (Proxy :: Proxy (b == m))



class Below' b m (eq :: Bool) where
  hoistImpl :: Proxy eq -> b a -> m a

instance b ~ m => Below' b m 'True where
  hoistImpl _ = id

instance (MonadTrans t, Monad m, Below b m) => Below' b (t m) 'False where
  hoistImpl _ = lift . hoist
