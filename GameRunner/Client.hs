{-# LANGUAGE OverloadedStrings #-}
module Client where

import Prelude hiding (mapM_)
import Control.Monad (forever, void)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)
import Network.Simple.TCP

main :: IO ()
main = listen "0.0.0.0" "9001" $ \(s, _) ->
  forever $ accept s $ \(c, _) -> void . runMaybeT $
    forever $ do
      MaybeT (recv c 4096) >>= lift . print
      lift $ send c "PAPER"