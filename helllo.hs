{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Data.Text.Lazy (pack) 

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev

  get "/hello" $ do
    text (pack "Testando Scooty Haskell") 