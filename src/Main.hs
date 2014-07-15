{-# LANGUAGE OverloadedStrings #-}
 
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import Data.Monoid
import Data.Text (Text)
import Control.Eff

main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app

app :: Application 
app req resp = 
    resp $ case pathInfo req of
        ["yay"] -> yay
        x -> index x
 
yay :: Response
yay = responseBuilder status200 [ ("Content-Type", "text/plain") ] $ mconcat $ map copyByteString
    [ "yay" ]

index :: [Text] -> Response
index x = responseBuilder status200 [("Content-Type", "text/html")] $ mconcat $ map copyByteString
    [ "<p>Hello from ", BU.fromString $ show x, "!</p>"
    , "<p><a href='/yay'>yay</a></p>\n" ]


-- writeCookie :: (Typeable c, Member (WriteCookie c)) r => c -> Eff r ()
-- writeCookie = undefined

-- execSQL :: (Typeable c, Member (ExecSQL c) r) => (Query rel a -> c) -> Eff r a
-- execSQL = undefined

-- writeSession :: Member (WriteSession s) r => s -> Eff r ()
-- writeSession = undefined


-- runSQL :: Eff (ExecSQL c :> r) a -> Eff r a
-- runSQL = undefined

-- runWriteCookie :: Eff (WriteCookie c :> r) () -> Eff r ()
-- runWriteCookie = undefined

-- runReadCookie :: Eff (ReadCookie c :> r) a -> Eff r a
-- runReadCookie = undefined

data Session e = Session { sessionStore :: IO (Map String [String]) }

getFromSession :: String -> Eff (Session v) (Maybe [String])
getFromSession key = do
  
runGetSession :: Eff (Session e v :> r) a -> Eff r a
runGetSession action = loop (admin action) where
  loop (Val x) = return x
  loop (E u) = handleRelay u loop (\(Session s) -> 

firstTime = do
  return $ exists <- getFromSession "firstTime" >>= return . maybe True id

go = do
  let session = FileSession ""
  run $ runGetSession session firstTime 
    

