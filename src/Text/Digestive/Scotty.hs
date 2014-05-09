{-|
Module providing the scotty backend for the digestive-functors library

>{-# LANGUAGE OverloadedStrings #-}
>
>import Web.Scotty
>
>import Text.Digestive
>import Text.Digestive.Scotty
>
>import Control.Applicative ((<$>), (<*>))
>
>data FormData = FormData { field1 :: String
>                         , field2 :: Integer
>                         }
>
>testForm :: Monad m => Form String m FormData
>testForm = FormData
>           <$> "field1" .: string Nothing
>           <*> "field2" .: stringRead "read failed" Nothing
>
>main :: IO ()
>main = scotty 3000 $ do
>    get "/" $ do
>        setHeader "Content-Type" "text/html"
>        raw " <html><body><form enctype='multipart/form-data' method='post'> \
>              \ <input type='text' name='test-form.field1'/> \
>              \ <input type='text' name='test-form.field2'/> \
>              \ <input type='submit'/> \
>              \ </form></body></html>"
>
>    post "/" $ do
>        (view, result) <- runForm "test-form" testForm
>        case result of
>            Just d -> ... Success! `d` contains constructed FormData.
>            _ -> ... Failure! Use `view` variable to access error messages.
-}
module Text.Digestive.Scotty
    ( runForm
    ) where

import Control.Monad ( liftM )
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Lazy as TL
import qualified Web.Scotty.Trans as Scotty
import Network.Wai ( requestMethod )
import Network.Wai.Parse ( fileName )
import Network.HTTP.Types ( methodGet )

import Text.Digestive.Form
import Text.Digestive.Types
import Text.Digestive.View

scottyEnv :: (Monad m, Scotty.ScottyError e) => Env (Scotty.ActionT e m)
scottyEnv path = do
    inputs <- parse (TextInput . TL.toStrict) Scotty.params
    files  <- parse (FileInput . B.unpack . fileName) Scotty.files
    return $ inputs ++ files
  where parse f = liftM $ map (f . snd) . filter ((== name) . fst)
        name    = TL.fromStrict . fromPath $ path

-- | Runs a form with the HTTP input provided by Scotty.
runForm :: (Monad m, Scotty.ScottyError e)
        => T.Text                                 -- ^ Name of the form
        -> Form v (Scotty.ActionT e m) a          -- ^ Form to run
        -> (Scotty.ActionT e m) (View v, Maybe a) -- ^ Result
runForm name form = Scotty.request >>= \rq ->
    if requestMethod rq == methodGet
        then getForm name form >>= \v -> return (v, Nothing)
        else postForm name form $ const (return scottyEnv)

