```haskell
{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import Text.Digestive
import Text.Digestive.Scotty

import Control.Applicative ((<$>), (<*>))

data FormData = FormData { field1 :: String
                         , field2 :: Integer
                         }

testForm :: Monad m => Form String m FormData
testForm = FormData
           <$> "field1" .: string Nothing
           <*> "field2" .: stringRead "read failed" Nothing

main :: IO ()
main = scotty 3000 $ do
    get "/" $ do
        setHeader "Content-Type" "text/html"
        raw " <html><body><form enctype='multipart/form-data' method='post'> \
            \ <input type='text' name='test-form.field1'/> \
            \ <input type='text' name='test-form.field2'/> \
            \ <input type='submit'/> \
            \ </form></body></html>"

    post "/" $ do
        (view, result) <- runForm "test-form" testForm
        case result of
            Just d -> ... Success! `d` contains constructed FormData.
            _ -> ... Failure! Use `view` variable to access error messages.
```

