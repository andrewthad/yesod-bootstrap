{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Control.Applicative ((<$>), (<*>))
import           Data.Text           (Text)
import           Data.Time           (Day)
import           Yesod
import           Yesod.Static
import           Yesod.Markdown
import           Yesod.Form.Jquery
import           Yesod.Form.Generic.Bootstrap as B
import           Yesod.Form.Generic.Bootstrap.Internal
import           Yesod.Form.Generic (GForm)
import           Yesod.Bootstrap
import           Control.Applicative
import           Lens.Family
import           Data.Char
import qualified Data.Text as Text

data App = App
  { appStatic :: Static
  }

mkYesod "App" [parseRoutes|
/static StaticR   Static         appStatic
/mkd    MarkdownR MarkdownRender getMarkdownRender

/ HomeR GET
/person PersonR POST
|]

instance Yesod App

instance YesodUpload App where
  uploadDirectory _ = "example/upload"
  uploadRoute (UploadFilename t) = StaticR (StaticRoute [t] [])

instance YesodMarkdownRender App where
  markdownRenderSubsite = MarkdownR

-- Tells our application to use the standard English messages.
-- If you want i18n, then you can supply a translating function instead.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- And tell us where to find the jQuery libraries. We'll just use the defaults,
-- which point to the Google CDN.
instance YesodJquery App

-- The datatype we wish to receive from the form
data Person = Person
  { personName          :: Text
  , personBirthday      :: Maybe Day
  , personFavoriteColor :: Maybe Text
  -- , personEmail         :: Text
  , personAge           :: Maybe Int
  , personWebsite       :: Text
  , personAlive         :: Bool
  , personUpload        :: UploadFilename
  , personDescription   :: Markdown
  } deriving Show

personGForm :: GForm Widget Handler Person
personGForm = Person
  <$> B.text (label "Name" & fcValue .~ Just "Drew")
  <*> B.dayCheck (label "Birthday") 
  <*> B.textCheck (label "Favorite Color" & fcValue .~ Just Nothing) 
  -- <*> B.email (label "Email") 
  <*> B.intCheck (label "Age" & fcValue .~ Just (Just 44)) 
  <*> B.select websites ("Website" & fcValue .~ Just "drew.com")
  <*> B.bool (label "Currently Living") 
  <*> B.file ("File" & fcValue .~ Just (UploadFilename "VBFWZXVZNSBCCCSDWGMMQNNF.png"))
  <*> B.markdown "Description"
  <*  B.submit Primary "Create"
  where websites = optionsPairs $ map (\a -> (a,a))
          ["google.com","drew.com","yahoo.com"]

startsCapital :: Text -> Handler (Either (SomeMessage App) Text)
startsCapital t = return $ case Text.unpack t of
  (c:_) -> if isUpper c then Right t else Left (SomeMessage ("Must start with a capital letter." :: Text))
  [] -> Left (SomeMessage ("Should not be empty." :: Text))

addThings :: Widget
addThings = do
  addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
  addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/2.1.4/jquery.min.js"
  addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"

-- The GET handler displays the form
getHomeR :: Handler Html
getHomeR = do
    -- Generate the form to be displayed
    (widget, enctype) <- generateFormPost (B.render personGForm)
    defaultLayout $ addThings >> 
        [whamlet|
            <div.container>
                <p>
                    The widget generated contains only the contents
                    of the form, not the form tag itself. So...
                <form method=post action=@{PersonR} enctype=#{enctype}>
                    ^{widget}
        |]

-- The POST handler processes the form. If it is successful, it displays the
-- parsed person. Otherwise, it displays the form again with error messages.
postPersonR :: Handler Html
postPersonR = do
    ((result, widget), enctype) <- runFormPost (B.render personGForm)
    case result of
        FormSuccess person -> defaultLayout [whamlet|<p>#{show person}|]
        _ -> defaultLayout $ addThings >> 
            [whamlet|
                <div.container>
                    <p>Invalid input, let's try again.
                    <form method=post action=@{PersonR} enctype=#{enctype}>
                        ^{widget}
            |]

main :: IO ()
main = do
  s <- static "example/upload"
  warp 3000 (App s)
