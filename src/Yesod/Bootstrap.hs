module Yesod.Bootstrap where

import Prelude hiding (div)
import Yesod.Core
import Yesod.Core.Widget
import Yesod.Core.Handler
import Data.Text (Text)
import Data.List
import Data.Monoid
import Control.Monad
import Text.Blaze.Html (toHtml)
import qualified Data.Text as Text
import Control.Monad.Writer.Class
import Control.Monad.Writer.Strict
import qualified Data.List as List
import Data.Function (on)

data Context = Success | Info | Warning | Danger | Default | Primary | Link | Error
data Size = ExtraSmall | Small | Medium | Large
data ColSize = ColSize Size Int
data Flow = Block | Inline
data Panel site = Panel 
  { panelTitle :: (WidgetT site IO ()) 
  , panelBody :: (WidgetT site IO ())
  , panelContext :: Context
  }

basicPanel :: Text -> WidgetT site IO () -> Panel site
basicPanel t c = Panel (tw t) c Default

div_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
div_ attrs inner = [whamlet|<div *{mkStrAttrs attrs}>^{inner}|]

span_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
span_ attrs inner = [whamlet|<span *{mkStrAttrs attrs}>^{inner}|]

label_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
label_ attrs inner = [whamlet|<label *{mkStrAttrs attrs}>^{inner}|]

input_ :: [(Text,Text)] -> WidgetT site IO ()
input_ attrs = [whamlet|<input *{mkStrAttrs attrs}>|]

img_ :: [(Text,Text)] -> WidgetT site IO ()
img_ attrs = [whamlet|<img *{mkStrAttrs attrs}>|]

h1_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
h1_ attrs inner = [whamlet|<h1 *{mkStrAttrs attrs}>^{inner}|]

h2_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
h2_ attrs inner = [whamlet|<h2 *{mkStrAttrs attrs}>^{inner}|]

h3_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
h3_ attrs inner = [whamlet|<h3 *{mkStrAttrs attrs}>^{inner}|]

h4_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
h4_ attrs inner = [whamlet|<h4 *{mkStrAttrs attrs}>^{inner}|]

h5_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
h5_ attrs inner = [whamlet|<h5 *{mkStrAttrs attrs}>^{inner}|]

h6_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
h6_ attrs inner = [whamlet|<h6 *{mkStrAttrs attrs}>^{inner}|]

p_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
p_ attrs inner = [whamlet|<p *{mkStrAttrs attrs}>^{inner}|]

ul_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
ul_ attrs inner = [whamlet|<ul *{mkStrAttrs attrs}>^{inner}|]

li_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
li_ attrs inner = [whamlet|<li *{mkStrAttrs attrs}>^{inner}|]

a_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
a_ attrs inner = [whamlet|<a *{mkStrAttrs attrs}>^{inner}|]

button_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
button_ attrs inner = [whamlet|<button *{mkStrAttrs attrs}>^{inner}|]

mkStrAttrs :: [(Text,Text)] -> [(String,String)]
mkStrAttrs = map $ \(a,b) -> (Text.unpack a, Text.unpack b)

row :: WidgetT site IO () -> WidgetT site IO ()
row = div_ [("class","row")]

container :: WidgetT site IO () -> WidgetT site IO ()
container = div_ [("class","container")]

col :: [ColSize] -> WidgetT site IO () -> WidgetT site IO ()
col cs = div_ [("class", Text.intercalate " " (map mkAttr cs))]
  where mkAttr (ColSize s n) = Text.concat ["col-", colSizeShortName s, "-", Text.pack (show n)]

checkbox :: WidgetT site IO () -> WidgetT site IO ()
checkbox = div_ [("class","checkbox")]

alert :: Context -> WidgetT site IO () -> WidgetT site IO ()
alert ctx = div_ [("class","alert alert-" <> contextName ctx)]

glyphicon :: Text -> WidgetT site IO ()
glyphicon s = span_ [("class","glyphicon glyphicon-" <> s)] mempty

glyphiconFeedback :: Text -> WidgetT site IO ()
glyphiconFeedback s = span_ [("class",Text.concat ["glyphicon glyphicon-", s, " form-control-feedback"])] mempty

formGroup :: WidgetT site IO () -> WidgetT site IO ()
formGroup = div_ [("class","form-group")]

formGroupFeedback :: Context -> WidgetT site IO () -> WidgetT site IO ()
formGroupFeedback ctx = div_ [("class",Text.concat ["form-group has-", contextName ctx, " has-feedback"])]

inputGroup :: WidgetT site IO () -> WidgetT site IO ()
inputGroup = div_ [("class","input-group")] 

inputGroupAddon :: WidgetT site IO () -> WidgetT site IO ()
inputGroupAddon = span_ [("class","input-group-addon")] 

controlLabel :: WidgetT site IO () -> WidgetT site IO ()
controlLabel = label_ [("class","control-label")]

helpBlock :: WidgetT site IO () -> WidgetT site IO ()
helpBlock = div_ [("class","help-block")]

anchorButton :: Context -> Route site -> WidgetT site IO () -> WidgetT site IO ()
anchorButton ctx route inner = do
  render <- getUrlRender
  a_ [("href",render route),("class","btn btn-" <> contextName ctx)] inner

panelAccordion :: [Panel site] -> WidgetT site IO ()
panelAccordion tcs = do 
  groupId <- newIdent 
  div_ [("class","panel-group"),("id",groupId),("role","tablist")] $ do
    forM_ (zip [1..] tcs) $ \(i,Panel title content ctx) -> do
      headingId <- newIdent
      panelId <- newIdent
      div_ [("class","panel panel-" <> contextName ctx)] $ do
        div_ [("class", "panel-heading"),("role","tab"),("id",headingId)] $ do
          h4_ [("class","panel-title")] $ do
            a_ [("href","#" <> panelId),("role","button"),("data-toggle","collapse"),("data-parent","#" <> groupId)] $ do
              title
        div_ [("id",panelId),("class","panel-collapse collapse" <> (if i == 1 then " in" else "")),("role","tabpanel"),("aria-labelledby",headingId)] $ do
          div_ [("class","panel-body")] $ do
            content

colSizeShortName :: Size -> Text
colSizeShortName s = case s of
  ExtraSmall -> "xs" 
  Small -> "sm" 
  Medium -> "md" 
  Large -> "lg" 

contextName :: Context -> Text
contextName c = case c of 
  Success -> "success"
  Info -> "info"
  Warning -> "warning"
  Default -> "default"
  Primary -> "primary"
  Link -> "link"
  Error -> "error"

-- Stands for text widget
tw :: Text -> WidgetT site IO ()
tw = toWidget . toHtml 

-- Togglable tabs
data ToggleTab site = ToggleSection Text (WidgetT site IO ()) | ToggleDropdown Text [(Text,WidgetT site IO ())]

togglableTabs :: [ToggleTab site] -> WidgetT site IO ()
togglableTabs tabs = do
  (nav,bodies) <- execWriterT $ forM_ (zip [1..] tabs) $ \(i,tab) -> case tab of
    ToggleSection title body -> do
      theId <- lift newIdent
      let tabAAttrs = [("role","tab"),("href","#" <> theId),("data-toggle","tab")]
          tabLiAttrs = (if isFirst then addClass "active" else id) [("role","presentation")]
          paneClasses = (if isFirst then addClass "active" else id)
            [("class","tab-pane"),("role","tabpanel"),("id",theId)]
          isFirst = (i == (1 :: Int))
      tellFst $ li_ tabLiAttrs $ a_ tabAAttrs $ tw title
      tellSnd $ div_ paneClasses body
    _ -> error "figure this out"
  div_ [] $ do
    ul_ [("class","nav nav-tabs"),("role","tablist")] nav
    div_ [("class","tab-content")] bodies
  where tellFst a = tell (a,mempty)
        tellSnd b = tell (mempty,b)
        addClass :: Text -> [(Text,Text)] -> [(Text,Text)]
        addClass klass attrs = case List.lookup "class" attrs of
          Nothing -> ("class",klass) : attrs
          Just c -> ("class",c <> " " <> klass) : List.deleteBy ((==) `on` fst) ("class","") attrs
