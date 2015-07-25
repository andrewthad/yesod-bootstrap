module Yesod.Bootstrap where

import Prelude hiding (div)
import Yesod.Core.Widget
import Data.Text (Text)
import Data.List
import Data.Monoid
import Text.Blaze.Html (toHtml)
import qualified Data.Text as Text

data Context = Success | Info | Warning | Danger | Default | Primary | Link | Error
data Size = ExtraSmall | Small | Medium | Large
data ColSize = ColSize Size Int
data Flow = Block | Inline

div_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
div_ attrs inner = [whamlet|<div *{mkStrAttrs attrs}>^{inner}|]

span_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
span_ attrs inner = [whamlet|<span *{mkStrAttrs attrs}>^{inner}|]

label_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
label_ attrs inner = [whamlet|<label *{mkStrAttrs attrs}>^{inner}|]

input_ :: [(Text,Text)] -> WidgetT site IO ()
input_ attrs = [whamlet|<input *{mkStrAttrs attrs}>|]

ul_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
ul_ attrs inner = [whamlet|<ul *{mkStrAttrs attrs}>^{inner}|]

li_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
li_ attrs inner = [whamlet|<li *{mkStrAttrs attrs}>^{inner}|]

button_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
button_ attrs inner = [whamlet|<button *{mkStrAttrs attrs}>^{inner}|]

mkStrAttrs :: [(Text,Text)] -> [(String,String)]
mkStrAttrs = map $ \(a,b) -> (Text.unpack a, Text.unpack b)

row :: WidgetT site IO () -> WidgetT site IO ()
row = div_ [("class","row")]

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

