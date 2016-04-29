module Yesod.Bootstrap where

import Prelude hiding (div)
import Data.Foldable
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
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Data.Function (on)
import Data.String (IsString(..))
import Data.Char (isDigit)
import Text.Julius (rawJS)

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

strong_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
strong_ attrs inner = [whamlet|$newline never
<strong *{mkStrAttrs attrs}>^{inner}|]

em_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
em_ attrs inner = [whamlet|$newline never
<em *{mkStrAttrs attrs}>^{inner}|]

s_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
s_ attrs inner = [whamlet|<s *{mkStrAttrs attrs}>^{inner}|]

nav_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
nav_ attrs inner = [whamlet|<nav *{mkStrAttrs attrs}>^{inner}|]

form_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
form_ attrs inner = [whamlet|<form *{mkStrAttrs attrs}>^{inner}|]

script_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
script_ attrs inner = [whamlet|<script *{mkStrAttrs attrs}>^{inner}|]

label_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
label_ attrs inner = [whamlet|<label *{mkStrAttrs attrs}>^{inner}|]

pre_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
pre_ attrs inner = [whamlet|<pre *{mkStrAttrs attrs}>^{inner}|]

code_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
code_ attrs inner = [whamlet|<code *{mkStrAttrs attrs}>^{inner}|]

input_ :: [(Text,Text)] -> WidgetT site IO ()
input_ attrs = [whamlet|<input *{mkStrAttrs attrs}>|]

hr_ :: [(Text,Text)] -> WidgetT site IO ()
hr_ attrs = [whamlet|<hr *{mkStrAttrs attrs}>|]

br_ :: [(Text,Text)] -> WidgetT site IO ()
br_ attrs = [whamlet|<br *{mkStrAttrs attrs}>|]

img_ :: [(Text,Text)] -> WidgetT site IO ()
img_ attrs = [whamlet|<img *{mkStrAttrs attrs}>|]

textarea_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
textarea_ attrs inner = [whamlet|<textarea *{mkStrAttrs attrs}>^{inner}|]

td_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
td_ attrs inner = [whamlet|<td *{mkStrAttrs attrs}>^{inner}|]

th_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
th_ attrs inner = [whamlet|<th *{mkStrAttrs attrs}>^{inner}|]

table_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
table_ attrs inner = [whamlet|<table *{mkStrAttrs attrs}>^{inner}|]

tbody_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
tbody_ attrs inner = [whamlet|<tbody *{mkStrAttrs attrs}>^{inner}|]

tfoot_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
tfoot_ attrs inner = [whamlet|<tfoot *{mkStrAttrs attrs}>^{inner}|]

thead_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
thead_ attrs inner = [whamlet|<thead *{mkStrAttrs attrs}>^{inner}|]

tr_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
tr_ attrs inner = [whamlet|<tr *{mkStrAttrs attrs}>^{inner}|]

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

ol_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
ol_ attrs inner = [whamlet|<ol *{mkStrAttrs attrs}>^{inner}|]

li_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
li_ attrs inner = [whamlet|<li *{mkStrAttrs attrs}>^{inner}|]

blockquote_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
blockquote_ attrs inner = [whamlet|<blockquote *{mkStrAttrs attrs}>^{inner}|]

small_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
small_ attrs inner = [whamlet|<small *{mkStrAttrs attrs}>^{inner}|]

i_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
i_ attrs inner = [whamlet|<i *{mkStrAttrs attrs}>^{inner}|]

a_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
a_ attrs inner = [whamlet|$newline never
<a *{mkStrAttrs attrs}>^{inner}|]

audio_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
audio_ attrs inner = [whamlet|<audio *{mkStrAttrs attrs}>^{inner}|]

source_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
source_ attrs inner = [whamlet|<source *{mkStrAttrs attrs}>^{inner}|]

anchor :: Route site -> WidgetT site IO () -> WidgetT site IO ()
anchor route inner = [whamlet|<a href="@{route}">^{inner}|]

anchorEmail :: Text -> WidgetT site IO () -> WidgetT site IO ()
anchorEmail email inner = [whamlet|<a href="mailto:#{email}">^{inner}|]

anchorPhone :: Text -> WidgetT site IO ()
anchorPhone phone = [whamlet|<a href="tel:#{cleanedPhone}">#{phone}|]
  where 
  strippedPhone = Text.filter ((||) <$> isDigit <*> (== '+')) phone
  cleanedPhone = case Text.uncons strippedPhone of
    Nothing -> Text.empty
    Just (c,cs) -> if c == '+' 
      then strippedPhone
      else Text.append "+1" strippedPhone

button_ :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
button_ attrs inner = [whamlet|<button *{mkStrAttrs attrs}>^{inner}|]

mkStrAttrs :: [(Text,Text)] -> [(String,String)]
mkStrAttrs = map $ \(a,b) -> (Text.unpack a, Text.unpack b)

row :: WidgetT site IO () -> WidgetT site IO ()
row = div_ [("class","row")]

container :: WidgetT site IO () -> WidgetT site IO ()
container = div_ [("class","container")]

well :: Size -> WidgetT site IO () -> WidgetT site IO ()
well size = div_ [("class","well well-" <> colSizeShortName size)]

col :: [ColSize] -> WidgetT site IO () -> WidgetT site IO ()
col cs = div_ [("class", Text.intercalate " " (map mkAttr cs))]
  where mkAttr (ColSize s n) = Text.concat ["col-", colSizeShortName s, "-", Text.pack (show n)]

checkbox :: WidgetT site IO () -> WidgetT site IO ()
checkbox = div_ [("class","checkbox")]

alert :: Context -> WidgetT site IO () -> WidgetT site IO ()
alert ctx = div_ [("class","alert alert-" <> contextName ctx)]

alertHtml :: Context -> Html -> Html
alertHtml ctx inner = H.div H.! HA.class_ (fromString $ Text.unpack $ "alert alert-" <> contextName ctx) $ inner

alertHtmlDismiss :: Context -> Html -> Html
alertHtmlDismiss ctx inner = H.div H.! HA.class_ (fromString $ Text.unpack $ "alert alert-dismissable alert-" <> contextName ctx) $ do
  H.button H.! HA.class_ "close" H.! HA.type_ "button" H.! H.dataAttribute "dismiss" "alert" $ H.preEscapedToHtml ("&times;" :: Text)
  inner

caret :: WidgetT site IO ()
caret = span_ [("class","caret")] mempty

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

button :: Context -> Size -> WidgetT site IO () -> WidgetT site IO ()
button ctx size inner = do
  button_ [("class","btn btn-" <> contextName ctx <> " btn-" <> colSizeShortName size)] inner

buttonRaised :: Context -> Size -> WidgetT site IO () -> WidgetT site IO ()
buttonRaised ctx size inner = do
  button_ [("class","btn btn-raised btn-" <> contextName ctx <> " btn-" <> colSizeShortName size)] inner

formButtonPost :: Context -> Size -> Route site -> WidgetT site IO () -> WidgetT site IO ()
formButtonPost ctx size route inner = do
  render <- getUrlRender
  form_ [("method","POST"),("action",render route)] $ do
    button ctx size inner

formButtonRaisedPost :: Context -> Size -> Route site -> WidgetT site IO () -> WidgetT site IO ()
formButtonRaisedPost ctx size route inner = do
  render <- getUrlRender
  form_ [("method","POST"),("action",render route)] $ do
    buttonRaised ctx size inner

anchorButton :: Context -> Size -> Route site -> WidgetT site IO () -> WidgetT site IO ()
anchorButton ctx size route inner = do
  render <- getUrlRender
  a_ [("href",render route),("class","btn btn-" <> contextName ctx <> " btn-" <> colSizeShortName size)] inner

anchorButtonRaised :: Context -> Size -> Route site -> WidgetT site IO () -> WidgetT site IO ()
anchorButtonRaised ctx size route inner = do
  render <- getUrlRender
  a_ [("href",render route),("class","btn btn-raised btn-" <> contextName ctx <> " btn-" <> colSizeShortName size)] inner

anchorButtonBlock :: Context -> Size -> Route site -> WidgetT site IO () -> WidgetT site IO ()
anchorButtonBlock ctx size route inner = do
  render <- getUrlRender
  a_ [("href",render route),("class","btn btn-block btn-" <> contextName ctx <> " btn-" <> colSizeShortName size)] inner

anchorButtonRaisedBlock :: Context -> Size -> Route site -> WidgetT site IO () -> WidgetT site IO ()
anchorButtonRaisedBlock ctx size route inner = do
  render <- getUrlRender
  a_ [("href",render route),("class","btn btn-raised btn-block btn-" <> contextName ctx <> " btn-" <> colSizeShortName size)] inner

label :: Context -> WidgetT site IO () -> WidgetT site IO ()
label ctx = span_ [("class","label label-" <> contextName ctx)]

badge :: WidgetT site IO () -> WidgetT site IO ()
badge = span_ [("class","badge")]

panel :: Panel site -> WidgetT site IO ()
panel (Panel title content ctx) = do
  div_ [("class","panel panel-" <> contextName ctx)] $ do
    div_ [("class", "panel-heading")] $ do
      h4_ [("class","panel-title")] title
    div_ [("class","panel-body")] $ do
      content

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

textSubmitGroupGetForm :: Route site -> Context -> Size -> Text -> Text -> Text -> WidgetT site IO () -> Bool -> WidgetT site IO ()
textSubmitGroupGetForm route ctx size name placeholder value buttonContent buttonIsLeft = do
  render <- getUrlRender
  form_ [("method","GET"),("action",render route)] $ do
    div_ [("class","form-group")] $ do
      div_ [("class","input-group input-group-" <> colSizeShortName size)] 
        $ mconcat
        $ (if buttonIsLeft then reverse else id)
        [ input_ [("class","form-control"),("type","text"),("name",name),("placeholder",placeholder),("value",value)]
        , span_ [("class","input-group-btn")] $ do
            button_ [("class","btn btn-" <> contextName ctx)] buttonContent
        ]
        

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
  Danger -> "danger"

data NavbarTheme = NavbarDefault | NavbarInverse | NavbarOtherTheme Text
data NavbarPosition = NavbarStandard | NavbarStaticTop | NavbarFixedTop
data NavbarItem site 
  = NavbarLink (Route site) (WidgetT site IO ())
  | NavbarDropdown (WidgetT site IO ()) [NavbarDropdownItem site]

data NavbarDropdownItem site 
  = NavbarDropdownLink (Route site) (WidgetT site IO ())
  | NavbarDropdownHeader (WidgetT site IO ())
  | NavbarDropdownSeparator

navbar :: 
     NavbarTheme
  -> NavbarPosition 
  -> Route site 
  -> WidgetT site IO ()
  -> [NavbarItem site]
  -> [NavbarItem site]
  -> WidgetT site IO ()
navbar theme pos headerRoute headerContent items rightItems = do
  navbarId <- newIdent
  render <- getUrlRender
  nav_ [("class","navbar " <> themeClass <> " " <> posClass)] $ do
    div_ [("class",containerClass)] $ do
      div_ [("class", "navbar-header")] $ do
        button_ [ ("class", "navbar-toggle collapsed"),("type","button")
                , ("data-toggle", "collapse"), ("aria-expanded", "false")
                , ("aria-controls", navbarId),("data-target", "#" <> navbarId)
                ] $ do
          span_ [("class","sr-only")] $ tw "Toggle Navigation"
          replicateM_ 3 $ span_ [("class","icon-bar")] mempty
        a_ [("href", render headerRoute),("class","navbar-brand")] headerContent
      div_ [("class","navbar-collapse collapse"), ("id", navbarId)] $ do
        ul_ [("class","nav navbar-nav")] $ mapM_ navbarItem items
        ul_ [("class","nav navbar-nav navbar-right")] $ mapM_ navbarItem rightItems
  where 
  themeClass = case theme of
    NavbarDefault -> "navbar-default" 
    NavbarInverse -> "navbar-inverse" 
    NavbarOtherTheme t -> "navbar-" <> t
  posClass = case pos of
    NavbarStandard -> ""
    NavbarStaticTop -> "navbar-static-top"
    NavbarFixedTop -> "navbar-fixed-top"
  containerClass = case pos of
    NavbarStandard -> "container-fluid"
    NavbarStaticTop -> "container"
    NavbarFixedTop -> "container"
  
navbarItem :: NavbarItem site -> WidgetT site IO ()
navbarItem item = do
  render <- getUrlRender
  li_ [] $ case item of
    NavbarLink route name -> anchor route name
    NavbarDropdown name children -> do
      a_ [ ("class","dropdown-toggle"), ("href", "#")
         , ("role", "button"), ("data-toggle", "dropdown")
         ] name
      ul_ [("class","dropdown-menu")] $ mapM_ navbarDropdownItem children

navbarDropdownItem :: NavbarDropdownItem site -> WidgetT site IO ()
navbarDropdownItem item = do
  render <- getUrlRender
  case item of
    NavbarDropdownLink route name -> li_ [] $ anchor route name
    NavbarDropdownHeader name -> li_ [("class","dropdown-header")] name
    NavbarDropdownSeparator -> li_ [("class","separator"),("role","divider")] mempty

-- Stands for text widget
tw :: Text -> WidgetT site IO ()
tw = toWidget . toHtml 

data CarouselItem site = CarouselItem
  { ciImage   :: Route site
  , ciLink    :: Maybe (Route site)
  , ciCaption :: Maybe (WidgetT site IO ())
  }

data CarouselIndicators = CarouselIndicatorsOn | CarouselIndicatorsOff
  deriving (Eq)
data CarouselControls = CarouselControlsOn | CarouselControlsOff
  deriving (Eq)

-- Carousel Element 
carousel :: CarouselIndicators -> CarouselControls -> [CarouselItem site] -> WidgetT site IO ()
carousel indicators controls items = if length items == 0 then mempty else do
  render <- getUrlRender
  carouselId <- newIdent
  div_ [("class","carousel slide"),("data-ride","carousel"),("id",carouselId)] $ do
    when (indicators == CarouselIndicatorsOn) $ do
      ol_ [("class","carousel-indicators")] $ do
        forM_ (zip [0,1..] itemsActive) $ \(i,(active,_)) -> do
          li_ [ ("data-target", "#" <> carouselId)
              , ("data-slide-to", Text.pack (show i))
              , ("class", if active then "active" else "")
              ] mempty
    div_ [("class","carousel-inner"), ("role","listbox")] $ do
      forM_ itemsActive $ \(active,item) -> do
        div_ [("class","item " <> if active then "active" else "")] $ do
          wrapWithLink (ciLink item) mempty
          img_ [("src",render (ciImage item))]
          for_ (ciCaption item) $ \caption -> do
            div_ [("class","carousel-caption")] caption
    when (controls == CarouselControlsOn) $ do
      a_ [ ("class","left carousel-control")
         , ("href","#" <> carouselId)
         , ("role","button")
         , ("data-slide","prev")
         ] $ do
         glyphicon "chevron-left"
         span_ [("class","sr-only")] "Previous"
      a_ [ ("class","right carousel-control")
         , ("href","#" <> carouselId)
         , ("role","button")
         , ("data-slide","next")
         ] $ do
         glyphicon "chevron-right"
         span_ [("class","sr-only")] "Next"
  where 
  itemsActive = zip (True : repeat False) items
  wrapWithLink :: Maybe (Route site) -> WidgetT site IO () -> WidgetT site IO ()
  wrapWithLink mroute w = (\ww -> maybe w ww mroute) $ \route -> do
    render <- getUrlRender
    a_ [("href", render route),("style","position:absolute;left:0;right:0;width:100%;height:100%;")] w

-- Togglable tabs
data ToggleTab site = ToggleSection Text (WidgetT site IO ()) | ToggleDropdown Text [(Text,WidgetT site IO ())]
data ToggleStyle = ToggleStyleTab | ToggleStylePill

togglableTabs :: ToggleStyle -> [ToggleTab site] -> WidgetT site IO ()
togglableTabs s tabs = do
  (nav,bodies) <- execWriterT $ forM_ (zip [1..] tabs) $ \(i,tab) -> case tab of
    ToggleSection title body -> do -- WriterT (Widget,Widget) over a WidgetT
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
    let styleText = case s of
          ToggleStyleTab -> "nav-tabs"
          ToggleStylePill -> "nav-pills"
    ul_ [("class","nav " <> styleText),("role","tablist")] nav
    div_ [("class","tab-content")] bodies
  where 
  tellFst a = tell (a,mempty)
  tellSnd b = tell (mempty,b)
  addClass :: Text -> [(Text,Text)] -> [(Text,Text)]
  addClass klass attrs = case List.lookup "class" attrs of
    Nothing -> ("class",klass) : attrs
    Just c -> ("class",c <> " " <> klass) : List.deleteBy ((==) `on` fst) ("class","") attrs

radioButtons :: Context -> Text -> [(Text, WidgetT site IO ())] -> WidgetT site IO ()
radioButtons ctx name xs = do
  div_ [("class","btn-group"),("data-toggle","buttons")] $ do
    forM_ (zip trueThenFalse xs) $ \(isFirst, (theValue,w)) -> do
      label_ [("class", "btn btn-" <> contextName ctx <> if isFirst then " active" else "")] $ do
        input_ $ (if isFirst then [("checked","checked")] else [])
              ++ [("type","radio"),("name",name),("value",theValue),("autocomplete","off")] 
        " "
        w

listGroupLinked :: [(Route site,WidgetT site IO ())] -> WidgetT site IO ()
listGroupLinked items = do
  render <- getUrlRender
  div_ [("class","list-group")] $ forM_ items $ \(route,name) -> do
    a_ [("href",render route),("class","list-group-item")] name

breadcrumbsList :: [(Route site,WidgetT site IO ())] -> WidgetT site IO ()
breadcrumbsList allCrumbs = case reverse allCrumbs of
  (_,lastCrumbWidget):crumbs -> ol_ [("class","breadcrumb")] $ do
    forM_ (reverse crumbs) $ \(route,name) -> li_ [] $ anchor route name
    li_ [("class","active")] lastCrumbWidget
  [] -> mempty

popover :: WidgetT site IO () -> WidgetT site IO () -> WidgetT site IO () -> WidgetT site IO ()
popover title popup inner = do
  innerId <- newIdent
  popupWrapId <- newIdent
  titleWrapId <- newIdent
  a_ [("href","javascript://"),("id",innerId)] inner
  div_ [("id",popupWrapId),("style","display:none;")] $ do
    popup
  div_ [("id",titleWrapId),("style","display:none;")] $ do
    title
  toWidget [julius|
$().ready(function(){
  $('##{rawJS innerId}').popover(
    { html: true
    , trigger: 'focus'
    , content: function() { return $('##{rawJS popupWrapId}').html(); }
    , title: function() { return $('##{rawJS titleWrapId}').html(); }
    }
  );
});
|]

trueThenFalse :: [Bool]
trueThenFalse = True : repeat False

