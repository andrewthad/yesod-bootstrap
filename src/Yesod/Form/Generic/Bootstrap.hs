module Yesod.Form.Generic.Bootstrap where

import Yesod.Form
import Yesod.Form.Generic
import Yesod.Core
import Yesod.Core.Widget
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Text.Email.Validate as Email
import qualified Data.Yaml as Yaml
import Text.Shakespeare.I18N (RenderMessage)
import Yesod.Bootstrap
import Data.Maybe
import Text.Read (readMaybe)
import Control.Applicative
import Control.Monad
import Data.Time (Day)
import Data.Monoid
import Lens.Family
import Lens.Family2.TH
import Text.Julius (rawJS)
import Debug.Trace
import Data.Either.Combinators
import Data.String
import Control.Monad.Random
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import Yesod.Markdown
import Data.Conduit
import Data.Conduit.Lazy (lazyConsume)
import qualified Data.Conduit.Text as Conduit
import Text.Blaze.Html (preEscapedToHtml)
import Yesod.Form.Generic.Bootstrap.Internal 

class YesodMarkdownRender site where
  markdownRenderSubsite :: Route MarkdownRender -> Route site

instance YesodSubDispatch MarkdownRender (HandlerT master IO) where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesMarkdownRender)

getMarkdownRender :: a -> MarkdownRender
getMarkdownRender = const MarkdownRender

postMarkdownRenderR :: HandlerT MarkdownRender (HandlerT site IO) Html
postMarkdownRenderR = do
  ts <- lazyConsume $ rawRequestBody =$= Conduit.decode Conduit.utf8
  let mkd = Markdown $ Text.concat ts
  return $ markdownToHtmlCustom mkd

markdownToHtmlCustom :: Markdown -> Html
markdownToHtmlCustom m@(Markdown t)
  | m == mempty = preEscapedToHtml ("<span class=\"text-muted\">Preview</span>" :: Text)
  | otherwise   = case markdownToHtml (Markdown (Text.filter (/= '\r') t)) of
      Left _ -> preEscapedToHtml ("<span class=\"text-muted\">Could not render</span>" :: Text)
      Right a -> a

data FieldConfig m a = FieldConfig
  { _fcLabel :: Maybe (WidgetT (HandlerSite m) IO ())
  , _fcPlaceholder :: Maybe (SomeMessage (HandlerSite m))
  , _fcTooltip :: Maybe (SomeMessage (HandlerSite m))
  , _fcId :: Maybe Text
  , _fcName :: Maybe Text
  , _fcValue :: Maybe a
  , _fcReadonly :: Bool
  , _fcValidate :: a -> m (Either (SomeMessage (HandlerSite m)) a)
  }
makeLenses ''FieldConfig

instance Monad m => Monoid (FieldConfig m a) where
  mempty = FieldConfig Nothing Nothing Nothing Nothing Nothing Nothing False (return . Right)
  FieldConfig a1 b1 c1 d1 e1 f1 r1 v1 `mappend` FieldConfig a2 b2 c2 d2 e2 f2 r2 v2 = 
    FieldConfig (a2 <|> a1) (b2 <|> b1) (c2 <|> c1) (d2 <|> d1) (e2 <|> e1) (f2 <|> f1) (r1 || r2) 
    $ \a -> do
      e <- v1 a
      case e of 
        Left err -> return $ Left err
        Right a' -> return $ Right a'

instance Monad m => IsString (FieldConfig m a) where
  fromString s = mempty {_fcLabel = Just (toWidget (toHtml s))}

render :: Monad m => GForm (WidgetT site IO ()) m a -> Html -> MForm m (FormResult a, WidgetT site IO ())
render g h = gFormToForm (monoidToGForm (toWidget h) *> g)

greenOnSuccess :: Bool
greenOnSuccess = False

simple :: (MonadHandler m, HandlerSite m ~ site, RenderMessage site FormMessage)
  => Text -- ^ input type
  -> ([Text] -> [FileInfo] -> m (FormResult a))
  -> (a -> Text)
  -> FieldConfig m a -> GForm (WidgetT site IO ()) m a
simple typ parser display config = ghelper UrlEncoded (fullValidate parser (_fcValidate config)) $ \name vals res -> 
  let baseInput = labelAndInput (fromMaybe mempty $ _fcLabel config) name typ (_fcReadonly config)
  in case res of
    FormMissing -> formGroup $ baseInput $ maybe "" display (_fcValue config)
    FormSuccess a -> (if greenOnSuccess then formGroupFeedback Success else formGroup) $ do
      baseInput (display a)
    FormFailure errs -> formGroupFeedback Error $ do
      baseInput $ fromMaybe "" $ listToMaybe vals
      glyphiconFeedback "remove"
      helpBlock $ ul_ [("class","list-unstyled")] $ mapM_ (li_ [] . tw) errs

class YesodTypeahead site where
  routeTypeaheadJs :: site -> Route site
  routeTypeaheadCss :: site -> Route site

-- The result from the route is expected to be a JSON array. Also, the route
-- is expected to respond to a POST request with the body as the text we are
-- searching for.
typeahead :: (YesodTypeahead site, MonadHandler m, HandlerSite m ~ site, RenderMessage site FormMessage)
  => Route site -> FieldConfig m Text -> GForm (WidgetT site IO ()) m Text
typeahead route config = ghelper UrlEncoded (fullValidate parser (_fcValidate config)) $ \name vals res -> do
  inputId <- newIdent
  let baseAttrs = [("id",inputId),("class","form-control"),("name",name),("type","text")]
      addReadonly = if _fcReadonly config then (("readonly","readonly"):) else id
      attrs = addReadonly baseAttrs
      baseInput t = do
        whenMaybe (_fcLabel config) controlLabel
        input_ attrs
  yesod <- getYesod
  addStylesheet $ routeTypeaheadCss yesod
  addScript $ routeTypeaheadJs yesod
  typeaheadJs route inputId 
  case res of
    FormMissing -> formGroup $ baseInput $ maybe "" display (_fcValue config)
    FormSuccess a -> (if greenOnSuccess then formGroupFeedback Success else formGroup) $ do
      baseInput (display a)
    FormFailure errs -> formGroupFeedback Error $ do
      baseInput $ fromMaybe "" $ listToMaybe vals
      glyphiconFeedback "remove"
      helpBlock $ ul_ [("class","list-unstyled")] $ mapM_ (li_ [] . tw) errs
  where parser = (gparseHelper (return . Right) Nothing)
        display = id

typeaheadJs :: Route site -> Text -> WidgetT site IO ()
typeaheadJs route inputId = toWidget [julius|
$().ready(function(){
  var serNum = $('##{rawJS inputId}');
  serNum.typeahead({
    source: function (query, process) {
      return $.post('@{route}', query, function(data){
        return process(data);
      });
    },
    items: 'all',
    minLength: 2,
    afterSelect: function(ser) { 
      // serNum.closest('form').submit();
    }
  });     
});
|]

simpleCheck :: (MonadHandler m, HandlerSite m ~ site, RenderMessage site FormMessage)
  => Text -- ^ input type
  -> ([Text] -> [FileInfo] -> m (FormResult (Maybe a)))
  -> (Maybe a -> Text)
  -> FieldConfig m (Maybe a) -> GForm (WidgetT site IO ()) m (Maybe a)
simpleCheck typ parser display config = formToGForm $ do
  (checkId, inputId) <- (,) <$> newIdent <*> newIdent
  (checkRes, checkWidget) <- mghelper UrlEncoded (fullValidate (gparseHelper (return . checkBoxParser) (Just False)) (return . Right)) $ \name _vals res -> do
    val <- decipherCheckRes res
    input_ $ boolAttrs [("checked",val)] ++ [("id",checkId),("name",name),("value","yes"),("type","checkbox")]
  (inputRes, inputWidget) <- mghelper UrlEncoded (fullValidate parser (_fcValidate config)) $ \name vals res -> do
    isChecked <- decipherCheckRes checkRes
    let baseInputGroup val = inputGroup $ do
          inputGroupAddon checkWidget
          input_ $ boolAttrs [("readonly", not isChecked)] ++ 
            [("id",inputId),("class","form-control"),("type",typ),("name",name),("value",val)]
    case (isChecked,res) of
      (_, FormMissing) -> formGroup $ do
        maybe mempty controlLabel (_fcLabel config)
        baseInputGroup $ maybe "" display (_fcValue config)
      (_, FormSuccess a) -> (if greenOnSuccess then formGroupFeedback Success else formGroup) $ do
        maybe mempty controlLabel (_fcLabel config)
        baseInputGroup (display a)
      (False,FormFailure errs) -> (if greenOnSuccess then formGroupFeedback Success else formGroup) $ do
        maybe mempty controlLabel (_fcLabel config)
        baseInputGroup ""
      (True,FormFailure errs) -> formGroupFeedback Error $ do
        maybe mempty controlLabel (_fcLabel config)
        baseInputGroup $ fromMaybe "" $ listToMaybe vals
        glyphiconFeedback "remove"
        helpBlock $ ul_ [("class","list-unstyled")] $ mapM_ (li_ [] . tw) errs
  return ( checkRes `bindFormResult` \b -> if traceShowId b then inputRes else pure Nothing
         , inputWidget <> simpleCheckJs checkId inputId )
  where decipherCheckRes r = case r of
          FormSuccess b -> return b
          FormFailure _ -> permissionDenied "Bootstrap checkbox field somehow failed" 
          FormMissing   -> return $ isJust $ join (_fcValue config)

select :: (RenderMessage site FormMessage, Eq a)
  => HandlerT site IO (OptionList a) 
  -> FieldConfig (HandlerT site IO) a 
  -> GForm (WidgetT site IO ()) (HandlerT site IO) a
select opts c = ghelper UrlEncoded 
  (fieldParseToGParse parse) $ \name vals res -> do
    theId <- newIdent 
    let r = case res of
              FormSuccess a -> Right a
              FormMissing -> case _fcValue c of
                Nothing -> Left ""
                Just a -> Right a
              FormFailure _ -> Left ""
    formGroup $ do
      whenMaybe (_fcLabel c) controlLabel
      view theId name [("class","form-control")] r True
  where Field parse view enctype = selectField opts

newtype UploadFilename = UploadFilename { getUploadFilename :: Text }
  deriving (PersistField, PersistFieldSql, Show, Read)

class YesodUpload site where
  uploadDirectory :: site -> String
  uploadRoute :: UploadFilename -> Route site

yaml :: (FromJSON a, ToJSON a, HandlerSite m ~ site, MonadHandler m, RenderMessage site FormMessage) 
  => a -> FieldConfig m a -> GForm (WidgetT site IO ()) m a
yaml example c = ghelper UrlEncoded (fullValidate (gparseHelper (return . mapLeft (SomeMessage . Text.pack) . Yaml.decodeEither . Text.encodeUtf8) Nothing) (_fcValidate c))
  $ \name vals res -> do
    preId    <- newIdent
    buttonId <- newIdent
    yamlJs preId buttonId
    let thePre = pre_ [("id",preId),("style","display:none")] $ tw $ yamlEncodeText example
        baseAttrs = [("class","form-control"),("name",name),("rows","5")]
        addReadonly = if (_fcReadonly c) then (("readonly","readonly"):) else id
        attrs = addReadonly baseAttrs
        baseInput t = do
          whenMaybe (_fcLabel c) controlLabel
          textarea_ attrs (tw t)
    case res of
      FormMissing -> formGroup $ do
        baseInput $ maybe "" yamlEncodeText (_fcValue c)
        button_ [("class","btn btn-link"),("id",buttonId),("type","button")] $ tw "Show Example"
        thePre
      FormSuccess a -> (if greenOnSuccess then formGroupFeedback Success else formGroup) $ do
        baseInput (yamlEncodeText a)
        button_ [("class","btn btn-link"),("id",buttonId),("type","button")] $ tw "Show Example"
        thePre
      FormFailure errs -> formGroupFeedback Error $ do
        baseInput $ fromMaybe "" $ listToMaybe vals
        glyphiconFeedback "remove"
        helpBlock $ ul_ [("class","list-unstyled")] $ mapM_ (li_ [] . tw) errs
        button_ [("class","btn btn-link"),("id",buttonId),("type","button")] $ tw "Show Example"
        thePre
  where yamlEncodeText = Text.decodeUtf8 . Yaml.encode
  
yamlJs :: Text -> Text -> WidgetT site IO ()
yamlJs preId buttonId = toWidget [julius|
$().ready(function(){
  var showing = false;
  var button = $('##{rawJS buttonId}');
  var pre = $('##{rawJS preId}');
  button.click(function(){
    showing = !showing;
    pre.slideToggle();
    if(showing)
      button.text("Hide Example");
    else
      button.text("Show Example");
  });
});
|]

markdown :: (YesodMarkdownRender site, MonadHandler m, HandlerSite m ~ site, RenderMessage site FormMessage)
  => FieldConfig m Markdown -> GForm (WidgetT site IO ()) m Markdown
markdown c = ghelper UrlEncoded (fullValidate (gparseHelper (return . Right . Markdown . Text.filter (/= '\r')) Nothing) (_fcValidate c)) 
  $ \name vals res -> do
    wellId <- newIdent
    inputId <- newIdent
    render <- getUrlRender
    markdownJs wellId inputId (render $ markdownRenderSubsite MarkdownRenderR)
    let theWell = helpBlock . div_ [("class","well well-sm"),("id",wellId)] . toWidget
        baseAttrs = [("class","form-control"),("name",name),("rows","5"),("id",inputId)]
        addReadonly = if (_fcReadonly c) then (("readonly","readonly"):) else id
        attrs = addReadonly baseAttrs
        baseInput t = do
          whenMaybe (_fcLabel c) controlLabel
          textarea_ attrs (tw t)
    case res of
      FormMissing -> formGroup $ do
        baseInput $ maybe "" unMarkdown (_fcValue c)
        theWell $ markdownToHtmlCustom $ fromMaybe mempty (_fcValue c)
      FormSuccess a -> (if greenOnSuccess then formGroupFeedback Success else formGroup) $ do
        baseInput (unMarkdown a)
        theWell $ markdownToHtmlCustom a
      FormFailure errs -> formGroupFeedback Error $ do
        baseInput $ fromMaybe "" $ listToMaybe vals
        glyphiconFeedback "remove"
        theWell $ markdownToHtmlCustom mempty
        helpBlock $ ul_ [("class","list-unstyled")] $ mapM_ (li_ [] . tw) errs

markdownJs :: Text -> Text -> Text -> WidgetT site IO ()
markdownJs wellId inputId url = toWidget [julius|
$().ready(function(){
  var hasChanged = false;
  var input = $('##{rawJS inputId}');
  var well = $('##{rawJS wellId}');
  var runUpdate = function(){
    if(!hasChanged) return;
    $.ajax({ type: "POST"
           , url: "#{rawJS url}"
           , data: input.val()
           , dataType: 'html'
           , success: function(data) {
               well.html(data);
               hasChanged = false;
             } 
           });
  };
  setInterval(runUpdate, 1000);
  input.on('input',function(){
    hasChanged = true;
  });
});
|]

file :: (YesodUpload site, MonadHandler m, HandlerSite m ~ site, RenderMessage site FormMessage)
  => FieldConfig m UploadFilename -> GForm (WidgetT site IO ()) m UploadFilename
file c = ghelper Multipart
  (fullValidate (fileParseHelper (_fcValue c)) (_fcValidate c)) $ \name _vals res -> do
    formGroup $ do
      whenMaybe (_fcLabel c) controlLabel
      input_ [("type","file"),("name",name)] 
      case res of
        FormFailure errs -> helpBlock $ ul_ [("class","list-unstyled")] $ mapM_ (li_ [] . tw) errs
        _ -> mempty
      whenMaybe (_fcValue c) $ \filename -> do
        render <- getUrlRender
        helpBlock $ img_ [("width","140"),("src",render $ uploadRoute filename),("class","img-thumbnail")]
    
  
fileParseHelper :: (YesodUpload site, MonadHandler m, HandlerSite m ~ site, RenderMessage site FormMessage)
  => (Maybe UploadFilename) -- ^ If this is Just, then the field is not required.
  -> [Text] -> [FileInfo] -> m (FormResult UploadFilename)
fileParseHelper mdef _ [] = return $ case mdef of
  Nothing -> FormMissing
  Just a -> FormSuccess a
fileParseHelper _ _ (x:_) = do
  app <- getYesod
  name <- liftIO $ moveIt (uploadDirectory app) x
  return (FormSuccess name)

moveIt :: String -> FileInfo -> IO UploadFilename
moveIt dir fi = do
  baseFilename <- randomUpperConsonantText 24
  let ext = snd $ Text.breakOn "." (fileName fi)
      fullFileName = baseFilename <> ext
  fileMove fi $ Text.unpack $ mempty 
    <> Text.pack dir 
    <> "/" 
    <> fullFileName
  return $ UploadFilename fullFileName

randomUpperConsonantText :: Int -> IO Text
randomUpperConsonantText n = id
  $ fmap Text.pack 
  $ evalRandIO 
  $ replicateM n
  $ uniform allConsonants
  where allConsonants = filter (not . isVowel) ['A'..'Z']

isVowel :: Char -> Bool
isVowel c = case c of
  'a' -> True
  'e' -> True
  'i' -> True
  'o' -> True
  'u' -> True
  'A' -> True
  'E' -> True
  'I' -> True
  'O' -> True
  'U' -> True
  _   -> False

simpleCheckJs :: Text -> Text -> WidgetT site IO ()
simpleCheckJs checkId inputId = toWidget [julius|
$().ready(function(){
  $('##{rawJS checkId}').change(function(){
    var enabled = this.checked;
    var input = $('##{rawJS inputId}');
    if (!enabled) input.val("");
    input.prop('readonly',!enabled);
  });
});
|]

bindFormResult :: FormResult a -> (a -> FormResult b) -> FormResult b
bindFormResult ra f = case ra of
  FormFailure errs -> FormFailure errs
  FormMissing -> FormMissing
  FormSuccess a -> f a

ifA :: Applicative f => f Bool -> f a -> f a -> f a
ifA t c a = g <$> t <*> c <*> a where g b x y = if b then x else y

boolAttrs :: [(Text,Bool)] -> [(Text,Text)]
boolAttrs = map (\t -> (fst t, fst t)) . filter snd

labelAndInput :: WidgetT site IO () -> Text -> Text -> Bool -> Text -> WidgetT site IO ()
labelAndInput labelWidget name typ readonly val = do
  let baseAttrs = [("class","form-control"),("type",typ),("name",name),("value",val)]
      addReadonly = if readonly then (("readonly","readonly"):) else id
  controlLabel labelWidget
  input_ (addReadonly $ baseAttrs)

fieldParseToGParse :: (MonadHandler m)
  => ([Text] -> [FileInfo] -> m (Either (SomeMessage (HandlerSite m)) (Maybe a)))
  -> [Text] -> [FileInfo] -> m (FormResult a)
fieldParseToGParse parse ts fs = do
  e <- parse ts fs 
  case e of
    Left msg -> do 
      langs <- languages
      site  <- getYesod
      return $ FormFailure [renderMessage site langs msg]
    Right Nothing -> return FormMissing
    Right (Just a) -> return (FormSuccess a)

fullValidate :: MonadHandler m 
  => ([Text] -> [FileInfo] -> m (FormResult a)) 
  -> (a -> m (Either (SomeMessage (HandlerSite m)) a))
  -> [Text] -> [FileInfo] -> m (FormResult a)
fullValidate parser validate ts fs = do
  res <- parser ts fs 
  case res of
    FormSuccess a -> do
      e <- validate a
      case e of
        Left msg -> do
          langs <- languages
          site  <- getYesod
          return $ FormFailure [renderMessage site langs msg]
        Right b -> return $ FormSuccess b
    _ -> return res

text :: (MonadHandler m, HandlerSite m ~ site, RenderMessage site FormMessage)
  => FieldConfig m Text -> GForm (WidgetT site IO ()) m Text
text = simple "text" 
  (gparseHelper (return . Right) Nothing) id

textOpt :: (MonadHandler m, HandlerSite m ~ site, RenderMessage site FormMessage)
  => FieldConfig m (Maybe Text) -> GForm (WidgetT site IO ()) m (Maybe Text)
textOpt = simple "text" (gparseHelper (return . Right . Just) (Just Nothing)) (fromMaybe "")

textCheck :: (MonadHandler m, HandlerSite m ~ site, RenderMessage site FormMessage)
  => FieldConfig m (Maybe Text) -> GForm (WidgetT site IO ()) m (Maybe Text)
textCheck = simpleCheck "text" 
  (gparseHelper (return . Right . Just) Nothing) 
  (fromMaybe "")

int :: (MonadHandler m, HandlerSite m ~ site, RenderMessage site FormMessage)
  => FieldConfig m Int -> GForm (WidgetT site IO ()) m Int
int = simple "number" (gparseHelper (return . parseInt) Nothing) (Text.pack . show)

intCheck :: (MonadHandler m, HandlerSite m ~ site, RenderMessage site FormMessage)
  => FieldConfig m (Maybe Int) -> GForm (WidgetT site IO ()) m (Maybe Int)
intCheck = simpleCheck "number" 
  (gparseHelper (return . fmap Just . parseInt) Nothing) 
  (maybe "" (Text.pack . show))

day :: (MonadHandler m, HandlerSite m ~ site, RenderMessage site FormMessage)
  => FieldConfig m Day -> GForm (WidgetT site IO ()) m Day
day = simple "date" 
  (gparseHelper (return . mapLeft SomeMessage . parseDate . Text.unpack) Nothing) 
  (Text.pack . show)

dayCheck :: (MonadHandler m, HandlerSite m ~ site, RenderMessage site FormMessage)
  => FieldConfig m (Maybe Day) -> GForm (WidgetT site IO ()) m (Maybe Day)
dayCheck = simpleCheck "date" 
  (gparseHelper (return . fmap Just . mapLeft SomeMessage . parseDate . Text.unpack) Nothing)
  (maybe "" (Text.pack . show))
-- 
-- email :: (MonadHandler m, HandlerSite m ~ site, RenderMessage site FormMessage)
--   => WidgetT site IO () -> Maybe Text -> GForm (WidgetT site IO ()) m Text
-- email = simple "email" (gparseHelper textEmailValidate Nothing) id
-- 
bool :: (MonadHandler m, HandlerSite m ~ site, RenderMessage site FormMessage)
  => FieldConfig m Bool -> GForm (WidgetT site IO ()) m Bool
bool config = ghelper UrlEncoded (fullValidate (gparseHelper (return . checkBoxParser) (Just False)) (_fcValidate config)) $ \name _vals res -> do
  val <- case res of
    FormSuccess b -> return b
    FormFailure _ -> permissionDenied "Bootstrap checkbox field somehow failed" -- should be impossible
    FormMissing   -> return $ fromMaybe False (_fcValue config)
  let applyVal = if val then (("checked","checked"):) else id
  checkbox $ label_ [] $ do 
    input_ $ applyVal [("name",name),("value","yes"),("type","checkbox")]
    fromMaybe mempty $ _fcLabel config

checkBoxParser :: Text -> Either (SomeMessage site) Bool
checkBoxParser x = case x of
  "yes" -> Right True 
  "on"  -> Right True
  _     -> Right False

textEmailValidate :: Text -> Either FormMessage Text
textEmailValidate t = if Email.isValid (Text.encodeUtf8 t)
  then Right t
  else Left (MsgInvalidEmail t)

submit :: Monad m => Context -> Text -> GForm (WidgetT site IO ()) m ()
submit ctx t = monoidToGForm $ button_ [("type","submit"),("class","btn btn-" <> contextName ctx)] $ tw t

parseInt :: RenderMessage site FormMessage => Text -> Either (SomeMessage site) Int
parseInt t = case readMaybe (Text.unpack t) of
  Nothing -> Left (SomeMessage (MsgInvalidInteger t))
  Just n -> Right n
  
-- specialRight :: a -> Either Text a 
-- specialRight = Right

gparseHelper :: (MonadHandler m, HandlerSite m ~ site, RenderMessage site FormMessage)
             => (Text -> m (Either (SomeMessage site) a))
             -> (Maybe a) -- ^ If this is Just, then the field is not required.
             -> [Text] -> [FileInfo] -> m (FormResult a)
gparseHelper _ mdef [] _ = return $ case mdef of
  Nothing -> FormMissing
  Just a -> FormSuccess a
gparseHelper _ mdef ("":_) _ = case mdef of
  Nothing -> do
    langs <- languages
    site <- getYesod
    return $ FormFailure [renderMessage site langs MsgValueRequired]
  Just a -> return (FormSuccess a)
gparseHelper f _ (x:_) _  = do
  e <- f x
  case e of
    Left msg -> do
      langs <- languages
      site <- getYesod
      return $ FormFailure [renderMessage site langs msg]
    Right a -> return (FormSuccess a)

whenMaybe :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenMaybe Nothing _ = pure ()
whenMaybe (Just a) f = f a

