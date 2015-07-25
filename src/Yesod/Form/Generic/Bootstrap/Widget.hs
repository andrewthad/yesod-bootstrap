module Yesod.Form.Generic.Bootstrap.Widget where

import Yesod.Form
import Yesod.Form.Generic
import Yesod.Core
import Yesod.Core.Widget
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Text.Email.Validate as Email
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

label :: Monad m => Text -> FieldConfig m a
label t = mempty { _fcLabel = Just (tw t) }

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
      glyphiconFeedback "ok"
    FormFailure errs -> formGroupFeedback Error $ do
      baseInput $ fromMaybe "" $ listToMaybe vals
      glyphiconFeedback "remove"
      helpBlock $ ul_ [("class","list-unstyled")] $ mapM_ (li_ [] . tw) errs

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
        glyphiconFeedback "ok"
      (False,FormFailure errs) -> (if greenOnSuccess then formGroupFeedback Success else formGroup) $ do
        maybe mempty controlLabel (_fcLabel config)
        baseInputGroup ""
        glyphiconFeedback "ok"
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

-- select :: (MonadHandler m, HandlerSite m ~ site, RenderMessage site FormMessage)
--   => HandlerT site IO (OptionList a) -> FieldConfig m Text -> GForm (WidgetT site IO ()) m Text
-- select opts config = let Field parse view enctype = selectField opts in do

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
labelAndInput label name typ readonly val = do
  let baseAttrs = [("class","form-control"),("type",typ),("name",name),("value",val)]
      addReadonly = if readonly then (("readonly","readonly"):) else id
  controlLabel label
  input_ (addReadonly $ baseAttrs)

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
text = simple "text" (gparseHelper (return . specialRight) Nothing) id

textOpt :: (MonadHandler m, HandlerSite m ~ site, RenderMessage site FormMessage)
  => FieldConfig m (Maybe Text) -> GForm (WidgetT site IO ()) m (Maybe Text)
textOpt = simple "text" (gparseHelper (return . specialRight . Just) (Just Nothing)) (fromMaybe "")

textCheck :: (MonadHandler m, HandlerSite m ~ site, RenderMessage site FormMessage)
  => FieldConfig m (Maybe Text) -> GForm (WidgetT site IO ()) m (Maybe Text)
textCheck = simpleCheck "text" (gparseHelper (return . specialRight . Just) Nothing) (fromMaybe "")

int :: (MonadHandler m, HandlerSite m ~ site, RenderMessage site FormMessage)
  => FieldConfig m Int -> GForm (WidgetT site IO ()) m Int
int = simple "number" (gparseHelper (return . parseInt) Nothing) (Text.pack . show)

intCheck :: (MonadHandler m, HandlerSite m ~ site, RenderMessage site FormMessage)
  => FieldConfig m (Maybe Int) -> GForm (WidgetT site IO ()) m (Maybe Int)
intCheck = simpleCheck "number" (gparseHelper (return . fmap Just . parseInt) Nothing) (maybe "" (Text.pack . show))

day :: (MonadHandler m, HandlerSite m ~ site, RenderMessage site FormMessage)
  => FieldConfig m Day -> GForm (WidgetT site IO ()) m Day
day = simple "date" (gparseHelper (return . parseDate . Text.unpack) Nothing) (Text.pack . show)

dayCheck :: (MonadHandler m, HandlerSite m ~ site, RenderMessage site FormMessage)
  => FieldConfig m (Maybe Day) -> GForm (WidgetT site IO ()) m (Maybe Day)
dayCheck = simpleCheck "date" (gparseHelper (return . fmap Just . parseDate . Text.unpack) Nothing) (maybe "" (Text.pack . show))
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

checkBoxParser :: Text -> Either Text Bool
checkBoxParser x = case x of
  "yes" -> Right True :: Either Text Bool
  "on"  -> Right True
  _     -> Right False

textEmailValidate :: Text -> Either FormMessage Text
textEmailValidate t = if Email.isValid (Text.encodeUtf8 t)
  then Right t
  else Left (MsgInvalidEmail t)

submit :: Monad m => Context -> Text -> GForm (WidgetT site IO ()) m ()
submit ctx t = monoidToGForm $ button_ [("type","submit"),("class","btn btn-" <> contextName ctx)] $ tw t

parseInt :: Text -> Either FormMessage Int
parseInt t = case readMaybe (Text.unpack t) of
  Nothing -> Left (MsgInvalidInteger t)
  Just n -> Right n
  
specialRight :: a -> Either Text a 
specialRight = Right

gparseHelper :: (MonadHandler m, HandlerSite m ~ site, RenderMessage site FormMessage, RenderMessage site msg)
             => (Text -> m (Either msg a))
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

