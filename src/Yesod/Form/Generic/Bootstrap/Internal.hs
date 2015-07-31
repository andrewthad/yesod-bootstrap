module Yesod.Form.Generic.Bootstrap.Internal where

import Yesod.Core

data MarkdownRender = MarkdownRender
mkYesodSubData "MarkdownRender" [parseRoutes|
/markdown/render MarkdownRenderR POST
|]

