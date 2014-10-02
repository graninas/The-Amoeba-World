module Amoeba.GameLogic.Language.Parsing.Common where

import Amoeba.Middleware.Parsing.Facade as P
import Amoeba.GameLogic.Language.RawToken

import Control.Monad (liftM)

emptyToken :: GenParser Char st RawToken
emptyToken = eol >> return EmptyToken

comment :: GenParser Char st RawToken
comment = liftM CommentToken commentString