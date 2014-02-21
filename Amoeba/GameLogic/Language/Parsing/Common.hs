module GameLogic.Language.Parsing.Common where

import Middleware.Parsing.Facade as P
import GameLogic.Language.RawToken

import Control.Monad (liftM)

emptyToken :: GenParser Char st RawToken
emptyToken = eol >> return EmptyToken

comment :: GenParser Char st RawToken
comment = liftM CommentToken commentString