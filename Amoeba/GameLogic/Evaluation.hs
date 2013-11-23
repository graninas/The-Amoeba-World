{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module GameLogic.Evaluation where

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Either as E
import Control.Monad
import Control.Lens
import qualified Data.Sequence as Seq
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, listToMaybe)
import Prelude hiding (read)

import GameLogic.Geometry
import GameLogic.Object
import GameLogic.AI as AI
import GameLogic.GenericAI as GAI
import qualified GameLogic.GenericWorld as GW
import Misc.Descriptions

-- TODO: replace _originalObject by objectId when it is introduced.
-- | Transaction Nothing (Just obj)      : obj  is added
-- | Transaction (Just obj1) (Just obj2) : obj1 is updated and now it's obj2
-- | Transaction (Just obj) Nothing      : obj  is removed
-- | Transaction Nothing Nothing         : nonsense, no operation
-- TODO: replace object: [Transaction obj1 Nothing, Transaction Nothing obj2)
--                   or: [Transaction obj1 obj2] ?

data Transaction = Transaction { _sourceObject :: Maybe Object
                               , _actualObject  :: Maybe Object }
  deriving (Show, Read, Eq)
type TransactionMap = GW.GenericMap Transaction

type Query = Object -> Bool
type TransQuery = Transaction -> Bool

data EvalError = ENoSuchProperty String
               | ENoActedObjectSet
               | ENotFound
               | EOverlappedObjects Objects
  deriving (Show, Read, Eq)

type EvalType ctx res = EitherT EvalError (State ctx) res
type Eval res = EvalType EvaluationContext res
type EvalState res = State EvaluationContext res

data DataContext = DataContext { _dataObjects :: Eval Objects
                               , _dataObjectGraph :: Eval (NeighboursFunc -> ObjectGraph)
                               , _dataObjectAt :: Point -> Eval (Maybe Object) }

data EvaluationContext = EvaluationContext { _ctxData :: DataContext
                                           , _ctxTransactionMap :: TransactionMap
                                           , _ctxActedObject :: Maybe Object
                                           , _ctxNextRndNum :: Eval Int }

makeLenses ''DataContext
makeLenses ''EvaluationContext
makeLenses ''Transaction

eNoSuchProperty = ENoSuchProperty
eNoActedObjectSet = ENoActedObjectSet
eNotFound = ENotFound
eOverlappedObjects = EOverlappedObjects
isEOverlappedObjects (EOverlappedObjects _) = True
isEOverlappedObjects _ = False
isENotFound ENotFound = True
isENotFound _ = False

noSuchProperty prop obj = E.left $ eNoSuchProperty $ describeNoProperty prop obj
notFound = E.left eNotFound
noActedObjectSet = E.left eNoActedObjectSet
overlappedObjects objs = E.left $ eOverlappedObjects objs

-- context
noActedObject = Nothing

dataContext = DataContext
context dtCtx = EvaluationContext dtCtx GW.emptyMap noActedObject

nextRndNum :: Eval Int
nextRndNum = get >>= _ctxNextRndNum

ctxObjects     = ctxData . dataObjects
ctxObjectGraph = ctxData . dataObjectGraph
ctxObjectAt    = ctxData . dataObjectAt

-- helpers

-- Naming convention:
-- getXyz :: Eval Xyz
-- findXyz :: Eval (Maybe Xyz)
-- queryXyz :: Eval Xyzs

-- TODO: replace by Graph
getObjectAt :: Point -> Eval (Maybe Object)
getObjectAt p = get >>= flip (_dataObjectAt . _ctxData) p

getObjects :: Eval Objects
getObjects = get >>= (_dataObjects . _ctxData)

getObjectGraph :: NeighboursFunc -> Eval ObjectGraph
getObjectGraph nsFunc = do
    nsObjectGraphFunc <- get >>= (_dataObjectGraph . _ctxData)
    return $ nsObjectGraphFunc nsFunc

fromMap m = m ^.. folding id

getTransactionMap :: Eval TransactionMap
getTransactionMap = get >>= E.right . _ctxTransactionMap

rolloutTransactionMap = M.fold f []
  where
    f (Transaction _ (Just obj)) objs = obj : objs
    f (Transaction (Just obj) Nothing) objs = obj : objs
    f (Transaction Nothing Nothing) _ = error "rolloutTransactionMap: impossible"

getTransactionObjects :: Eval Objects
getTransactionObjects = do
    transMap <- getTransactionMap
    return $ rolloutTransactionMap transMap

sourcedTransaction obj = Transaction (Just obj) Nothing
actuatedTransaction obj = Transaction Nothing (Just obj)
replacedTransaction obj1 obj2 = Transaction (Just obj1) (Just obj2)
 

isJustTrue :: Maybe Bool -> Bool
isJustTrue (Just x) = x
isJustTrue Nothing = False

queryProperty prop pred obj = let
    mbVal = obj ^? prop
    mbRes = liftM pred mbVal
    in if isJustTrue mbRes
            then mbVal
            else Nothing

filterObjects q = liftM (filter q)
filterTransactionMap q = liftM (M.filter (transQuery q))

transQuery :: Query -> TransQuery
transQuery q (Transaction _ (Just obj)) = q obj
transQuery q (Transaction (Just obj) Nothing) = q obj
transQuery _ (Transaction Nothing Nothing) = error "transQuery: impossible"

qTrans  q = filterObjects q getTransactionObjects
qSrc    q = filterObjects q getObjects
qActual q = do
    transMap <- filterTransactionMap q getTransactionMap
    objs     <- filterObjects q getObjects
    return $ mixObjects transMap objs
  where
    mixObjects transM [] = rolloutTransactionMap transM
    mixObjects transM (o:objs) | M.null transM = objs
                               | otherwise = let objDisl = o ^. singular objectDislocation
                                             in case M.lookup objDisl transM of -- TODO: make it safe!
                                                Just (Transaction (Just _) Nothing) -> mixObjects (M.delete objDisl transM) objs
                                                Just (Transaction _ (Just tObj)) -> tObj : mixObjects (M.delete objDisl transM) objs
                                                Just (Transaction Nothing Nothing) -> error "mixObjects impossible"
                                                Nothing -> o : mixObjects transM objs

querySpec qStrategy q = do
    objs <- qStrategy q
    case objs of
        [] -> notFound
        xs -> E.right xs

singleSpec qStrategy q = do
    founds <- querySpec qStrategy q
    case founds of
        []     -> notFound
        (x:[]) -> E.right x
        xs     -> overlappedObjects xs

-- querying

(~&~) p1 p2 obj = p1 obj && p2 obj
infixr 3 ~&~

is prop val        = isJust . queryProperty prop (val ==) :: Query
suchThat prop pred = isJust . queryProperty prop pred     :: Query
justAll :: Query
justAll _ = True

singleActual :: Query -> Eval Object
singleActual = singleSpec qActual

single :: Query -> Eval Object
single = singleSpec qSrc

query :: Query -> Eval Objects
query = querySpec qSrc

find :: Query -> Eval (Maybe Object)
find q  = liftM listToMaybe (query q) :: Eval (Maybe Object)

withDefault defVal (EitherT m) = m >>= \z -> case z of
    Left  _ -> E.right defVal
    Right r -> E.right r

getProperty prop obj = maybe (noSuchProperty prop obj) E.right (obj ^? prop)

read prop = getActedObject >>= getProperty prop

-- evaluation

setupActedObject :: Object -> EvalState ()
setupActedObject obj = ctxActedObject .= Just obj
dropActedObject :: EvalState ()
dropActedObject = ctxActedObject .= Nothing
getActedObject :: Eval Object
getActedObject = do
    mbActedObject <- use ctxActedObject
    maybe noActedObjectSet E.right mbActedObject

-- FIXME: not working
updateTransactionMap f p = do
    transMap <- getTransactionMap
    let newTransMap = M.update f p transMap
    ctx <- get
    let (res, newCtx) = runState (ctxTransactionMap .= newTransMap) ctx
    put newCtx
    return res

save :: Object -> Eval ()
save obj = do
    p <- getProperty objectDislocation obj
    transMap <- getTransactionMap
    void $ updateTransactionMap f p
  where
    f _ = Just $ actuatedTransaction obj -- TODO: merge target cell to save.

replace :: Object -> Object -> Eval ()
replace obj1 obj2 = do
    p <- getProperty objectDislocation obj1
    void $ updateTransactionMap f p
  where
    f _ = Just $ replacedTransaction obj1 obj2 -- TODO: merge target cell to save.

rollback :: TransactionMap -> EvalError -> EvalState String
rollback m err = do
    ctxTransactionMap .= m
    return $ "Transaction error: " ++ show err
commit :: String -> EvalState String
commit = return

transact :: Eval String -> Object -> EvalState String
transact act obj = do
        setupActedObject obj
        oldTransMap <- use ctxTransactionMap
        actRes <- eitherT (rollback oldTransMap) commit act
        dropActedObject
        return actRes

forProperty prop act = doForProperty :: Eval [String]
  where
    doForProperty = do
        objs <- filterObjects (has prop) getObjects
        evalTransact act objs

evaluatePlacementAlg PlaceToNearestEmptyCell l obj = do
    p        <- getProperty objectDislocation obj
    objGraph <- getObjectGraph sideNeighbours
    let mbRes = nearestEmpty (obj ^? ownership) l obj objGraph
    maybe notFound extractPoint mbRes
  where
        extractPoint [] = notFound
        extractPoint ps = E.right $ nodePoint $ last ps

evaluateMovingAlg m obj = do
    p <- getProperty objectDislocation obj
    return $ AI.move p m

evalTransact :: Eval String -> Objects -> Eval [String]
evalTransact act [] = return []
evalTransact act (o:os) = do
    ctx <- get
    let (res, newCtx) = runState (transact act o) ctx
    put newCtx
    ress <- evalTransact act os
    return (res : ress)

evaluate scenario = evalState (runEitherT scenario)
execute scenario = execState (runEitherT scenario)
run scenario = runState (runEitherT scenario)
