-- | Internal module containing the store definitions.
{-# LANGUAGE AllowAmbiguousTypes, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module React.Flux.Store (
    ReactStoreRef(..)
  , StoreData(..)

  -- * Old Stores
  , SomeStoreAction(..)
  , ReactStore(..)
  , mkStore
  , getStoreData
  , alterStore
  , executeAction

  -- * New Stores
  , NewReactStoreHS(..)
  , someStoreAction
  , registerInitialStore
  , transformStore
  , readStoreData

  -- * Util
  , typeJsKey
) where

import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, readMVar)
import Control.DeepSeq
import Data.Typeable
import System.IO.Unsafe (unsafePerformIO)

#ifdef __GHCJS__
import Data.Monoid ((<>))
import GHCJS.Types (JSVal, isNull, IsJSVal, JSString)
import React.Flux.Export
import GHC.Fingerprint.Type
import Data.JSString.Int (decimal)
#else
type JSVal = ()
type JSString = ()
class IsJSVal a
#endif

-- | A store contains application state, receives actions from the dispatcher, and notifies
-- controller-views to re-render themselves.  You can have multiple stores; it should be the case
-- that all of the state required to render the page is contained in the stores.  A store keeps a
-- global reference to a value of type @storeData@, which must be an instance of 'StoreData'.
--
-- Stores also work when compiled with GHC instead of GHCJS.  When compiled with GHC, the store is
-- just an MVar containing the store data and there are no controller views.  'alterStore' can still
-- be used, but it just 'transform's the store and does not notify any controller-views since there
-- are none.  Compiling with GHC instead of GHCJS can be helpful for unit testing, although GHCJS
-- plus node can also be used for unit testing.
--
-- >data Todo = Todo {
-- >    todoText :: Text
-- >  , todoComplete :: Bool
-- >  , todoIsEditing :: Bool
-- >} deriving (Show, Typeable)
-- >
-- >newtype TodoState = TodoState {
-- >    todoList :: [(Int, Todo)]
-- >} deriving (Show, Typeable)
-- >
-- >data TodoAction = TodoCreate Text
-- >                | TodoDelete Int
-- >                | TodoEdit Int
-- >                | UpdateText Int Text
-- >                | ToggleAllComplete
-- >                | TodoSetComplete Int Bool
-- >                | ClearCompletedTodos
-- >  deriving (Show, Typeable, Generic, NFData)
-- >
-- >instance StoreData TodoState where
-- >    type StoreAction TodoState = TodoAction
-- >    transform action (TodoState todos) = ...
-- >
-- >initTodoStore :: IO ()
-- >initTodoStore = registerInitialStore $ TodoState
-- >    [ (0, Todo "Learn react" True False)
-- >    , (1, Todo "Learn react-flux" False False)
-- >    ]
class Typeable storeData => StoreData storeData where
    -- | The actions that this store accepts
    type StoreAction storeData

    -- | Transform the store data according to the action.  This is the only place in your app where
    -- @IO@ should occur.  The transform function should complete quickly, since the UI will not be
    -- re-rendered until the transform is complete.  Therefore, if you need to perform some longer
    -- action, you should fork a thread from inside 'transform'.  The thread can then call 'alterStore'
    -- with another action with the result of its computation.  This is very common to communicate with
    -- the backend using AJAX.  Indeed, the 'React.Flux.Combinators.jsonAjax' utility function
    -- implements exactly this strategy since it is so common.
    --
    -- Note that if the transform throws an exception, the transform will be aborted and the old
    -- store data will be kept unchanged.  The exception will then be thrown from 'alterStore'.
    --
    -- For the best performance, care should be taken in only modifying the part of the store data
    -- that changed (see below for more information on performance).
    transform :: StoreAction storeData -> storeData -> IO storeData

-- | An existential type for some store action.  It is used as the output of the dispatcher.
-- The 'NFData' instance is important for performance, for details see below.
data SomeStoreAction = forall storeData. (StoreData storeData, NFData (StoreAction storeData))
      => SomeStoreAction (ReactStore storeData) (StoreAction storeData)
    | forall storeData. (StoreData storeData, NFData (StoreAction storeData))
     => SomeNewStoreAction (Proxy storeData) (StoreAction storeData)

instance NFData SomeStoreAction where
    rnf (SomeStoreAction _ action) = action `deepseq` ()
    rnf (SomeNewStoreAction _ action) = action `deepseq` ()

-- | Create some store action.  You must use a type-argument to specify the storeData type (because technically, the same
-- store action type could be used for different stores).  I strongly suggest you keep a one-to-one correspondence between
-- stores and store actions, but GHC does not know that.  For example,
--
-- >todoAction :: TodoAction -> SomeStoreAction
-- >todoAction a = someStoreAction @TodoStore a
someStoreAction :: forall storeData. (StoreData storeData, NFData (StoreAction storeData)) => StoreAction storeData -> SomeStoreAction
someStoreAction = SomeNewStoreAction (Proxy :: Proxy storeData)

-- | Call 'alterStore' on the store and action.
executeAction :: SomeStoreAction -> IO ()
executeAction (SomeStoreAction store action) = alterStore store action
executeAction (SomeNewStoreAction p a) = transformStore p a

--------------------------------------------------------------------------------
-- Old Version Store Definition
--------------------------------------------------------------------------------

-- | This type is used to represent the foreign javascript object part of the store.
newtype ReactStoreRef storeData = ReactStoreRef JSVal
instance IsJSVal (ReactStoreRef storeData)

data NewReactStoreHS = NewReactStoreHS {
    newStoreLock :: MVar ()
  } deriving Typeable

data ReactStore storeData = ReactStore {
    -- | A reference to the foreign javascript part of the store.
    storeRef :: ReactStoreRef storeData

    -- | An MVar containing the current store data.  Normally, the MVar is full and contains the
    -- current store data.  When applying an action, the MVar is kept empty for the entire operation
    -- of transforming to the new data and sending the new data to all component views.  This
    -- effectively operates as a lock allowing only one thread to modify the store at any one time.
    -- This lock is safe because only the 'alterStore' function ever writes this MVar.
  , storeData :: MVar storeData
}

----------------------------------------------------------------------------------------------------
-- Store operations
----------------------------------------------------------------------------------------------------

-- | Create a store from the initial data.
mkStore :: StoreData storeData => storeData -> ReactStore storeData
{-# NOINLINE mkStore #-}

-- | Register the initial store data.  This function must be called exactly once from your main function before
-- the initial rendering occurs.  Store data is global and so there can be only one store data value for each
-- store type.
registerInitialStore :: forall storeData. (Typeable storeData, StoreData storeData) => storeData -> IO ()

-- | First, 'transform' the store data according to the given action.  Next, if compiled with GHCJS,
-- notify all registered controller-views to re-render themselves.  (If compiled with GHC, the store
-- data is just transformed since there are no controller-views.)
--
-- Only a single thread can be transforming the store at any one time, so this function will block
-- on an 'MVar' waiting for a previous transform to complete if one is in process.
alterStore :: StoreData storeData => ReactStore storeData -> StoreAction storeData -> IO ()

-- | First, 'transform' the store data according to the given action and then notify all registered
-- controller-views to re-render themselves.
--
-- Only a single thread can be transforming the store at any one time, so this function will block
-- on an 'MVar' waiting for a previous transform to complete if one is in process.
--
-- This function will 'error' if 'registerInitialStore' has not been called.
transformStore :: forall storeData. StoreData storeData => Proxy storeData -> StoreAction storeData -> IO ()

-- | Obtain the store data from a store.  Note that the store data is stored in an MVar, so
-- 'readStoreData' can block since it uses 'readMVar'.  The 'MVar' is empty exactly when the store is
-- being transformed, so there is a possiblity of deadlock if two stores try and access each other's
-- data during transformation.
--
-- This function will 'error' if 'registerInitialStore' has not been called.
readStoreData :: forall storeData. (Typeable storeData, StoreData storeData) => IO storeData

-- | Obtain the store data from a store.  Note that the store data is stored in an MVar, so
-- 'getStoreData' can block since it uses 'readMVar'.  The 'MVar' is empty exactly when the store is
-- being transformed, so there is a possiblity of deadlock if two stores try and access each other's
-- data during transformation.
getStoreData :: ReactStore storeData -> IO storeData
getStoreData (ReactStore _ mvar) = readMVar mvar

typeJsKey :: TypeRep -> JSString

#ifdef __GHCJS__

-- | The new type of stores, introduced in version 1.3, keep the data in a javascript dictionary indexed
-- by the fingerprint of the type.  This allows any code to lookup the store by knowing the type.
newtype NewReactStore storeData = NewReactStore JSVal
instance IsJSVal (NewReactStore storeData)

-- | New stores are kept in a javascript dictionary by type.  This computes the key into this dictionary.
storeJsKey :: Typeable ty => Proxy ty -> JSString
storeJsKey p = typeJsKey (typeRep p)

typeJsKey t = decimal f1 <> "-" <> decimal f2
  where
    Fingerprint f1 f2 = typeRepFingerprint t

-- old store API
mkStore initial = unsafePerformIO $ do
    i <- export initial
    ref <- js_createOldStore i
    storeMVar <- newMVar initial
    return $ ReactStore ref storeMVar

-- new store API
registerInitialStore initial = do
    dataRef <- initial `seq` export initial
    store <- NewReactStoreHS <$> newMVar ()
    storeE <- export store
    js_createNewStore (storeJsKey (Proxy :: Proxy storeData)) dataRef storeE

-- old transform
alterStore store action = modifyMVar_ (storeData store) $ \oldData -> do
    newData <- transform action oldData

    -- There is a hack in PropertiesAndEvents that the fake event store for propagation and prevent
    -- default does not have a javascript store, so the store is nullRef.
    case storeRef store of
        ReactStoreRef ref | not $ isNull ref -> do
            newDataE <- export newData
            js_UpdateStore (storeRef store) newDataE
        _ -> return ()

    return newData

-- new transform
transformStore _ action = do
    store :: NewReactStore storeData <- js_getNewStore (storeJsKey (Proxy :: Proxy storeData))
    storeHS <- getNewStoreHS store
    modifyMVar_ (newStoreLock storeHS) $ \() -> do
      mold <- js_getNewStoreData store >>= derefExport
      oldData <- maybe (error "Unable to decode store data") return mold
      newData <- transform action oldData
      newDataE <- newData `seq` export newData
      js_UpdateNewStore store newDataE

readStoreData = do
    store :: NewReactStore storeData <- js_getNewStore (storeJsKey (Proxy :: Proxy storeData))
    mdata <- js_getNewStoreData store >>= derefExport
    maybe (error "Unable to decode store data") return mdata

-- | Perform the update, swapping the old export and the new export and then notifying the component views.
foreign import javascript unsafe
    "hsreact$transform_store($1, $2)"
    js_UpdateStore :: ReactStoreRef storeData -> Export storeData -> IO ()

foreign import javascript unsafe
    "hsreact$storedata[$1]"
    js_getNewStore :: JSString -> IO (NewReactStore storeData)

foreign import javascript unsafe
    "$1.hs"
    js_getNewStoreHS :: NewReactStore storeData -> IO (Export NewReactStoreHS)

getNewStoreHS :: NewReactStore storeData -> IO NewReactStoreHS
getNewStoreHS s = js_getNewStoreHS s >>= parseExport
{-# NOINLINE getNewStoreHS #-}

foreign import javascript unsafe
    "$1.sdata"
    js_getNewStoreData :: NewReactStore storeData -> IO (Export storeData)

-- | Create the javascript half of the store.
foreign import javascript unsafe
    "{sdata:$1, views: []}"
    js_createOldStore :: Export storeData -> IO (ReactStoreRef storeData)

foreign import javascript unsafe
    "hsreact$storedata[$1] = {sdata: $2, views: {}, hs: $3}"
    js_createNewStore :: JSString -> Export storeData -> Export NewReactStoreHS -> IO ()

-- | Perform the update, swapping the old export and the new export and then notifying the component views.
foreign import javascript unsafe
    "hsreact$transform_new_store($1, $2)"
    js_UpdateNewStore :: NewReactStore storeData -> Export storeData -> IO ()

#else

mkStore initial = unsafePerformIO $ do
    storeMVar <- newMVar initial
    return $ ReactStore (ReactStoreRef ()) storeMVar

registerInitialStore _ = return ()

alterStore store action = modifyMVar_ (storeData store) (transform action)

readStoreData = error "Can't call readStoreRef from GHC, only GHCJS"

transformStore _ _ = return ()

typeJsKey _ = ()

getNewStoreJs _ = return $ ReactStoreRef ()

#endif
