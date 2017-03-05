-- | Internal module containing the view definitions
{-# LANGUAGE UndecidableInstances, AllowAmbiguousTypes, TypeApplications, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module React.Flux.Views
  ( View(..)
  , ViewPropsToElement
  , ViewEventHandler
  , StatefulViewEventHandler
  , liftViewToStateHandler
  , mkView
  , view_
  , mkStatefulView
  , StoreArg
  , StoreField
  , HasField(..)
  , ControllerViewToElement
  , mkControllerView
  , exportReactViewToJavaScript
  , callbackRenderingView
  , ViewProps(..)
  , ControllerViewStores(..)
  , ExportViewProps(..)
  , StoreToState(..)
  , JsState(..)
  ) where

import Data.Typeable
import Control.DeepSeq
import qualified Data.HashMap.Strict as M

import React.Flux.Store
import React.Flux.Internal

#ifdef __GHCJS__
import Control.Monad
import System.IO.Unsafe (unsafePerformIO)
import React.Flux.Export
import JavaScript.Array
import GHCJS.Foreign.Callback
import GHCJS.Types (JSVal, IsJSVal, nullRef)
import GHCJS.Marshal (ToJSVal(..), FromJSVal(..))
import qualified JavaScript.Array as JSA

#else
type JSVal = ()
type JSArray = ()
type Callback x = ()
class FromJSVal a
#endif

---------------------------------------------------------------------------------
-- Public API
---------------------------------------------------------------------------------

newtype View (props :: [*]) = View (ReactViewRef ())

type family ViewPropsToElement (props :: [*]) (handler :: *) where
  ViewPropsToElement '[] handler = ReactElementM handler ()
  ViewPropsToElement (a ': rest) handler = a -> ViewPropsToElement rest handler

-- | Event handlers in a controller-view and a view transform events into actions, but are not
-- allowed to perform any 'IO'.
type ViewEventHandler = [SomeStoreAction]

-- | A stateful-view event handler produces a list of store actions and potentially a new state.  If
-- the new state is nothing, no change is made to the state (which allows an optimization in that we
-- do not need to re-render the view).
--
-- Changing the state causes a re-render which will cause a new event handler to be created.  If the
-- handler closes over the state passed into the rendering function, there is a race if multiple
-- events occur before React causes a re-render.  Therefore, the handler takes the current state as
-- input.  Your handlers therefore should ignore the state passed into the render function and
-- instead use the state passed directly to the handler.
type StatefulViewEventHandler state = state -> ([SomeStoreAction], Maybe state)

-- | Change the event handler from 'ViewEventHandler' to 'StatefulViewEventHandler' to allow you to embed
-- combinators with 'ViewEventHandler's into a stateful view.  Each such lifted handler makes no change to
-- the state.
liftViewToStateHandler :: ReactElementM ViewEventHandler a -> ReactElementM (StatefulViewEventHandler st) a
liftViewToStateHandler = transHandler (\h _ -> (h, Nothing))

mkView :: forall (props :: [*]). ViewProps props => JSString -> ViewPropsToElement props ViewEventHandler -> View props

view_ :: forall props handler. ViewProps (props :: [*]) => View props -> JSString -> ViewPropsToElement props handler

mkStatefulView :: forall (state :: *) (props :: [*]). (Typeable state, NFData state, ViewProps props)
               => JSString -- ^ A name for this view, used only for debugging/console logging
               -> state -- ^ The initial state
               -> (state -> ViewPropsToElement props (StatefulViewEventHandler state))
               -> View props

data StoreArg store
data StoreField store (field :: k) a

class HasField (x :: k) r a | x r -> a where
  getField :: r -> a

type family ControllerViewToElement (stores :: [*]) (props :: [*]) (handler :: *) where
  ControllerViewToElement '[] props handler = ViewPropsToElement props handler
  ControllerViewToElement (StoreArg store ': rest) props handler = store -> ControllerViewToElement rest props handler
  ControllerViewToElement (StoreField store field a ': rest) props handler = a -> ControllerViewToElement rest props handler

mkControllerView :: forall (stores :: [*]) (props :: [*]). (ViewProps props, ControllerViewStores stores)
                 => JSString -> ControllerViewToElement stores props ViewEventHandler -> View props

exportReactViewToJavaScript :: forall (props :: [*]). (ExportViewProps props) => View props -> IO JSVal

callbackRenderingView :: forall (props :: [*]) handler. (ExportViewProps props) => JSString -> View props -> PropertyOrHandler handler

--------------------------------------------------------------------------------
-- View Props Classes
--------------------------------------------------------------------------------

class ViewProps (props :: [*]) where
  viewPropsToJs :: Proxy props -> Proxy handler -> ReactViewRef () -> JSString -> (NewJsProps -> IO ()) -> ViewPropsToElement props handler
  applyViewPropsFromJs :: Proxy props -> ViewPropsToElement props handler -> NewJsProps -> Int -> IO (ReactElementM handler ())

class ExportViewProps (props :: [*]) where
  applyViewPropsFromArray :: Proxy props -> JSArray -> Int -> NewJsProps -> IO ()

instance ViewProps '[] where
  viewPropsToJs _ _ ref k props = elementToM () $ NewViewElement ref k props
  applyViewPropsFromJs _ x _ _ = return x
  {-# INLINE viewPropsToJs #-}
  {-# INLINE applyViewPropsFromJs #-}

instance ExportViewProps '[] where
  applyViewPropsFromArray _ _ _ _ = return ()

instance (ViewProps rest, Typeable a) => ViewProps (a ': (rest :: [*])) where
#ifdef __GHCJS__
  viewPropsToJs _ h ref k props = \a -> viewPropsToJs (Proxy :: Proxy rest) h ref k (\p -> props p >> pushProp a p)
  {-# INLINE viewPropsToJs #-}

  applyViewPropsFromJs _ f props i = do
    val <- getProp props i
    applyViewPropsFromJs (Proxy :: Proxy rest) (f val) props (i+1)
  {-# INLINE applyViewPropsFromJs #-}
#else
  viewPropsToJs _ _ _ _ = error "Views work only with GHCJS"
  applyViewPropsFromJs _ _ _ _ = error "Views work only with GHCJS"
#endif

instance forall (rest :: [*]) a. (ExportViewProps rest, Typeable a, FromJSVal a) => ExportViewProps (a ': rest) where
#ifdef __GHCJS__
  applyViewPropsFromArray _ inputArr k outputArr =
    do ma <- fromJSVal $ if k >= JSA.length inputArr then nullRef else JSA.index k inputArr
       a :: a <- maybe (error "Unable to decode callback argument") return ma
       pushProp a outputArr
       applyViewPropsFromArray (Proxy :: Proxy rest) inputArr (k+1) outputArr
#else
  applyViewPropsFromArray _ _ _ _ = return ()
#endif

--------------------------------------------------------------------------------
-- Controller View Props Classes
--------------------------------------------------------------------------------

newtype JsState = JsState JSVal -- javascript map from store typerep fingerprint to value

data StoreToState = StoreState Int
                  | StoreDerivedState
                    { storeDerivedOffset :: Int
                    , storeToStateCallback :: IO (Callback (JSVal -> IO ()))
                    }

class ControllerViewStores (stores :: [*]) where
  applyControllerViewFromJs :: forall props handler. ViewProps props
                            => Proxy stores
                            -> Proxy props
                            -> ControllerViewToElement stores props handler
                            -> JsState
                            -> NewJsProps
                            -> Int
                            -> IO (ReactElementM handler ())
  stateForView :: Proxy stores -> Int -> M.HashMap TypeRep [StoreToState]


instance ControllerViewStores '[] where
  applyControllerViewFromJs _ p f _ props _ = applyViewPropsFromJs p f props 0
  stateForView _ _ = mempty
  {-# INLINE applyControllerViewFromJs #-}
  {-# INLINE stateForView #-}

instance (ControllerViewStores rest, StoreData store, Typeable store)
   => ControllerViewStores (StoreArg store ': (rest :: [*])) where
#ifdef __GHCJS__
  applyControllerViewFromJs _ p f st props i = do
    sval <- findFromState i st
    applyControllerViewFromJs (Proxy :: Proxy rest) p (f sval) st props (i+1)

  stateForView _ i = M.insertWith (++) storeT [StoreState i] (stateForView (Proxy :: Proxy rest) (i+1))
    where
      storeT = typeRep (Proxy :: Proxy store)
  {-# INLINE applyControllerViewFromJs #-}
  {-# INLINE stateForView #-}
#else
  applyControllerViewFromJs _ _ _ _ _ _ = error "Views work only with GHCJS"
  stateForView _ _ = error "Views work only with GHCJS"
#endif

instance (ControllerViewStores rest, StoreData store, HasField field store a, Typeable store, Typeable a)
   => ControllerViewStores (StoreField store field a ': (rest :: [*])) where
#ifdef __GHCJS__
  applyControllerViewFromJs _ p f st props i = do
    sval <- findFromState i st
    applyControllerViewFromJs (Proxy :: Proxy rest) p (f sval) st props (i+1)

  stateForView _ i = M.insertWith (++) storeT [StoreDerivedState i derive] (stateForView (Proxy :: Proxy rest) (i+1))
    where
      storeT = typeRep (Proxy :: Proxy store)
      derive =
        syncCallback1 ThrowWouldBlock $ \arg -> do
          storeD :: store <- getStoreJs arg
          let a :: a = getField @field storeD
          aE <- a `seq` unsafeExport a
          js_setDeriveOutput arg aE
  {-# INLINE applyControllerViewFromJs #-}
  {-# INLINE stateForView #-}

getStoreJs :: Typeable store => JSVal -> IO store
getStoreJs arg = js_getDeriveInput arg >>= parseExport
{-# NOINLINE getStoreJs #-} -- if this is inlined, GHCJS does not properly compile the getField callback

#else
  applyControllerViewFromJs _ _ _ _ _ _ = error "Views work only with GHCJS"
  stateForView _ _ = error "Views work only with GHCJS"
#endif

---------------------------------------------------------------------------------
-- Public API Implementation
---------------------------------------------------------------------------------

#ifdef __GHCJS__

view_ (View ref) key = viewPropsToJs (Proxy :: Proxy props) (Proxy :: Proxy handler) ref key (const $ return ())

mkView name buildNode = unsafePerformIO $ do
  renderCb <- syncCallback2 ContinueAsync $ \thisRef argRef -> do
    let this = ReactThis thisRef
        arg = RenderCbArg argRef
    props <- js_PropsList this
    node <- applyViewPropsFromJs (Proxy :: Proxy props) buildNode props 0
    (element, evtCallbacks) <- mkReactElement (runViewHandler this) this node
    evtCallbacksRef <- toJSVal evtCallbacks
    js_RenderCbSetResults arg evtCallbacksRef element

  View <$> js_createNewView name renderCb
{-# NOINLINE mkView #-}

mkStatefulView name initial buildNode = unsafePerformIO $ do
    initialRef <- export initial
    renderCb <- syncCallback2 ContinueAsync $ \thisRef argRef -> do
      let this = ReactThis thisRef
          arg = RenderCbArg argRef
      props <- js_PropsList this
      state <- js_ReactGetState this >>= parseExport
      node <- applyViewPropsFromJs (Proxy :: Proxy props) (buildNode state) props 0
      (element, evtCallbacks) <- mkReactElement (runStateViewHandler this) this node
      evtCallbacksRef <- toJSVal evtCallbacks
      js_RenderCbSetResults arg evtCallbacksRef element

    View <$> js_createNewStatefulView name initialRef renderCb
{-# NOINLINE mkStatefulView #-}

mkControllerView name buildNode = unsafePerformIO $ do
  renderCb <- syncCallback2 ContinueAsync $ \thisRef argRef -> do
    let this = ReactThis thisRef
        arg = RenderCbArg argRef
    props <- js_PropsList this
    st <- js_NewStateDict this
    node <- applyControllerViewFromJs (Proxy :: Proxy stores) (Proxy :: Proxy props) buildNode st props 0
    (element, evtCallbacks) <- mkReactElement (runViewHandler this) this node
    evtCallbacksRef <- toJSVal evtCallbacks
    js_RenderCbSetResults arg evtCallbacksRef element

  artifacts <- js_emptyArtifacts
  forM_ (M.toList $ stateForView (Proxy :: Proxy stores) 0) $ \(ty, states) -> do
    art <- js_newArtifact
    forM_ states $ \s ->
      case s of
        StoreState i -> js_addStoreState art i
        StoreDerivedState i mkCall -> do
          callback <- mkCall
          js_addStoreDerivedState art i callback
    js_setArtifact artifacts (typeJsKey ty) art

  View <$> js_createNewCtrlView name renderCb artifacts
{-# NOINLINE mkControllerView #-}

exportReactViewToJavaScript (View v) = do
    (_callbackToRelease, wrappedCb) <- exportNewViewToJs v $ \arr -> do
      props <- js_newEmptyPropList
      applyViewPropsFromArray (Proxy :: Proxy props) arr 0 props
      return props
    return wrappedCb

callbackRenderingView name (View v) = CallbackPropertyReturningNewView name v getProps
  where
    getProps arr = do
      props <- js_newEmptyPropList
      applyViewPropsFromArray (Proxy :: Proxy props) arr 0 props
      return props

#else

mkView _ _ = View (ReactViewRef ())
view_ _ _ = error "Views work only in GHCJS"
mkStatefulView _ _ _ = View (ReactViewRef ())
mkControllerView _ _ = View (ReactViewRef ())
exportReactViewToJavaScript _ = return ()
callbackRenderingView _ _ = error "Views only work in GHCJS"

#endif

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

#ifdef __GHCJS__

-- | Transform a controller view handler to a raw handler.
runViewHandler :: ReactThis state props -> ViewEventHandler -> IO ()
runViewHandler _ handler = handler `deepseq` mapM_ executeAction handler

-- | Transform a stateful view event handler to a raw event handler
runStateViewHandler :: (Typeable state, NFData state)
                    => ReactThis state props -> StatefulViewEventHandler state -> IO ()
runStateViewHandler this handler = do
    st <- js_ReactGetState this >>= parseExport

    let (actions, mNewState) = handler st

    case mNewState of
        Nothing -> return ()
        Just newState -> do
            newStateRef <- newState `deepseq` export newState
            js_ReactUpdateAndReleaseState this newStateRef

    -- nothing above here should block, so the handler callback should still be running syncronous,
    -- so the deepseq of actions should still pick up the proper event object.
    actions `deepseq` mapM_ executeAction actions

foreign import javascript unsafe
    "[]"
    js_newEmptyPropList :: IO NewJsProps

foreign import javascript unsafe
  "$1['props'].hs"
  js_PropsList :: ReactThis state props -> IO NewJsProps

foreign import javascript unsafe
  "$1['state'].hs"
  js_NewStateDict :: ReactThis state props -> IO JsState

foreign import javascript unsafe
  "$1[$2]"
  js_getPropFromList :: NewJsProps -> Int -> IO (Export a)

foreign import javascript unsafe
  "$1.push($2)"
  js_pushProp :: NewJsProps -> Export a -> IO ()

foreign import javascript unsafe
    "hsreact$mk_new_view($1, $2)"
    js_createNewView :: JSString -> Callback (JSVal -> JSVal -> IO ()) -> IO (ReactViewRef props)

foreign import javascript unsafe
    "hsreact$mk_new_stateful_view($1, $2, $3)"
    js_createNewStatefulView :: JSString -> Export state -> Callback (JSVal -> JSVal -> IO ()) -> IO (ReactViewRef props)

getProp :: Typeable a => NewJsProps -> Int -> IO a
getProp p i = js_getPropFromList p i >>= parseExport

pushProp :: Typeable a => a -> NewJsProps -> IO ()
pushProp val props = do
  valE <- val `seq` export val -- this will be released in the lifecycle callbacks of the class
  js_pushProp props valE

findFromState :: Typeable a => Int -> JsState -> IO a
findFromState i s = js_findFromState i s >>= unsafeDerefExport

foreign import javascript unsafe
  "$2[$1]"
  js_findFromState :: Int -> JsState -> IO (Export a)

foreign import javascript unsafe
  "$1.input"
  js_getDeriveInput :: JSVal -> IO (Export storeData)

foreign import javascript unsafe
  "$1.output = $2"
  js_setDeriveOutput :: JSVal -> Export a -> IO ()

newtype Artifacts = Artifacts JSVal
instance IsJSVal Artifacts

newtype Artifact = Artifact JSVal
instance IsJSVal Artifact

foreign import javascript unsafe
  "{}"
  js_emptyArtifacts :: IO Artifacts

foreign import javascript unsafe
  "[]"
  js_newArtifact :: IO Artifact

foreign import javascript unsafe
  "$1.push({i: $2})"
  js_addStoreState :: Artifact -> Int -> IO ()

foreign import javascript unsafe
  "$1.push({i: $2, call: $3})"
  js_addStoreDerivedState :: Artifact -> Int -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe
  "$1[$2] = $3"
  js_setArtifact :: Artifacts -> JSString -> Artifact -> IO ()

foreign import javascript unsafe
  "hsreact$mk_new_ctrl_view($1, $2, $3)"
  js_createNewCtrlView :: JSString -> Callback (JSVal -> JSVal -> IO ()) -> Artifacts -> IO (ReactViewRef props)

foreign import javascript unsafe
    "$1['state'].hs"
    js_ReactGetState :: ReactThis state props -> IO (Export state)

foreign import javascript unsafe
    "$1._updateAndReleaseState($2)"
    js_ReactUpdateAndReleaseState :: ReactThis state props -> Export state -> IO ()

newtype RenderCbArg = RenderCbArg JSVal
instance IsJSVal RenderCbArg

foreign import javascript unsafe
    "$1.newCallbacks = $2; $1.elem = $3;"
    js_RenderCbSetResults :: RenderCbArg -> JSVal -> ReactElementRef -> IO ()

#endif
