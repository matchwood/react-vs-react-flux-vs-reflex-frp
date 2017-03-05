-- | At some point this should be replaced by GHCJS.Foreign.Export, but GHCJS.Foreign.Export
-- currently causes a bug in the Todo example application to appear.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module React.Flux.Export where

#ifdef __GHCJS__

import Data.Typeable (Typeable)
import Unsafe.Coerce

import GHCJS.Types

newtype Export a = Export JSVal

foreign import javascript unsafe
    "hsreact$export($1)"
    js_export :: Double -> IO (Export a)

export :: Typeable a => a -> IO (Export a)
export = js_export . unsafeCoerce

foreign import javascript unsafe
    "hsreact$derefExport($1)"
    js_deref :: Export a -> IO Double

derefExport :: Typeable a => Export a -> IO (Maybe a)
derefExport e = (Just . unsafeCoerce) <$> js_deref e

parseExport :: Typeable a => Export a -> IO a
parseExport a = (unsafeCoerce <$> js_deref a)

foreign import javascript unsafe
    "$r = $1"
    js_unsafeExport :: Double -> IO (Export a)

-- | A variation on export which does not retain the value for garbage collection
unsafeExport :: Typeable a => a -> IO (Export a)
unsafeExport = js_unsafeExport . unsafeCoerce

foreign import javascript unsafe
    "$r = $1"
    js_unsafeDeref :: Export a -> IO Double

unsafeDerefExport :: Typeable a => Export a -> IO a
unsafeDerefExport e = unsafeCoerce <$> js_unsafeDeref e

#endif
