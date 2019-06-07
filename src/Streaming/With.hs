{- |
   Module      : Streaming.With
   Description : with/bracket-style idioms for use with streaming
   Copyright   : Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Streaming.With
  ( -- * File-handling
    withFile
  , withBinaryFile
    -- ** Common file-handling cases
  , writeBinaryFile
  , appendBinaryFile
  , withBinaryFileContents
    -- ** Temporary files
  , withSystemTempFile
  , withTempFile
  , withSystemTempDirectory
  , withTempDirectory
    -- * Re-exports
    -- $reexports
  , MonadUnliftIO
  , bracket
  ) where

import           Data.ByteString.Streaming (ByteString)
import qualified Data.ByteString.Streaming as B

import           Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import           Control.Monad.IO.Class  (liftIO)
import           System.IO               (Handle, IOMode(..), hClose,
                                          openBinaryFile, openFile)
import qualified System.IO.Temp          as T
import           UnliftIO.Exception      (bracket)

--------------------------------------------------------------------------------

-- | A lifted variant of 'System.IO.withFile'.
--
--   You almost definitely don't want to use this; instead, use
--   'withBinaryFile' in conjunction with "Data.ByteString.Streaming".
withFile :: (MonadUnliftIO m) => FilePath -> IOMode -> (Handle -> m r) -> m r
withFile fp md = bracket (liftIO (openFile fp md)) (liftIO . hClose)

-- | A lifted variant of 'System.IO.withBinaryFile'.
withBinaryFile :: (MonadUnliftIO m) => FilePath -> IOMode -> (Handle -> m r) -> m r
withBinaryFile fp md = bracket (liftIO (openBinaryFile fp md)) (liftIO . hClose)

-- | Write to the specified file.
writeBinaryFile :: (MonadUnliftIO m) => FilePath -> ByteString m r -> m r
writeBinaryFile fp = withBinaryFile fp WriteMode . flip B.hPut

-- | Append to the specified file.
appendBinaryFile :: (MonadUnliftIO m) => FilePath -> ByteString m r -> m r
appendBinaryFile fp = withBinaryFile fp AppendMode . flip B.hPut

-- | Apply a function to the contents of the file.
--
--   Note that a different monadic stack is allowed for the
--   'ByteString' input, as long as it later gets resolved to the
--   required output type (e.g. remove transformer).
withBinaryFileContents :: (MonadUnliftIO m, MonadUnliftIO n) => FilePath
                          -> (ByteString n () -> m r) -> m r
withBinaryFileContents fp f = withBinaryFile fp ReadMode (f . B.hGetContents)

--------------------------------------------------------------------------------

-- | /This is 'T.withSystemTempFile' from the @temporary@ package/
--   /with the continuation re-structured to only take one argument./
--
--   Create and use a temporary file in the system standard temporary
--   directory.
--
--   Behaves exactly the same as 'withTempFile', except that the
--   parent temporary directory will be that returned by
--   'T.getCanonicalTemporaryDirectory'.
--
--   @since 0.1.1.0
withSystemTempFile :: (MonadUnliftIO m)
                   => String -- ^ File name template.  See 'T.openTempFile'
                   -> ((FilePath, Handle) -> m r)
                   -> m r
withSystemTempFile template action =
  withRunInIO $ \runInIO -> 
    T.withSystemTempFile template $ \path handle ->
      runInIO $ action (path, handle)

-- | /This is 'T.withTempFile' from the @temporary@ package with the/
--   /continuation re-structured to only take one argument./
--
--   Use a temporary filename that doesn't already exist.
--
--   Creates a new temporary file inside the given directory, making
--   use of the template. The temp file is deleted after use. For
--   example:
--
--   > withTempFile "src" "sdist." $ \(tmpFile, hFile) -> ...
--
--   The @tmpFile@ will be file in the given directory, e.g.
--   @src/sdist.342@.
--
--   @since 0.1.1.0
withTempFile :: (MonadUnliftIO m)
             => FilePath -- ^ Temp dir to create the file in
             -> String   -- ^ File name template.  See
                         --   'T.openTempFile'.
             -> ((FilePath, Handle) -> m r)
             -> m r
withTempFile dir template action = 
  withRunInIO $ \runInIO -> 
    T.withTempFile dir template $ \path handle ->
      runInIO $ action (path, handle)

-- | /This is 'T.withSystemTempDirectory' from the @temporary@ package./
withSystemTempDirectory :: (MonadUnliftIO m)
                        => String -- ^ File name template.  See 'T.openTempFile'
                        -> (FilePath -> m r)
                        -> m r
withSystemTempDirectory template action =
  withRunInIO $ \runInIO ->
    T.withSystemTempDirectory template $ \path ->
      runInIO $ action path

-- | /This is 'T.withTempDirectory' from the @temporary@ package./
withTempDirectory :: (MonadUnliftIO m)
                  => FilePath -- ^ Temp dir to create the file in
                  -> String   -- ^ File name template.  See
                              --   'T.openTempFile'.
                  -> (FilePath -> m r)
                  -> m r
withTempDirectory dir template action = 
  withRunInIO $ \runInIO -> 
    T.withTempDirectory dir template $ \path ->
      runInIO $ action path


--------------------------------------------------------------------------------

{- $reexports

These may assist in writing your own bracket-style functions.

Note that not everything is re-exported: for example, 'Handle' isn't
re-exported for use with 'withFile' as it's unlikely that you will
write another wrapper around it, and furthermore it wouldn't be a
common enough extension to warrant it.

-}
