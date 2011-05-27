-- |
-- Module      :  System.FilePath
-- Copyright   :  (c) Thomas Schilling 2011
-- License     :  BSD-style
-- 
-- Maintainer  :  nominolo@googlemail.com
-- Stability   :  stable
-- Portability :  portable
--
-- Abstract data type for canonical file paths.
--
-- Due to the context sensitive way in which file paths are specified
-- on today's computer systems it is useful to have the notion of a
-- /canonical/ 'FilePath'.
--
-- The basic feature is that two canonical file paths @cfp1@ and
-- @cfp2@ refer to the same file if and only if @cfp1 == cfp2@.
-- This property can be achieved using 'canonicalizePath'.
-- However, if given an arbitrary @FilePath@ we don't know whether
-- it is canonical so having a separate type is useful.  Secondly,
-- @canonicalizePath@ might fail if the target path does not exist
-- on the system.
-- 
-- This module therefore provides an abstract type that represents
-- paths that have been canonicalised.  The intended use
-- straightforward, just use 'canonical' to create a
-- 'CanonicalFilePath'.
--
-- @example = do 
--  cfp1 \<- 'canonical' \"./foo\"
--  curr \<- getCurrentDirectory
--  cfp2 \<- canonical (curr \</> \"foo\")
--  print (cfp1 == cfp2)  \-- should print \"True\"
-- @
--
-- To extract a canonical @FilePath@ use 'canonicalFilePath' (don't
-- use 'show').
module System.FilePath.Canonical
  ( -- * Abstract Type
    CanonicalFilePath,
    -- * Creation
    canonical,
    -- * Deconstruction 
    originalFilePath, canonicalFilePath,
    -- * Unsafe\/Internal Operations
    unsafeCanonicalise
  )
where

import Control.DeepSeq
import Control.Monad ( liftM2 )
import System.FilePath ( equalFilePath, (</>) )
import System.Directory ( canonicalizePath, getCurrentDirectory,
                          doesFileExist, doesDirectoryExist )
{-
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Codec.Binary.UTF8.String as UTF8
-}

-- | A canonical 'FilePath'.
--
-- Construct values of this type using the 'canonical' function.
--
-- Note that it is not always possible to guarantee a truly canonical
-- path due to various file system features.  For more information see
-- the documentation of @realpath@ in your favourite man page.
--
data CanonicalFilePath =
  UnsafeCanonicalise {
    -- | The original input to 'canonical'.
    originalFilePath :: FilePath,
    -- | The canonical path.
    canonicalFilePath :: FilePath }

instance Show CanonicalFilePath where
  show = originalFilePath

instance Eq CanonicalFilePath where
  cfp1 == cfp2 =
    canonicalFilePath cfp1 `equalFilePath` canonicalFilePath cfp2

instance Ord CanonicalFilePath where
  cfp1 `compare` cfp2 =
    canonicalFilePath cfp1 `compare` canonicalFilePath cfp2

instance NFData CanonicalFilePath where
  rnf (UnsafeCanonicalise a b) = rnf a `seq` rnf b

{-
instance Binary CanonicalFilePath where
  get = liftM2 UnsafeCanonicalise getUTF8String getUTF8String
   where
     getUTF8String = do
       n <- getWord32le
       fmap UTF8.decode $ sequence (replicate (fromIntegral n) getWord8)

  put (UnsafeCanonicalise fp1 fp2) = putUTF8String fp1 >> putUTF8String fp2
   where
     putUTF8String s_ = do
       let s = UTF8.encode s_
       putWord32le (fromIntegral (length s))
       mapM_ putWord8 s
-}

-- | Construct a canonical file path from the given path.
--
-- Unlike 'canonicalizePath' this operation works even if the file or
-- its containing directory does not exist, but it may fail due to
-- other reasons (e.g., permission errors).
--
canonical :: FilePath -> IO CanonicalFilePath
canonical fp = do
  exists <- liftM2 (||) (doesFileExist fp) (doesDirectoryExist fp)
  if exists
    then fmap (UnsafeCanonicalise fp) $ canonicalizePath fp
    else fmap (UnsafeCanonicalise fp . (</> fp)) getCurrentDirectory

-- | Unsafely constructs a 'CanonicalFilePath'.
--
-- It is unsafe in the sense that it is up to the caller to guarantee
-- that the second argument is indeed a canonical file path.
--
-- This function is intended mainly to aid in writing custom
-- serialisation instances.
unsafeCanonicalise :: FilePath -- ^ Original file path
                   -> FilePath -- ^ Canonical file path.
                   -> CanonicalFilePath
unsafeCanonicalise = UnsafeCanonicalise

{- from the docs
test = do
  cfp1 <- canonical "./foo"
  curr <- getCurrentDirectory
  cfp2 <- canonical (curr </> "foo")
  print (cfp1 == cfp2)  -- should print "True"
-}
