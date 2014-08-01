{-# LANGUAGE FlexibleContexts #-}
import ReadFile
import WriteFile
import OpenFile2
import Control.Eff
import Control.Eff.Lift

-- This function can be used as-is.
cp :: FilePath -> FilePath -> IO ()
cp src dest = runLift $ runWriteFile dest $ runReadFile src $ do
     contents <- readAll
     writeAll contents

-- This function can't be used w/o handling the WriteableFile/WriteFile effects.
cp_r :: (SetMember Lift (Lift IO) r, Member WriteFile r) => 
     FilePath 
     -> FilePath
     -> Eff r ()
cp_r src dest = runReadFile src $ do
     h <- readChar
     writeChar h

-- This function can't be used w/o handling the ReadableFile/ReadFile effects.
cp_w :: (SetMember Lift (Lift IO) r, Member ReadFile r) => 
     FilePath 
     -> FilePath
     -> Eff r ()
cp_w src dest = runWriteFile  dest $ do
     h <- readChar
     writeChar h


