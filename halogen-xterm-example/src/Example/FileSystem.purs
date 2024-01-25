module Example.FileSystem where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Level.DB (LevelDB)
import Level.DB as DB

newtype FilePath = FilePath String
derive newtype instance EncodeJson FilePath
derive newtype instance DecodeJson FilePath

type FileSystem = LevelDB FilePath String

openFileSystem :: Effect FileSystem 
openFileSystem = DB.open "filesystem"

listFiles :: FileSystem -> Aff (Array FilePath)
listFiles = DB.allKeys 

readFile :: FileSystem -> FilePath -> Aff (Maybe String)
readFile fs name = DB.get fs name

writeFile :: FileSystem -> FilePath -> String -> Aff Unit
writeFile fs name content = DB.put fs name content

deleteFile :: FileSystem -> FilePath -> Aff Unit
deleteFile fs name = DB.del fs name

