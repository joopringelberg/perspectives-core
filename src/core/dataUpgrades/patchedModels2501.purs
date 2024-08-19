module Perspectives.DataUpgrade.PatchModels.PDR2501 where

import Data.Tuple (Tuple(..))
import Foreign.Object (Object, fromFoldable)

foreign import bodiesWithAccounts :: String 
foreign import brokerServices :: String 
foreign import couchdbManagement :: String 
foreign import disconnect :: String 
foreign import hypertext :: String 
foreign import introduction :: String 
foreign import perspectivesSysteem :: String 
foreign import simpleChat :: String 

replacements :: Object String
replacements = fromFoldable
  [ Tuple "model://perspectives.domains#BodiesWithAccounts" bodiesWithAccounts
  , Tuple "model://perspectives.domains#BrokerServices" brokerServices
  , Tuple "model://perspectives.domains#CouchdbManagement" couchdbManagement
  , Tuple "model://joopringelberg.nl#Disconnect" disconnect
  , Tuple "model://perspectives.domains#HyperContext" hypertext
  , Tuple "model://perspectives.domains#Introduction" introduction
  , Tuple "model://perspectives.domains#System" perspectivesSysteem
  , Tuple "model://perspectives.domains#SimpleChat" simpleChat
  
  ]