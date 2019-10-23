module Test.Identifiers where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Perspectives.Identifiers (isDomeinURI, isStandardNamespaceCURIE, getPrefix, getStandardNamespace, getNamespace)

test :: Eff (console :: CONSOLE) Unit
test = do
  log "========================================================="
  log ("isDomeinURI 'model:EenDomein'= " <> show (isDomeinURI "model:EenDomein#"))
  log ("isDomeinURI 'model:EenDomein'= " <> show (isDomeinURI "http://www.lcn.nl"))
  log "========================================================="

  log ("isStandardNamespaceCURIE 'rdfs:label'= " <> show (isStandardNamespaceCURIE "rdfs:label"))
  log ("isStandardNamespaceCURIE 'model:EenDomein#'= " <> show (isStandardNamespaceCURIE "model:EenDomein#"))
  log "========================================================="

  log ("getPrefix 'rdfs:label'= " <> show (getPrefix "rdfs:label"))
  log ("getPrefix 'notAnURI'= " <> show (getPrefix "notAnURI"))
  log "========================================================="

  log ("getStandardNamespace 'rdfs:label'= " <> show (getStandardNamespace "rdfs:label"))
  log ("getStandardNamespace 'model:EenDomein#'= " <> show (getStandardNamespace "model:EenDomein#"))
  log ("getStandardNamespace 'notAnURI'= " <> show (getStandardNamespace "notAnURI"))
  log "========================================================="

  log ("getNamespace 'rdfs:label'= " <> show (getNamespace "rdfs:label"))
  log ("getNamespace 'rdfs:label'= " <> show (getNamespace "model:EenDomein#"))
  log ("getNamespace 'notAnURI'= " <> show (getNamespace "notAnURI"))
  log "========================================================="
