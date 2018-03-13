module Perspectives.Effects where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Now (NOW)
import Network.HTTP.Affjax (AJAX)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP)

type AjaxAvar e = (avar :: AVAR, ajax :: AJAX | e)

type AjaxAvarCache e = (avar :: AVAR, ajax :: AJAX, gm :: GLOBALMAP | e)

type AvarCache e = (avar :: AVAR, gm :: GLOBALMAP | e)

type TransactieEffects e = (avar :: AVAR, ajax :: AJAX, gm :: GLOBALMAP, now :: NOW | e)
