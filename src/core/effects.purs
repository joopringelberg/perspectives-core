module Perspectives.Effects where

import Control.Aff.Sockets (SOCKETIO)
import Effect.Aff.AVar (AVAR)
import Effect (kind Effect)
import Effect.Now (NOW)
import Affjax (AJAX)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP)

-- | The REACT Effect labels the effects in a React Gui interface.
foreign import data REACT :: Effect

type AjaxAvar e = (avar :: AVAR, ajax :: AJAX | e)

type AjaxAvarCache e = (avar :: AVAR, ajax :: AJAX, gm :: GLOBALMAP | e)

type AjaxCache e = (ajax :: AJAX, gm :: GLOBALMAP | e)

type AvarCache e = (avar :: AVAR, gm :: GLOBALMAP | e)

type TransactieEffects e = AjaxAvarCache( now :: NOW | e)

type ApiEffects e = AjaxAvarCache (react :: REACT | e)

type TcpApiEffects e = AjaxAvarCache (react :: REACT, socketio :: SOCKETIO | e)
