module Perspectives.Proxy
(
  handleClientRequest,
  createRequestEmitter,
  retrieveRequestEmitter
)
where

import Control.Coroutine.Aff (Emitter, Step(..))
import Control.Promise (Promise)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn3, runEffectFn1, runEffectFn3)
import Foreign (Foreign)
import Prelude (Unit)


-----------------------------------------------------------
-- PASSING ON handleClientRequest
-----------------------------------------------------------
foreign import handleClientRequest :: Foreign

-----------------------------------------------------------
-- REQUEST, RESPONSE AND CHANNEL
-----------------------------------------------------------
foreign import createRequestEmitterImpl :: EffectFn3
  (Foreign -> Step Foreign Unit)
  (Unit -> Step Foreign Unit)
  (Emitter Effect Foreign Unit)
  Unit

-- createRequestEmitter :: Emitter Foreign Unit
createRequestEmitter :: Emitter Effect Foreign Unit -> Effect Unit
createRequestEmitter = runEffectFn3 createRequestEmitterImpl Emit Finish

foreign import retrieveRequestEmitterImpl :: EffectFn1
  (Emitter Effect Foreign Unit)
  Unit

-- createRequestEmitter :: Emitter Foreign Unit
retrieveRequestEmitter :: Emitter Effect Foreign Unit -> Effect Unit
retrieveRequestEmitter = runEffectFn1 retrieveRequestEmitterImpl
