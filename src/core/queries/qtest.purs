module Perspectives.Qtest where


import Perspectives.Query.QueryTypes
import Perspectives.Representation.ADT
import Perspectives.Representation.QueryFunction
import Perspectives.Representation.ThreeValuedLogic
import Perspectives.Representation.TypeIdentifiers 

test1 :: QueryFunctionDescription
test1 = SQD
  (CDOM (ST (ContextType "model:System$PerspectivesSystem")))
  (RolGetter (ENR (EnumeratedRoleType "model:System$perspectivesSystem$User")))
  (RDOM (ST (EnumeratedRoleType "model:System$perspectivesSystem$User")))
  True
  False

-- show result:
-- (SQD (CDOM (ST ContextType model:System$PerspectivesSystem)) (RolGetter ENR (EnumeratedRoleType "model:System$perspectivesSystem$User")) (RDOM (ST (EnumeratedRoleType "model:System$perspectivesSystem$User"))) True False)
