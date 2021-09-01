-- Copyright Joop Ringelberg and Cor Baars 2019
domain Couchdb

-- externalFunctions =
--   [ Tuple "model:Couchdb$Models" {func: unsafeCoerce models, nArgs: 0}
--   , Tuple "model:Couchdb$AddModelToLocalStore" {func: unsafeCoerce addModelToLocalStore, nArgs: 1}
--   , Tuple "model:Couchdb$UploadToRepository" {func: unsafeCoerce uploadToRepository, nArgs: 2}
--   , Tuple "model:Couchdb$RoleInstances" {func: unsafeCoerce roleInstancesFromCouchdb, nArgs: 1}
--   , Tuple "model:Couchdb$PendingInvitations" {func: unsafeCoerce pendingInvitations, nArgs: 0}
--   , Tuple "model:Couchdb$RemoveModelFromLocalStore" {func: unsafeCoerce removeModelFromLocalStore, nArgs: 1}
--   , Tuple "model:Couchdb$ContextInstances" {func: unsafeCoerce contextInstancesFromCouchdb, nArgs: 1}
--   , Tuple "model:Couchdb$UpdateModel" {func: unsafeCoerce updateModel, nArgs: 2}
--   , Tuple "model:Couchdb$CreateDatabase" {func: unsafeCoerce createDatabase, nArgs: 2}
--   , Tuple "model:Couchdb$DeleteDatabase" {func: unsafeCoerce deleteDatabase, nArgs: 2}
--   , Tuple "model:Couchdb$ReplicateContinuously" {func: unsafeCoerce replicateContinuously, nArgs: 4}
--   , Tuple "model:Couchdb$EndReplication" {func: unsafeCoerce replicateContinuously, nArgs: 3}
--   , Tuple "model:Couchdb$CreateUser" {func: unsafeCoerce createUser, nArgs: 3}
--   , Tuple "model:Couchdb$DeleteUser" {func: unsafeCoerce deleteUser, nArgs: 2}
--   , Tuple "model:Couchdb$MakeAdminOfDb" {func: unsafeCoerce makeAdminOfDb, nArgs: 3}
--   , Tuple "model:Couchdb$RemoveAsAdminFromDb" {func: unsafeCoerce removeAsAdminFromDb, nArgs: 3}
--   , Tuple "model:Couchdb$MakeMemberOf" {func: unsafeCoerce makeMemberOf, nArgs: 3}
--   , Tuple "model:Couchdb$RemoveAsMemberOf" {func: unsafeCoerce removeAsMemberOf, nArgs: 3}
--   , Tuple "model:Couchdb$ResetPassword" {func: unsafeCoerce resetPassword, nArgs: 3}
-- ]
