-- Copyright Joop Ringelberg and Cor Baars 2021
domain CouchdbManagement
  use sys for model:System
  use cm for model:CouchdbManagement
  use mod for model:Models
  use acc for model:BodiesWithAccounts
  use cdb for model:Couchdb
  use utl for model:Utilities

  -- The model description case.
  -- REMOVE ONCE WE CREATE INSTANCES WITH AN ACTION
  case Model
    aspect sys:Model
    external
      aspect sys:Model$External

  -- The INDEXED context cm:MyCouchdbApp, that is the starting point containing all CouchdbServers.
  case CouchdbManagementApp
    indexed cm:MyCouchdbApp
    aspect sys:RootContext
    external
      aspect sys:RootContext$External

    -- Every user manages his own CouchdbServers.
    -- This manager is not necessarily the admin of a Couchdb installation!
    user Manager = sys:Me
      perspective on CouchdbServers
        defaults

    context CouchdbServers filledBy CouchdbServer

  -- PUBLIC
  -- This contexts implements the BodyWithAccounts pattern.
  -- NOTE: a PerspectivesSystem$User can only fill either Admin, or Accounts!
  -- The PDR looks for credentials in either role and should find just one pair.
  case CouchdbServer
    aspect acc:Body
    --storage public
    external
      property Url (mandatory, String)
      property Name (mandatory, String)

    -- This role should be in private space.
    -- Admin in Couchdb of a particular server.
    user Admin filledBy sys:PerspectivesSystem$User
      -- As Admin, has full perspective on Accounts.
      aspect acc:Body$Admin
      state Root = true
        state Remove = ToBeRemoved
          on entry
            do
              callEffect cdb:RemoveAsAdminFromDb( context >> extern >> Url, context >> extern >> Name + "_write", UserName )
              callEffect cdb:RemoveAsAdminFromDb( context >> extern >> Url, context >> extern >> Name + "_read", UserName )
              remove currentobject
        state IsFilled = exists binding
          on entry
            do
              -- Only the CouchdbServer$Admin has a Create and Fill perspective on
              -- Repository$Admin. So when this state arises, we can be sure that
              -- the current user is, indeed, a CouchdbServer$Admin.
              -- Hence the PDR will authenticate with Server Admin credentials.
              callEffect cdb:MakeAdminOfDb( context >> extern >> Url, context >> extern >> Name + "_write", UserName )
              callEffect cdb:MakeAdminOfDb( context >> extern >> Url, context >> extern >> Name + "_read", UserName )
      property ToBeRemoved (Boolean)

      perspective on Repositories
        defaults

      -- The Admin should be able to create and fill the Repository$Admin.
      perspective on Repositories >> binding >> context >> Admin
        defaults

      -- The credentials for being a database admin have to be entered;
      -- there is no way to create a database admin through InPlace.
      perspective on Admin
        defaults

      -- Admin should not be able to modify this role, because
      -- it would cause changes to be distributed to all Accounts, violating
      -- privacy. Instead, Accounts create Storages.
      -- However, we have the Storage$Admin role modify the remote Couchdb server.
      perspective on Storages
        -- TODO: Provide a perspective on relevant properties

    -- Note that the aspect acc:Body introduces a Guest role
    -- with a perspective that allows it to create an Account.

    -- This role should be in private space.
    user Accounts (unlinked) filledBy sys:PerspectivesSystem$User
      aspect acc:Body$Accounts

      state Root = true
        state IsFilled = exists binding
          on entry
            do for Admin
              letA
                  pw <- callExternal utl:GenSym() returns String
              in
                callEffect cdb:CreateUser( context >> extern >> Url, binding, pw )
                Password = pw for currentobject

        state Remove = ToBeRemoved
          on entry
            do for Admin
              callEffect cdb:DeleteUser( context >> extern >> Url, binding )
              --remove currentobject

        -- After CouchdbServer$Admin provides the first password, he no longer
        -- has a perspective on it. The new value provided below is thus really private.
        state ResetPassword = PasswordReset
          on entry
            do
              callEffect cdb:ResetPassword( context >> extern >> Url, UserName, callExternal utl:GenSym() returns String )
              PasswordReset = true for currentobject

      property ToBeRemoved (Boolean)
      -- TODO: add a condition that allows an Account to see non-public repositories
      -- that he is Admin of.
      perspective on filter Repositories with IsPublic
        verbs (Consult)

      -- Even though Accounts can create Storages, they cannot actually
      -- create databases on the remote server, nor give themselves acces to
      -- them.
      perspective on Storages
        defaults
        action RequestStorage
          letA
            store <- createContext Storage in Storages
          in
            bind Party to currentrole in store -- KLOPT DIT WEL?


    -- This role should be in public space.
    context Repositories filledBy Repository
      --storage public
      property ToBeRemoved (Boolean)
      state Root = true
        state IsNamed = exists Name
          on entry
            do for Admin
              callEffect cdb:CreateDatabase( context >> extern >> Url, Name + "_read" )
              callEffect cdb:CreateDatabase( context >> extern >> Url, Name + "_write" )
              callEffect cdb:ReplicateContinuously( context >> extern >> Url, Name, Name + "_write", Name + "_read" )
        state Remove = ToBeRemoved
          on entry
            do for Admin
              callEffect cdb:EndReplication( context >> extern >> Url, Name + "_write", Name + "_read" )
              callEffect cdb:DeleteDatabase( context >> extern >> Url, Name + "_read" )
              callEffect cdb:DeleteDatabase( context >> extern >> Url, Name + "_write" )

    context Storages filledBy Storage

  -- PUBLIC
  -- This contexts implements the BodyWithAccounts pattern.
  case Repository
    aspect acc:Body
    --storage public
    external
      -- Only public repositories will be visible to Accounts of CouchdbServers.
      property IsPublic (mandatory, Boolean)
      property Name (mandatory, String)
      property Url (mandatory, String)

    user Admin filledBy CouchdbServer$Accounts, CouchdbServer$Admin
      -- As Admin, has a full perspective on Accounts.
      -- Should also be able to give them read access to the repo,
      -- and to retract that again.
      aspect acc:Body$Admin

      -- The admin can also create an Author and give him/her the right to add and
      -- remove models to the repo.
      perspective on Authors
        defaults

      -- The Admin can, of course, consult all models that are stored locally
      -- or in contributing Repositories.
      perspective on AvailableModels
        verbs (Consult)

    -- This role should be stored in private space.
    -- TODO. Is het mogelijk ook deze rol het aspect acc:Body$Accounts te geven?
    -- Guest kan dan kiezen of hij een Account wil, of een Author wil worden.
    user Authors filledBy CouchdbServer$Accounts, CouchdbServer$Admin
      state Root = true
        state IsFilled = exists binding
          on entry
            do for Admin
              -- As only the PDR of a user with role Repository$Admin will execute this,
              -- and Repository$Admin is a Db Admin, this will be allowed.
              callEffect cdb:MakeMemberOf( context >> extern >> Url, context >> extern >> Name + "_write", binding >> UserName )
              callEffect cdb:MakeMemberOf( context >> extern >> Url, context >> extern >> Name + "_read", binding >> UserName )
        state Remove = ToBeRemoved
          on entry
            do for Admin
              callEffect cdb:RemoveAsMemberOf( context >> extern >> Url, context >> extern >> Name + "_write", binding >> UserName)
              callEffect cdb:RemoveAsMemberOf( context >> extern >> Url, context >> extern >> Name + "_read", binding >> UserName)
              remove currentobject

      -- Admin can set this to true to remove the Author from the Repository.
      -- By using this mechanism instead of directly removing the role,
      -- we can remove the Author from the write-db.
      property ToBeRemoved (Boolean)

      -- The Authors can, of course, consult all models that are stored locally
      -- or in contributing Repositories.
      perspective on AvailableModels
        verbs (Consult)

    -- This role should be stored in private space.
    -- No further credentials are needed to access a Repository.
    -- This is because, in Couchdb, access to a database can be determined
    -- through (Couchdb database) roles or by membership.
    user Accounts filledBy CouchdbServer$Accounts, CouchdbServer$Admin
      aspect acc:Body$Accounts
      property ToBeRemoved (Boolean)
      state Root = true
        state IsFilled = exists binding
          on entry
            do for Admin
              -- As only the PDR of a user with role Repository$Admin will execute this,
              -- and Repository$Admin is a Db Admin, this will be allowed.
              callEffect cdb:MakeMemberOf( context >> extern >> Url, context >> extern >> Name + "_read", binding >> UserName )
        state Remove = ToBeRemoved
          on entry
            do for Admin
              callEffect cdb:RemoveAsMemberOf( context >> extern >> Url, context >> extern >> Name + "_read", binding >> UserName)
              remove currentobject
      in state Accepted
          -- An account that is accepted has a perspective on available models.
          perspective on AvailableModels
            verbs (Consult)

    -- Note that the aspect acc:Body introduces a Guest role
    -- with a perspective that allows it to create an Account.

    -- This role should be stored in public space. These are all models that
    -- are stored in this Repository.
    context AvailableModels filledBy mod:ModelDescription
      --storage public

  case Storage
    state CreateStore = exists Name && not InUse && not Terminated
      on entry
        do for Admin
          -- Create the store
          InUse = true
      state MakePrivate =
    user Party filledBy CouchdbServer$Accounts
      perspective on Store
        all roleverbs
        props (Name, Terminated, Private) verbs (SetPropertyValue, Consult)

    user Admin = context >> extern >> binder Storages >> Admin

    thing Store
      property Name (String)
      property InUse (Boolean)
      property Terminated (Boolean)
      property Private (Boolean)
