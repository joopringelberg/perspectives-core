-- Copyright Joop Ringelberg and Cor Baars 2021
domain CouchdbManagement
  use sys for model:System
  use cm for model:CouchdbManagement
  use mod for model:Models
  use acc for model:BodiesWithAccounts
  use cdb for model:Couchdb
  use util for model:Utilities

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
    -- This manager should be the Server admin of each CouchdbServer, in order
    -- to create databases and members.
    -- Becoming a Couchdb Server Admin should be managed outside Perspectives.
    user Manager = sys:Me
      perspective on CouchdbServers
        only (CreateAndFill)
        props (Name, Url) verbs (Consult)
      -- Manager needs this perspective for others to accept Admins created in state NoAdmin.
      perspective on CouchdbServers >> binding >> context >> CouchdbServer$Admin
        only (Create, Fill)

    -- A new CouchdbServers instance comes complete with a CouchdbServer$Admin role
    -- filled with CouchdbManagementApp$Admin.
    context CouchdbServers filledBy CouchdbServer
      state NoAdmin = not exists binding >> context >> CouchdbServer$Admin
        on entry
          do for Manager
            bind context >> Manager to Admin in binding >> context

  -- PUBLIC
  -- This contexts implements the BodyWithAccounts pattern.
  -- NOTE: a PerspectivesSystem$User should only fill either Admin, or Accounts!
  -- The PDR looks for credentials in either role and should find them just once.
  case CouchdbServer
    aspect acc:Body
    --storage public
    external
      property Url (mandatory, String)
      property Name (mandatory, String)
      state NotBound = not exists binder CouchdbServers
        on entry
          -- When a peer assigns the current user to the Accounts role,
          -- we make sure that the current user has the CouchdbServer bound
          -- in the CouchdbManagementApp.
          do for Accounts
            bind origin to CouchdbServers in cm:MyCouchdbApp
          notify Accounts
            "You now have an account with CouchdbServer {Name}"
    -- This role should be in private space.
    -- Admin in Couchdb of a particular server.
    user Admin filledBy CouchdbManagementApp$Manager
      -- As acc:Body$Admin, has full perspective on Accounts.
      aspect acc:Body$Admin

      -- TODO. Remove this as soon as we do not need the view anymore for the GUI.
      view AdminProps (UserName, Password)

      perspective on CouchdbServer$Accounts
        props (ToBeRemoved) verbs (Consult, SetPropertyValue)

      perspective on Repositories
        defaults

      -- The Admin should be able to create and fill the Repository$Admin.
      perspective on Repositories >> binding >> context >> Repository$Admin
        defaults

      -- The CouchdbServer$Admin has to be able to enter the credentials that
      -- make him Server admin of the Couchdb installation.
      perspective on CouchdbServer$Admin
        view AdminProps verbs (SetPropertyValue, Consult)

      perspective on extern
        defaults

    -- Note that the aspect acc:Body introduces a Guest role
    -- with a perspective that allows it to create an Account.

    -- This role should be in private space.
    user Accounts (unlinked, relational) filledBy sys:PerspectivesSystem$User
      aspect acc:Body$Accounts

      -- We want to be able to use the state acc:Body$Accounts$IsFilled.
      state IsFilled = exists binding
        on entry
          do for Admin
            letA
                pw <- callExternal util:GenSym() returns String
            in
              callEffect cdb:CreateUser( context >> extern >> Url, binding, pw )
              -- The Password property comes from the aspect acc:Body$Accounts.
              Password = pw
              --IsAccepted = true

      state Remove = ToBeRemoved
        on entry
          do for Admin
            callEffect cdb:DeleteUser( context >> extern >> Url, binding )
            remove role origin

      state Resetpassword = PasswordReset
        on entry
          do
            -- After CouchdbServer$Admin provides the first password, he no longer
            -- has a perspective on it. The new value provided below is thus really private.
            letA
              pw <- callExternal util:GenSym() returns String
            in
              callEffect cdb:ResetPassword( context >> extern >> Url, UserName, pw )
              PasswordReset = true
              Password = pw

      property ToBeRemoved (Boolean)

      action RequestRepository
        letA
          myrepo <- create context Repository bound to Repositories
          measadmin <- create role Admin in myrepo >> binding >> context
        in
          bind_ currentactor to measadmin

      -- As an Account, one can see both public repositories and repositories managed by oneself (as Admin).
      perspective on filter Repositories with IsPublic or binding >> context >> Repository$Admin binds sys:Me
        only (CreateAndFill)
        verbs (Consult)
        props (Name) verbs (SetPropertyValue)

      perspective on extern
        props (Url, Name) verbs (Consult)

    -- This role should be in public space.
    -- A Repositories instance comes complete with an (empty) Admin role.
    -- Moreover, as a side effect, both a read- and write database are created
    -- in Couchdb and the write database replicates to the read database.
    context Repositories (relational) filledBy Repository
      --storage public
      property ToBeRemoved (Boolean)
      -- Note that as it stands, an Account can unconditionally create a new
      -- Repository. Add a Boolean that represents the Admin's consent.
      state IsNamed = exists Name
        on entry
          do for Admin
            callEffect cdb:CreateCouchdbDatabase( context >> extern >> Url, Name + "_read" )
            callEffect cdb:CreateCouchdbDatabase( context >> extern >> Url, Name + "_write" )
            callEffect cdb:ReplicateContinuously( context >> extern >> Url, Name, Name + "_write", Name + "_read" )
        -- Ad Admin may exist already if the Repository is created by Accounts.
        state NoAdmin = not exists binding >> context >> Repository$Admin
          on entry
            do for Admin
              create role Admin in binding >> context
      state Remove = ToBeRemoved
        on entry
          do for Admin
            callEffect cdb:EndReplication( context >> extern >> Url, Name + "_write", Name + "_read" )
            callEffect cdb:DeleteCouchdbDatabase( context >> extern >> Url, Name + "_read" )
            callEffect cdb:DeleteCouchdbDatabase( context >> extern >> Url, Name + "_write" )
            remove role binding >> context >> Repository$Admin

  -- PUBLIC
  -- This contexts implements the BodyWithAccounts pattern.
  case Repository
    aspect acc:Body
    --storage public
    external
      -- Only public repositories will be visible to Accounts of CouchdbServers.
      property IsPublic (mandatory, Boolean)
      property Name (mandatory, String)
      property Url = binder Repositories >> context >> extern >> Url + Name

    -- We need the ServerAdmin in this context in order to configure the local Admin.
    user ServerAdmin = extern >> binder Repositories >> context >> CouchdbServer$Admin

    user Admin filledBy CouchdbServer$Accounts, CouchdbServer$Admin
      -- As Admin, has a full perspective on Accounts.
      -- Should also be able to give them read access to the repo,
      -- and to retract that again.
      aspect acc:Body$Admin
      state IsFilled = (exists binding) and exists context >> extern >> Url
        on entry
          do for ServerAdmin
            -- Only the CouchdbServer$Admin has a Create and Fill perspective on
            -- Repository$Admin. So when this state arises, we can be sure that
            -- the current user is, indeed, a CouchdbServer$Admin.
            -- Hence the PDR will authenticate with Server Admin credentials.
            letA
              url <- context >> extern >> binder Repositories >> context >> extern >> Url
            in
              callEffect cdb:MakeAdminOfDb( url, context >> extern >> Name + "_write", UserName )
              callEffect cdb:MakeAdminOfDb( url, context >> extern >> Name + "_read", UserName )
      state Remove = ToBeRemoved
        on entry
          do for ServerAdmin
            callEffect cdb:RemoveAsAdminFromDb( context >> extern >> Url, context >> extern >> Name + "_write", UserName )
            callEffect cdb:RemoveAsAdminFromDb( context >> extern >> Url, context >> extern >> Name + "_read", UserName )
            remove role origin
      property ToBeRemoved (Boolean)

      -- The admin can also create an Author and give him/her the right to add and
      -- remove models to the repo.
      perspective on Authors

      -- The Admin can, of course, consult all models that are stored locally
      -- or in contributing Repositories.
      perspective on AvailableModels
        verbs (Consult)

    -- This role should be stored in private space.
    -- TODO. Is het mogelijk ook deze rol het aspect acc:Body$Accounts te geven?
    -- Guest kan dan kiezen of hij een Account wil, of een Author wil worden.
    user Authors filledBy CouchdbServer$Accounts, CouchdbServer$Admin
      state IsFilled = exists binding
        on entry
          do for Admin
            -- As only the PDR of a user with role Repository$Admin will execute this,
            -- and Repository$Admin is a Db Admin, this will be allowed in Couchdb.
            letA
              url <- context >> extern >> binder Repositories >> context >> extern >> Url
            in
              callEffect cdb:MakeMemberOf( url, context >> extern >> Name + "_write", binding >> UserName )
              callEffect cdb:MakeMemberOf( url, context >> extern >> Name + "_read", binding >> UserName )
      state Remove = ToBeRemoved
        on entry
          do for Admin
            letA
              url <- context >> extern >> binder Repositories >> context >> extern >> Url
            in
              callEffect cdb:RemoveAsMemberOf( url, context >> extern >> Name + "_write", binding >> UserName)
              callEffect cdb:RemoveAsMemberOf( url, context >> extern >> Name + "_read", binding >> UserName)
              remove role origin

      -- Admin can set this to true to remove the Author from the Repository.
      -- By using this mechanism instead of directly removing the role,
      -- we can remove the Author from the write-db.
      property ToBeRemoved (Boolean)

      view AuthorForAdmin (FirstName, LastName, ToBeRemoved)

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
            remove role origin
      state Accepted = IsAccepted
        -- An account that is accepted has a perspective on available models.
        perspective on AvailableModels
          verbs (Consult)

    -- Note that the aspect acc:Body introduces a Guest role
    -- with a perspective that allows it to create an Account.

    -- This role should be stored in public space. These are all models that
    -- are stored in this Repository.
    context AvailableModels (relational) filledBy mod:ModelDescription
      --storage public
