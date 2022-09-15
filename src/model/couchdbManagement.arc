-- CouchdbManagement - Copyright Joop Ringelberg and Cor Baars 2021 - 2022

domain CouchdbManagement
  use sys for model:System
  use cm for model:CouchdbManagement
  use mod for model:Models
  use acc for model:BodiesWithAccounts
  use cdb for model:Couchdb
  use util for model:Utilities

  -------------------------------------------------------------------------------
  ---- MODEL
  -------------------------------------------------------------------------------
  -- The model description case.
  -- REMOVE ONCE WE CREATE INSTANCES WITH AN ACTION
  case Model
    aspect sys:Model
    external
      aspect sys:Model$External

  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  -- The INDEXED context cm:MyCouchdbApp, that is the starting point containing all CouchdbServers.
  case CouchdbManagementApp
    indexed cm:MyCouchdbApp
    aspect sys:RootContext
    external
      aspect sys:RootContext$External

    -- Every user manages his own CouchdbServers.
    -- This manager should be the Server admin of each CouchdbServer,
    -- in order to create databases and members.
    -- Becoming a Couchdb Server Admin should be managed outside Perspectives.
    user Manager = sys:Me
      perspective on CouchdbServers
        only (CreateAndFill, Remove)
        props (Name, Url) verbs (Consult)
        action CreateServer
          create role CouchdbServers

      -- Manager needs this perspective for others to accept Admins created in state NoAdmin.
      -- NOTE that model:TopContext can be used as Aspect instead.
      perspective on CouchdbServers >> binding >> context >> CouchdbServer$Admin
        only (Create, Fill)
      
      perspective on PrivatelyManagedServers
        props (ServerName) verbs (Consult)
        only (CreateAndFill, Remove)
        in object state Ready
          action CreateCouchdbServer
            letA 
              server <- create context CouchdbServer named (ServerUrl + "cw_servers_and_repositories/" + ServerName) bound to CouchdbServers
            in
              Url = ServerUrl for server
              Name = ServerName for server
      
      perspective on PrivatelyManagedServers >> binding >> context >> PManager
        only (Create, Fill)
              

    context PrivatelyManagedServers (relational) filledBy PrivatelyManagedServer
      -- De expressie hieronder levert de melding dat beide zijden niet vergeleken kunnen worden. 
      -- Waarschijnlijk omdat ze niet functioneel zijn.
      state Ready = HasDatabase -- and not exists filter context >> CouchdbServers with Url == origin >> ServerUrl
      state NoAdmin = (exists binding) and not exists binding >> context >> PManager
        on entry
          do for Manager
            bind context >> Manager to PManager in binding >> context


    -- A new CouchdbServers instance comes complete with a CouchdbServer$Admin role
    -- filled with CouchdbManagementApp$Admin.
    -- Also, the public database "servers_and_repositories" is created on the CouchdbServer_.
    context CouchdbServers (relational) filledBy CouchdbServer

      state NoAdmin = (exists binding) and not exists binding >> context >> CouchdbServer$Admin
        on entry
          do for Manager
            bind context >> Manager to Admin in binding >> context

  -------------------------------------------------------------------------------
  ---- PRIVATELYMANAGEDSERVER
  -------------------------------------------------------------------------------
  -- This context is not public. It allows us to 
  --    * register credentials for the ServerAdmin;
  --    * create the default database `servers_and_repositories`;
  --    * create a public CouchdbServer from it.
  case PrivatelyManagedServer
  
    external
      property ServerUrl (mandatory, String)
        -- pattern = /^https://[^\\/]+\\/$/ "An url with the https scheme, ending on a slash"
      property ServerName (mandatory, String)
      property HasDatabase (Boolean)

      state NoDatabase = (not HasDatabase) and (exists ServerUrl) and (exists context >> PManager >> Password)
        on entry
          do for PManager
            -- Notice that Pouchdb only creates a database if it does not yet exist.
            -- This allows us to reconnect to a CouchdbServer_ if its CouchdbServer had been lost.
            AuthorizedDomain = ServerUrl for context >> PManager
            callEffect cdb:AddCredentials( ServerUrl, context >> PManager >> Password)
            callEffect cdb:CreateCouchdbDatabase( ServerUrl, "cw_servers_and_repositories" )
            HasDatabase = true

    user PManager filledBy sys:PerspectivesSystem$User
      aspect acc:WithCredentials

      perspective on PManager
        only (CreateAndFill)
        props (UserName, Password) verbs (SetPropertyValue)
      
      perspective on extern 
        props (ServerUrl, ServerName) verbs (SetPropertyValue)
        props (HasDatabase) verbs (Consult)
      


  -------------------------------------------------------------------------------
  ---- COUCHDBSERVER
  -------------------------------------------------------------------------------
  -- PUBLIC
  -- This contexts implements the BodyWithAccounts pattern.
  -- NOTE: a PerspectivesSystem$User should only fill either Admin, or Accounts!
  -- The PDR looks for credentials in either role and should find them just once.
  case CouchdbServer
    aspect acc:Body
    --storage public
    external
      property Url (String)
      property Name (String)

      -- If we do not refer to my indexed version of the CouchdbApp, this condition will fail because another user
      -- will have shared his App, too!
      state NotBound = not exists filter cm:MyCouchdbApp >> CouchdbServers with filledBy origin
        -- Accounts needs this perspective to be able to add the CouchdbServer to his cm:MyCouchdbApp!
        perspective of Accounts 
          perspective on extern >> binder CouchdbServers
            only (Create, Fill)
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
    user Admin filledBy CouchdbManagementApp$Manager -- unlinked??
      -- As acc:Body$Admin, has full perspective on Accounts.
      aspect acc:Body$Admin

      perspective on extern
        defaults

      perspective on Repositories
        all roleverbs
        props (Endorsed, IsPublic, Name, AdminLastName) verbs (Consult)
        props (IsPublic) verbs (SetPropertyValue)
        in object state WithoutExternalDatabase
          props (Endorsed, Name) verbs (SetPropertyValue)
        in object state WithExternalDatabase
          props (IsPublic) verbs (SetPropertyValue)
      
      perspective on Accounts
        only (Create, Fill, Remove, RemoveFiller)
        props (FirstName, LastName, IsAccepted, IsRejected) verbs (Consult)
      
      perspective on WaitingAccounts
        props (FirstName, LastName) verbs (Consult)
        props (IsAccepted, IsRejected) verbs (SetPropertyValue)
        action Accept
          IsAccepted = true
        action Reject
          IsRejected = true
      
      screen "Couchdb Server"
        tab "The server"
          row
            form External
          row
            form Admin
        tab "Accounts"
          row
            table Accounts
        tab "Repositories"
          row 
            table Repositories
        tab "Applicants"
          row 
            table WaitingAccounts

    -- The aspect acc:Body introduces a Guest role
    -- with a perspective that allows it to request an Account when he has none.
    aspect user acc:Body$Guest

    -- This role should be in private space.
    user Accounts (unlinked, relational) filledBy sys:PerspectivesSystem$User
      aspect acc:Body$Accounts

      -- Our execution model allows us to query the entire intact structure in the 
      -- actions on exit, before the role is actually detached and removed.
      on exit of acc:Body$Accounts$IsFilled
        do for Admin
          callEffect cdb:DeleteUser( context >> extern >> Url, binding )

      on entry of subject state acc:Body$Accounts$IsFilled$Accepted
        do for Admin
          letA
              pw <- callExternal util:GenSym() returns String
          in
            callEffect cdb:CreateUser( context >> extern >> Url, binding, pw )
            -- The Password property comes from the aspect acc:Body$Accounts.
            Password = pw
            IsAccepted = true

      perspective on Accounts
        action ResetPassword
          -- After CouchdbServer$Admin provides the first password, he no longer
          -- has a perspective on it. The new value provided below is thus really private.
          letA
            pw <- callExternal util:GenSym() returns String
          in
            callEffect cdb:ResetPassword( context >> extern >> Url, UserName, pw )
            Password = pw

      perspective on Repositories
        props (IsPublic, Name) verbs (Consult)
        only (CreateAndFill)

      perspective on MyRepositories
        only (Remove)
        props (Name, Endorsed) verbs (Consult)
        props (IsPublic) verbs (SetPropertyValue)
        in object state WithoutExternalDatabase
          props (Name) verbs (SetPropertyValue)

      perspective on PublicRepositories
        props (Name) verbs (Consult)
        
      perspective on Repositories >> binding >> context >> Admin
        only (Create, Fill, Remove)

      perspective on extern
        props (Url, Name) verbs (Consult)
      
      in state acc:Body$Accounts$IsFilled$Accepted
        action RequestRepository
          letA
            myrepo <- create context Repository bound to Repositories
          in
            bind currentactor to Admin in myrepo >> binding >> context

      screen "Couchdb Server"
        tab "Server information"
          row
            form External
          row
            table Accounts
        tab "Repositories"
          row 
            table MyRepositories
          row
            table PublicRepositories

    user WaitingAccounts = filter Accounts with (not IsRejected) and not IsAccepted

    context MyRepositories = filter Repositories with binding >> context >> Repository$Admin filledBy sys:Me

    context PublicRepositories = filter Repositories with IsPublic

    -- This role should be in public space.
    -- A Repositories instance comes complete with an (empty) Admin role.
    -- Moreover, as a side effect, both a read- and write database are created
    -- in Couchdb and the write database replicates to the read database.
    context Repositories (relational) filledBy Repository
      property Endorsed (Boolean)
      on exit of WithExternalDatabase
        do for Admin
          callEffect cdb:EndReplication( context >> extern >> Url, Name + "_write", Name + "_read" )
          callEffect cdb:DeleteCouchdbDatabase( context >> extern >> Url, Name + "_read" )
          callEffect cdb:DeleteCouchdbDatabase( context >> extern >> Url, Name + "_write" )
          remove role binding >> context >> Repository$Admin

      -- Note that as it stands, an Account can unconditionally create a new
      -- Repository. Add a Boolean that represents the Admin's consent.
      state WithExternalDatabase = (exists Name) and Endorsed
        on entry
          do for Admin
            callEffect cdb:CreateCouchdbDatabase( context >> extern >> Url, Name + "_read" )
            callEffect cdb:CreateCouchdbDatabase( context >> extern >> Url, Name + "_write" )
            callEffect cdb:ReplicateContinuously( context >> extern >> Url, Name, Name + "_write", Name + "_read" )
      state WithoutExternalDatabase = (not exists Name) or not Endorsed
        -- Ad Admin may exist already if the Repository is created by Accounts.
        -- state NoAdmin = Endorsed and not exists binding >> context >> Repository$Admin
        --   on entry
        --     do for Admin
        --       -- create role Admin in binding >> context
        --       bind context >> Admin to Admin in binding >> context

  -------------------------------------------------------------------------------
  ---- REPOSITORY
  -------------------------------------------------------------------------------
  -- PUBLIC
  -- This contexts implements the BodyWithAccounts pattern.
  case Repository
    aspect acc:Body
    aspect sys:ManifestCollection
    --storage public
    -- on exit
      -- Verwijder de inhoud?
    external
      -- Only public repositories will be visible to Accounts of CouchdbServers.
      property IsPublic (mandatory, Boolean)
      property Name (mandatory, String)
        -- Only lowercase characters (a-z), digits (0-9), and any of the characters _, $, (, ), +, -, and / are allowed. Must begin with a letter.
        -- However, the parser refuses (, ) and /.
        -- ^[a-z][a-z0-9_$()+/-]*$ according to https://docs.couchdb.org/en/3.2.0/api/database/common.html and https://localhost:6984//_utils/docs/api/database/common.html#specifying-the-document-id
        pattern = /[a-z]([a-z]|[0-9]|[_$+-])*/ "Only lowercase characters (a-z), digits (0-9), and any of the characters _, $, + and - are allowed. Must begin with a letter."
      property ReadDb = Name + "_read"
      property WriteDb = Name + "_write"
      property Url = binder Repositories >> context >> extern >> Url + Name
      property AdminLastName = context >> Admin >> LastName

    -- We need the ServerAdmin in this context in order to configure the local Admin.
    user ServerAdmin = extern >> binder Repositories >> context >> CouchdbServer$Admin
      perspective on Admin
        only (Create, Fill, RemoveFiller, Remove)
        props (FirstName, LastName) verbs (Consult)

    user Admin filledBy CouchdbServer$Accounts, CouchdbServer$Admin
      -- As Admin, has a full perspective on Accounts.
      -- Should also be able to give them read access to the repo,
      -- and to retract that again.
      aspect acc:Body$Admin

      state IsFilled = exists binding

      on exit 
        notify "You are no longer the administrator of the repository { context >> extern >> Name }."

      on exit of IsFilled
        do for ServerAdmin
          callEffect cdb:RemoveAsAdminFromDb( context >> extern >> Url, context >> extern >> WriteDb, UserName )
          callEffect cdb:RemoveAsAdminFromDb( context >> extern >> Url, context >> extern >> ReadDb, UserName )
          remove role origin

      on entry of IsFilled
        do for ServerAdmin
          -- Only the CouchdbServer$Admin has a Create and Fill perspective on
          -- Repository$Admin. So when this state arises, we can be sure that
          -- the current user is, indeed, a CouchdbServer$Admin.
          -- Hence the PDR will authenticate with Server Admin credentials.
          letA
            url <- context >> extern >> binder Repositories >> context >> extern >> Url
          in
            callEffect cdb:MakeAdminOfDb( url, context >> extern >> WriteDb, UserName )
            callEffect cdb:MakeAdminOfDb( url, context >> extern >> ReadDb, UserName )

      -- The admin can also create an Author and give him/her the right to add and
      -- remove models to the repo.
      perspective on Authors
        only (Create, Fill, Remove)
        props (FirstName, LastName) verbs (Consult)
      
      perspective on External
        props (IsPublic, Name) verbs (Consult)

      -- The Admin can, of course, consult all models that are stored locally
      -- or in contributing Repositories.
      perspective on sys:ManifestCollection$Manifests
        props (ModelName) verbs (Consult)
      
      screen "Repository"
        tab "This repository"
          row 
            form External
          row
            form Admin
        tab "Accounts"
          row
            table Accounts
        tab "Authors"
          row
            table Authors
        tab "Manifests"
          row
            table sys:ManifestCollection$Manifests
      
    -- This role should be stored in private space.
    -- TODO. Is het mogelijk ook deze rol het aspect acc:Body$Accounts te geven?
    -- Guest kan dan kiezen of hij een Account wil, of een Author wil worden.
    user Authors (relational) filledBy CouchdbServer$Accounts, CouchdbServer$Admin
      aspect sys:ManifestCollection$Manager
      on exit 
        notify "You are no longer an Author of the repository { context >> extern >> Name }."
      on exit of IsFilled
        do for Admin
          letA
            url <- context >> extern >> binder Repositories >> context >> extern >> Url
          in
            callEffect cdb:RemoveAsMemberOf( url, context >> extern >> Name + "_write", binding >> UserName)
            callEffect cdb:RemoveAsMemberOf( url, context >> extern >> Name + "_read", binding >> UserName)
            remove role origin

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

      -- The Authors can, of course, consult all models that are stored locally
      -- or in contributing Repositories.
      perspective on sys:ManifestCollection$Manifests
        props (ModelName) verbs (Consult)

    -- This role should be stored in private space.
    -- No further credentials are needed to access a Repository.
    -- This is because, in Couchdb, access to a database can be determined
    -- through (Couchdb database) roles or by membership.
    user Accounts (relational) filledBy CouchdbServer$Accounts, CouchdbServer$Admin
      aspect acc:Body$Accounts
      on exit 
        notify "You no longer have an Account with the repository { context >> extern >> Name }."
      on exit of IsFilled
        do for Admin
          callEffect cdb:RemoveAsMemberOf( context >> extern >> Url, context >> extern >> ReadDb, binding >> UserName)
          remove role origin
      
      state IsFilled = exists binding
        on entry
          do for Admin
            -- As only the PDR of a user with role Repository$Admin will execute this,
            -- and Repository$Admin is a Db Admin, this will be allowed.
            callEffect cdb:MakeMemberOf( context >> extern >> Url, context >> extern >> ReadDb, binding >> UserName )
      
      -- in state acc:Body$Accounts$IsFilled$Accepted
      --   -- An account that is accepted has a perspective on available models.
        perspective on sys:ManifestCollection$Manifests
          props (ModelName) verbs (Consult)

    -- Note that the aspect acc:Body introduces a Guest role
    -- with a perspective that allows it to create an Account.

    -- This role should be stored in public space. These are all models that
    -- are stored in this Repository.
    aspect context sys:ManifestCollection$Manifests
      --storage public
