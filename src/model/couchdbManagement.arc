-- CouchdbManagement - Copyright Joop Ringelberg and Cor Baars 2021 - 2022

domain model://perspectives.domains#CouchdbManagement
  use sys for model://perspectives.domains#System
  use cm for model://perspectives.domains#CouchdbManagement
  use acc for model://perspectives.domains#BodiesWithAccounts
  use cdb for model://perspectives.domains#Couchdb
  use util for model://perspectives.domains#Utilities
  use p for model://perspectives.domains#Parsing

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- We must first create the context and then later bind it.
          -- If we try to create and bind it in a single statement, 
          -- we find that the Installer can just create RootContexts
          -- as they are the allowed binding of StartContexts.
          -- As a consequence, no context is created.
          app <- create context CouchdbManagementApp
          indexedcontext <- create role IndexedContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind app >> extern to StartContexts in sys:MySystem
          Name = "Couchdb Management App" for app >> extern
          bind_ app >> extern to indexedcontext
          IndexedContexts$Name = app >> indexedName for indexedcontext

  aspect user sys:PerspectivesSystem$Installer
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  -- The INDEXED context cm:MyCouchdbApp, that is the starting point containing all CouchdbServers.
  -- There is NO PUBLIC PERSPECTIVE on this App.
  case CouchdbManagementApp
    indexed cm:MyCouchdbApp
    aspect sys:RootContext
    external
      aspect sys:RootContext$External
    
    on exit
      do for Manager
        delete context bound to PrivatelyManagedServers
        delete context bound to CouchdbServers

    -- Every user manages his own CouchdbServers.
    -- This manager should be the Server admin of each CouchdbServer,
    -- in order to create databases and members.
    -- Becoming a Couchdb Server Admin should be managed outside Perspectives.
    user Manager = sys:Me
      perspective on CouchdbServers
        only (CreateAndFill, Remove, Delete)
        props (Name, Url) verbs (Consult)
        action CreateServer
          create role CouchdbServers

      -- Manager needs this perspective for others to accept Admins created in state NoAdmin.
      -- NOTE that model:TopContext can be used as Aspect instead.
      perspective on CouchdbServers >> binding >> context >> CouchdbServer$Admin
        only (Create, Fill)
      
      perspective on PrivatelyManagedServers
        props (ServerName) verbs (Consult)
        only (CreateAndFill, Remove, Delete)
        in object state Ready
          action CreateCouchdbServer
            letA 
              -- Dit kunnen we vervangen door een publiek perspectief in CouchdbServer dat gepubliceerd wordt in Serverurl/cw_servers_and_repositories
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
  -- There is NO PUBLIC PERSPECTIVE on this context. It allows us to 
  --    * register credentials for the ServerAdmin;
  --    * create the default database `servers_and_repositories`;
  --    * create a public CouchdbServer from it.
  case PrivatelyManagedServer
  
    external
      property ServerUrl (mandatory, String)
        pattern = "^https://[^\\/]+\\/$" "An url with the https scheme, ending on a slash"
      property ServerName (mandatory, String)
      property HasDatabase (Boolean)

      state NoDatabase = (not HasDatabase) and (exists ServerUrl) and (exists context >> PManager >> Password)
        on entry
          do for PManager
            -- Notice that Pouchdb only creates a database if it does not yet exist.
            -- This allows us to reconnect to a CouchdbServer_ if its CouchdbServer had been lost.
            -- AuthorizedDomain = ServerUrl for context >> PManager
            -- Adds credentials to Perspectives State; not to the Couchdb_!
            callEffect cdb:AddCredentials( ServerUrl, context >> PManager >> Password)
            callEffect cdb:CreateCouchdbDatabase( ServerUrl, "cw_servers_and_repositories" )
            callEffect cdb:CreateCouchdbDatabase( ServerUrl, "cw_servers_and_repositories_write" )
            callEffect cdb:ReplicateContinuously( ServerUrl, "cw_servers_and_repositories_write", "cw_servers_and_repositories" )
            callEffect cdb:MakeDatabasePublic( ServerUrl, "cw_servers_and_repositories" )
            HasDatabase = true
      on exit
        do for PManager
          callEffect cdb:DeleteCouchdbDatabase( ServerUrl, "cw_servers_and_repositories" )
          callEffect cdb:DeleteCouchdbDatabase( ServerUrl, "cw_servers_and_repositories_write" )
          callEffect cdb:EndReplication( ServerUrl, "cw_servers_and_repositories_write", "cw_servers_and_repositories" )

    user PManager filledBy sys:PerspectivesSystem$User
      aspect sys:WithCredentials

      perspective on PManager
        only (CreateAndFill)
        props (Password) verbs (SetPropertyValue)
        props (UserName) verbs (Consult)
      
      perspective on extern 
        props (ServerUrl, ServerName) verbs (SetPropertyValue)
        props (HasDatabase) verbs (Consult)
      


  -------------------------------------------------------------------------------
  ---- COUCHDBSERVER
  -------------------------------------------------------------------------------
  -- This context has a PUBLIC USER Visitor that causes a version of the context to be published.
  -- Visitor cn see the Repositories and their descriptions, and sign up.
  -- To sign up, he should be able to see the admin.
  
  -- This contexts implements the BodyWithAccounts pattern.
  -- NOTE: a PerspectivesSystem$User should only fill either Admin, or Accounts!
  -- The PDR looks for credentials in either role and should find them just once.
  case CouchdbServer
    aspect acc:Body
    --storage public
    external
      -- The location of the CouchdbServer_.
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

    on exit
      do for Admin
        delete context bound to Repositories

    -- publish at extern >> Url
    --   perspective on extern
    --     props (Name) verbs (Consult)
    --   perspective on Admin
    --     props (FirstName, LastName)
    --   perspective on PublicRepositories
    --     props (RepositoryName) verbs (Consult)


    -- This role should be in public space insofar that e.g. acc:Body$Guest should be able to see it.
    -- Admin in Couchdb of a particular server.
    user Admin filledBy CouchdbManagementApp$Manager 
      -- As acc:Body$Admin, has full perspective on Accounts.
      aspect acc:Body$Admin

      action CreateRepository
        create role Repositories

      perspective on extern
        props (Url, Name) verbs (Consult)

      perspective on Repositories
        all roleverbs
        props (Endorsed, IsPublic, AdminLastName) verbs (Consult)
        props (IsPublic, RepositoryName) verbs (SetPropertyValue)
        in object state WithoutExternalDatabase
          props (Endorsed) verbs (SetPropertyValue)
        in object state WithExternalDatabase
          props (IsPublic) verbs (SetPropertyValue)
      
      -- Additional properties for Admin's perspective on Accounts.
      perspective on Accounts
        props (IsAccepted, IsRejected) verbs (Consult)
      
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
              props (Url, Name) verbs (Consult)
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
    -- with a perspective that allows it to request an Account when he has none
    -- and to synchronize that with Admin.
    -- LET OP: deze rol gaat er waarschijnlijk (voorlopig) uit. De modellering met dit aspect wordt te complex.
    aspect user acc:Body$Guest

    user Accounts (unlinked, relational) filledBy sys:PerspectivesSystem$User private
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

    public Visitor at Url = sys:Me
      perspective on PublicRepositories
        props (RepositoryName) verbs (Consult)
      
      -- Perspective on Admin in order to be able to sign up as an Account.
      perspective on Admin
        props (FirstName, LastName) verbs (Consult)

    context MyRepositories = filter Repositories with binding >> context >> Repository$Admin filledBy sys:Me

    context PublicRepositories = filter Repositories with IsPublic

    -- This role should be in public space.
    -- A Repositories instance comes complete with an (empty) Admin role.
    -- Moreover, as a side effect, both a read- and write database are created
    -- in Couchdb and the write database replicates to the read database.
    context Repositories (relational) filledBy Repository
      property Endorsed (Boolean)
      property RepositoryName (String)

      state HasName = exists RepositoryName
        on entry
          do for Admin
            create_ context Repository named (context >> extern >> Url + "cw_servers_and_repositories/" + RepositoryName) bound to origin
            Name = RepositoryName for origin >> binding

      -- Note that as it stands, an Account can unconditionally create a new
      -- Repository. Add a Boolean that represents the Admin's consent.
      state WithExternalDatabase = (exists Name) and Endorsed
        on entry
          do for Admin
            letA
              baseurl <- context >> extern >> Url
              readmodels <- "models_" + Name
              writemodels <- "models_" + Name + "_write"
              readinstances <- "cw_" + Name
              writeinstances <- "cw_" + Name + "_write"
            in 
              -- models
              callEffect cdb:CreateCouchdbDatabase( baseurl, readmodels )
              callEffect cdb:MakeDatabasePublic( baseurl, readmodels )
              callEffect cdb:CreateCouchdbDatabase( baseurl, writemodels )
              callEffect cdb:ReplicateContinuously( baseurl, writemodels, readmodels )
              -- instances
              callEffect cdb:CreateCouchdbDatabase( baseurl, readinstances )
              callEffect cdb:MakeDatabasePublic( baseurl, readinstances )
              callEffect cdb:CreateCouchdbDatabase( baseurl, writeinstances )
              callEffect cdb:ReplicateContinuously( baseurl, writeinstances, readinstances )
        on exit
          do for Admin
            letA
              baseurl <- context >> extern >> Url
              readmodels <- "models_" + Name
              writemodels <- "models_" + Name + "_write"
              readinstances <- "cw_" + Name
              writeinstances <- "cw_" + Name + "_write"
            in 
              -- models
              callEffect cdb:EndReplication( baseurl, writemodels, readmodels )
              callEffect cdb:DeleteCouchdbDatabase( baseurl, readmodels )
              callEffect cdb:DeleteCouchdbDatabase( baseurl, writemodels )
              -- instances
              callEffect cdb:EndReplication( baseurl, writeinstances, readinstances )
              callEffect cdb:DeleteCouchdbDatabase( baseurl, readinstances )
              callEffect cdb:DeleteCouchdbDatabase( baseurl, writeinstances )
              remove role binding >> context >> Repository$Admin

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
  -- This context has a public perspective that allows the visitor to see the models.
  -- This contexts implements the BodyWithAccounts pattern.
  case Repository
    aspect acc:Body
    aspect sys:ManifestCollection

    external
      -- Only public repositories will be visible to Accounts of CouchdbServers.
      property IsPublic (mandatory, Boolean)
      property Name (mandatory, String)
        -- Only lowercase characters (a-z), digits (0-9), and any of the characters _, $, (, ), +, -, and / are allowed. Must begin with a letter.
        -- However, the parser refuses (, ) and /.
        -- ^[a-z][a-z0-9_$()+/-]*$ according to https://docs.couchdb.org/en/3.2.0/api/database/common.html and https://localhost:6984//_utils/docs/api/database/common.html#specifying-the-document-id
        pattern = "[a-z]([a-z]|[0-9]|[_$+-])*" "Only lowercase characters (a-z), digits (0-9), and any of the characters _, $, + and - are allowed. Must begin with a letter."
      property Authority = Name >> callExternal util:Replace( "_", "." ) returns String
      property ReadModels = "models_" + Name
      property WriteModels = "models_" + Name + "_write"
      property ReadInstances = "cw_" + Name
      property WriteInstances = "cw_" + Name + "_write"
      -- This equals the identifier of the Repository itself.
      property RepositoryUrl = binder Repositories >> context >> extern >> Url + Name
      -- The URL that identifies the models database for this repository (for writing).
      property ModelsUrl = binder Repositories >> context >> extern >> Url + WriteModels + "/"
      -- The URL that identifies the instances database for this repository (for reading).
      property InstancesUrl = binder Repositories >> context >> extern >> Url + ReadInstances + "/"
      property AdminLastName = context >> Admin >> LastName

    on exit
      do for Admin
        delete context bound to Manifests

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

      action CreateManifest
        create role Manifests

      state IsFilled = exists binding

      on exit 
        notify "You are no longer the administrator of the repository { context >> extern >> Name }."

      on exit of IsFilled
        do for ServerAdmin
          letA
            couchdbserverurl <- context >> extern >> binder Repositories >> context >> extern >> Url
          in
            -- models
            callEffect cdb:RemoveAsAdminFromDb( couchdbserverurl, context >> extern >> WriteModels, UserName )
            callEffect cdb:RemoveAsAdminFromDb( couchdbserverurl, context >> extern >> ReadModels, UserName )
            -- instances
            callEffect cdb:RemoveAsAdminFromDb( couchdbserverurl, context >> extern >> WriteInstances, UserName )
            callEffect cdb:RemoveAsAdminFromDb( couchdbserverurl, context >> extern >> ReadInstances, UserName )
            remove role origin

      on entry of IsFilled
        do for ServerAdmin
          -- Only the CouchdbServer$Admin has a Create and Fill perspective on
          -- Repository$Admin. So when this state arises, we can be sure that
          -- the current user is, indeed, a CouchdbServer$Admin.
          -- Hence the PDR will authenticate with Server Admin credentials.
          letA
            couchdbserverurl <- context >> extern >> binder Repositories >> context >> extern >> Url
          in
            -- models
            callEffect cdb:MakeAdminOfDb( couchdbserverurl, context >> extern >> WriteModels, UserName )
            callEffect cdb:MakeAdminOfDb( couchdbserverurl, context >> extern >> ReadModels, UserName )
            -- instances
            callEffect cdb:MakeAdminOfDb( couchdbserverurl, context >> extern >> WriteInstances, UserName )
            callEffect cdb:MakeAdminOfDb( couchdbserverurl, context >> extern >> ReadInstances, UserName )

      -- The admin can also create an Author and give him/her the right to add and
      -- remove models to the repo.
      perspective on Authors
        only (Create, Fill, Remove)
        props (FirstName, LastName) verbs (Consult)
      
      perspective on External
        props (IsPublic, Name) verbs (Consult)

      -- The Admin can, of course, consult all models that are stored locally
      -- or in contributing Repositories.
      perspective on Manifests
        only (Create, Fill, Delete, Remove)
        props (Manifests$ModelName) verbs (SetPropertyValue)
        props (Description) verbs (Consult)
      
      perspective on Manifests >> binding >> context >> Author
        only (Create, Fill)

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
            table Manifests
      
    -- This role should be stored in private space.
    -- TODO. Is het mogelijk ook deze rol het aspect acc:Body$Accounts te geven?
    -- Guest kan dan kiezen of hij een Account wil, of een Author wil worden.
    user Authors (relational) filledBy CouchdbServer$Accounts, CouchdbServer$Admin
      on exit 
        notify "You are no longer an Author of the repository { context >> extern >> Name }."
      on exit of IsFilled
        do for Admin
          letA
            couchdbserverurl <- context >> extern >> binder Repositories >> context >> extern >> Url
          in
            -- models
            callEffect cdb:RemoveAsMemberOf( couchdbserverurl, context >> extern >> WriteModels, binding >> UserName)
            callEffect cdb:RemoveAsMemberOf( couchdbserverurl, context >> extern >> ReadModels, binding >> UserName)
            -- instances
            callEffect cdb:RemoveAsMemberOf( couchdbserverurl, context >> extern >> WriteInstances, binding >> UserName)
            callEffect cdb:RemoveAsMemberOf( couchdbserverurl, context >> extern >> ReadInstances, binding >> UserName)
            remove role origin

      state IsFilled = exists binding
        on entry
          do for Admin
            -- As only the PDR of a user with role Repository$Admin will execute this,
            -- and Repository$Admin is a Db Admin, this will be allowed in Couchdb.
            letA
              couchdbserverurl <- context >> extern >> binder Repositories >> context >> extern >> Url
            in
              -- models
              callEffect cdb:MakeMemberOf( couchdbserverurl, context >> extern >> WriteModels, binding >> UserName )
              callEffect cdb:MakeMemberOf( couchdbserverurl, context >> extern >> ReadModels, binding >> UserName )
              -- instances
              callEffect cdb:MakeMemberOf( couchdbserverurl, context >> extern >> WriteInstances, binding >> UserName )
              callEffect cdb:MakeMemberOf( couchdbserverurl, context >> extern >> ReadInstances, binding >> UserName )

      -- The Authors can, of course, consult all models that are stored locally
      -- or in contributing Repositories.
      perspective on Manifests
        props (Manifests$ModelName) verbs (Consult)

    -- This role should be stored in private space.
    -- No further credentials are needed to access a Repository.
    -- This is because, in Couchdb, access to a database can be determined
    -- through (Couchdb database) roles or by membership.
    user Accounts (relational, unlinked) filledBy CouchdbServer$Accounts, CouchdbServer$Admin
      aspect acc:Body$Accounts
      on exit 
        notify "You no longer have an Account with the repository { context >> extern >> Name }."
      on exit of IsFilled
        do for Admin
          callEffect cdb:RemoveAsMemberOf( context >> extern >> binder Repositories >> context >> extern >> Url, context >> extern >> ReadModels, binding >> UserName)
          remove role origin
      
      state IsFilled = exists binding
        on entry
          do for Admin
            -- As only the PDR of a user with role Repository$Admin will execute this,
            -- and Repository$Admin is a Db Admin, this will be allowed.
            callEffect cdb:MakeMemberOf( context >> extern >> binder Repositories >> context >> extern >> Url, context >> extern >> ReadModels, binding >> UserName )
      
      -- in state acc:Body$Accounts$IsFilled$Accepted
      --   -- An account that is accepted has a perspective on available models.
        perspective on Manifests
          props (Manifests$ModelName) verbs (Consult)

    -- Note that the aspect acc:Body introduces a Guest role
    -- with a perspective that allows it to create an Account.

    public Visitor at extern >> InstancesUrl
      perspective on Manifests
        props (ModelName) verbs (Consult)

    -- This role should be stored in public space. These are all models that
    -- are stored in this Repository.
    context Manifests filledBy ModelManifest
      aspect sys:ManifestCollection$Manifests
      state ReadyToMake = (exists Manifests$ModelName) and not exists binding
        on entry
          do for Admin
            create_ context ModelManifest named (context >> extern >> InstancesUrl + origin >> Manifests$ModelName) bound to origin
            bind currentactor to Author in origin >> binding >> context

  -- This context has NO PUBLIC PERSPECTIVE.
  case ModelManifest
    aspect sys:ModelManifest

    external
      aspect sys:ModelManifest$External
      -- Notice that we have to register the ModelName on the context role in the collection,
      -- to serve in the pattern that creates a DNS URI, so it can be a public resource.
      property ModelName = binder Manifests >> Manifests$ModelName
      -- Two properties we replicate from the Repository:
      -- The URL that identifies the instances database in the Repository (the reading version). 
      -- Use this as the base for public resource identifiers.
      -- property InstancesUrl = binder Manifests >> context >> extern >> InstancesUrl
    --   -- The URL that identifies the models database for this repository (for writing).
      -- property ModelsUrl = binder Manifests >> context >> extern >> ModelsUrl
      -- property Authority = binder Manifests >> context >> extern >> Authority
      -- property RepositoryName = binder Manifests >> context >> extern >> Name
    
    context Repository = extern >> binder Manifests >> context >> extern

    user Author filledBy Repository$Author
      perspective on extern
        props (ModelName) verbs (Consult)
        props (Description) verbs (SetPropertyValue)
      perspective on Versions
        only (Create, Fill, Remove)
        props (Versions$Version, VersionedModelManifest$External$Description) verbs (SetPropertyValue)
      perspective on Versions >> binding >> context >> Author
        only (Fill, Create)
      action CreateVersion
        create role Versions
    
    -- In order to add this model to one's installation, one should become an ActiveUser of the Manifest.
    user ActiveUser filledBy Repository$Account
      perspective on Versions
        props (Version) verbs (Consult)

    context Versions (relational) filledBy VersionedModelManifest
      aspect sys:ModelManifest$Versions
      state ReadyToMake = (exists Versions$Version) and not exists binding
        on entry
          do for Author
            -- The Model URI (the 'logical name' of the versioned model)
            -- ModelIdentifier = "model://" + (context >> extern >> ( Authority + "/" + ModelName ) )
            -- -- The model URL (the internet address of the versioned model in the DNS)
            -- ModelUrl = context >> extern >> (ModelsUrl + "model:" + ModelManifest$External$ModelName) + "@" + Versions$Version
            -- create_ context VersionedModelManifest named ( (context >> extern >> (InstancesUrl + ModelName)) + "@" + Versions$Version ) bound to origin
            -- IN TERMEN VAN REPOSITORY
            ModelIdentifier = "model://" + context >> Repository >> Authority + "/" + context >> extern >> ModelName
            ModelUrl = context >> Repository >> ModelsUrl + "model:" + context >> extern >> ModelName + "@" + Versions$Version
            create_ context VersionedModelManifest named ( context >> (Repository >> InstancesUrl + extern >> ModelName) + "@" + Versions$Version) bound to origin
            bind currentactor to VersionedModelManifest$Author in origin >> binding >> context

  -- This context has NO PUBLIC PERSPECTIVE.
  case VersionedModelManifest
    aspect sys:VersionedModelManifest
    aspect sys:ContextWithNotification
    state UploadToRepository = extern >> (ArcOK and SourcesChanged)
      on entry
        do for Author
          callEffect p:UploadToRepository2( extern >> ArcSource )
          SourcesChanged = false for extern
        notify Author
          "Version {extern >> Version} has been uploaded to the repository { extern >> binder Versions >> context >> Repository >> Name}."

    external
      aspect sys:VersionedModelManifest$External
      -- The Version property is registered on ModelManifest$Versions so we can use it to create a DNS URN for it (it must be a public resource)
      property Version = binder Versions >> Versions$Version
      property ArcSource (mandatory, String)
        minLength = 81 -- shows as a textarea
      property ArcFeedback (String)
        minLength = 81
      property ArcOK = ArcFeedback matches regexp "^OK"
      property SourcesChanged (Boolean)
      property DomeinFile (File)
      
      on exit
        do for Author
          -- Delete the DomeinFile.
          callEffect cdb:DeleteDocument( binder Versions >> ModelUrl )

      state ReadyToCompile = ((exists ArcSource) and exists External$Version)

      state ProcessArc = (exists ArcSource) and not exists ArcFeedback
        on entry
          do for Author
            ArcFeedback = callExternal p:ParseAndCompileArc( ArcSource ) returns String
            SourcesChanged = true
  
    user Author filledBy sys:PerspectivesSystem$User
      perspective on extern
        props (ModelIdentifier, Version, ArcOK) verbs (Consult)
        props (ArcSource, ArcFeedback, Description) verbs (SetPropertyValue)

        in object state ReadyToCompile
          action RestoreState
            ArcFeedback = "Explicitly restoring state"
          action CompileArc
            delete property ArcFeedback

    user ActiveUser = extern >> binder Versions >> context >> ActiveUser
      perspective on Author
        props (FirstName, LastName) verbs (Consult)
      perspective on extern
        props (Version, ArcSource, DomeinFile)

      screen "Manifest"
        row
          form External

