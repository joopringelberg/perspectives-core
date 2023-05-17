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
  -- There is NO PUBLIC PERSPECTIVE on this case.
  case CouchdbManagementApp
    indexed cm:MyCouchdbApp
    aspect sys:RootContext
    external
      aspect sys:RootContext$External
    
    on exit
      do for Manager
        delete context bound to CouchdbServers

    -- Every user manages his own CouchdbServers.
    -- This manager should be the Server admin of each CouchdbServer,
    -- in order to create databases and members.
    -- Becoming a Couchdb Server Admin should be managed outside Perspectives.
    user Manager = sys:Me
      perspective on CouchdbServers
        only (CreateAndFill, Remove, Delete)
        props (Name, Url) verbs (SetPropertyValue)

      -- Manager needs this action so he can set the Url before the CouchdbServer$Visitor tries to publish.
      action CreateServer
        create role CouchdbServers

      -- Manager needs this perspective for others to accept Admins created in state CouchdbServers$NoAdmin.
      perspective on CouchdbServers >> binding >> context >> CouchdbServer$Admin
        only (Create, Fill)
      
    -- A new CouchdbServers instance comes complete with a CouchdbServer$Admin role
    -- filled with CouchdbManagementApp$Admin.
    context CouchdbServers (relational) filledBy CouchdbServer
      property Url (mandatory, String)
        pattern = "^https://[^\\/]+\\/$" "An url with the https scheme, ending on a slash"

      state CreateServer = exists Url
        on entry
          do for Manager
            create_ context CouchdbServer bound to origin

      state NoAdmin = (exists binding) and not exists binding >> context >> CouchdbServer$Admin
        on entry
          do for Manager
            bind context >> Manager to Admin in binding >> context

  -------------------------------------------------------------------------------
  ---- COUCHDBSERVER
  -------------------------------------------------------------------------------
  -- For each CouchdbServer_ (=remote server with a Couchdb installation), there should be only one CouchdbServer instance.
  -- This context has a PUBLIC USER Visitor that causes a version of the context to be published.
  -- Visitor can see the Repositories and their descriptions, and sign up.
  -- To sign up, he should be able to see the admin.
  
  -- This context implements the BodyWithAccounts pattern.
  -- NOTE: a PerspectivesSystem$User should only fill either Admin, or Accounts!
  -- The PDR looks for credentials in either role and should find them just once.
  case CouchdbServer
    aspect acc:Body
    --storage public
    external
      -- The location of the CouchdbServer_. 
      property ServerUrl = binder CouchdbServers >> Url
      property Name (String)
      property HasDatabase (Boolean)

      -- This covers the case we get a CouchdbServer that we did not create ourselves.
      -- If we do not refer to my indexed version of the CouchdbApp, this condition will fail because another user
      -- will have shared his App, too!
      state AddReceivedServer = not exists filter cm:MyCouchdbApp >> CouchdbServers with filledBy origin
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

      -- Add credentials to Perspectives State (not to the Couchdb_).
      -- This means that the PDR can now authenticate on behalf of the Admin with Couchdb_.
      state AddCredentials = (exists ServerUrl) and exists context >> Admin >> Password
        on entry
          do for Admin
            callEffect cdb:AddCredentials( ServerUrl, context >> Admin >> Password)
        
        state CreateDatabases = not HasDatabase
          on entry
            do for Admin
              -- Create the databases in CouchdbServer_.
              -- Notice that Pouchdb only creates a database if it does not yet exist.
              -- This allows us to reconnect to a CouchdbServer_ if its CouchdbServer had been lost.
              callEffect cdb:CreateCouchdbDatabase( ServerUrl, "cw_servers_and_repositories" )
              callEffect cdb:CreateCouchdbDatabase( ServerUrl, "cw_servers_and_repositories_write" )
              callEffect cdb:ReplicateContinuously( ServerUrl, "cw_servers_and_repositories_write", "cw_servers_and_repositories" )
              callEffect cdb:MakeDatabasePublic( ServerUrl, "cw_servers_and_repositories" )
              HasDatabase = true

      -- Exiting CouchdbServer$External
      on exit
        do for Admin
          callEffect cdb:DeleteCouchdbDatabase( ServerUrl, "cw_servers_and_repositories" )
          callEffect cdb:DeleteCouchdbDatabase( ServerUrl, "cw_servers_and_repositories_write" )
          callEffect cdb:EndReplication( ServerUrl, "cw_servers_and_repositories_write", "cw_servers_and_repositories" )
    
    -- Exiting CouchdbServer
    on exit
      do for Admin
        delete context bound to Repositories

    -- This role should be in public space insofar that e.g. acc:Body$Guest should be able to see it.
    -- Admin in Couchdb of a particular server.
    user Admin filledBy CouchdbManagementApp$Manager 
      -- As acc:Body$Admin, has full perspective on Accounts.
      aspect acc:Body$Admin

      action CreateRepository
        create role Repositories

      perspective on extern
        props (ServerUrl, Name) verbs (Consult)

      perspective on Repositories
        all roleverbs
        props (AdminEndorses, IsPublic, AdminLastName) verbs (Consult)
        props (IsPublic, Repositories$NameSpace) verbs (SetPropertyValue)
        in object state WithoutExternalDatabase
          props (AdminEndorses) verbs (SetPropertyValue)
        in object state CreateDatabases
          props (IsPublic) verbs (SetPropertyValue)
      
      screen "Couchdb Server"
        tab "The server"
          row
            form External
              props (ServerUrl, Name) verbs (Consult)
          row
            form Admin
        tab "Repositories"
          row 
            table Repositories

    user Accounts (unlinked, relational) filledBy sys:PerspectivesSystem$User
      aspect acc:Body$Accounts

    -- The instance of CouchdbServer is published in the cw_servers_and_repositories database.
    public Visitor at extern >> ServerUrl + "cw_servers_and_repositories/" = sys:Me
      perspective on PublicRepositories
        props (Repositories$NameSpace) verbs (Consult)
      
      -- Perspective on Admin in order to be able to sign up as an Account.
      perspective on Admin
        props (FirstName, LastName) verbs (Consult)

    context PublicRepositories = filter Repositories with IsPublic

    -- A Repositories instance comes complete with an (empty) Admin role.
    -- Moreover, as a side effect, both a read- and write database are created
    -- in Couchdb and the write database replicates to the read database.
    context Repositories (relational) filledBy Repository
      property AdminEndorses (Boolean)
      -- The top level domain preceded by at least one subdomain, e.g. "perspectives.domains".
      property NameSpace (String)
        -- Only lowercase characters (a-z), digits (0-9), and any of the characters _, $, (, ), +, -, and / are allowed. Must begin with a letter.
        -- However, the parser refuses (, ) and /.
        -- ^[a-z][a-z0-9_$()+/-]*$ according to https://docs.couchdb.org/en/3.2.0/api/database/common.html and https://localhost:6984//_utils/docs/api/database/common.html#specifying-the-document-id
        pattern = "[a-z]([a-z]|[0-9]|[\\._$+-])*" "Only lowercase characters (a-z), digits (0-9), and any of the characters ., _, $, + and - are allowed. Must begin with a letter."

      state CreateRepository = exists Repositories$NameSpace
        on entry
          do for Admin
            create_ context Repository bound to origin
            -- Copy the namespace to the Repository, but replace dots with underscores.
            NameSpace_ = Repositories$NameSpace >> callExternal util:Replace( ".", "_" ) returns String for origin >> binding

      state CreateDatabases = (exists NameSpace_) and AdminEndorses
        on entry
          do for Admin
            letA
              baseurl <- context >> extern >> ServerUrl
              readmodels <- "models_" + NameSpace_
              writemodels <- "models_" + NameSpace_ + "_write"
              readinstances <- "cw_" + NameSpace_
              writeinstances <- "cw_" + NameSpace_ + "_write"
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
              baseurl <- context >> extern >> ServerUrl
              readmodels <- "models_" + NameSpace_
              writemodels <- "models_" + NameSpace_ + "_write"
              readinstances <- "cw_" + NameSpace_
              writeinstances <- "cw_" + NameSpace_ + "_write"
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

      state WithoutExternalDatabase = (not exists NameSpace_) or not AdminEndorses
        -- Ad Admin may exist already if the Repository is created by Accounts.
        -- state NoAdmin = AdminEndorses and not exists binding >> context >> Repository$Admin
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

    -- on exit
    --   do for Admin
    --     delete context bound to Manifests

    external
      -- Only public repositories will be visible to Accounts of CouchdbServers.
      property IsPublic (mandatory, Boolean)
      -- This is the namespace of the models in this repository, but dots are replaced by underscores.
      -- It is computed from Repositories$NameSpace on creating the repository.
      property NameSpace_ (mandatory, String)
      property NameSpace = binder Repositories >> Repositories$NameSpace
      property ReadModels = "models_" + NameSpace_
      property WriteModels = "models_" + NameSpace_ + "_write"
      property ReadInstances = "cw_" + NameSpace_
      property WriteInstances = "cw_" + NameSpace_ + "_write"
      -- The location of the CouchdbServer_. 
      property ServerUrl = binder Repositories >> context >> extern >> ServerUrl
      property AdminLastName = context >> Admin >> LastName

    -- We need the ServerAdmin in this context in order to configure the local Admin.
    user ServerAdmin = extern >> binder Repositories >> context >> CouchdbServer$Admin
      perspective on Admin
        only (Create, Fill, RemoveFiller, Remove)
        props (FirstName, LastName) verbs (Consult)

    user Admin filledBy CouchdbServer$Admin
      -- As Admin, has a full perspective on Accounts.
      -- Should also be able to give them read access to the repo,
      -- and to retract that again.
      aspect acc:Body$Admin

      action CreateManifest
        create role Manifests

      state IsFilled = exists binding
        on entry
          do for ServerAdmin
            -- Only the CouchdbServer$Admin has a Create and Fill perspective on
            -- Repository$Admin. So when this state arises, we can be sure that
            -- the current user is, indeed, a CouchdbServer$Admin.
            -- Hence the PDR will authenticate with Server Admin credentials.
            -- Admin adds Manifests to Repository, so must be able to store Repository on behalf of the public Visitor.
            letA
              serverurl <- context >> extern >> ServerUrl
            in
              callEffect cdb:MakeMemberOf( serverurl, "cw_servers_and_repositories", UserName )
              -- models
              callEffect cdb:MakeAdminOfDb( serverurl, context >> extern >> WriteModels, UserName )
              callEffect cdb:MakeAdminOfDb( serverurl, context >> extern >> ReadModels, UserName )
              -- instances
              callEffect cdb:MakeAdminOfDb( serverurl, context >> extern >> WriteInstances, UserName )
              callEffect cdb:MakeAdminOfDb( serverurl, context >> extern >> ReadInstances, UserName )

        on exit
          do for ServerAdmin
            letA
              serverurl <- context >> extern >> ServerUrl
            in
              -- models
              callEffect cdb:RemoveAsMemberOf( serverurl, "cw_servers_and_repositories", UserName )
              callEffect cdb:RemoveAsAdminFromDb( serverurl, context >> extern >> WriteModels, UserName )
              callEffect cdb:RemoveAsAdminFromDb( serverurl, context >> extern >> ReadModels, UserName )
              -- instances
              callEffect cdb:RemoveAsAdminFromDb( serverurl, context >> extern >> WriteInstances, UserName )
              callEffect cdb:RemoveAsAdminFromDb( serverurl, context >> extern >> ReadInstances, UserName )
              remove role origin
      on exit 
        notify "You are no longer the administrator of the repository { context >> extern >> NameSpace_ }."

      
      perspective on External
        props (IsPublic, NameSpace_) verbs (Consult)

      -- The desogm pattern for nested public contexts requires that Admin has write access
      -- to both the cw_servers_and_repositories and to the Repository database.
      perspective on Manifests
        only (Create, Fill, Delete, Remove)
        props (Manifests$LocalModelName) verbs (SetPropertyValue)
        props (Description) verbs (Consult)
      
      perspective on Manifests >> binding >> context >> Author
        only (Create, Fill)

      screen "Repository"
        tab "This repository"
          row 
            form External
          row
            form Admin
        tab "Manifests"
          row
            table Manifests
      
    user Authors (relational) filledBy CouchdbServer$Admin

    -- The instances of Repository are published in the cw_servers_and_repositories database.
    public Visitor at extern >> ServerUrl + "cw_servers_and_repositories/" = sys:Me
      perspective on Manifests
        props (Manifests$LocalModelName) verbs (Consult)
      perspective on extern
        props (NameSpace_) verbs (Consult)

    -- This role is in the public Visitor perspective. These are all models that
    -- are stored in this Repository.
    context Manifests filledBy ModelManifest
      aspect sys:ManifestCollection$Manifests
      -- Manifests$LocalModelName
      state ReadyToMake = (exists Manifests$LocalModelName) and not exists binding
        on entry
          do for Admin
            -- As the PDR derives this name from the modelURI, we have to name the ModelManifest with its LocalModelName.
            create_ context ModelManifest named Manifests$LocalModelName bound to origin
            bind currentactor to Author in origin >> binding >> context

  case ModelManifest
    aspect sys:ModelManifest

    external
      aspect sys:ModelManifest$External
      -- Notice that we have to register the LocalModelName on the filled context role in the collection,
      -- so we can create ModelManifest with a user-defined name. 
      -- Here we repeat that name so we can conveniently use it within ModelManifest.
      property LocalModelName = binder Manifests >> Manifests$LocalModelName
      -- The Model URI (the 'logical name' of the model), e.g. model://perspectives.domains#System.
      property ModelURI = "model://" + context >> Repository >> Repository$External$NameSpace + "#" + LocalModelName
      -- The location of the CouchdbServer_. 
      property ServerUrl = binder Manifests >> context >> extern >> ServerUrl
    
    -- The external role of the Repository.
    context Repository = extern >> binder Manifests >> context >> extern

    user Author filledBy Repository$Authors, Repository$Admin
      perspective on extern
        props (LocalModelName) verbs (Consult)
        props (Description) verbs (SetPropertyValue)
      perspective on Versions
        only (Create, Fill, Remove)
        props (Versions$Version, VersionedModelManifest$External$Description) verbs (SetPropertyValue)
      perspective on Versions >> binding >> context >> Author
        only (Fill, Create)
      action CreateVersion
        create role Versions
    
    -- A public version of ModelManifest is available in the database cw_<NameSpace>.
    public Visitor at extern >> ServerUrl + Repository >> ReadInstances = sys:Me

    context Versions (relational) filledBy VersionedModelManifest
      aspect sys:ModelManifest$Versions
      -- Versions$Version (mandatory, String)
      -- Versions$LocalModelName = context >> extern >> binder Manifests >> LocalModelName + "@" + Version
      -- Versions$DomeinFileName (mandatory, String)
      state ReadyToMake = (exists Versions$Version) and not exists binding
        on entry
          do for Author
            -- As the PDR derives this name from the modelURI, we have to name the ModelManifest with its LocalModelName.
            create_ context VersionedModelManifest named LocalModelName bound to origin
            DomeinFileName = (context >> Repository >> NameSpace_ + "-" + Versions$LocalModelName + ".json") for origin
            bind currentactor to VersionedModelManifest$Author in origin >> binding >> context

  case VersionedModelManifest
    aspect sys:VersionedModelManifest
    aspect sys:ContextWithNotification
    state UploadToRepository = extern >> (ArcOK and SourcesChanged)
      on entry
        do for Author
          callEffect p:UploadToRepository2( extern >> ArcSource )
          SourcesChanged = false for extern
        notify Author
          "Version {extern >> Version} has been uploaded to the repository for { extern >> binder Versions >> context >> Repository >> NameSpace}."

    external
      aspect sys:VersionedModelManifest$External
      -- VersionedModelManifest$Description
      -- VersionedModelManifest$DomeinFileName
      -- The Version property is registered on ModelManifest$Versions so we can use it to create a DNS URN for it (it must be a public resource)
      property Version = binder Versions >> Versions$Version
      property ModelURI = binder Versions >> context >> extern >> ModelURI + VersionedModelManifest$External$Version
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
          callEffect p:RemoveFromRepository( ModelURI )

      state ReadyToCompile = ((exists ArcSource) and exists External$Version)

      state ProcessArc = (exists ArcSource) and not exists ArcFeedback
        on entry
          do for Author
            ArcFeedback = callExternal p:ParseAndCompileArc( ArcSource ) returns String
            SourcesChanged = true
  
    user Author filledBy cm:ModelManifest$Author
      perspective on extern
        props (DomeinFileName, Version, ArcOK) verbs (Consult)
        props (ArcSource, ArcFeedback, Description) verbs (SetPropertyValue)

        in object state ReadyToCompile
          action RestoreState
            ArcFeedback = "Explicitly restoring state"
          action CompileArc
            delete property ArcFeedback

