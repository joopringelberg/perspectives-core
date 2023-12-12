-- CouchdbManagement - Copyright Joop Ringelberg and Cor Baars 2021 - 2022

domain model://perspectives.domains#CouchdbManagement
  use sys for model://perspectives.domains#System
  use cm for model://perspectives.domains#CouchdbManagement
  use acc for model://perspectives.domains#BodiesWithAccounts
  use cdb for model://perspectives.domains#Couchdb
  use util for model://perspectives.domains#Utilities
  use p for model://perspectives.domains#Parsing
  use files for model://perspectives.domains#Files
  use sensor for model://perspectives.domains#Sensor

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
          couchdbapp <- create context CouchdbManagementApp
          indexedcontext <- create role IndexedContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind couchdbapp >> extern to StartContexts in sys:MySystem
          Name = "Couchdb Management App" for couchdbapp >> extern
          bind_ couchdbapp >> extern to indexedcontext
          IndexedContexts$Name = couchdbapp >> indexedName for indexedcontext

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (cm:MyCouchdbApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (cm:MyCouchdbApp >> extern)
      in
        remove context indexedcontext
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  -- The INDEXED context cm:MyCouchdbApp, that is the starting point containing all CouchdbServers.
  -- There is NO PUBLIC PERSPECTIVE on this case.
  -- The end user user (playing Manager) should have a Server Admin account for each CouchdbServer that is added to the App.
  -- The credentials of each such Server Admin go into the CouchdbServer$Admin roles.
  case CouchdbManagementApp
    indexed cm:MyCouchdbApp
    aspect sys:RootContext
    external
      aspect sys:RootContext$External
    
    -- the exit is triggered on removing a role instance of StartContexts
    on exit
      do for Manager
        delete context bound to CouchdbServers
        -- remove this context from PerspectivesSystem$IndexedContexts
        -- remove the model from ModelsInUse

    -- Every user manages his own CouchdbServers.
    -- This manager should be the Server admin of each CouchdbServer,
    -- in order to create databases and members.
    -- Becoming a Couchdb Server Admin should be managed outside Perspectives.
    -- The username should be the PerspectivesSystem$User ID.
    user Manager = sys:Me
      perspective on CouchdbServers
        only (CreateAndFill, Remove, Delete)
        props (Name) verbs (Consult)
        props (Url, CouchdbServers$CouchdbPort, AdminUserName, AdminPassword, Name) verbs (SetPropertyValue)

      -- Manager needs this action so he can set the Url before the CouchdbServer$Visitor tries to publish.
      action CreateServer
        create role CouchdbServers

      -- Manager needs this perspective for others to accept Admins created in state CouchdbServers$NoAdmin.
      perspective on CouchdbServers >> binding >> context >> CouchdbServer$Admin
        only (Create, Fill)
      
      screen "Couchdb Server Administration"
        row
          table CouchdbServers
            -- No restriction on properties, but the Create and CreateAndFill verbs are omitted.
            only (Remove)

      
    -- A new CouchdbServers instance comes complete with a CouchdbServer$Admin role
    -- filled with CouchdbManagementApp$Admin.
    context CouchdbServers (relational) filledBy CouchdbServer
      property Url (mandatory, String)
        pattern = "^https://[^\\/]+\\/$" "An url with the https scheme, ending on a slash"
      property CouchdbPort (mandatory, String)
        pattern = "^\\d{1,5}$" "A port number written as a string, consisting of up to 5 digits, maximally 65535."
      -- The Password and UserName of Manager (and therefore CouchdbServer$Admin) for Url.
      property AdminPassword (mandatory, String)
      property AdminUserName (mandatory, String)

      -- Add credentials to Perspectives State (not to the Couchdb_).
      -- This means that the PDR can now authenticate on behalf of the Admin with Couchdb_.
      state AddCredentials = ((exists Url) and exists AdminPassword) and exists AdminUserName
        on entry
          do for Manager
            callEffect cdb:AddCredentials( Url, AdminUserName, AdminPassword )

        state CreateServer = exists Url -- This might be replaced by 'true'.
          on entry
            do for Manager
              -- Create the databases in CouchdbServer_.
              -- Notice that Pouchdb only creates a database if it does not yet exist.
              -- This allows us to reconnect to a CouchdbServer_ if its CouchdbServer had been lost.
              letA 
                couchdburl <- "http://localhost:" + CouchdbServers$CouchdbPort + "/"
              in 
                callEffect cdb:CreateEntitiesDatabase( Url, "cw_servers_and_repositories" )
                callEffect cdb:MakeDatabaseWriteProtected( Url, "cw_servers_and_repositories" )
                callEffect cdb:MakeDatabasePublic( Url, "cw_servers_and_repositories" )
                -- As the databases are now ready, we can create and publish the CouchdbServer.
                create_ context CouchdbServer bound to origin

      state NoAdmin = (exists binding) and not exists binding >> context >> CouchdbServer$Admin
        on entry
          do for Manager
            -- Tie these credentials to the CouchdbServer$Admin role.
            bind context >> Manager to Admin in binding >> context
            AuthorizedDomain = Url for  binding >> context >> Admin
            Password = AdminPassword for binding >> context >> Admin
            SpecificUserName = AdminUserName for binding >> context >> Admin

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
      property ServerUrl (functional) = binder CouchdbServers >> Url
      property CouchdbPort (functional) = binder CouchdbServers >> CouchdbServers$CouchdbPort
      property Name (String)

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
    -- TODO Hernoem dit naar ServerAdmin.
    user Admin filledBy CouchdbManagementApp$Manager 
      -- As acc:Body$Admin, has full perspective on Accounts.
      -- These properties come from sys:WithCredentials
      -- WithCredentials$UserName - CALCULATED, either the SpecificUserName or the ID of the PerspectivesSystem$User.
      -- The next two properties are set on creating the admin (state CouchdbServerApp$CouchdbServers$NoAdmin)
      -- WithCredentials$SpecificUserName
      -- WithCredentials$Password
      -- WithCredentials$AuthorizedDomain
      aspect acc:Body$Admin

      action CreateRepository
        create role Repositories

      perspective on extern
        props (ServerUrl) verbs (Consult)
        props (Name) verbs (SetPropertyValue, Consult)

      perspective on Repositories
        all roleverbs
        props (Repositories$NameSpace, AdminEndorses, IsPublic, AdminLastName) verbs (Consult)
        props (IsPublic) verbs (SetPropertyValue)
        in object state WithoutExternalDatabase
          props (AdminEndorses) verbs (SetPropertyValue)
        in object state CreateDatabases
          props (IsPublic) verbs (SetPropertyValue)
        in object state NoNameSpace
          props (Repositories$NameSpace) verbs (SetPropertyValue)
        
        -- This is currently not very useful, because a Repositories instance will not enter state WithoutManifests 
        -- when its last Manifest is deleted. Negation by failure breaks on removing instances!
        -- in object state WithoutManifests
        --   action RemoveRepository
        --     remove role origin
      
      screen "Couchdb Server"
        tab "The server"
          row
            form External
              props (ServerUrl, Name) verbs (Consult)
              --props (Name) verbs (SetPropertyValue)
          row
            form Admin
        tab "Repositories"
          row 
            table Repositories
        tab "Accounts"
          row
            table Accounts

    user Accounts (unlinked, relational) filledBy sys:PerspectivesSystem$User
      -- WithCredentials$Password
      -- WithCredentials$SpecificUserName
      -- WithCredentials$AuthorizedDomain
      aspect acc:Body$Accounts
      -- TODO. Is dit nodig? 
      on entry
        do for Admin
          letA
            serverurl <- context >> extern >> ServerUrl
          in
            callEffect cdb:MakeWritingMemberOf( serverurl, "cw_servers_and_repositories", UserName ) -- UserName is the ID of the PerspectivesSystem$User.
      on exit
        do for Admin
          letA
            serverurl <- context >> extern >> ServerUrl
          in
            callEffect cdb:RemoveAsWritingMemberOf( serverurl, "cw_servers_and_repositories", UserName ) -- UserName is the ID of the PerspectivesSystem$User.


    -- The instance of CouchdbServer is published in the cw_servers_and_repositories database.
    -- TODO: als omkering van filtered queries volledig is, beperk dan het perspectief van Visitor tot PublicRepositories.
    public Visitor at extern >> ServerUrl + "cw_servers_and_repositories/" = sys:Me
      perspective on External
        props (Name) verbs (Consult)
      perspective on Repositories
        props (Repositories$NameSpace, IsPublic) verbs (Consult)
      
      -- Perspective on Admin in order to be able to sign up as an Account.
      perspective on Admin
        props (FirstName, LastName) verbs (Consult)
      
      screen "Couchdb Server"
        row
          form "This server" External
        row 
          table "Repositories" Repositories
            props (Repositories$NameSpace) verbs (Consult)


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
            letA
              reponame <- Repositories$NameSpace >> callExternal util:Replace( ".", "_" ) returns String
            in
              create_ context Repository named reponame bound to origin
              -- Copy the namespace to the Repository, but replace dots with underscores.
              NameSpace_ = reponame for origin >> binding

      state CreateDatabases = (exists NameSpace_) and AdminEndorses and exists context >> Admin >> Password
        on entry
          do for Admin
            letA
              baseurl <- context >> extern >> ServerUrl
              couchdburl <- "http://localhost:" + context >> extern >> CouchdbPort + "/"
              readmodels <- "models_" + NameSpace_
              readinstances <- "cw_" + NameSpace_
            in 
              -- models
              callEffect cdb:CreateCouchdbDatabase( baseurl, readmodels )
              callEffect cdb:MakeDatabaseWriteProtected( baseurl, readmodels )
              callEffect cdb:MakeDatabasePublic( baseurl, readmodels )
              -- instances
              callEffect cdb:CreateEntitiesDatabase( baseurl, readinstances )
              callEffect cdb:MakeDatabasePublic( baseurl, readinstances )
              callEffect cdb:MakeDatabaseWriteProtected( baseurl, readinstances )
        on exit
          do for Admin
            letA
              baseurl <- context >> extern >> ServerUrl
              readmodels <- "models_" + NameSpace_
              readinstances <- "cw_" + NameSpace_
            in 
              -- models
              callEffect cdb:DeleteCouchdbDatabase( baseurl, readmodels )
              -- instances
              callEffect cdb:DeleteCouchdbDatabase( baseurl, readinstances )

      -- THIS STATE WILL NOT BE REVISITED WHEN ALL MANIFESTS ARE REMOVED,
      -- because the role instance state is re-evaluated BEFORE the instance is actually thrown away.
      -- state WithoutManifests = not exists binding >> context >> Manifests

      state WithoutExternalDatabase = (not exists NameSpace_) or not AdminEndorses

      state NoNameSpace = not exists Repositories$NameSpace

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

    -- Embedded contexts are not removed automatically with their embedder!
    on exit
      do for Admin
        delete context bound to Manifests

    external
      aspect sys:ManifestCollection$External
        -- NameSpace_
        -- Domain
      -- Only public repositories will be visible to Accounts of CouchdbServers.
      property IsPublic (mandatory, Boolean)
      -- The toplevel domain with at least one subdomain, such as perspectives.domains or professional.joopringelberg.nl.
      property NameSpace (functional) = binder Repositories >> Repositories$NameSpace
      property ReadModels = "models_" + NameSpace_
      property ReadInstances = "cw_" + NameSpace_
      -- The location of the CouchdbServer_. 
      property ServerUrl (functional) = binder Repositories >> context >> extern >> ServerUrl
      property RepositoryUrl = "https://" + NameSpace + "/"
      property AdminLastName = context >> Admin >> LastName

    -- We need the ServerAdmin in this context in order to configure the local Admin and to give Authors write access.
    user ServerAdmin = extern >> binder Repositories >> context >> CouchdbServer$Admin >>= first
      perspective on Admin
        only (Create, Fill, RemoveFiller, Remove)
        props (SpecificUserName, Password) verbs (SetPropertyValue)
        props (FirstName, LastName, Password, UserName) verbs (Consult)
        props (AuthorizedDomain) verbs (SetPropertyValue)

    -- The filler provides autentication on Couchdb_.
    -- Should also be able to give them read access to the repo,
    -- and to retract that again.
    -- NOTE: later, not only CouchdbServer$Admin is allowed to fill this role.
    user Admin filledBy CouchdbServer$Admin
      aspect acc:Body$Admin
        -- WithCredentials$SpecificUserName
        -- We must use the next three properties to provide the autentication details to the PDR for the RepositoryUrl.
        -- WithCredentials$UserName (either the SpecificUserName or the User identifier)
        -- WithCredentials$Password
        -- WithCredentials$AuthorizedDomain
        -- As Admin, has a full perspective on Accounts.

      action CreateManifest
        create role Manifests

      state IsFilled = exists binding
        on entry
          do for ServerAdmin
            -- Only the CouchdbServer$Admin has a Create and Fill perspective on
            -- Repository$Admin. So when this state arises, we can be sure that
            -- the current user is, indeed, a CouchdbServer$Admin.
            -- Hence the PDR will authenticate with his credentials.
            -- Admin adds Manifests to Repository, so must be able to store Repository on behalf of the public Visitor.
            letA
              serverurl <- context >> extern >> ServerUrl
            in
              -- models
              -- callEffect cdb:MakeAdminOfDb( serverurl, context >> extern >> WriteModels, UserName )
              callEffect cdb:MakeAdminOfDb( serverurl, context >> extern >> ReadModels, UserName )
              -- instances
              -- callEffect cdb:MakeAdminOfDb( serverurl, context >> extern >> WriteInstances, UserName )
              callEffect cdb:MakeAdminOfDb( serverurl, context >> extern >> ReadInstances, UserName )
              -- Make authentication details available to the PDR (in fact only useful when the filler of Admin is not CouchdbServer$Admin)
              AuthorizedDomain = context >> extern >> RepositoryUrl
              callEffect cdb:AddCredentials( AuthorizedDomain, UserName, Password)


        on exit
          do for ServerAdmin
            letA
              serverurl <- context >> extern >> ServerUrl
            in
              -- models
              -- callEffect cdb:RemoveAsAdminFromDb( serverurl, context >> extern >> WriteModels, UserName )
              callEffect cdb:RemoveAsAdminFromDb( serverurl, context >> extern >> ReadModels, UserName )
              -- instances
              -- callEffect cdb:RemoveAsAdminFromDb( serverurl, context >> extern >> WriteInstances, UserName )
              callEffect cdb:RemoveAsAdminFromDb( serverurl, context >> extern >> ReadInstances, UserName )

      on exit 
        notify "You are no longer the administrator of the repository { context >> extern >> NameSpace_ }."

      
      perspective on External
        props (IsPublic, NameSpace_, RepositoryUrl) verbs (Consult)

        action CompileRepositoryModels
          callEffect p:CompileRepositoryModels( RepositoryUrl + ReadModels, RepositoryUrl + ReadInstances )

      -- The desogm pattern for nested public contexts requires that Admin has write access
      -- to both the cw_servers_and_repositories and to the Repository database.
      perspective on Manifests
        only (Create, Fill, Delete, Remove)
        props (Description, LocalModelName) verbs (Consult)
        in object state NoLocalModelName
          props (LocalModelName) verbs (SetPropertyValue)
      
      perspective on Manifests >> binding >> context >> Author
        only (Create, Fill)
      
      -- Moet in staat zijn om een instantie toe te voegen aan Accounts.
      -- perspective on Accounts

      screen "Repository"
        tab "This repository"
          row 
            form "Repository information" External
          row
            form "Administrator" Admin
        tab "Manifests"
          row
            table Manifests
      
    user Authors (relational) filledBy CouchdbServer$Accounts
      state Filled = exists binding
        on entry
          do for ServerAdmin
            letA
              serverurl <- context >> extern >> ServerUrl
            in
              callEffect cdb:MakeWritingMemberOf( serverurl, context >> extern >> ReadModels, UserName ) -- UserName is the ID of the PerspectivesSystem$User.
              callEffect cdb:MakeWritingMemberOf( serverurl, context >> extern >> ReadInstances, UserName ) -- UserName is the ID of the PerspectivesSystem$User.
        on exit
          do for ServerAdmin
            letA
              serverurl <- context >> extern >> ServerUrl
            in
              callEffect cdb:RemoveAsWritingMemberOf( serverurl, context >> extern >> ReadModels, UserName ) -- UserName is the ID of the PerspectivesSystem$User.
              callEffect cdb:RemoveAsWritingMemberOf( serverurl, context >> extern >> ReadInstances, UserName ) -- UserName is the ID of the PerspectivesSystem$User.


    
    -- Levert een pattern match failure in Perspectives.Representation.Perspective (line 220, column 29 - line 220, column 51): CP
    user Accounts (relational, unlinked) filledBy CouchdbServer$Accounts, CouchdbServer$Admin

    -- The instances of Repository are published in the cw_servers_and_repositories database.
    public Visitor at extern >> ServerUrl + "cw_servers_and_repositories/" = sys:Me
      perspective on Manifests
        props (LocalModelName) verbs (Consult)
      perspective on extern
        props (NameSpace_, NameSpace) verbs (Consult)

      screen "Repository"
        row
          form "Repository information" External
            props (NameSpace) verbs (Consult)
        row
          table "Manifests" Manifests


    -- This role is in the public Visitor perspective. These are all models that
    -- are stored in this Repository.
    context Manifests (relational) filledBy ModelManifest
      aspect sys:ManifestCollection$Manifests
      -- LocalModelName
      state NoLocalModelName = not exists LocalModelName
      state ReadyToMake = (exists LocalModelName) and not exists binding
        on entry
          do for Admin
            letA 
              manifestname <- (context >> extern >> NameSpace_ + "-" + LocalModelName)
            in
              -- As the PDR derives this name from the modelURI, we have to name the ModelManifest with its LocalModelName.
              create_ context ModelManifest named manifestname bound to origin
              bind currentactor to Author in origin >> binding >> context
              -- dit werkt.
              DomeinFileName = manifestname + ".json" for origin >> binding

  case ModelManifest 
    aspect sys:ModelManifest

    -- If a Version was recommended but is no longer so, the action in this state
    -- will set the RecommendedVersion.
    state NoRecommendations = not exists filter Versions with IsRecommended
      on entry
        do for Author
          VersionToInstall = extern >> HighestVersion for External

    external
      aspect sys:ModelManifest$External
      -- Description
      -- IsLibrary
      -- DomeinFileName
      -- Notice that we have to register the LocalModelName on the filled context role in the collection,
      -- so we can create ModelManifest with a user-defined name. 
      -- The Model URI (the 'logical name' of the model), e.g. model://perspectives.domains#System.
      property ModelURI (functional) = "model://" + context >> Repository >> Repository$External$NameSpace + "#" + binder Manifests >> LocalModelName >>= first
      -- The location of the CouchdbServer_. 
      property ServerUrl (functional) = binder Manifests >> context >> extern >> ServerUrl
      -- The location where we publish ModelManifest and its versions.
      property PublicUrl = ServerUrl + context >> Repository >> ReadInstances + "/"
      -- The highest version number
      property HighestVersion = context >> Versions >> Versions$Version >>= maximum
      -- The version recommended by the Author
      property RecommendedVersion = (filter context >> Versions with IsRecommended) >> Versions$Version
      -- The version to install. It depends on a calculation, but we have to store it explicitly 
      -- so we can even retrieve it on system startup.
      -- This property must be published.
      -- PDRDEPENDENCY
      property VersionToInstall (String)
    
    -- Embedded contexts are not removed automatically with their embedder!
    on exit
      do for Author
        delete context bound to Versions
    
    -- The external role of the Repository.
    context Repository = extern >> binder Manifests >> context >> extern >>= first

    user Author filledBy Repository$Authors, Repository$Admin
      perspective on extern
        props (Description, IsLibrary, VersionToInstall) verbs (SetPropertyValue)
      perspective on Versions
        only (Create, Fill, Remove, CreateAndFill)
        props (Versions$Version, Description) verbs (SetPropertyValue)
      perspective on Versions >> binding >> context >> Author
        only (Fill, Create)
        props (FirstName, LastName) verbs (Consult)
      action CreateVersion
        create role Versions
      
      screen "Model Manifest"
        row 
          form "This Manifest" External
        row
          table "Available Versions" Versions
            only (Remove)
    
    -- A public version of ModelManifest is available in the database cw_<NameSpace>.
    public Visitor at extern >> PublicUrl = sys:Me
      perspective on extern
        props (Description, IsLibrary, VersionToInstall, DomeinFileName) verbs (Consult)
      -- NOTA BENE: betekent dit niet dat instanties van ModelsInUse gepubliceerd worden?
      perspective on sys:MySystem >> ModelsInUse
        only (Fill, Remove)
      perspective on Versions
        props (Versions$Version, Description, VersionedModelURI, VersionedModelManifest$External$DomeinFileName) verbs (Consult)
        action StartUsing
          -- This method also adds an instance of ModelsInUse and adds the VersionedModelURI to property ModelToRemove.
          -- It also sets InstalledPatch and InstalledBuild.
          callEffect cdb:AddModelToLocalStore( VersionedModelURI )
          
        action UpdateModel
          letA
            -- DomeinFileName without version of the origin.
            dfilename <- context >> extern >> DomeinFileName
            -- Notice that because Versions is a published role, there are no backlinks to roles it fills.
            basicmodel <- (filter sys:MySystem >> ModelsInUse with VersionedModelManifest$External$DomeinFileName == dfilename) >>= first
          in 
            callEffect cdb:UpdateModel( VersionedModelURI, false )
            ModelToRemove = VersionedModelURI for basicmodel
            bind_ origin >> binding to basicmodel
          -- notify Visitor
          --   "You updated to version {VersionedModelURI}."
        -- Update imported models first.
        -- NOTE: the registration of imported models is not updated! I.e. their ModelsInUse roles will still be bound to old versions.
        action UpdateModelWithDependencies
          callEffect cdb:UpdateModel( VersionedModelURI, true )
      
      screen "Model Manifest"
        row 
          form "This Manifest" External
            props (Description, VersionToInstall) verbs (Consult)
        row
          table "Model versions" Versions

    -- In order to add this model to one's installation, one should become an ActiveUser of the Manifest.
    -- BUT HOW?
    user ActiveUser filledBy Repository$Accounts
      perspective on Versions
        props (Versions$Version, Description) verbs (Consult)

    context Versions (relational) filledBy VersionedModelManifest
      aspect sys:ModelManifest$Versions
      -- Version
      -- VersionedLocalModelName
      state ReadyToMake = (exists Versions$Version) and not exists binding
        on entry
          do for Author
            letA
              v <- (context >> extern >> RecommendedVersion) orElse (context >> extern >> HighestVersion)
              versionname <- context >> extern >> binder Manifests >> context >> extern >> NameSpace_ >>= first + "-" + VersionedLocalModelName
            in
              -- As the PDR derives this name from the modelURI, we have to name the ModelManifest with its LocalModelName.
              create_ context VersionedModelManifest named versionname bound to origin
              bind currentactor to VersionedModelManifest$Author in origin >> binding >> context
              -- NOTE that we conceivably might add a version with a lower number than the highest.
              VersionToInstall = v for context >> extern
              -- dit werkt
              Patch = 0
              Build = 0

  case VersionedModelManifest
    aspect sys:VersionedModelManifest
    aspect sys:ContextWithNotification
    state UploadToRepository = extern >> (ArcOK and SourcesChanged)
      on entry
        do for Author
          callEffect p:UploadToRepository( extern >> VersionedModelManifest$External$VersionedModelURI, 
            callExternal util:ReplaceR( "bind publicrole.*in sys:MySystem", "", extern >> ArcSource ) returns String)
          SourcesChanged = false for extern
          LastChangeDT = (callExternal sensor:ReadSensor( "clock", "now" ) returns DateTime) for extern
          Build = extern >> Build + 1 for extern
        notify Author
          "Version {extern >> External$Version} has been uploaded to the repository for { extern >> binder Versions >> context >> Repository >> NameSpace >>= first}."

    external
      aspect sys:VersionedModelManifest$External
      -- VersionedModelManifest$External$Description
      -- VersionedModelManifest$External$DomeinFileName
      -- VersionedModelManifest$External$Version
      -- The Version property is registered on ModelManifest$Versions so we can use it to create a DNS URN for it (it must be a public resource)
      -- PDRDEPENDENCY
      property ModelURI (functional) = binder Versions >> context >> extern >> ModelManifest$External$ModelURI
      property VersionedModelURI = VersionedModelManifest$External$ModelURI + "@" + External$Version
      property ArcFile (File)
        pattern = "text/arc" "Only .arc files (Perspectives Language source files) are allowed, so use `text//arc."
      property ArcSource = callExternal files:FileText( ArcFile ) returns String
      property ArcFeedback (String)
        minLength = 81
      property ArcOK = ArcFeedback matches regexp "^OK"
      property SourcesChanged (Boolean)
      -- property DomeinFile (File)
      property PublicUrl (functional) = binder Versions >> context >> extern >> PublicUrl
      -- Only one VersionedModelManifest can be the recommended version at a time.
      property IsRecommended (Boolean)
      -- The (Javascript) DateTime value of the last upload.
      property LastChangeDT (DateTime)
      -- The readable date and time of the last upload.
      property LastUpload = callExternal util:FormatDateTime( LastChangeDT, "nl-NL", "{\"dateStyle\": \"short\", \"timeStyle\": \"short\"}" ) returns String

      state BecomesRecommended = IsRecommended
        on entry
          do for Author
            letA
              previousversion <- binder Versions >> context >> extern >> VersionToInstall >>= first
            in
              -- no other version can be recommended
              IsRecommended = false for filter binder Versions >> context >> Versions with Versions$Version == previousversion
              -- Set the version to download.
              VersionToInstall = Version for binder Versions >> context >> extern
      
      on exit
        do for Author
          -- Delete the DomeinFile.
          callEffect p:RemoveFromRepository( VersionedModelManifest$External$VersionedModelURI )

      state ReadyToCompile = (exists External$Version) and (exists ArcSource)
      
      state CompileAutomatically = (exists ArcSource) and callExternal sensor:ReadSensor("clock", "now") returns DateTime > LastChangeDT
        on entry
          do for Author
            delete property ArcFeedback

      state ProcessArc = (exists ArcSource) and not exists ArcFeedback
        on entry
          do for Author
            delete property SourcesChanged
            ArcFeedback = callExternal p:ParseAndCompileArc( ArcSource ) returns String
            SourcesChanged = true
    

    user Author filledBy cm:ModelManifest$Author
      perspective on extern
        props (DomeinFileName, Version, ArcOK, ArcSource, LastUpload) verbs (Consult)
        props (ArcFile, ArcFeedback, Description, IsRecommended, Build, Patch) verbs (SetPropertyValue)

        in object state ReadyToCompile
          action RestoreState
            ArcFeedback = "Explicitly restoring state"
          action CompileArc
            delete property ArcFeedback
      perspective on Manifest
        props (VersionToInstall) verbs (SetPropertyValue)
    
    public Visitor at (extern >> PublicUrl) = sys:Me
      perspective on extern
        props (Version, Description, IsRecommended, Patch, Build) verbs (Consult) -- ModelURI geeft een probleem. Probeer VersionedModelManifest$External$ModelURI.
      perspective on Manifest
        props (ModelURI) verbs (Consult) 
      screen "Model version"
        row
          form "This version" External
        row
          form "All versions" Manifest 

    context Manifest = extern >> binder Versions >> context >> extern >>= first


    user ActiveUser = extern >> binder Versions >> context >> ActiveUser
      perspective on Author
        props (FirstName, LastName) verbs (Consult)
      perspective on extern
        props (Version) verbs (Consult)

      screen "Manifest"
        row
          form External
