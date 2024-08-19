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
          start <- create role StartContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind_ couchdbapp >> extern to start
          Name = "Couchdb Management App" for start

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
    user Manager = sys:SocialMe
      perspective on CouchdbServers
        only (CreateAndFill, Remove, Delete, Create, Fill)
        props (Name) verbs (Consult)
        props (Url, CouchdbServers$CouchdbPort, AdminUserName, AdminPassword, Name) verbs (SetPropertyValue)

      -- Manager needs this action so he can set the Url before the CouchdbServer$Visitor tries to publish.
      action CreateServer
        create role CouchdbServers

      -- Manager needs this perspective for others to accept Admins created in state CouchdbServers$NoAdmin.
      perspective on CouchdbServers >> binding >> context >> CouchdbServer$Admin
        only (Create, Fill)
        props (AuthorizedDomain, Password, SpecificUserName) verbs (SetPropertyValue)
      
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
      -- These credentials are only effective in the current session. 
      -- They are added (persistently) to CouchdbServer$Admin as soon as that role is filled. From then on,
      -- this installation can authenticate with the server at Url.
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
            -- Manager 
            -- Tie these credentials to the CouchdbServer$Admin role. As this has aspect WithCredentials,
            -- from now on this set of credentials will be added to Perspectives State on startup.
            bind context >> Manager to Admin in binding >> context
            AuthorizedDomain = Url for  binding >> context >> Admin     -- zet dit wel voor een nieuwe CouchdbServer$Admin. Password en eventueel SpecificUserName moet hij zelf zetten.
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
    external
      -- The location of the CouchdbServer_. 
      property ServerUrl (functional) = binder CouchdbServers >> Url
      property CouchdbPort (functional) = binder CouchdbServers >> CouchdbServers$CouchdbPort
      property Name (String)

      -- This covers the case we get a CouchdbServer that we did not create ourselves.
      -- If we do not refer to my indexed version of the CouchdbApp, this condition will fail because another user
      -- will have shared his App, too!
      state AddReceivedServer = not exists filter cm:MyCouchdbApp >> CouchdbServers with filledBy origin
        perspective of Admin 
          perspective on extern >> binder CouchdbServers
            only (Create, Fill)
        perspective of Accounts
          perspective on extern >> binder CouchdbServers
            only (Create, Fill)
        on entry
          do for Admin
            bind origin to CouchdbServers in cm:MyCouchdbApp
          do for Accounts
            bind origin to CouchdbServers in cm:MyCouchdbApp
          notify Admin
            "You now have an account with CouchdbServer {Name}"
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

    -- Admin in Couchdb of a particular server.
    -- This role requires credentials for the ServerUrl, because it updates CouchdbServer and creates and updates Repositories and Accounts.
    -- It requires write access to cw_servers_and_repositories.
    -- TODO Hernoem dit naar ServerAdmin.
    user Admin (relational) filledBy CouchdbManagementApp$Manager 
      -- As acc:Body$Admin, has full perspective on Accounts.
      -- These properties come from sys:WithCredentials
      -- WithCredentials$UserName - CALCULATED, either the SpecificUserName or the ID of the PerspectivesSystem$User.
      -- The next two properties are set on creating the admin (state CouchdbServerApp$CouchdbServers$NoAdmin)
      -- WithCredentials$SpecificUserName
      -- WithCredentials$Password
      -- WithCredentials$AuthorizedDomain
      aspect acc:Body$Admin
      aspect sys:ContextWithNotification$NotifiedUser

      action CreateRepository
        create role Repositories

      perspective on External
        props (ServerUrl, CouchdbPort, ServerUrl) verbs (Consult)
        props (Name) verbs (SetPropertyValue, Consult)

      perspective on Repositories
        all roleverbs
        props (Repositories$NameSpace, AdminEndorses, IsPublic, AdminLastName) verbs (Consult)
        props (IsPublic, NameSpace_, HasDatabases) verbs (SetPropertyValue)
        in object state WithoutExternalDatabase
          props (AdminEndorses) verbs (SetPropertyValue)
        in object state CreateDatabases
          props (IsPublic) verbs (SetPropertyValue)
        in object state NoNameSpace
          props (Repositories$NameSpace) verbs (SetPropertyValue)
        
      perspective on Accounts
        all roleverbs
        props (FirstName, LastName) verbs (Consult)
        props (Password, AuthorizedDomain) verbs (SetPropertyValue)
      
      perspective on BespokeDatabases
        all roleverbs
        props (OwnerName, Description) verbs (Consult)
        props (Endorsed) verbs (SetPropertyValue)
      
      perspective on Admin
        props (FirstName, UserName) verbs (Consult)
        props (SpecificUserName, Password) verbs (SetPropertyValue)
        all roleverbs

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
            table "Admins" Admin
        tab "Repositories" default
          row 
            table Repositories
        tab "Accounts"
          row
            table Accounts
        tab "Bespoke databases"
          row
            table BespokeDatabases

    -- This role requires credentials for the ServerUrl, because it can remove itself.
    -- It requires write access to cw_servers_and_repositories.
    user Accounts (unlinked, relational) filledBy sys:TheWorld$PerspectivesUsers
      -- WithCredentials$Password
      -- WithCredentials$SpecificUserName
      -- WithCredentials$AuthorizedDomain
      aspect acc:Body$Accounts
      aspect sys:ContextWithNotification$NotifiedUser
      state Filled = exists binding
        on entry
          do for Admin
            letA
              serverurl <- context >> extern >> ServerUrl
            in
              -- This will set the properties of the object in transition, i.e. the Accounts instance; NOT the Admin instance!
              Password = callExternal util:GenSym() returns String
              AuthorizedDomain = serverurl
              -- NOTICE: we do not check whether UserName is already a registered user of the Couchdb installation at serverurl.
              callEffect cdb:CreateUser( serverurl, UserName, Password )
              callEffect cdb:MakeWritingMemberOf( serverurl, "cw_servers_and_repositories", UserName ) -- UserName is the ID of the PerspectivesSystem$User.
          do for Accounts
              callEffect cdb:AddCredentials( AuthorizedDomain, UserName, Password)
        state ThisIsMe = origin filledBy sys:SocialMe >> binding
          -- Accounts needs this perspective to be able to add the CouchdbServer to his cm:MyCouchdbApp!
          perspective of Accounts 
            perspective on extern >> binder CouchdbServers
              only (Create, Fill)
          on entry
            -- When a peer assigns the current user to the Accounts role,
            -- we make sure that the current user has the CouchdbServer bound
            -- in the CouchdbManagementApp.
            do for Accounts
              bind context >> extern to CouchdbServers in cm:MyCouchdbApp
            notify Accounts
              "You now have an account with CouchdbServer {context >> extern >> Name}"

      on exit
        do for Admin
          letA
            serverurl <- context >> extern >> ServerUrl
          in
            callEffect cdb:RemoveAsWritingMemberOf( serverurl, "cw_servers_and_repositories", UserName ) -- UserName is the ID of the PerspectivesSystem$User.
            callEffect cdb:DeleteUser( serverurl, UserName, Password )
      
      perspective on Accounts
        selfonly
        -- Add FirstName to make sure the filler of this role is sent to the peer himself!
        props (FirstName, Password, AuthorizedDomain) verbs (Consult)

      perspective on External
        props (ServerUrl, Name) verbs (Consult)

      perspective on Admin
        props (FirstName, LastName) verbs (Consult)
      
      perspective on Repositories
        props (Repositories$NameSpace) verbs (Consult)
      
      perspective on BespokeDatabases
        only (CreateAndFill, Remove)
        props (Description) verbs (Consult, SetPropertyValue)
      
      perspective on BespokeDatabases >> binding >> context >> Owner
        only (Fill)

      action RequestDatabase
        letA
          db <- create context BespokeDatabase bound to BespokeDatabases
        in
          bind (sys:Me >> binder CouchdbServer$Accounts >>= first) to Owner in db >> binding >> context 
      
      perspective on MyBespokeDatabases
        props (Description) verbs (Consult)

      screen "Couchdb Server"
        tab "The server"
          row
            form External
          row
            table "Admins" Admin
          row
            table Accounts
        tab "Repositories" default
          row
            table Repositories
        tab "My databases"
          row
            table MyBespokeDatabases

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
        pattern = "^[a-z]([a-z]|[0-9]|[\\._$+-])*$" "Only lowercase characters (a-z), digits (0-9), and any of the characters ., _, $, + and - are allowed. Must begin with a letter."
      property HasDatabases (Boolean)

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
              HasDatabases = true
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
              HasDatabases = false

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

    context BespokeDatabases (relational) filledBy BespokeDatabase
    context MyBespokeDatabases = filter BespokeDatabases with binding >> context >> Owner filledBy sys:Me
    aspect thing sys:ContextWithNotification$Notifications
  -------------------------------------------------------------------------------
  ---- BESPOKEDATABASE
  -------------------------------------------------------------------------------
  case BespokeDatabase
    external
      property DatabaseLocation (String)
      property DatabaseName (String)
      property Endorsed (Boolean)
      property BaseUrl = binder BespokeDatabases >> context >> extern >> ServerUrl
      property OwnerName = context >> Owner >> LastName
      property Description (String)
      property Public (Boolean)
      state CreateDb = Endorsed and exists context >> Owner
        on entry
          do for CBAdmin
            DatabaseName = "cw_" + callExternal util:GenSym() returns String + "/" 
            callEffect cdb:CreateCouchdbDatabase( BaseUrl, DatabaseName )
            DatabaseLocation = BaseUrl + DatabaseName
            callEffect cdb:MakeAdminOfDb( BaseUrl, DatabaseName, context >> Owner >> UserName )
        state Publish = Public
          on entry
            do for CBAdmin
              callEffect cdb:MakeDatabasePublic( BaseUrl, DatabaseName )
        -- on exit
        --   do for CBAdmin
        --     callEffect cdb:MakeDatabasePrivate( BaseUrl, DatabaseName )
      on exit
        do for CBAdmin
          callEffect cdb:DeleteCouchdbDatabase( BaseUrl, DatabaseName )

    -- Owner will be an Admin of the BespokeDatabase.
    user Owner filledBy (CouchdbServer$Accounts, CouchdbServer$Admin)
      property BespokeDatabaseUrl = context >> extern >> DatabaseLocation
      perspective on External
        props (Public, Description) verbs (Consult, SetPropertyValue)
        props (DatabaseLocation) verbs (Consult)

    user CBAdmin = extern >> binder BespokeDatabases >> context >> Admin
      perspective on External
        props (DatabaseLocation, Endorsed, DatabaseName) verbs (Consult, SetPropertyValue)
        props (BaseUrl) verbs (Consult)
      perspective on Owner
        all roleverbs
        props (LastName) verbs (Consult)

  -------------------------------------------------------------------------------
  ---- REPOSITORY
  -------------------------------------------------------------------------------
  -- This context has a public perspective that allows the visitor to see the models.
  -- This contexts implements the BodyWithAccounts pattern.
  case Repository
    aspect acc:Body
    aspect sys:ManifestCollection

    state Endorsed = extern >> binder Repositories >> AdminEndorses
      perspective of Admin
        perspective on Authors
          only (Create, Fill, Remove)
          props (FirstName, LastName) verbs (Consult)

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
      property ModelsDatabase = "models_" + NameSpace_
      property InstancesDatabase = "cw_" + NameSpace_
      -- The location of the CouchdbServer_. 
      property ServerUrl (functional) = binder Repositories >> context >> extern >> ServerUrl
      property RepositoryUrl = "https://" + NameSpace + "/"
      property AdminLastName = context >> Admin >> LastName
      property RepoHasDatabases (functional) = binder Repositories >> HasDatabases

    -- We need the ServerAdmin in this context in order to configure the local Admin and to give Authors write access.
    user ServerAdmin (functional) = extern >> binder Repositories >> context >> CouchdbServer$Admin
      perspective on Admin
        only (Create, Fill, RemoveFiller, Remove)
        props (SpecificUserName, Password) verbs (SetPropertyValue)
        props (FirstName, LastName, Password, UserName) verbs (Consult)
        props (AuthorizedDomain) verbs (SetPropertyValue)
      perspective on Authors
        props (AuthorizedDomain) verbs (SetPropertyValue)

    -- This role requires credentials for the ServerUrl. It 'inherits' them from its filler.
    -- It also requires credentials for the RepositoryUrl, because it creates and updates Manifests.
    -- It requires write access to cw_servers_and_repositories and to cw_RepositoryUrl. 
    -- The latter is guaranteed as it is DatabaseAdmin of both databases. 
    -- NOTE: later, CouchdbServer$Accounts will also be allowed to fill this role.
    user Admin filledBy CouchdbServer$Admin
      aspect acc:Body$Admin
      aspect sys:ContextWithNotification$NotifiedUser
        -- WithCredentials$SpecificUserName
        -- We must use the next three properties to provide the autentication details to the PDR for the RepositoryUrl.
        -- WithCredentials$UserName (either the SpecificUserName or the User identifier)
        -- WithCredentials$Password
        -- WithCredentials$AuthorizedDomain
        -- As Admin, has a full perspective on Accounts.

      action CreateManifest
        create role Manifests

      state IsFilled = (exists binding) and context >> extern >> RepoHasDatabases
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
              callEffect cdb:MakeAdminOfDb( serverurl, context >> extern >> ModelsDatabase, UserName )
              -- instances
              -- callEffect cdb:MakeAdminOfDb( serverurl, context >> extern >> WriteInstances, UserName )
              callEffect cdb:MakeAdminOfDb( serverurl, context >> extern >> InstancesDatabase, UserName )
              -- Make authentication details available to the PDR (in fact only useful when the filler of Admin is not CouchdbServer$Admin)
              AuthorizedDomain = context >> extern >> RepositoryUrl
        
        state HasPassword = exists Password
          on entry
            do for ServerAdmin
              callEffect cdb:AddCredentials( AuthorizedDomain, UserName, Password)


        on exit
          do for ServerAdmin
            letA
              serverurl <- context >> extern >> ServerUrl
            in
              -- models
              -- callEffect cdb:RemoveAsAdminFromDb( serverurl, context >> extern >> WriteModels, UserName )
              callEffect cdb:RemoveAsAdminFromDb( serverurl, context >> extern >> ModelsDatabase, UserName )
              -- instances
              -- callEffect cdb:RemoveAsAdminFromDb( serverurl, context >> extern >> WriteInstances, UserName )
              callEffect cdb:RemoveAsAdminFromDb( serverurl, context >> extern >> InstancesDatabase, UserName )

      on exit 
        notify "You are no longer the administrator of the repository { context >> extern >> NameSpace_ }."

      
      perspective on External
        props (IsPublic, NameSpace_, RepositoryUrl) verbs (Consult)

        action CompileRepositoryModels
          callEffect p:CompileRepositoryModels( RepositoryUrl + ModelsDatabase, RepositoryUrl + InstancesDatabase )
      
      perspective on Admin
        props (FirstName, SpecificUserName, UserName, Password) verbs (Consult)

      -- The design pattern for nested public contexts requires that Admin has write access
      -- to both the cw_servers_and_repositories and to the Repository database.
      perspective on Manifests
        only (Create, Fill, Delete, Remove)
        props (Description, LocalModelName) verbs (Consult)
        props (DomeinFileName) verbs (SetPropertyValue)
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
        tab "Manifests" default
          row
            table Manifests
              props (LocalModelName, Description) verbs (Consult)
        tab "Authors"
          row
            table Authors
      
    -- This role requires credentials for the ServerUrl. It 'inherits' them from its filler.
    -- It also requires credentials for the RepositoryUrl, because it creates and updates Manifests.
    -- It requires write access to cw_servers_and_repositories and to cw_RepositoryUrl.
    -- It actually does not require write access to models_RepositoryUrl, but as Authors fill ModelManifest$Author
    -- (and that role actually requires write access to models_RepositoryUrl), this is a convenient place to arrange that.
    user Authors (relational) filledBy CouchdbServer$Accounts
      aspect acc:Body$Accounts
      state Filled = exists binding
        on entry
          do for ServerAdmin
            letA
              serverurl <- context >> extern >> ServerUrl
            in
              AuthorizedDomain = context >> extern >> RepositoryUrl
              callEffect cdb:MakeWritingMemberOf( serverurl, context >> extern >> ModelsDatabase, UserName ) -- UserName is the ID of the PerspectivesSystem$User.
              callEffect cdb:MakeWritingMemberOf( serverurl, context >> extern >> InstancesDatabase, UserName ) -- UserName is the ID of the PerspectivesSystem$User.
        state Domain = exists AuthorizedDomain
          on entry
            do
              callEffect cdb:AddCredentials( AuthorizedDomain, UserName, Password)
        on exit
          do for ServerAdmin
            letA
              serverurl <- context >> extern >> ServerUrl
            in
              callEffect cdb:RemoveAsWritingMemberOf( serverurl, context >> extern >> ModelsDatabase, UserName ) -- UserName is the ID of the PerspectivesSystem$User.
              callEffect cdb:RemoveAsWritingMemberOf( serverurl, context >> extern >> InstancesDatabase, UserName ) -- UserName is the ID of the PerspectivesSystem$User.

      perspective on External
        props (IsPublic, NameSpace_, RepositoryUrl) verbs (Consult)

      perspective on Manifests
        only (Create, Fill, Delete, Remove)
        props (Description, LocalModelName) verbs (Consult)
        props (DomeinFileName) verbs (SetPropertyValue)
        in object state NoLocalModelName
          props (LocalModelName) verbs (SetPropertyValue)
      
      perspective on Manifests >> binding >> context >> Author
        only (Create, Fill)
      
      perspective on Authors
        props (FirstName, LastName, AuthorizedDomain) verbs (Consult)

      screen "Repository"
        tab "This repository"
          row 
            form "Repository information" External
              props (RepositoryUrl) verbs (Consult)
          row 
            table "Authors" Authors
        tab "Manifests" default
          row
            table Manifests
    
    
    user Accounts (relational, unlinked) filledBy (CouchdbServer$Accounts, CouchdbServer$Admin)

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
              DomeinFileName = manifestname + ".json" for origin >> binding

          do for Authors
            letA 
              manifestname <- (context >> extern >> NameSpace_ + "-" + LocalModelName)
            in
              -- As the PDR derives this name from the modelURI, we have to name the ModelManifest with its LocalModelName.
              create_ context ModelManifest named manifestname bound to origin
              bind currentactor to Author in origin >> binding >> context
              DomeinFileName = manifestname + ".json" for origin >> binding
    aspect thing sys:ContextWithNotification$Notifications

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
      -- The URL of the Repository (and it will refer to the ServerUrl).
      property RepositoryUrl (functional) = binder Manifests >> context >> extern >> RepositoryUrl
      -- The URL of the Instances database of the Repository.
      -- Rename to InstancesURL
      property PublicUrl = RepositoryUrl + context >> Repository >> InstancesDatabase + "/"
      -- The highest version number
      property HighestVersion = context >> Versions >> Versions$Version >>= maximum
      -- The version recommended by the Author
      property RecommendedVersion = (filter context >> Versions with IsRecommended) >> Versions$Version
      -- The version to install. It depends on a calculation, but we have to store it explicitly 
      -- so we can even retrieve it on system startup.
      -- This property must be published.
      -- PDRDEPENDENCY
      property VersionToInstall (String)
      property RecomputeVersionToInstall (Boolean)
      
      -- This might become a standard pattern.
      -- It is triggered by setting RecomputeVersionToInstall to true on removing a Version.
      state RecomputeVersion = RecomputeVersionToInstall
        on entry
          do for Author
            -- However, when we enter this state, the Version role instance hasn't yet been actually removed.
            -- We then cause this state to exit.
            RecomputeVersionToInstall = false
        on exit
          do for Author
            -- By the time the exit is evaluated, the Version role instance has been removed and HighestVersion 
            -- evaluates to the correct value.
            VersionToInstall = HighestVersion
    
    -- Embedded contexts are not removed automatically with their embedder!
    on exit
      do for Author
        delete context bound to Versions
    
    -- The external role of the Repository.
    context Repository (functional) = extern >> binder Manifests >> context >> extern

    -- Inherits credentials for the ServerUrl and RepositoryUrl, and write access to the ModelsDatabase and the InstancesDatabase of the Repository.
    user Author (relational) filledBy (Repository$Authors, Repository$Admin)
      perspective on extern
        props (Description, IsLibrary, VersionToInstall, RecomputeVersionToInstall) verbs (Consult, SetPropertyValue)
      perspective on Versions
        only (Create, Fill, Remove, CreateAndFill, Delete)
        props (Versions$Version, Description, Patch, Build) verbs (Consult, SetPropertyValue)
      perspective on Versions >> binding >> context >> Author
        only (Fill, Create)
        props (FirstName, LastName) verbs (Consult)
      perspective on Author
        all roleverbs
        props (FirstName, LastName) verbs (Consult)
      action CreateVersion
        create role Versions
      
      screen "Model Manifest"
        tab "Versions" default
          row 
            form "This Manifest" External
          row
            table "Available Versions" Versions
              only (Remove)
        tab "Authors"
          row
            table "Authors" Author
    
    -- A public version of ModelManifest is available in the database cw_<NameSpace>.
    public Visitor at extern >> PublicUrl = sys:Me
      perspective on extern
        props (Description, IsLibrary, VersionToInstall, DomeinFileName) verbs (Consult)
      -- NOTA BENE: betekent dit niet dat instanties van ModelsInUse gepubliceerd worden?
      perspective on sys:MySystem >> ModelsInUse
        only (Fill, Remove)
      perspective on sys:MySystem >> ModelsInUse
        only (Fill)
        props (ModelToRemove) verbs (SetPropertyValue)
      perspective on Versions
        props (Versions$Version, Description, VersionedModelURI, VersionedModelManifest$External$DomeinFileName, Patch, Build) verbs (Consult)
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

    context Versions (relational) filledBy VersionedModelManifest
      aspect sys:ModelManifest$Versions
      -- Version
      -- VersionedLocalModelName
      state ReadyToMake = (exists Versions$Version) and not exists binding
        on entry
          do for Author
            letA
              v <- (context >> extern >> RecommendedVersion) orElse (context >> extern >> HighestVersion)
              versionname <- (context >> extern >> binder Manifests >> context >> extern >> NameSpace_ >>= first) + "-" + VersionedLocalModelName
            in
              -- As the PDR derives this name from the modelURI, we have to name the ModelManifest with its LocalModelName.
              create_ context VersionedModelManifest named versionname bound to origin
              bind currentactor to VersionedModelManifest$Author in origin >> binding >> context
              -- NOTE that we conceivably might add a version with a lower number than the highest.
              VersionToInstall = v for context >> extern
              -- dit werkt
              Patch = 0
              Build = 0
      on exit
        do for Author
          RecomputeVersionToInstall = true for context >> extern

  case VersionedModelManifest
    aspect sys:VersionedModelManifest
    aspect sys:ContextWithNotification

    external
      aspect sys:VersionedModelManifest$External
      -- VersionedModelManifest$External$Description
      -- VersionedModelManifest$External$DomeinFileName
      -- VersionedModelManifest$External$Version
      -- VersionedModelManifest$External$Patch
      -- VersionedModelManifest$External$Build
      -- The Version property is registered on ModelManifest$Versions so we can use it to create a DNS URN for it (it must be a public resource)
      -- PDRDEPENDENCY
      property ModelURI (functional) = binder Versions >> context >> extern >> ModelManifest$External$ModelURI
      property VersionedModelURI = VersionedModelManifest$External$ModelURI + "@" + External$Version
      property ArcFile (File)
        pattern = "text/arc" "Only .arc files (Perspectives Language source files) are allowed, so use `text//arc."
      property ArcSource = callExternal files:FileText( ArcFile ) returns String
      property ArcFeedback (String)
        minLength = 81
      -- property DomeinFile (File)
      property PublicUrl (functional) = binder Versions >> context >> extern >> PublicUrl
      -- Only one VersionedModelManifest can be the recommended version at a time.
      property IsRecommended (Boolean)
      -- The (Javascript) DateTime value of the last upload.
      property LastChangeDT (DateTime)
      -- The readable date and time of the last upload.
      property LastUpload = callExternal util:FormatDateTime( LastChangeDT, "nl-NL", "{\"dateStyle\": \"short\", \"timeStyle\": \"short\"}" ) returns String
      property MustUpload (Boolean)

      on exit
        do for Author
          -- Delete the DomeinFile.
          callEffect p:RemoveFromRepository( VersionedModelManifest$External$VersionedModelURI )

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
      
      state ProcessArc = (exists ArcSource) and ((callExternal sensor:ReadSensor( "clock", "now" ) returns DateTime > LastChangeDT) or not exists LastChangeDT)
        on entry
          do for Author
            ArcFeedback = callExternal p:ParseAndCompileArc( ModelURI, ArcSource ) returns String
            LastChangeDT = callExternal sensor:ReadSensor( "clock", "now" ) returns DateTime
            MustUpload = true

      state UploadToRepository = (ArcFeedback matches regexp "^OK") and MustUpload
        on entry
          do for Author
            callEffect p:UploadToRepository( VersionedModelManifest$External$VersionedModelURI, 
              callExternal util:ReplaceR( "bind publicrole.*in sys:MySystem", "", ArcSource ) returns String)
            Build = Build + 1
            MustUpload = false
          notify Author
            "Version {External$Version} (build {Build}) has been uploaded to the repository for {binder Versions >> context >> Repository >> NameSpace >>= first}."

    user Author (relational) filledBy cm:ModelManifest$Author
      aspect sys:ContextWithNotification$NotifiedUser
      perspective on extern
        props (DomeinFileName, Version, ArcSource, LastUpload) verbs (Consult)
        props (ArcFile, ArcFeedback, Description, IsRecommended, Build, Patch, LastChangeDT, MustUpload) verbs (SetPropertyValue)
      perspective on Manifest
        props (VersionToInstall) verbs (Consult, SetPropertyValue)
      perspective on Manifest >> context >> Versions
        props (IsRecommended) verbs (SetPropertyValue)
      perspective on Author
        all roleverbs
        props (FirstName, LastName) verbs (Consult)
      screen "Model version"
        tab "Version" default
          row
            form External
        tab "Authors"
          row
            table Author
    
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

    context Manifest (functional) = extern >> binder Versions >> context >> extern
    aspect thing sys:ContextWithNotification$Notifications