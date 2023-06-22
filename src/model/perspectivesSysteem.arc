-- Copyright Joop Ringelberg and Cor Baars 2019, 2020, 2021, 2022, 2023
domain model://perspectives.domains#System
  use sys for model://perspectives.domains#System
  use cdb for model://perspectives.domains#Couchdb
  use ser for model://perspectives.domains#Serialise
  use sensor for model://perspectives.domains#Sensor
  use util for model://perspectives.domains#Utilities

  -- model:System (short for model://perspectives.domains#System) is booted in a unique way.
  -- Other models rely on there being an instance of sys:PerspectivesSystem and a Installer role in it.
  -- That precondition obviously fails for this model.
  -- Consequently, we create both in code. 
  -- We also create their indexed names, so we can refer to them below.
  -- Yet, we still have to create instances of IndexedRoles and IndexedContexts and provide the indexed names for them,
  -- otherwise sys:Me and sys:MySystem will be lost as the initial session ends.
  -- The initialisation routine below therefore is somewhat simpler.

  on entry
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- create role IndexedContexts in sys:MySystem
        indexedrole <- create role IndexedRoles in sys:MySystem
      in 
        bind sys:MySystem >> extern to StartContexts in sys:MySystem
        Name = "My System" for sys:MySystem >> extern
        bind_ sys:MySystem >> extern to indexedcontext
        -- TODO. Zonder kwalificatie zegt de compiler dat "Name" niet bestaat voor IndexedContexts. Maar er is een naamconflict met RootContext$Extern$Name
        IndexedContexts$Name = sys:MySystem >> indexedName for indexedcontext
        bind_ sys:Me to indexedrole
        Name = sys:Me >> indexedName for indexedrole
        -- Equivalently:
        -- Name = "model://perspectives.domains#System$Me"

        -- NOTE that we should uncomment this line but cannot do so until 
        --    * this resource and its model is available;
        --    * we can compile model://perspectives.domains/System in a MyContexts installation that holds model://perspectives.domains/CouchdbManagement.
        -- This is because the line below is a forward reference to a CouchdbManagement type.
        -- This is a Catch22 situation.
        -- Now add the perspectives.domains repository as BaseRepository:
        bind publicrole pub:https://perspectives.domains/cw_servers_and_repositories/#perspectives_domains$External to BaseRepository in sys:MySystem

  aspect user sys:PerspectivesSystem$Installer

  -- Used as model:System$RoleWithId$Id in the PDR code.
  thing RoleWithId
    property Id = callExternal util:RoleIdentifier() returns String

  user WithCredentials filledBy sys:PerspectivesSystem$User
    -- | The value of field systemIdentifier of the PouchdbUser object that is passed in to runPDR.
    -- | Currently, this is the raw username that is used to log into MyContexts, e.g. "dev1".
    property UserName = callExternal util:SystemIdentifier() returns String
    property Password (String)
    property AuthorizedDomain (String)

  case PerspectivesSystem
    indexed sys:MySystem
    aspect sys:RootContext
    aspect sys:ContextWithNotification

    state NoCaches = not exists SystemCaches
      on entry
        do for User
          create context Caches bound to SystemCaches

    state NoBaseRepository = not exists BaseRepository
      perspective of User
        action UploadCouchdb
          callEffect cdb:AddModelToLocalStore( "model://perspectives.domains#CouchdbManagement" )

    external
      aspect sys:RootContext$External
      property ConnectedToAMQPBroker (Boolean)
      property CardClipBoard (String)
      property ShowLibraries (Boolean)

      view ShowLibraries (ShowLibraries)

    user User (mandatory)
      property LastName (mandatory, relational, String)
      property FirstName (mandatory, relational, String)
      property Channel = (binder Initiator union binder ConnectedPartner) >> context >> extern >> ChannelDatabaseName
      -- User instances need not have a value for this property. It is used in the PDR to
      -- ensure serialisation of the User role.
      property Id (String)
      -- property Id = callExternal util:RoleIdentifier() returns String
      indexed sys:Me
      view VolledigeNaam (FirstName, LastName)
      action UploadCouchdb_old
        callEffect cdb:AddModelToLocalStore( "model://perspectives.domains#CouchdbManagement" )
      action ReloadCouchdb
        callEffect cdb:UpdateModel( "model://perspectives.domains#CouchdbManagement", false )
      action UploadTestPublicRole
        callEffect cdb:AddModelToLocalStore( "model://perspectives.domains#TestPublicRole" )
      action ReloadTestPublicRole
        callEffect cdb:UpdateModel( "model://perspectives.domains#TestPublicRole", false )
      action ReloadSystem
        callEffect cdb:UpdateModel( "model://perspectives.domains#System", false )
      perspective on User
        only (CreateAndFill)
        props (LastName, FirstName) verbs (SetPropertyValue)
        props (Channel) verbs (Consult)
      -- perspective on ModelsInUse
      --   defaults
      --   --in object state
      --   action Refresh
      --     PerformUpdate = true
      --   action RefreshWithDependencies
      --     IncludingDependencies = true
      --     PerformUpdate = true
      perspective on StartContexts
        defaults
        action StopUsing
          callDestructiveEffect cdb:RemoveModelFromLocalStore ()
      perspective on Contacts
        props (FirstName, LastName) verbs (Consult)
      perspective on External
        view ShowLibraries verbs (Consult, SetPropertyValue)
      perspective on BasicModels
        props (Manifests$LocalModelName, Description) verbs (Consult)
      -- Notice that these roles are filled with the public version of VersionedModelManifest$External.
      -- We can actually only show properties that are in that perspective.
      perspective on BasicModelsInUse
        only (Remove)
        props (VersionedModelManifest$External$LocalModelName, Description) verbs (Consult)

      perspective on PendingInvitations
        view ForInvitee verbs (Consult)
      perspective on SystemCaches
        defaults

      screen "Home"
        tab "SystemCaches"
          row
            form SystemCaches
        tab "Manage new models"
          row
            table BasicModels
          row 
            table BasicModelsInUse
              props (VersionedModelManifest$External$LocalModelName, Description) verbs (Consult)
        tab "Start contexts"
          row
            table StartContexts
              props (Name) verbs (Consult)
        tab "User and Contacts"
          row
            form User
              props (FirstName, LastName) verbs (SetPropertyValue)
          row 
            table Contacts
        tab "Invitations"
          row
            table PendingInvitations
              props (InviterLastName, Message) verbs (Consult)


    user Contacts = filter (callExternal cdb:RoleInstances( "model://perspectives.domains#System$PerspectivesSystem$User" ) returns sys:PerspectivesSystem$User) with not filledBy sys:Me

    user Installer
      perspective on StartContexts
        only (CreateAndFill)
      perspective on IndexedContexts
        only (Create, Fill)
        props (IndexedContexts$Name) verbs (SetPropertyValue)
      perspective on IndexedRoles
        only (Create, Fill)
        props (IndexedRoles$Name) verbs (SetPropertyValue)
      perspective on BaseRepository
        only (CreateAndFill)

    context BaseRepository filledBy ManifestCollection

    context BasicModels = filter BaseRepository >> binding >> context >> Manifests with not IsLibrary

    context BasicModelsInUse (relational) filledBy sys:VersionedModelManifest
      -- on exit
      --   do for User
      --     callDestructiveEffect cdb:RemoveModelFromLocalStore ( DomeinFileName )


    -- All context types that have been declared to be 'indexed' have an instance that fills this role.
    context IndexedContexts (mandatory) filledBy sys:RootContext
      property Name (mandatory, String)

    -- All role types that have been declared to be 'indexed' have an instance that fills this role.
    thing IndexedRoles (relational)
      property Name (mandatory, String)


    context Channels = User >> (binder Initiator union binder ConnectedPartner) >> context >> extern

    -- StartContexts should be bound to Contexts that share an Aspect and that Aspect should have a name on the External role.
    -- These are the 'apps' of Perspectives.
    context StartContexts (relational) filledBy sys:RootContext

    -- This will become obsolete when we start using model:CouchdbManagement.
    -- context ModelsInUse (relational) filledBy Model
    --   property PerformUpdate (Boolean)
    --   property IncludingDependencies (Boolean)
    --   property HasBeenInstalled (Boolean)
    --   --on exit
    --     --notify User
    --       --"Model {origin >> binding >> ModelIdentification} has been removed completely."
    --   state Update = PerformUpdate
    --     on entry
    --       do for User
    --         callEffect cdb:UpdateModel( ModelIdentification, IncludingDependencies )
    --         PerformUpdate = false
    --         -- Updating without dependencies is the default.
    --         IncludingDependencies = false
    --       notify User
    --         "Model {ModelIdentification} has been updated."
    --   state NotInStartContexts = (not HasBeenInstalled) and exists (binding >> context >> IndexedContext >> filter binding with not exists binder StartContexts)
    --     -- Create an entry in StartContexts if its model has been taken in use.
    --     on entry
    --       do for User
    --         bind binding >> context >> IndexedContext >> binding to StartContexts
    --         HasBeenInstalled = true
    --       notify User
    --         "{ binding >> ModelIdentification } added!"
    --   state Dangles = (not exists binding >> context >> IndexedContext >> binding) and HasBeenInstalled
    --     -- If the user has removed the App, this automatic action will clear away the corresponding entry in ModelsInuse.
    --     -- We also remove the Model itself (the entry to the functionality).
    --     on entry
    --       do for User
    --         remove context origin
    --         callDestructiveEffect cdb:RemoveModelFromLocalStore ( ModelIdentification )
    --   view ModelInUsePresentation (Description, Name, PerformUpdate)

    context PendingInvitations = callExternal cdb:PendingInvitations() returns sys:Invitation$External

    thing Databases (mandatory, relational)
      -- Name is one of post, data, models.
      property Name (mandatory, String)
      property Identifier (mandatory, String)

    -- A calculated role representing all available Notifications (from any context).
    thing AllNotifications = callExternal cdb:RoleInstances( "model://perspectives.domains#System$ContextWithNotification$Notifications" ) returns sys:ContextWithNotification$Notifications

    context SystemCaches (mandatory) filledBy Caches

  -- This will become obsolete when we start using model:CouchdbManagement.
  -- case Model public NAMESPACESTORE
  --   aspect sys:RootContext
  --   external
  --     aspect sys:RootContext$External
  --     property Description (mandatory, String)
  --     property ModelIdentification (mandatory, String)
  --     property Url (mandatory, String)
  --     property IsLibrary (mandatory, Boolean)
  --     view ModelPresentation (Description, ModelIdentification, IsLibrary)
  --   user Author filledBy User
  --     aspect sys:RootContext$RootUser
  --     perspective on extern
  --   context IndexedContext (mandatory) filledBy sys:RootContext
  --     -- Dit wordt alleen gebruikt in model:System.
  --     property Name (mandatory, String)
  --   thing IndexedRole (relational)
  --     property Name (mandatory, String)

  -- A Collection of System Caches.
  case Caches
    aspect sys:ContextWithScreenState

    state NoCaches = not exists Cache
      on entry 
        do for Manager
          letA
            contextcache <- create role Cache
            rolecache <- create role Cache
            domeincache <- create role Cache
          in
            Name = "contextcache" for contextcache
            Name = "rolecache" for rolecache
            Name = "domaincache" for domeincache

    external
      aspect sys:ContextWithScreenState$External

    user Manager = sys:Me
      perspective on Cache
        defaults

    thing Cache (relational)
      property Name (String)
      property Size (Number)

      state StartReading = context >> extern >> IsOnScreen
        on entry
          do for Manager every 2 Seconds
            Size = callExternal sensor:ReadSensor ( Name, "size" ) returns Number


  -- Use this as an aspect in contexts that should store their own notifications.
  case ContextWithNotification
    thing Notifications (relational)
      property Message (String)
    -- As soon as we have perspective contextualisation, add NotifiedUser as an Aspect to all users that can be notified.
    user NotifiedUser
      perspective on Notifications
        action DeleteNotifications
          delete role Notifications

  case ContextWithScreenState
    external 
      property IsOnScreen (Boolean)

  case PhysicalContext
    user UserWithAddress
      -- The public URL of the PDR of the UserWithAddress.
      property Host (String)
      -- The port where Couchdb listens.
      property Port (Number)
      -- The public URL of the RelayServer of the UserWithAddress
      property RelayHost (String)
      -- The port where Couchdb listens on the RelayServer.
      property RelayPort (String)

  -- A Channel is shared by just two users.
  case Channel
    aspect sys:PhysicalContext
    external
      property ChannelDatabaseName (mandatory, String)
    user Initiator filledBy sys:PerspectivesSystem$User
      aspect sys:PhysicalContext$UserWithAddress
      perspective on ConnectedPartner
      perspective on Initiator
    user ConnectedPartner filledBy sys:PerspectivesSystem$User
      -- The public URL of the PDR of the partner.
      aspect sys:PhysicalContext$UserWithAddress
      perspective on Initiator
      perspective on ConnectedPartner
    user Me = filter (Initiator union ConnectedPartner) with filledBy sys:Me
    user You = filter (Initiator union ConnectedPartner) with not filledBy sys:Me

  case RootContext
    external
      property Name (mandatory, String)
    user RootUser filledBy sys:PerspectivesSystem$User

  case Invitation
    external
      property IWantToInviteAnUnconnectedUser (Boolean)
      property SerialisedInvitation (String)
      property Message (String)
      property InviterLastName = context >> Inviter >> LastName
      state InviteUnconnectedUser = IWantToInviteAnUnconnectedUser and exists Message
        on entry
          do for Inviter
            SerialisedInvitation = callExternal ser:SerialiseFor( filter origin >> context >> contextType >> roleTypes with specialisesRoleType model://perspectives.domains#System$Invitation$Invitee ) returns String

      view ForInvitee (InviterLastName, Message)

    user Inviter (mandatory) filledBy sys:PerspectivesSystem$User
      perspective on Invitee
        props (FirstName, LastName) verbs (Consult)

    user Invitee (mandatory) filledBy Guest
      perspective on Inviter
        props (FirstName, LastName) verbs (Consult)
      perspective on extern
        view ForInvitee verbs (Consult)

    -- Without the filter, the Inviter will count as Guest and its bot will fire for the Inviter, too.
    user Guest = filter sys:Me with not fills (currentcontext >> Inviter)
      perspective on Invitee
        only (Fill, Create)

  -- To be used as Aspect in model:CouchdbManagement$Repository
  case ManifestCollection
    context Manifests (relational) filledBy ModelManifest
      -- This is the local name, unqualified, of the model, e.g. "JoopsModel" or "System".
      -- It must be entered through a form. We need it on this role so we can create an instance
      -- of ModelManifest with this name.
      property LocalModelName (String)

  case ModelManifest
    aspect sys:RootContext
    external
      aspect sys:RootContext$External
      property Description (mandatory, String)
      property IsLibrary (mandatory, Boolean)

    context Versions (relational) filledBy VersionedModelManifest
      -- This value must be entered by the user. 
      -- We need it on this role so we can create an instance of VersionedModelManifest based on
      -- the Version value.
      property Version (mandatory, String)
      -- E.g. "System@1.0.0"
      property LocalModelName = context >> extern >> binder Manifests >> Manifests$LocalModelName + "@" + Versions$Version
      -- The value of this property will be set automatically by the Couchdb:VersionedModelManifest$Author.
      -- It must be a DomeinFileId, e.g. perspectives_domains-System@1.0.0.json
      property DomeinFileName (mandatory, String)

  case VersionedModelManifest
    external
      property Description (mandatory, String)
      -- Notice that we have to register the DomeinFileName on the context role in the collection (ModelManifest$Versions),
      -- to serve in the pattern that creates a DNS URI, so it can be a public resource.
      property DomeinFileName = binder Versions >> Versions$DomeinFileName
      property Version = binder Versions >> Versions$Version
      property LocalModelName = binder Versions >> Versions$LocalModelName
    user Visitor = sys:Me
      perspective on extern
        props (Description, DomeinFileName) verbs (Consult)
