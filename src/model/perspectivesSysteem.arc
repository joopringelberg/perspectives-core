-- Copyright Joop Ringelberg and Cor Baars 2019, 2020, 2021, 2022, 2023

-- PDRDEPENDENCY
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
    -- PDRDEPENDENCY
    property Id = callExternal util:RoleIdentifier() returns String

  -- PDRDEPENDENCY
  user WithCredentials filledBy sys:PerspectivesSystem$User
    -- | The value of field systemIdentifier of the PouchdbUser object that is passed in to runPDR.
    -- | Currently, this is the raw username that is used to log into MyContexts, e.g. "dev1".
    -- PDRDEPENDENCY
    property UserName = SpecificUserName orElse callExternal util:SystemIdentifier() returns String
    property SpecificUserName (String)
    -- PDRDEPENDENCY
    property Password (String)
    -- PDRDEPENDENCY
    property AuthorizedDomain (String)

  -- PDRDEPENDENCY
  case PerspectivesSystem
    -- PDRDEPENDENCY
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
      -- PDRDEPENDENCY
      property ConnectedToAMQPBroker (Boolean)
      property CardClipBoard (String)
      property ShowLibraries (Boolean)

      view ShowLibraries (ShowLibraries)

    -- PDRDEPENDENCY
    user User (mandatory)
      property LastName (mandatory, relational, String)
      property FirstName (mandatory, relational, String)
      -- PDRDEPENDENCY
      property Channel = (binder Initiator union binder ConnectedPartner) >> context >> extern >> ChannelDatabaseName
      -- User instances need not have a value for this property. It is used in the PDR to
      -- ensure serialisation of the User role.
      -- PDRDEPENDENCY
      property Id (String)
      -- property Id = callExternal util:RoleIdentifier() returns String

      -- PDRDEPENDENCY
      indexed sys:Me
      view VolledigeNaam (FirstName, LastName)
      perspective on User
        only (CreateAndFill)
        props (LastName, FirstName) verbs (SetPropertyValue)
        props (Channel) verbs (Consult)
      perspective on StartContexts
        props (Name) verbs (Consult)
      perspective on Contacts
        props (FirstName, LastName) verbs (Consult)
      perspective on OutgoingInvitations
        only (CreateAndFill, Remove)
        props (InviterLastName) verbs (Consult)
      perspective on External
        view ShowLibraries verbs (Consult, SetPropertyValue)
      -- Notice that these roles are filled with the public version of VersionedModelManifest$External.
      -- We can actually only show properties that are in that perspective.
      perspective on ModelsInUse
        only (Remove)
        props (ModelName, Description, Version, Patch, Build) verbs (Consult)
        props (InstalledPatch, InstalledBuild, UpdateOnBuild) verbs (SetPropertyValue)
      perspective on ModelsToUpdate
        props (ModelName, Description, Version) verbs (Consult)
        in object state InstallUpdate
          action InstallPatch
            callEffect cdb:UpdateModel( ModelToRemove, false )
            InstalledPatch = Patch
            InstalledBuild = Build
        in object state InstallBuild
          action UpdateBuild
            callEffect cdb:UpdateModel( ModelToRemove, false )
            InstalledBuild = Build
      perspective on BaseRepository
        props (Domain) verbs (Consult)
      perspective on Repositories
        only (CreateAndFill, Remove)
        props (Domain) verbs (Consult)
      perspective on PendingInvitations
        props (InviterLastName, Message) verbs (Consult)
      perspective on SystemCaches
        defaults

      screen "Home"
        tab "SystemCaches"
          row
            form SystemCaches
        tab "Manage new models"
          row 
            form "Default (Base) Repository" BaseRepository
          row 
            table "Other repositories" Repositories
          row 
            table "Models that have been patched" ModelsToUpdate
          row 
            table "Models in use" ModelsInUse
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
          row 
            table "Invitations I have created" OutgoingInvitations
        tab "Invitations"
          row
            table PendingInvitations
              props (InviterLastName, Message) verbs (Consult)


    user Contacts = filter (callExternal cdb:RoleInstances( "model://perspectives.domains#System$PerspectivesSystem$User" ) returns sys:PerspectivesSystem$User) with not filledBy sys:Me

    -- PDRDEPENDENCY
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

    context OutgoingInvitations (relational) filledBy Invitation

    context BaseRepository filledBy ManifestCollection

    context Repositories (relational) filledBy ManifestCollection

    context AllRepositories = BaseRepository union Repositories

    -- PDRDEPENDENCY
    context ModelsInUse (relational) filledBy sys:VersionedModelManifest
      -- PDRDEPENDENCY
      property ModelToRemove (String)
      -- PDRDEPENDENCY
      property InstalledPatch (Number)
      -- PDRDEPENDENCY
      property InstalledBuild (Number)
      property UpdateOnBuild (Boolean)
      state InstallBuild = UpdateOnBuild and InstalledBuild < Build
        on entry
          notify User
            "Installing a new build of { ModelName }."
          do for User
            callEffect cdb:UpdateModel( ModelToRemove, false )
            InstalledBuild = Build

      state InstallUpdate = InstalledPatch < Patch
        on entry
          notify User
            "Installing a new patch for { ModelName }."
          do for User
            callEffect cdb:UpdateModel( ModelToRemove, false )
            InstalledPatch = Patch
      on exit
        -- notify User
        --   "Model {ModelToRemove} has been removed completely."
        do for User
          callDestructiveEffect cdb:RemoveModelFromLocalStore ( ModelToRemove )
    
    context ModelsToUpdate = filter ModelsInUse with isInState InstallUpdate or isInState InstallBuild

    -- context ModelsToUpdate = filter ModelsInUse with 
    --   -- Compare the Patch part of the Semantic Versioning string (the least or rightmost part).
    --   callExternal util:SelectR( "\\.(\\d+)$", ModelToRemove ) returns Number
    --   -- Dit is gebaseerd op het verkeerde idee dat ik in een VersionedModel het patch deel zou aanpassen.
    --     < callExternal util:SelectR( "\\.(\\d+)$", Version ) returns Number

    -- All context types that have been declared to be 'indexed' have an instance that fills this role.
    -- PDRDEPENDENCY
    context IndexedContexts (mandatory) filledBy sys:RootContext
      -- PDRDEPENDENCY
      property Name (mandatory, String)

    -- All role types that have been declared to be 'indexed' have an instance that fills this role.
    -- PDRDEPENDENCY
    thing IndexedRoles (relational)
      -- PDRDEPENDENCY
      property Name (mandatory, String)


    context Channels = User >> (binder Initiator union binder ConnectedPartner) >> context >> extern

    -- StartContexts should be bound to Contexts that share an Aspect and that Aspect should have a name on the External role.
    -- These are the 'apps' of Perspectives.
    context StartContexts (relational) filledBy sys:RootContext

    context PendingInvitations = callExternal cdb:PendingInvitations() returns sys:Invitation$External

    thing Databases (mandatory, relational)
      -- Name is one of post, data, models.
      property Name (mandatory, String)
      property Identifier (mandatory, String)

    -- A calculated role representing all available Notifications (from any context).
    thing AllNotifications = callExternal cdb:RoleInstances( "model://perspectives.domains#System$ContextWithNotification$Notifications" ) returns sys:ContextWithNotification$Notifications

    context SystemCaches (mandatory) filledBy Caches

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
  -- PDRDEPENDENCY
  case ContextWithNotification
    -- PDRDEPENDENCY
    thing Notifications (relational)
      -- PDRDEPENDENCY
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
      -- PDRDEPENDENCY
      property Host (String)
      -- The port where Couchdb listens.
      -- PDRDEPENDENCY
      property Port (Number)
      -- The public URL of the RelayServer of the UserWithAddress
      -- PDRDEPENDENCY
      property RelayHost (String)
      -- The port where Couchdb listens on the RelayServer.
      -- PDRDEPENDENCY
      property RelayPort (String)

  -- A Channel is shared by just two users.
  -- PDRDEPENDENCY
  case Channel
    aspect sys:PhysicalContext
    external
      -- PDRDEPENDENCY
      property ChannelDatabaseName (mandatory, String)
    -- PDRDEPENDENCY
    user Initiator filledBy sys:PerspectivesSystem$User
      aspect sys:PhysicalContext$UserWithAddress
      perspective on ConnectedPartner
      perspective on Initiator
    -- PDRDEPENDENCY
    user ConnectedPartner filledBy sys:PerspectivesSystem$User
      -- The public URL of the PDR of the partner.
      aspect sys:PhysicalContext$UserWithAddress
      perspective on Initiator
      perspective on ConnectedPartner
    user Me = filter (Initiator union ConnectedPartner) with filledBy sys:Me
    user You = filter (Initiator union ConnectedPartner) with not filledBy sys:Me

  case RootContext
    -- PDRDEPENDENCY
    external
      property Name (mandatory, String)
    -- PDRDEPENDENCY
    user RootUser filledBy sys:PerspectivesSystem$User

  case Invitation
    state NoInviter = not exists Inviter

    external
      property SerialisedInvitation (File)
      property Message (String)
      property ConfirmationCode (Number)
      property Confirmation (Number)
        minInclusive = 100000
        maxInclusive = 999999
      property InviterLastName = context >> Inviter >> LastName
      property IWantToInviteAnUnconnectedUser (Boolean)
      state Message = exists Message
      state Invitation = exists SerialisedInvitation
      state Checks = Confirmation == ConfirmationCode
        on entry
          notify Inviter
            "The confirmation code you entered is correct!"

    user Inviter (mandatory) filledBy sys:PerspectivesSystem$User
      perspective on Invitee
        props (FirstName, LastName) verbs (Consult)
      perspective on External
        props (Message, ConfirmationCode, SerialisedInvitation) verbs (SetPropertyValue, Consult)
        in object state Message
          action CreateInvitation
            letA
              text <- callExternal ser:SerialiseFor( ((filter origin >> context >> contextType >> roleTypes with specialisesRoleType model://perspectives.domains#System$Invitation$Invitee) orElse [role model://perspectives.domains#System$Invitation$Invitee])) returns String
            in
              ConfirmationCode = callExternal util:Random(100000, 999999) returns Number
              create file ("invitation_of_" + InviterLastName + ".json") as "text/json" in SerialisedInvitation for origin
                text
        in object state Invitation
          props (Confirmation) verbs (SetPropertyValue)
      
      screen "Invite someone"
        row 
          form External
            -- NOTE: the file control should preferrably not show the upload button in this case.
            props (Message) verbs (SetPropertyValue)
        row 
          form External
            props (SerialisedInvitation) verbs (Consult)
            -- props (SerialisedInvitation) verbs (Consult)
        row
          form "Invitee" Invitee

    user Invitee (mandatory) filledBy Guest
      perspective on Inviter
        props (FirstName, LastName) verbs (Consult)
      perspective on extern
        props (InviterLastName, Message, ConfirmationCode) verbs (Consult)
      screen "Invitation"
        row 
          form "You are invited by:" Inviter
        row
          form "Message and confirmation code" External
            props (Message, ConfirmationCode) verbs (Consult)

    -- Without the filter, the Inviter will count as Guest and its bot will fire for the Inviter, too.
    user Guest = filter sys:Me with not fills (currentcontext >> Inviter)
      perspective on extern
        props (InviterLastName, Message) verbs (Consult)
      perspective on Invitee
        only (Fill, Create)
        props (FirstName, LastName) verbs (Consult)
      perspective on Inviter
        props (FirstName, LastName) verbs (Consult)
        in context state NoInviter
          only (CreateAndFill)
          props (FirstName, LastName) verbs (Consult)
      screen "Invitation"
        row 
          form "You are invited by:" Inviter
        row
          form "Message from the inviter" External
            props (Message) verbs (Consult)
        row 
          form "Accept by filling this role with yourself" Invitee

  -- To be used as Aspect in model:CouchdbManagement$Repository
  case ManifestCollection
    external
      -- This is the namespace of the models in this repository, but dots are replaced by underscores.
      -- It is computed from Repositories$NameSpace on creating the repository (CouchdbManagement)
      property NameSpace_ (mandatory, String)
      -- This we show on screen.
      property Domain = NameSpace_ >> callExternal util:Replace( "_", "." ) returns String
    
    context Manifests (relational) filledBy ModelManifest
      -- This is the local name, unqualified, of the model, e.g. "JoopsModel" or "System".
      -- It must be entered through a form. We need it on this role so we can create an instance
      -- of ModelManifest with this name.
      property LocalModelName (String)

  case ModelManifest
    aspect sys:RootContext
    -- PDRDEPENDENCY
    external
      aspect sys:RootContext$External
      property Description (mandatory, String)
      property IsLibrary (mandatory, Boolean)
      -- The value of this property will be set automatically by the Couchdb:VersionedModelManifest$Author.
      -- It must be a local DomeinFileId, e.g. perspectives_domains-System.json (WITHOUT the version!)
      -- PDRDEPENDENCY
      property DomeinFileName (mandatory, String)
      
    context Versions (relational) filledBy VersionedModelManifest
      -- This value must be entered by the user. 
      -- We need it on this role so we can create an instance of VersionedModelManifest based on
      -- the Version value.
      property Version (mandatory, String)
        pattern = "^[0-9]+\\.[0-9]+$" "The form MAJOR.MINOR where both are integer numbers."
      -- E.g. "System@1.0.0"
      property VersionedLocalModelName = (context >> extern >> binder Manifests >> LocalModelName >>= first) + "@" + Versions$Version
      -- dit kan weer weg!
      property DomeinFileName (String)

  case VersionedModelManifest
    external
      property ModelName (functional) = binder Versions >> context >> extern >> binder Manifests >> LocalModelName
      property Description (mandatory, String)
        minLength = 81
      -- Notice that we have to register the DomeinFileName on the context role in the collection (ModelManifest$Versions),
      -- to serve in the pattern that creates a DNS URI, so it can be a public resource.
      -- PDRDEPENDENCY
      property DomeinFileName (functional) = binder Versions >> context >> extern >> ModelManifest$External$DomeinFileName
      property Version (functional)        = binder Versions >> Versions$Version
      -- PDRDEPENDENCY
      property Patch (Number)
      -- PDRDEPENDENCY
      property Build (Number)
    user Visitor = sys:Me
      perspective on extern
        props (Description, DomeinFileName) verbs (Consult)
