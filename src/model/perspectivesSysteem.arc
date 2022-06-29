-- Copyright Joop Ringelberg and Cor Baars 2019, 2020, 2021
domain System
  use sys for model:System
  use cdb for model:Couchdb
  use ser for model:Serialise
  use sensor for model:Sensor
  use util for model:Utilities

  -- Used as model:System$RoleWithId$Id in the PDR code.
  thing RoleWithId
    property Id = callExternal util:RoleIdentifier() returns String

  case PerspectivesSystem
    indexed sys:MySystem
    aspect sys:RootContext
    aspect sys:ContextWithNotification

    state NoCaches = not exists SystemCaches
      on entry
        do for User
          create context Caches bound to SystemCaches

    external
      aspect sys:RootContext$External
      property ConnectedToAMQPBroker (Boolean)
      property CardClipBoard (String)
      property ShowLibraries (Boolean)

      view ShowLibraries (ShowLibraries)

    user User (mandatory)
      property LastName (mandatory, relational, String)
      property FirstName (mandatory, relational, String)
      property Channel = (binder Initiator either binder ConnectedPartner) >> context >> extern >> ChannelDatabaseName
      -- User instances need not have a value for this property. It is used in the PDR to
      -- ensure serialisation of the User role.
      property Id (String)
      indexed sys:Me
      view VolledigeNaam (FirstName, LastName)
      view SyncId (Id)
      perspective on User
        defaults
      perspective on ModelsInUse
        defaults
        --in object state
        action Refresh
          PerformUpdate = true
        action RefreshWithDependencies
          IncludingDependencies = true
          PerformUpdate = true
      perspective on IndexedContexts
        defaults
        action StopUsing
          remove context origin
      perspective on IndexedContextOfModel
        defaults
      perspective on RootUsers
        defaults
      perspective on Contacts
        props (FirstName, LastName) verbs (Consult)
      perspective on External
        view ShowLibraries verbs (Consult, SetPropertyValue)
      perspective on Modellen
        action StartUsing
          callEffect cdb:AddModelToLocalStore( Url )
          bind origin to ModelsInUse in currentcontext
        view Modellen$ModelPresentation verbs (Consult)
      perspective on PendingInvitations
        view ForInvitee verbs (Consult)
      perspective on SystemCaches
        defaults

      screen "Home"
        tab "SystemCaches"
          row
            column
              form SystemCaches
        tab "Manage models"
          row
            table Modellen
              props (Name, Description) verbs (Consult)
          row 
            table ModelsInUse
              props (Name) verbs (Consult)
        tab "Start contexts"
          row
            table IndexedContexts
              props (Name) verbs (Consult)
        tab "User"
          row
            form User
              props (FirstName, LastName) verbs (SetPropertyValue)
          row 
            table Contacts
        tab "Invitations"
          row
            table PendingInvitations
              props (InviterLastName, Message) verbs (Consult)


    user Contacts = filter (callExternal cdb:RoleInstances( "model:System$PerspectivesSystem$User" ) returns sys:PerspectivesSystem$User) with not binds sys:Me

    context IndexedContextOfModel = ModelsInUse >> binding >> context >> IndexedContext

    thing RootUsers = IndexedContexts >> binding >> context >> RootUser

    context Channels = User >> (binder Initiator either binder ConnectedPartner) >> context >> extern

    -- This will become obsolete when we start using model:CouchdbManagement.
    context Modellen = letE
        showlibs <- extern >> ShowLibraries
      in
        filter callExternal cdb:Models() returns sys:Model$External with showlibs or (not IsLibrary)
      view ModelPresentation (Description, Name)

    --IndexedContexts should be bound to Contexts that share an Aspect and that Aspect should have a name on the External role.
    context IndexedContexts (relational) filledBy sys:RootContext

    -- This will become obsolete when we start using model:CouchdbManagement.
    context ModelsInUse (relational) filledBy Model
      property PerformUpdate (Boolean)
      property IncludingDependencies (Boolean)
      property HasBeenInstalled (Boolean)
      --on exit
        --notify User
          --"Model {origin >> binding >> ModelIdentification} has been removed completely."
      state Update = PerformUpdate
        on entry
          do for User
            callEffect cdb:UpdateModel( Url, ModelIdentification, IncludingDependencies )
            PerformUpdate = false
            -- Updating without dependencies is the default.
            IncludingDependencies = false
          notify User
            "Model {ModelIdentification} has been updated."
      state NotInIndexedContexts = (not HasBeenInstalled) and exists (binding >> context >> IndexedContext >> filter binding with not exists binder IndexedContexts)
        -- Create an entry in IndexedContexts if its model has been taken in use.
        on entry
          do for User
            bind binding >> context >> IndexedContext >> binding to IndexedContexts
            HasBeenInstalled = true
          notify User
            "{ binding >> ModelIdentification } added!"
      state Dangles = (not exists binding >> context >> IndexedContext >> binding) and HasBeenInstalled
        -- If the user has removed the App, this automatic action will clear away the corresponding entry in ModelsInuse.
        -- We also remove the Model itself (the entry to the functionality).
        on entry
          do for User
            remove context origin
            callDestructiveEffect cdb:RemoveModelFromLocalStore ( ModelIdentification )
      view ModelInUsePresentation (Description, Name, PerformUpdate)

    context PendingInvitations = callExternal cdb:PendingInvitations() returns sys:Invitation$External

    thing Databases (mandatory, relational)
      -- Name is one of post, data, models.
      property Name (mandatory, String)
      property Identifier (mandatory, String)

    -- A calculated role representing all available Notifications (from any context).
    thing AllNotifications = callExternal cdb:RoleInstances( "model:System$ContextWithNotification$Notifications" ) returns sys:ContextWithNotification$Notifications

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
    user Me = filter (Initiator either ConnectedPartner) with binds sys:Me
    user You = filter (Initiator either ConnectedPartner) with not binds sys:Me

  -- This will become obsolete when we start using model:CouchdbManagement.
  case Model
    aspect sys:RootContext
    external
      aspect sys:RootContext$External
      property Description (mandatory, String)
      property ModelIdentification (mandatory, String)
      property Url (mandatory, String)
      property IsLibrary (mandatory, Boolean)
      view ModelPresentation (Description, ModelIdentification, IsLibrary)
    user Author filledBy User
      aspect sys:RootContext$RootUser
      perspective on extern
    context IndexedContext (mandatory) filledBy sys:RootContext
      -- Dit wordt alleen gebruikt in model:System.
      property Name (mandatory, String)
    thing IndexedRole (relational)
      property Name (mandatory, String)

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
            SerialisedInvitation = callExternal ser:SerialiseFor( filter origin >> context >> contextType >> roleTypes with specialisesRoleType model:System$Invitation$Invitee ) returns String

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
    user Guest = filter sys:Me with not boundBy (currentcontext >> Inviter)
      perspective on Invitee
        only (Fill, Create)
