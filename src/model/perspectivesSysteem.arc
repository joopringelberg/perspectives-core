-- Copyright Joop Ringelberg and Cor Baars 2019, 2020, 2021
domain System
  use sys for model:System
  use cdb for model:Couchdb
  use ser for model:Serialise

  case TrustedCluster
    external
      property Naam (mandatory, String)
      view Kaartje (Naam)
    user ClusterGenoot (relational) filledBy User
      property Url (mandatory, String)
      view Adressering (Url, Voornaam)
      perspective on ClusterGenoot
        view Adressering (Consult)

  case PerspectivesSystem
    indexed sys:MySystem
    aspect sys:RootContext
    aspect sys:ContextWithNotification

    external
      aspect sys:RootContext$External
      property ConnectedToAMQPBroker (Boolean)
      property CardClipBoard (String)

    context TheTrustedCluster filledBy TrustedCluster

    user User (mandatory)
      property Achternaam (mandatory, relational, String)
      property Voornaam (mandatory, relational, String)
      property Channel = (binder Initiator either binder ConnectedPartner) >> context >> extern >> ChannelDatabaseName
      indexed sys:Me
      view VolledigeNaam (Voornaam, Achternaam)
      perspective on User
        defaults
      perspective on ModelsInUse
        defaults
      perspective on IndexedContexts
        defaults
      perspective on IndexedContextOfModel
        defaults
      perspective on RootUsers
        defaults
      perspective on Contacts
        props (Voornaam, Achternaam) verbs (Consult)

    user Contacts = filter (callExternal cdb:RoleInstances( "model:System$PerspectivesSystem$User" ) returns sys:PerspectivesSystem$User) with not binds sys:Me

    context IndexedContextOfModel = ModelsInUse >> binding >> context >> IndexedContext

    thing RootUsers = IndexedContexts >> binding >> context >> RootUser

    context Channels = User >> (binder Initiator either binder ConnectedPartner) >> context >> extern

    -- This will become obsolete when we start using model:CouchdbManagement.
    context Modellen = filter callExternal cdb:Models() returns sys:Model$External with not IsLibrary
      view ModelPresentation (Description, Name)

    --IndexedContexts should be bound to Contexts that share an Aspect and that Aspect should have a name on the External role.
    context IndexedContexts (relational) filledBy sys:RootContext
      state Dangles = not exists binding >> binder IndexedContext >> context >> extern >> binder ModelsInUse
        -- If the user has removed the model, this bot will clear away the corresponding entry in IndexedContexts.
        on entry
          do for User
            remove origin

    -- This will become obsolete when we start using model:CouchdbManagement.
    context ModelsInUse (relational) filledBy Model
      property PerformUpdate (Boolean)
      state Update = PerformUpdate
        on entry
          do for User
            callEffect cdb:UpdateModel( Url, ModelIdentification )
            PerformUpdate = false
          notify User
            "Model {ModelIdentification} has been updated."
      state NotInIndexedContexts = exists (binding >> context >> IndexedContext >> filter binding with not exists binder IndexedContexts)
        -- Create an entry in IndexedContexts if its model has been taken in use.
        on entry
          do for User
            bind binding >> context >> IndexedContext >> binding to IndexedContexts
          notify User
            "{ binding >> ModelIdentification }added!"
      view ModelInUsePresentation (Description, Name, PerformUpdate)

    context PendingInvitations = callExternal cdb:PendingInvitations() returns sys:Invitation$External

    thing Databases (mandatory, relational)
      -- Name is one of post, data, models.
      property Name (mandatory, String)
      property Identifier (mandatory, String)

    -- A calculated role representing all available Notifications (from any context).
    thing AllNotifications = callExternal cdb:RoleInstances( "model:System$ContextWithNotification$Notifications" ) returns sys:ContextWithNotification$Notifications

  -- Use this as an aspect in contexts that should store their own notifications.
  case ContextWithNotification
    thing Notifications (relational)
      property Message (String)

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
    user Author filledBy User
      aspect sys:RootContext$RootUser
      perspective on extern
    context IndexedContext (mandatory) filledBy sys:RootContext
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
      property InviterLastName = context >> Inviter >> Achternaam
      state InviteUnconnectedUser = IWantToInviteAnUnconnectedUser and exists Message
        on entry
          do for Inviter
            SerialisedInvitation = callExternal ser:SerialiseFor( filter origin >> context >> contextType >> roleTypes with specialisesRoleType model:System$Invitation$Invitee ) returns String

      view ForInvitee (InviterLastName, Message)

    user Inviter (mandatory) filledBy sys:PerspectivesSystem$User

    user Invitee (mandatory) filledBy Guest
      perspective on Inviter
        defaults
      perspective on extern
        view ForInvitee (Consult)

    -- Without the filter, the Inviter will count as Guest and its bot will fire for the Inviter, too.
    user Guest = filter sys:Me with not boundBy (currentcontext >> Inviter)
      perspective on Invitee
        defaults
