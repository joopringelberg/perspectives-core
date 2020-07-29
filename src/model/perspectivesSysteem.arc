-- Copyright Joop Ringelberg and Cor Baars 2019
domain: System
  use: sys for model:System
  use: cdb for model:Couchdb
  use: ser for model:Serialise

  case: TrustedCluster
    external:
      property: Naam (mandatory, functional, String)
      view: Kaartje (Naam)
    user: ClusterGenoot (not mandatory, not functional) filledBy: User
      property: Url (mandatory, functional, String)
      view: Adressering (Url, Voornaam)
      perspective on: ClusterGenoot (Adressering) Consult

  case: PerspectivesSystem
    external:
      aspect: sys:RootContext$External
      property: ModelOphaalTeller (mandatory, functional, Number)
    aspect: sys:RootContext
    indexed: sys:MySystem

    context: TheTrustedCluster (not mandatory, functional) filledBy: TrustedCluster

    user: User (mandatory, functional)
      property: Achternaam (mandatory, not functional, String)
      property: Voornaam (mandatory, not functional, String)
      property: Channel = (binder Initiator union binder ConnectedPartner) >> context >> extern >> ChannelDatabaseName
      indexed: sys:Me
      view: VolledigeNaam (Voornaam, Achternaam)
      perspective on: User
      perspective on: ModelsInUse
      perspective on: IndexedContextOfModel
      perspective on: RootUsers

    context: IndexedContextOfModel = ModelsInUse >> binding >> context >> IndexedContext

    thing: RootUsers = IndexedContexts >> binding >> context >> RootUser

    context: Channels = User >> (binder Initiator union binder ConnectedPartner) >> context >> extern

    context: Modellen = callExternal cdb:Models() returns: Model$External

    --IndexedContexts should be bound to Contexts that share an Aspect and that Aspect should have a name on the External role.
    context: IndexedContexts (not mandatory, not functional) filledBy: sys:RootContext

    context: ModelsInUse (not mandatory, not functional) filledBy: Model

    bot: for User
      -- This rule creates an entry in IndexedContexts if its model has been taken in use.
      perspective on: UnconnectedIndexedContext
        if exists UnconnectedIndexedContext then
          bind object to IndexedContexts

      -- If the user has removed the model, this bot will clear away the corresponding entry in IndexedContexts.
      perspective on: DanglingIndexedContext
        if exists DanglingIndexedContext then
          remove object

    context: UnloadedModel = filter ModelsInUse with not available (binding >> context)
    -- An entry in IndexedContexts is dangling if its model is not in use.
    context: DanglingIndexedContext = filter IndexedContexts with not exists binding >> binder IndexedContext >> context >> extern >> binder ModelsInUse
    -- On moving a model to ModelsInUse for the second time, the bot with the perspective on UnloadedModel will not work. We need a third rule for that.
    -- An UnconnectedIndexedContext is model in use that has no entry in IndexedContexts.
    context: UnconnectedIndexedContext = ModelsInUse >> binding >> context >> IndexedContext >> filter binding with not exists binder IndexedContexts

  case: PhysicalContext
    user: UserWithAddress
      -- The public URL of the PDR of the UserWithAddress.
      property: Host (not mandatory, functional, String)
      -- The port where Couchdb listens.
      property: Port (not mandatory, functional, Number)
      -- The public URL of the RelayServer of the UserWithAddress
      property: RelayHost (not mandatory, functional, String)
      -- The port where Couchdb listens on the RelayServer.
      property: RelayPort (not mandatory, functional, String)

  -- A Channel is shared by just two users.
  case: Channel
    external:
      property: ChannelDatabaseName (mandatory, functional, String)
    aspect: sys:PhysicalContext
    user: Initiator filledBy: sys:PerspectivesSystem$User
      aspect: sys:PhysicalContext$UserWithAddress
      perspective on: ConnectedPartner
      perspective on: Initiator
    user: ConnectedPartner filledBy: sys:PerspectivesSystem$User
      -- The public URL of the PDR of the partner.
      aspect: sys:PhysicalContext$UserWithAddress
      perspective on: Initiator
      perspective on: ConnectedPartner
    user: Me = filter (Initiator union ConnectedPartner) with binds sys:Me
    user: You = filter (Initiator union ConnectedPartner) with not binds sys:Me

  case: Model
    external:
      property: Name (mandatory, functional, String)
      property: Description (mandatory, functional, String)
      property: Url (mandatory, functional, String)
    user: Author (not mandatory, functional) filledBy: User
      perspective on: External
    context: IndexedContext (mandatory, functional) filledBy: sys:RootContext
      property: Name (mandatory, functional, String)
    thing: IndexedRole (not mandatory, not functional)
      property: Name (mandatory, functional, String)

  case: RootContext
    external:
      property: Name (mandatory, functional, String)
    user: RootUser filledBy: sys:PerspectivesSystem$User

  case: Invitation
    external:
      property: IWantToInviteAnUnconnectedUser (not mandatory, functional, Boolean)
      property: SerialisedInvitation (not mandatory, functional, String)
      property: Message (not mandatory, functional, String)
    user: Guest = sys:Me
      perspective on: Invitee
    user: Invitee (mandatory, functional) filledBy: Guest
      perspective on: Inviter
      perspective on: PrivateChannel
      -- Invitee needs to see the Channel's Initiator in order to access Host and Port.
      perspective on: ChannelInitiator
      perspective on: External
    bot: for Invitee
      perspective on: Invitee
        if (exists Invitee) and (exists PrivateChannel) then
          -- bind object to ConnectedPartner in PrivateChannel >> binding >> context
          callEffect ser:AddConnectedPartnerToChannel( object, PrivateChannel >> binding >> context )
    bot: for Guest
      perspective on: PrivateChannel
        if exists PrivateChannel then
          callEffect ser:CreateCopyOfChannelDatabase( PrivateChannel >> ChannelDatabaseName )
    user: ChannelInitiator = PrivateChannel >> binding >> context >> Initiator
    user: Inviter (mandatory, functional) filledBy: sys:PerspectivesSystem$User
      perspective on: PrivateChannel
    bot: for Inviter
      perspective on: External
        if extern >> IWantToInviteAnUnconnectedUser and exists (extern >> Message) then
          -- Creates a Channel, binds it to PrivateChannel.
          callEffect ser:AddChannel()
          SerialisedInvitation = extern >> callExternal ser:SerialiseFor( filter context >> contextType >> roleTypes with specialisesRoleType model:System$Invitation$Invitee ) returns: String
    context: PrivateChannel (not mandatory, functional) filledBy: Channel
