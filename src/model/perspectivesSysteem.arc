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
      perspective on : ClusterGenoot (Adressering) Consult

  case: PerspectivesSystem
    external:
      aspect: sys:NamedContext$External
      property: ModelOphaalTeller (mandatory, functional, Number)
    aspect: sys:NamedContext
    indexed: sys:MySystem
    context: TheTrustedCluster (not mandatory, functional) filledBy: TrustedCluster
    user: User (mandatory, functional)
      property: Achternaam (mandatory, not functional, String)
      property: Voornaam (mandatory, not functional, String)
      -- LET OP: dit zijn er dus heel veel! En tegelijk zijn het alleen de kanalen waar je zelf de initiatiefnemer bent.
      property: Channel = (binder Initiator union binder ConnectedPartner) >> context >> extern >> ChannelDatabaseName
      indexed: sys:Me
      view: VolledigeNaam (Voornaam, Achternaam)
      perspective on: User
    -- TODO: dit is eigenlijk overbodig
    context: Channels filledBy: Channel
    -- Het type van ModellenM bepalen we met de clause 'returns:'
    context: Modellen = callExternal cdb:Models() returns: Model$External
    --IndexedContexts should be bound to Contexts that share an Aspect and that Aspect should have a name on the External role.
    context: IndexedContexts (not mandatory, not functional) filledBy: sys:NamedContext
    context: ModelsInUse (not mandatory, not functional) filledBy: Model
    bot: for User
      -- This rule creates an entry in IndexedContexts if its model has been taken in use for the first time.
      perspective on: UnloadedModel
        if exists UnloadedModel then
          callEffect cdb:AddModelToLocalStore( object >> binding >> Url )
          bind object >> binding >> context >> IndexedContext >> binding to IndexedContexts

      -- If the user has removed the model, this bot will clear away the corresponding entry in IndexedContexts.
      perspective on: DanglingIndexedContext
        if exists DanglingIndexedContext then
          remove object

      -- This rule creates an entry in IndexedContexts if the model is taken in use again.
      perspective on: UnconnectedIndexedContext
        if exists UnconnectedIndexedContext then
          bind object to IndexedContexts

    context: UnloadedModel = filter ModelsInUse with not available (binding >> context)
    -- An entry in IndexedContexts is dangling if its model is no in use.
    context: DanglingIndexedContext = filter IndexedContexts with not exists binding >> binder IndexedContext >> context >> extern >> binder ModelsInUse
    -- On moving a model to ModelsInUse for the second time, the bot with the perspective on UnloadedModel will not work. We need a third rule for that.
    -- An UnconnectedIndexedContext is model in use that has no entry in IndexedContexts.
    context: UnconnectedIndexedContext = (filter ModelsInUse >> binding with available context) >> filter (context >> IndexedContext >> binding) with not exists binder IndexedContexts

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

  case: Model
    external:
      property: Name (mandatory, functional, String)
      property: Description (mandatory, functional, String)
      property: Url (mandatory, functional, String)
    user: Author (not mandatory, functional) filledBy: User
    context: IndexedContext (mandatory, functional) filledBy: sys:NamedContext
      property: Name (mandatory, functional, String)
    thing: IndexedRole (not mandatory, not functional)
      property: Name (mandatory, functional, String)

  case: NamedContext
    external:
      property: Name (mandatory, functional, String)

  case: Invitation
    external:
      property: IWantToInviteAnUnconnectedUser (not mandatory, functional, Boolean)
      property: SerialisedInvitation (not mandatory, functional, String)
    user: Guest = sys:Me
    user: Invitee (mandatory, functional) filledBy: Guest
      perspective on: Inviter
      perspective on: PrivateChannel
      -- Invitee needs to see the Channel's Initiator in order to access Host and Port.
      perspective on: ChannelInitiator
    bot: for Invitee
      perspective on: Invitee
        if exists Invitee then
          -- bind object to ConnectedPartner in PrivateChannel >> binding >> context
          callEffect ser:AddConnectedPartnerToChannel( object, PrivateChannel >> binding >> context )
    user: ChannelInitiator = PrivateChannel >> binding >> context >> Initiator
    user: Inviter (mandatory, functional) filledBy: sys:PerspectivesSystem$User
    bot: for Inviter
      perspective on: External
        if extern >> IWantToInviteAnUnconnectedUser then
          -- Creates a Channel, binds it to PrivateChannel.
          callEffect ser:AddChannel()
          SerialisedInvitation = extern >> callExternal ser:SerialiseFor( filter context >> contextType >> roleTypes with specialisesRoleType model:System$Invitation$Invitee ) returns: String
    context: PrivateChannel (not mandatory, functional) filledBy: Channel
