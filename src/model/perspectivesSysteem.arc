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
      -- LET OP: dit zijn er dus heel veel!
      property: Channel = binder ConnectedPartner >> context >> extern >> ChannelDatabaseName
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
      perspective on: UnloadedModel
        if exists UnloadedModel then
          callEffect cdb:AddModelToLocalStore( object >> binding >> Url )
          bind object >> binding >> context >> IndexedContext >> binding to IndexedContexts
      perspective on: DanglingIndexedContext
        if exists DanglingIndexedContext then
          remove object
      perspective on: UnconnectedIndexedContext
        if exists UnconnectedIndexedContext then
          bind object to IndexedContexts
    context: UnloadedModel = filter ModelsInUse with not available (binding >> context)
    context: DanglingIndexedContext = filter IndexedContexts with not exists binding >> binder IndexedContext >> context >> extern >> binder ModelsInUse
    -- On moving a model to ModelsInUse for the second time, the bot with the perspective on UnloadedModel will not work. We need a third rule for that.
    context: UnconnectedIndexedContext = filter (ModelsInUse >> binding >> context >> IndexedContext >> binding) with not exists binder IndexedContexts

  -- A Channel is shared by just two users.
  case: Channel
    external:
      property: ChannelDatabaseName (mandatory, functional, String)
    user: ConnectedPartner filledBy: sys:PerspectivesSystem$User
      -- The public URL of the PDR of the partner.
      property: Host (not mandatory, functional, String)
      -- The port where Couchdb listens.
      property: Port (not mandatory, functional, Number)
      -- The public URL of the RelayServer of the partner
      property: RelayHost (not mandatory, functional, String)
      -- The port where Couchdb listens on the RelayServer.
      property: RelayPort (not mandatory, functional, String)

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
      perspective on: Channel
    user: Inviter (mandatory, functional) filledBy: sys:PerspectivesSystem$User
    bot: for Inviter
      perspective on: External
        if extern >> IWantToInviteAnUnconnectedUser then
          SerialisedInvitation = extern >> callExternal ser:SerialiseFor( filter context >> contextType >> roleTypes with specialisesRoleType model:System$Invitation$Invitee ) returns: String
    thing: Channel (not mandatory, functional) filledBy: Channel
