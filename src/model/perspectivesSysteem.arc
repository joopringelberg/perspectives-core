-- Copyright Joop Ringelberg and Cor Baars 2019
domain: System
  use: sys for model:System

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
    context: TheTrustedCluster (not mandatory, functional) filledBy: TrustedCluster
    user: User (mandatory, functional)
      property: Achternaam (mandatory, not functional, String)
      property: Voornaam (mandatory, not functional, String)
      property: Channel = binder ConnectedPartner >> context >> extern >> ChannelDatabaseName
      view: VolledigeNaam (Voornaam, Achternaam)
    -- Het type van ModellenM bepalen we met de clause 'returns:'
    context: Channels filledBy: Channel
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

  case: Model
    external:
      property: Name (mandatory, functional, String)
      property: Description (mandatory, functional, String)
      property: Url (mandatory, functional, String)
    user: Author (not mandatory, functional) filledBy: User
    context: IndexedContext (mandatory, functional) filledBy: sys:NamedContext

  case: NamedContext
    external:
      property: Name (mandatory, functional, String)
