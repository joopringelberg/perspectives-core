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
      property: ModelOphaalTeller (mandatory, functional, Number)
    context: TheTrustedCluster (not mandatory, functional) filledBy: TrustedCluster
    user: User (mandatory, functional)
      property: Achternaam (mandatory, not functional, String)
      property: Voornaam (mandatory, not functional, String)
      view: VolledigeNaam (Voornaam, Achternaam)
    -- Het type van ModellenM bepalen we met de clause 'returns:'
    context: Modellen = callExternal cdb:Models() returns: Model$External
    thing: IndexedContexts (not mandatory, not functional)
    context: ModelsInUse (not mandatory, not functional) filledBy: Model
    context: UnloadedModel = filter ModelsInUse with not available (binding >> context)
    context: UnBoundModel = filter (filter ModelsInUse with available binding >> context) with not exists filter (binding >> context >> IndexedContext >> binding) with exists binder IndexedContexts
    bot: for User
      perspective on: UnloadedModel
        if exists UnloadedModel then callEffect cdb:AddModelToLocalStore( UnloadedModel >> Url )
      perspective on: IndexedContexts
        if exists UnBoundModel then
          bind UnBoundModel >> binding >> context >> IndexedContext >> binding to IndexedContexts

  case: Model
    external:
      property: Name (mandatory, functional, String)
      property: Description (mandatory, functional, String)
      property: Url (mandatory, functional, String)
    user: Author (not mandatory, functional) filledBy: User
    thing: IndexedContext (mandatory, functional)
