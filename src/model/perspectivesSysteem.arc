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
      property: ModelOphaalTeller (mandatory, verplicht, Number)
      thing: TheTrustedCluster (not mandatory, functional) filledBy TrustedCluster
      user: User (mandatory, functional)
        property: Achternaam (mandatory, not functional, String)
        property: Voornaam (mandatory, not functional, String)
        view: VolledigeNaam (Voornaam, Achternaam)
      -- Het type van ModellenM bepalen we met de clause 'returns:'
      thing: Modellen = apicall "ModellenM" returns: sys:Model
      thing: IndexedContexts (not mandatory, not functional)
      context: ModelsInUse (not mandatory, not functional) filledBy Model

  case: Model
    external:
      property: Auteur (mandatory, functional, String)
    thing: IndexContextTypes (not mandatory, not functional)
