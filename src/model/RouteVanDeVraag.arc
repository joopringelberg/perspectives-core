-- Route van de Vraag - Copyright Joop Ringelberg 2023

domain model://joopringelberg.nl#Route
  use sys for model://perspectives.domains#System
  use psys for model://perspectives.domains#System$PerspectivesSystem -- LET OP: moet hier een $ achter?
  use rvdv for model://joopringelberg.nl#Route

  -------------------------------------------------------------------------------
  ---- OPZETTEN
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- We must first create the context and then later bind it.
          -- If we try to create and bind it in a single statement, 
          -- we find that the Installer can just create RootContexts
          -- as they are the allowed binding of StartContexts.
          -- As a consequence, no context is created.
          routeapp <- create context RouteApp
          indexedcontext <- create role IndexedContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind routeapp >> extern to StartContexts in sys:MySystem
          Name = "Route van de Vraag App" for routeapp >> extern
          bind_ routeapp >> extern to indexedcontext
          IndexedContexts$Name = routeapp >> indexedName for indexedcontext

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (rvdv:MyRouteApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (rvdv:MyRouteApp >> extern)
      in
        remove context indexedcontext
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer


  -------------------------------------------------------------------------------
  ---- ROLLEN IN DIT DOMEIN
  -------------------------------------------------------------------------------
  thing Locatie
    property Straat (String)
    -- TODO: voeg pattern toe
    property Huisnummer (String)
    -- TODO: voeg pattern toe
    property Postcode (String)
    property Plaats (String)

  -- NOTA BENE. Verplaats naar System.
  user Persoon filledBy psys:User
    -- psys:User$FirstName
    -- psys:User$LastName
    aspect Locatie
    property Geslacht (String)
      enumeration ("Man", "Vrouw", "Anders")
    property Mail (Email)
    -- TODO: voeg pattern toe
    property BSN (Number)

  thing FinancieringsBasis

  thing WMO
    aspect FinancieringsBasis
  thing PGB
    aspect FinancieringsBasis
  thing WLZ
    aspect FinancieringsBasis
  thing Overig
    aspect FinancieringsBasis
    property Toelichting 

  case RouteApp
    indexed rvdv:MyRouteApp
    aspect sys:RootContext
    external
      aspect sys:RootContext$External

    user Betrokkene = sys:Me
      perspective on MijnRoute
        props (Organisatie) verbs (Consult)
      perspective on MijnCaseLoad
        props (Voornaam, Achternaam) verbs (Consult)
      perspective on MijnIntakes
        props (Voornaam, Achternaam) verbs (Consult)
      perspective on MijnVoorzieningen
        props (Naam) verbs (Consult)
      perspective on MijnPlekken
        props (Naam) verbs (Consult)

    context MijnRoute = Betrokkene >> fills Route$Deelnemer >> context >> extern

    context MijnCaseLoad = Betrokkene >> fills Route$Ondersteuner >> context >> extern

    context MijnIntakes = Betrokkene >> fills Route$Ambassadeur >> context >> extern

    context MijnPlekken = Betrokkene >> fills Verantwoordelijke >> context >> extern

    context MijnVoorzieningen = (MijnRoute union MijnCaseLoad union MijnIntakes) fills Routes >> context >> extern

  -- De organisatie is de overkoepeling van Locaties, met medewerkers enzovoort.
  party Organisatie
    extern
      property Naam (String)
      aspect Locatie
    
    -- Dit zijn personen die niet ook een gebruiker van Perspectives zijn.
    user NietGebruikers
      aspect Persoon

    -- NOTA BENE. Vul een Werknemers instantie met een NietGebruikers instantie als de betreffende werknemer geen Perspectives gebruiker is.
    -- Het zou mooi zijn als CreateAndFill daar ook op werkt.
    -- Alternatief: maak een Action.
    user Werknemers filledBy Persoon

    user Vrijwilligers filledBy Persoon

    user Coordinator filledBy Persoon
      perspective on Locaties
        only (CreateAndFill, Remove)
        props (Naam) verbs (SetPropertyValue)
      perspective on Werknemers
        only (CreateAndFill, Remove)
        -- TODO maak een view voor Persoon en laat Coordinator die geheel onderhouden.
        props (Voornaam, Achternaam)
      perspective on Vrijwilligers
        only (CreateAndFill, Remove)
        -- TODO maak een view voor Persoon en laat Coordinator die geheel onderhouden.
        props (Voornaam, Achternaam)
      

    context Routes (relational) filledBy Route

    context Locaties filledBy Locatie

  -- Een Locatie biedt Bijeenkomsten van een bepaald type.
  case Locatie
    extern 
      property Naam (String)
      aspect Locatie
    
    context Organisatie = extern >> fills Locaties >> context

    user Verantwoordelijke filledBy Organisatie$Werknemer
      perspective on Bijeenkomsten
        only (CreateAndFill, Remove)
        props (Vanaf, Tot) verbs (SetPropertyValue)
        props (AantalDeelnemers) verbs (Consult)
    
    thing Bijeenkomsten filledBy Bijeenkomst

  -- Een route is de persoonlijke weg die een deelnemer af wil leggen.
  case Route
    external
      property Datum (DateTime)
      property Plaats (String)
      property Voornaam = context >> Deelnemer >> FirstName
      property Achternaam = context >> Deelnemer >> LastName
      property Organisatie = fills Routes >> context >> extern
    
    user Deelnemer filledBy Persoon
      property Noodtelefoon (String)
      perspective on Bijeenkomsten

    user Ambassadeur filledBy Organisatie$Werknemer

    user Ondersteuner filledBy Organisatie$Werknemer

    thing Financiering filledBy FinancieringsBasis

    -- Kan ook op externe rol.
    thing Route
      property WelkomGesprek (String)
      property Startvraag (String)
      property VraagVerheldering (String)

    context Bijeenkomsten = Deelnemer >> fills Deelname$Deelnemer >> context >> extern >> fills Deelnames >> context >> extern

    thing Evaluaties (relational)
      property Datum (DateTime)
      property Verslag (String)

  -- Een workshop is een bepaald type activiteit voor deelnemers.
  case Workshop
  -- Een Bijeenkomst vindt plaats in het kader van een Workshop.
  activity Bijeenkomst
    external
      property Vanaf (DateTime)
      property Tot (DateTime)
      property AantalDeelnemers = context >> Deelnames >>= count
    user Initiator = extern >> fills Bijeenkomsten >> context >> Verantwoordelijke
      perspective on Leider
      perspective on Vrijwilliger
      perspective on Deelnames
        props (Voornaam, Achternaam, Vraag) verbs (Consult)
    user Leider filledBy Organisatie$Werknemer
      -- Nodig voor de action Registreer.
      perspective on Deelnames >> binding >> context >> Deelnemer
        only (CreateAndFill)
      perspective on Deelnames
        only (CreateAndFill, Remove)
        props (Voornaam, Achternaam, Vraag) verbs (Consult)

      -- Let op. Hier ontstaat een hele grote tabel. Liever wil je hier een veld met autocomplete. Dat is een nieuwe widget.
      perspective on PotentiëleDeelnemers
        action Registreer
          letA
            deelname <- create context Deelnames -- bound to ? -- moet erachter
          in
            bind origin to Deelnemer in deelname
    user Vrijwilligers (relational) filledBy Organisatie$Vrijwilliger

    user Bezoeker (relational) filledBy Deelnemer
      perspective on Leider
      perspective on Vrijwilligers
      perspective on extern
      perspective on Locatie

    context Locatie = extern >> fills Bijeenkomsten >> context 

    user PotentiëleDeelnemers = Locatie >> Organisatie >> Routes >> Deelnemer
