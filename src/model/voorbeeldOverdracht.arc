-- Dit is de gekwalificeerde naam van het model. De lokale naam is natuurlijk "VoorbeeldOverdracht".
-- De namespace van 'onze' modellen is altijd model://perspectives.domains.
domain model://perspectives.domains#VoorbeeldOverdracht
  -- Dit is puur gemak: een verkorting die je als prefix kan gebruiken, substitueer de verlenging in gedachten.
  use sys for model://perspectives.domains#System
  use vo for model://perspectives.domains#VoorbeeldOverdracht

  -- LET OP: INDENTERING TEN OPZICHTE VAN DOMAIN!
  -- De root context. Die wordt zichtbaar op het tabblad "Start contexts".
  case MyApp
    -- Declareer de indexed name. Die kun je dus intikken in de user interface.
    indexed vo:MyApp
    -- Dit is nodig om ervoor te zorgen dat MyApp als root context (dus zonder inbedding) geaccepteerd wordt.
    aspect sys:RootContext

    -- Calculated user role. Dit is een tamelijk vast patroon: anders kun je de app niet openen!
    user Admin = sys:Me
      perspective on MyContexts
        -- andere syntax is mogelijk, waarmee je werkwoorden kunt in- of uitsluiten. Komt later wel.
        all roleverbs
        props (Name) verbs (SetPropertyValue)
      perspective on MyContexts >> binding >> context >> A
        all roleverbs

  -------------

    -- **Ingebedde contextrol** (niet te verwarren met contextrol!). 
    -- De 'relational' qualifier maakt het een multirol.  
    context MyContexts (relational) filledBy MyContext
      on entry
        do for Admin
          bind Admin to A in binding >> context

  case MyContext
    -- Contextrol, met declaratie van een property.
    external Name (String)

    -- Hier gedefineerd als een functionele rol; voeg (relational) toe om er een multirol van te maken.
    user A filledBy Admin
      perspective on B
        all roleverbs
        props (P) verbs (SetPropertyValue)
    
    -- Hier gedefineerd als een functionele rol; voeg (relational) toe om er een multirol van te maken.
    thing B
      property P (String)