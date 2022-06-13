-- Copyright Joop Ringelberg and Cor Baars, 2022
-- A model to test the contextualisation of queries.

domain TestAliases
  use sys for model:System
  use ta for model:TestAliases

  -- The model description case.
  case Model
    aspect sys:Model
    external
      aspect sys:Model$External

  case TestAliasesApp
    indexed ta:App
    aspect sys:RootContext
    external
      aspect sys:RootContext$External
    
    user Manager = sys:Me
      perspective on Transports
        -- A choice of specialisations is offered when Manager wants to create a Transport.
        -- TESTS: CONTEXT CHOICE
        defaults
      -- Manager needs this perspective in order to be able to 
      -- create instances of Driver rightfully.
      perspective on Transports >> binding >> context >> Driver
        defaults

    context Transports  (relational) filledBy Transport
      on entry
        do for Manager
          -- With contextualisation of actions, a Pilot will be created 
          -- in a Plane (instead of a Driver).
          -- TESTS: FILLER ROLE SPECIALISATION
          bind sys:Me to Driver in binding >> context

  case Transport
    external 
      property Name (String)
    user Driver filledBy sys:PerspectivesSystem$User
      property VehicleName = context >> Vehicle >> Name
      perspective on Vehicle
        only (Create, Remove)
        props (Name) verbs (SetPropertyValue)
      perspective on Driver
        props (VehicleName, Voornaam, Achternaam) verbs (Consult)
      perspective on extern
        props (Name) verbs (SetPropertyValue)

    thing Vehicle
      property Name (String)
    
  case Flight
    aspect ta:Transport      
    user Pilot
      aspect ta:Transport$Driver
      -- A perspective on Plane is not necessary (other than for properties of Plane
      -- itself, or to add verbs)
      -- As a Driver, Pilot will have a perspective on Vehicle.
      -- We expect this perspective will be contextualised, so Pilot will create 
      -- and see a Plane rather than a Vehicle.
      -- TESTS: PERSPECTIVE ADDITION ON SPECIALISATION
    
    thing Plane
      aspect ta:Transport$Vehicle

  case Car
    aspect ta:Transport
    user Chauffeur
      aspect ta:Transport$Driver
    
    -- TESTS: PERSPECTIVE ADDITION ON ASPECT ROLE
    -- Because we have added the aspect role Vehicle as such, we expect Chauffeur to have 
    -- a perspective on it.
    -- Incidentally, since the external role of Transport is added automatically to 
    -- the external role of Car, we expect Chauffeur to have a perspective on 
    -- that external role as well.
    -- Additionally, notice that the tab in the generated screen should be called "Vehicle" - as
    -- the model provides no alternative.
    aspect thing ta:Transport$Vehicle

  case CarWithoutVehicle
    aspect ta:Transport
    -- Because we have NOT added the aspect role Vehicle, we expect Chauffeur just
    -- to have a perspective on himself and the external role.
    -- TESTS: PERSPECTIVE ADDITION ON ASPECT ROLE
    user Chauffeur
      aspect ta:Transport$Driver
    
