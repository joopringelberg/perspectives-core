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
        -- Ideally (but not planned) we expect a choice of specialisations to be offered when Manager wants to create a Transport.
        defaults

    context Transports  (relational) filledBy Transport
      on entry
        do for Manager
          -- With contextualisation of actions, we expect a Pilot to be created in a Plane.
          bind sys:Me to Driver in binding >> context

  case Transport
    user Driver filledBy sys:PerspectivesSystem$User
      property VehicleName = context >> Vehicle >> Name
      perspective on Vehicle
        defaults
      perspective on Driver
        defaults

    thing Vehicle
      property Name (String)
    
  case Flight
    aspect ta:Transport      
    user Pilot
      aspect ta:Transport$Driver
      -- We expect this perspective will be contextualised, so Pilot will create a Plane rather than a Vehicle.
      perspective on Plane
        defaults
    
    thing Plane
      aspect ta:Transport$Vehicle
