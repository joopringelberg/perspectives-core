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
      -- A perspective on Plane is not necessary (other than for properties of Plane
      -- itself, or to add verbs)
      -- As a Driver, Pilot will have a perspective on Vehicle.
      -- We expect this perspective will be contextualised, so Pilot will create 
      -- and see a Plane rather than a Vehicle.
    
    thing Plane
      aspect ta:Transport$Vehicle
