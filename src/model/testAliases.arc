-- Copyright Joop Ringelberg and Cor Baars, 2022
-- A model to test the contextualisation of queries.
-- Test for yourself by creating instances and checking the 'Fails' conditions
-- described below.

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
        -- Fails if no alternatives are offered.
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
          -- Fails if on opening a Transport created as a Flight, you are not in the Pilot role.
          bind sys:Me to Driver in binding >> context

  case Transport
    external 
      property Name (String)

    user Driver filledBy sys:PerspectivesSystem$User
      property VehicleName = context >> Vehicle >> Name
      perspective on Vehicle
        only (Create)
        props (Name) verbs (SetPropertyValue)
        props (NumberOfPassengers) verbs (Consult)
      perspective on Driver
        props (VehicleName, Voornaam, Achternaam) verbs (Consult)
      perspective on extern
        props (Name) verbs (SetPropertyValue)
      perspective on Schedules
        only (Remove, CreateAndFill, Create)
        props (VehicleName) verbs (Consult)

    thing Vehicle
      property Name (String)
      property NumberOfPassengers (Number)

    context Schedules (relational) filledBy Schedule

    case Schedule
      external
        -- In the query below, the step `binder Schedules` uses the generic name.
        -- In a Flight instance, there will be no instances of Schedules but
        -- FlightSchedules instead.
        -- Successful contextualisation of this query will retrieve those instances.
        -- TESTS: FILLS STEP CONTEXTUALISATION
        -- Create an instance of Flight and an instance of FlightSchedule. 
        -- Give the Plane a name.
        -- The test fails if the FlightSchedule does not show this name.
        property VehicleName = context >> extern >> binder Schedules >> context >> Vehicle >> Name
      
      -- TESTS: FILLS STEP CONTEXTUALISATION
      -- Create an instance of Flight and an instance of FlightSchedule. 
      -- The test fails if you cannot open the FlightSchedule (it will be because there is no Viewer instance).
      user Viewer = extern >> binder Schedules >> context >> Driver
        perspective on extern
          props (VehicleName) verbs (Consult)
    
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
      -- Fails if the screen for Pilot doesn't have a Plane tab.
      --
      -- Pilot acquires the calculated property VehicleName from Driver.
      -- This query should be contextualised when run, so Pilot can actually 
      -- see the name of his plane. 
      -- TESTS: ROLE STEP CONTEXTUALISATION
      -- Fails if Plane does have a value for the Name property,
      -- but Pilot does not have a VehicleName value.
    
    thing Plane
      aspect ta:Transport$Vehicle
    
    context FlightSchedules
      aspect ta:Transport$Schedules

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
    -- Fails if Chauffeur does not have a Vehicle tab or an External tab.
    aspect thing ta:Transport$Vehicle

  case CarWithoutVehicle
    aspect ta:Transport
    -- Because we have NOT added the aspect role Vehicle, we expect Chauffeur just
    -- to have a perspective on himself and the external role.
    -- TESTS: PERSPECTIVE ADDITION ON ASPECT ROLE
    -- Fails if Chauffeur has more tabs.
    user Chauffeur
      aspect ta:Transport$Driver
    
  case ShippingVoyage
    aspect ta:Transport
      user Captain
        aspect ta:Transport$Driver
        perspective on Ship
        -- The Driver can create a Vehicle, but not remove it.
        -- Captain can remove a Ship.
        -- TESTS: ROLE VERB ADDITION
        -- The test fails if you cannot remove the Ship from a ShippingVoyage.
        -- As a corrolary, we don't expect you to be able to remove a Plane from a Flight.
          only (Remove)
          -- The Driver can consult the number of passengers, but not change it.
          -- The Captain can.
          -- TESTS: PROPERTY VERB ADDITION
          -- Fails if the Captain cannot set it.
          props (NumberOfPassengers) verbs (SetPropertyValue)
      
      thing Ship
        aspect ta:Transport$Vehicle