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

    user Driver filledBy sys:TheWorld$PerspectivesUsers
      property VehicleName = context >> Vehicle >> Name

      -- Property License is used to define state HasLicense. 
      -- In specialisation Pilot we add to the Pilots' perspective on Plane (a specialisation of Vehicle)
      -- that is dependent on this (aspect) state.
      property License (String)
      -- In specialisation Pilot we map License to Certification. If property contextualisation works well,
      -- the computed property HasLicense should have value 'true' when we enter a value for Certification.
      property HasALicense = exists License
      state HasLicense = HasALicense
      perspective on Vehicle
        only (Create)
        props (Name) verbs (SetPropertyValue)
        props (NumberOfPassengers) verbs (Consult)
      perspective on Driver
        props (VehicleName, FirstName, LastName, HasALicense) verbs (Consult)
        props (License) verbs (SetPropertyValue)
        action ApproveLicense
          License = License + " Approved!"
      perspective on extern
        props (Name) verbs (Consult, SetPropertyValue)
      perspective on Schedules
        only (Remove, CreateAndFill, Create)
        props (VehicleName) verbs (Consult)
        action CreateASchedule
          create context Schedule bound to Schedules
        -- Action CreateScheduleInSteps has the same effect as CreateASchedule, but creates the role instance
        -- first and then creates a context instance in it.
        -- TESTS: CREATE ROLE CONTEXTUALISATION
        -- TESTS: CREATE_ CONTEXT CONTEXTUALISATION
        -- Test in Flight; fails if the result is not equal to that of CreateASchedule.
        action CreateScheduleInSteps
          letA
            role <- create role Schedules
          in
            create_ context Schedule bound to role
      perspective on Passengers
        props (Name) verbs (Consult, SetPropertyValue)
        only (Create, Remove)
        action CreateAPassenger
          create role Passengers

    thing Vehicle
      property Name (String)
      property NumberOfPassengers (Number)
    
    thing Passengers (relational)
      property Name (String)

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
    aspect sys:ContextWithNotification 
    user Pilot
      aspect ta:Transport$Driver where
        License is replaced by Certification
      aspect sys:ContextWithNotification$NotifiedUser
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
      --
      -- Pilot aquires the action CreateASchedule from Driver
      -- TESTS: ADD ASPECT ACTIONS
      -- The test fails if the actions is not visible on the schedules tab.
      
      property Certification (String)

      -- TESTS: CREATE CONTEXT CONTEXTUALISATION
      -- The test fails if the created schedule doesn't have properties IsInternational and IsCargoFlight.
      perspective on FlightSchedules
        props (IsInternational, IsCargoFlight) verbs (Consult, SetPropertyValue)
      
      -- TESTS: ASPECT STATE DEPENDENT PROPERTY VERBS
      -- Fails: ff Pilot cannot set NumberOfPassengers after he has a value for License.
      in state ta:Transport$Driver$HasLicense
        perspective on Plane
          props (NumberOfPassengers) verbs (Consult, SetPropertyValue)

      on entry of ta:Transport$Driver$HasLicense
        notify Pilot
          "You now have a license!"
    
    thing Plane
      aspect ta:Transport$Vehicle
    
    context FlightSchedules filledBy FlightSchedule
      aspect ta:Transport$Schedules
      property IsInternational (Boolean)
    
    aspect thing sys:ContextWithNotification$Notifications
    
    case FlightSchedule
      aspect ta:Transport$Schedule
      external 
        property IsCargoFlight (Boolean)

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
          props (NumberOfPassengers) verbs (Consult, SetPropertyValue)
        
        perspective on BoatPassenger
          props (Hut) verbs (Consult)
      
      thing Ship
        aspect ta:Transport$Vehicle
      
      thing BoatPassenger
        -- We expect that not an instance of Passengers, but an instance
        -- of BoatPassenger is made by the action CreatePassenger.
        -- TESTS: CREATE ROLE CONTEXTUALISATION
        -- fails if Hut is not visible.
        -- Also, we expect action CreateAPassenger to be available.
        -- TESTS: ADD ASPECT ACTIONS
        -- Fails if the action is not visible on the BoatPassenger tab.
        aspect ta:Transport$Passengers
        property Hut (Number)
        on entry
          do for Captain
            Hut = 0
      
      