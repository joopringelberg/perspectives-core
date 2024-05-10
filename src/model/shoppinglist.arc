-- Copyright Joop Ringelberg and Cor Baars, 2023
-- A model for a simple shared shopping list

----
---- SETTING UP
----
domain model://perspectives.domains#Shopping@1.0.0
  use sys for model://perspectives.domains#System@1.0.0
  use td for model://perspectives.domains#Shopping@1.0.0

  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          app <- create context ShoppingApp
        in
          bind app >> extern to StartContexts in sys:MySystem
          Name = "Shared Shopping Lists" for app >> extern

  aspect user sys:PerspectivesSystem$Installer

----
---- THE APP
----
-- The entry point (the `application`), available as td:Shopping.
  case ShoppingApp
    indexed td:Shopping
    aspect sys:RootContext

    external
      aspect sys:RootContext$External

    -- Every user can at least add himself to the Housemates.    
    user Initiator = sys:Me
      perspective on Housemates
        only (Create, Fill)

    -- As soon as a user is registered as one of the Housemates, he will take on that 
    -- role on opening the context.
    user Housemates (relational) filledBy sys:TheWorld$PerspectivesUsers
      perspective on ShoppingLists
        only (CreateAndFill)
        props (ToDate, Buyer)

      perspective on NextList
        props (FromDate, ToDate, Buyer)
      
      perspective on Housemates
        verbs (Create, Fill, Remove)
        props (FirstName) verbs (Consult)

    context ShoppingLists (relational) filledBy ShoppingList
      state New = exists binding
        -- A ShoppingList is created with a value for FromDate such that it is 
        -- the EndDate of the previous ShoppingList.
        on entry
          do for Housemates
            FromDate = context >> ShoppingLists >> ToDate >>= maximum
    
    context NextList = filter ShoppingLists with not Done
  
  ----
  ---- SHOPPINGLIST
  ----
  case ShoppingList
    external 
      property FromDate (DateTime)
      property ToDate (DateTime)
      property Done (Boolean)
      property Buyer = context >> Buyer >> FirstName
    
    thing ItemsToBuy (relational)
      property Name (String)
      -- An amount is represented as a string because we don't model its units.
      -- they may be grams, or number of pieces, or liters, etc. If unclear, 
      -- just include the unit name.
      property Amount (String)
      property Note (String)
    
    -- Any HouseMate can contribute items to the list.
    user Contributor = extern >> binder ShoppingLists >> context >> Housemates
      perspective on ItemsToBuy
        only (Create, Remove)
        props (Name, Amount, Note) verbs (SetPropertyValue)
      
      -- Any HouseMate can become the Buyer.
      perspective on Buyer
        only (Create, Fill, RemoveFiller)
        props (FirstName) verbs (Consult)

      perspective on extern
        props (FromDate, ToDate, Done) verbs (Consult)

    user Buyer filledBy Housemates
      perspective on extern
        props (FromDate) verbs (Consult)
        props (ToDate, Done) verbs (SetPropertyValue)
      perspective on ItemsToBuy
        only (Create, Remove)
        props (Name, Amount, Note) verbs (SetPropertyValue)

-- TODO: sluit de lijst als hij voltooid (done) is.
	
  
  context C
		role R filledBy R1
		role CR = R >> filler >> context >> R2
	context C1
		role R2

