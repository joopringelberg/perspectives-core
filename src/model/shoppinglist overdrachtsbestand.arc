
    case ShoppingApp

    user Me = sys:Me
      perspective on Housemates
        only (Create, Fill)

    user Housemates (relational)
      perspective on ShoppingLists
        only (CreateAndFill)
        props (ToDate, FromDate, Done)

      perspective on Housemates
        verbs (Create, Fill, Remove)
        props (FirstName) verbs (Consult)

    context ShoppingLists (relational) filledBy ShoppingList

  case ShoppingList
    external 
      property FromDate (DateTime)
      property ToDate (DateTime)
      property Done (Boolean)
      
    thing ItemsToBuy (relational)
      property Name (String)
      property Amount (String)
      property Note (String)
    
    user HousematesInShoppingLists filledBy Housemates
      perspective on ItemsToBuy
        only (Create, Remove)
        props (Name, Amount, Note) verbs (SetPropertyValue)
      
      -- Any HouseMate can become the Buyer.
      perspective on Buyer
        only (Create, Fill, RemoveFiller)
        props (FirstName) verbs (Consult)

      perspective on extern
        props (FromDate, ToDate, Done) verbs (Consult)

    user Buyer filledBy HousematesInShoppingLists
      perspective on extern
        props (FromDate) verbs (Consult)
        props (ToDate, Done) verbs (SetPropertyValue)
      perspective on ItemsToBuy
        only (Create, Remove)
        props (Name, Amount, Note) verbs (SetPropertyValue)
