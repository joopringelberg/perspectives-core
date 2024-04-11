// Simplified Birthday Party domain model focusing on roles and perspectives

domain BirthdayParty

 

  // Main context for the birthday party

  case PartyPlanning

 

    // Role definitions focusing on properties and perspectives without states

 

    // The individual whose birthday is being celebrated

    user BirthdayPerson

      property Name (String)

      property Age (Number)

      property BirthdayDate (Date)

 

    // Individual responsible for organizing the party

    user Organizer

      property Name (String)

      // Organizer's perspective on the birthday person, guests, venue, and catering

      perspective on BirthdayPerson

        view BasicInfo (Name, BirthdayDate) verbs (Consult)

      perspective on Guest

        view GuestList (Name, RSVP) verbs (Consult, Update)

      perspective on Venue

        view VenueSelection verbs (Consult, Update)

      perspective on Catering

        view CateringDetails verbs (Consult, Update)

 

    // Guests invited to the birthday party

    user Guest

      property Name (String)

      property RSVP (Boolean)

      // Guest's perspective on the party details

      perspective on Venue

        view VenueInfo (Name, Address) verbs (Consult)

      perspective on Entertainment

        view EntertainmentType (Type) verbs (Consult)

      perspective on Catering

        view MenuOptions (Menu) verbs (Consult)

 

    // Venue for the party

    thing Venue

      property Name (String)

      property Address (String)

      property BookingDate (Date)

      property Capacity (Number)

 

    // Entertainment options

    thing Entertainment

      property Type (String)

      property Booked (Boolean)

 

    // Catering details

    thing Catering

      property Menu (String)

      property DietaryRestrictions (String)

 

  // Party planning tasks and activities simplified without states

  activity PlanningTasks

    // Define roles or properties here if necessary for organizing planning tasks