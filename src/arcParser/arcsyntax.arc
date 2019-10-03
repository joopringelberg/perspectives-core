model:System$TheSystem = sys:

domain: Feest
  thing: Wens (mandatory, functional)
    property: Naam (mandatory, not functional, String)
    property: Bedrag (mandatory, not functional, Number)
    view: ViewOpWens (Naam, Bedrag)
  user: Gast (not mandatory, not functional) filledBy sys:User
    property: Naam (mandatory, functional, String)
    property: WellBehaved = Wens > Bedrag
    view: ViewOpGast
    perspective on: Wens (viewOpWens): Consults, Changes
    perspective on: Gast
      Consults with ViewOpGast
      Changes with AnotherView
      Binds with AnotherView
        indirectObject: ?AnotherRole (ViewOnAnotherRole)
        subjectView: ViewOpGast
  bot: for Gast
    perspective on: Wens (viewOpWens): Deletes
  user: Eregast = Gast where blabla
  context: partijtje (not mandatory, functional) filledBy VoetbalWedstrijd

  case : VoetbalWedstrijd
