domain: Feest
  --properties: AantalGasten (mandatory, functional) Number
  thing: Wens (mandatory, functional)
    property: Naam (mandatory, not functional, String)
    -- Correct errors
    property : Bedrag (mandatory, not functional, Number)
    -- Correct errors
    view: ViewOpWens (Naam, Bedrag)
  user: Gast (not mandatory, not functional) filledBy: sys:User
    property: Naam (mandatory, functional, String)
    view: ViewOpGast (Naam)
    perspective on: Wens (ViewOpWens): Consult, Change
    property: WellBehaved = Wens > Bedrag
    perspective on : Gast
      Consult with ViewOpGast
      Change with AnotherView
      Bind with AnotherView
        indirectObject: AnotherRole (ViewOnAnotherRole)
        subjectView: ViewOpGast
  bot: for Gast
    perspective on: Wens (ViewOpWens): Delete
  user: Eregast = Gast where blabla
  context: Partijtje (not mandatory, functional) filledBy: VoetbalWedstrijd
