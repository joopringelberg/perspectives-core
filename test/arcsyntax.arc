{-
TODO:
  * map properties from AspectRoles to local names?
  * map properties from AspectRoles to properties from the Binding of the role.
  * expression parser.
  * PRODUCT for binding
  * partial qualification: pre:..MyRole
-}
domain: Feest
  use: sys for model:System$System
  use: pre for model:MyAspectModel
  aspect: pre:MyAspect
  external:
    property: AantalGasten (mandatory, functional, Number)
  thing: Wens (mandatory, functional)
    property: Naam (mandatory, not functional, String)
    property : Bedrag (mandatory, not functional, Number)
    aspect: pre:MyAspect$MyAspectRole
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
  -- A nested context
  activity: EzeltjePrik
