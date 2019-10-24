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
    property: WellBehaved = context > Gast > Binding > Naam
    perspective on : Gast
      Consult with ViewOpGast
      Change with AnotherView
        if SomeProp > AnotherProp
      Bind with AnotherView
        indirectObject: AnotherRole (ViewOnAnotherRole)
        subjectView: ViewOpGast
  user: GoedeGast = filter Gast with WellBehaved
  bot: for Gast
    perspective on: Wens (ViewOpWens): Delete
  user: Eregast = Gast > binding
  bot: for Eregast
    perspective on: Wens
      -- Should generate an Action with a Condition and an Assignment.
      if Wens >> Bedrag > 10 then
        SomeProp = false
  context: Partijtje (not mandatory, functional) filledBy: VoetbalWedstrijd
    property: VoorBigBrother = Datum > '1995-12-17'
    -- comment
    property: Datum (mandatory, functional, DateTime)
  -- A nested context
  activity: EzeltjePrik
