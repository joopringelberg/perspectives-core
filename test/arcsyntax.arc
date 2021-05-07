{-
TODO
  * map properties from AspectRoles to local names?
  * map properties from AspectRoles to properties from the Binding of the role.
  * expression parser.
  * PRODUCT for binding
  * partial qualification pre:..MyRole
-}
domain Feest
  use sys for model:System$System
  use pre for model:MyAspectModel
  aspect pre:MyAspect
  state Rootstate = true
    perspective on Wens
      perspective of GoedeGast
        all roleverbs
    state BigParty = AantalGasten > 10
  external
    property AantalGasten (mandatory, functional, Number)
  thing Wens (mandatory, functional)
    state DureWens = Wens >> Bedrag > 10
      on exit
        notify Gast Alert
    property Naam (mandatory, String)
    property  Bedrag (functional, Number)
    aspect pre:MyAspect$MyAspectRole
    view ViewOpWens (Naam, Bedrag)
  user Gast filledBy sys:User
    aspect sys:User
    indexed TheGuest
    on exit SomeState
      notify GoedeGast Alert
    state SomeState = SomeProp > AnotherProp
      on entry
        do
          remove Wens
      perspective on Gast
        view AnotherView
          (SetPropertyValue)
    property Naam (String)
    property Unspecified
    property AantalGasten = this >>= sum
    view ViewOpGast (Naam)
    perspective on Wens
      view ViewOpWens (Consult, SetPropertyValue)
    property WellBehaved = context > Gast > Binding > Naam
    perspective on  Gast
      view ViewOpGast (Consult)
      in state SomeState
        view AnotherView (SetPropertyValue)
      all roleverbs
      props (SomeProp, AnotherProp) (SetPropertyValue)
      props (YetAnotherProp) (SetPropertyValue, AddPropertyValue)
  user GoedeGast = filter Gast with WellBehaved
    perspective on Gast >> binding
      only (Create)
    perspective on Eregast
  user Eregast = Gast > binding
    on entry DureWens
      do
        letA
            a <- createRole Gast
        in
          Bedrag = 10

  context Partijtje (functional) filledBy VoetbalWedstrijd
    property VoorBigBrother = Datum > '1995-12-17'
    -- comment
    property Datum (mandatory, functional, DateTime)
  -- A nested context
  activity EzeltjePrik
    indexed MyEzel
