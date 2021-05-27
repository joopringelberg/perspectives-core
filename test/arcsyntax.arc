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
    property AantalGasten (mandatory, Number)
  thing Wens (mandatory)
    state DureWens = Wens >> Bedrag > 10
      on exit
        notify Gast "Hello {Gast >> FirstName}!"
    property Naam (mandatory, String)
    property  Bedrag (Number)
    aspect pre:MyAspect$MyAspectRole
    view ViewOpWens (Naam, Bedrag)
  user Gast filledBy sys:User
    aspect sys:User
    indexed TheGuest
    on exit of SomeState
      notify GoedeGast "Goodbye {user >> FirstName}."
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
    view AnotherView (Unspecified)
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
    on entry of DureWens
      do
        letA
            a <- createRole Gast
        in
          Bedrag = 10

  context Partijtje filledBy VoetbalWedstrijd
    property VoorBigBrother = Datum > '1995-12-17'
    -- comment
    property Datum (mandatory, DateTime)
  -- A nested context
  activity EzeltjePrik
    indexed MyEzel
