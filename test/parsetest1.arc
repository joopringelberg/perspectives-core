Context : Domain : Feest
  Role : RoleInContext : Wens
    Mandatory : True
    Functional : False
    Property: StringProperty : Naam
      Mandatory : True
      Functional : False
    Property : NumberProperty : Bedrag
      Mandatory : True
      Functional : False
    View : View : ViewOpWens
      Property : PropertyRef : Naam
      Property : PropertyRef : Bedrag
    View : View : AnotherView
  Agent : UserRole : Gast
    Mandatory : False
    Functional : False
    Property : StringProperty : Naam
      Mandatory : True
      Functional : True
    View : View : ViewOpGast
    Perspective : Perspective : PerspectiveOpWens
      ObjectRef : Wens
      View : DefaultObjectViewRef : ViewOpWens
      Action : Consults : ConsultsWens
      Action : Changes : ChangesWens
        View : ObjectViewRef : AnotherView
        IndirectObjectRef : Wens
