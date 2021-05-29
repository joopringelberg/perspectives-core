domain Test

  case Case1
    thing RoleA filledBy RoleB
    thing RoleB
    thing Computed = RoleA >> binding

  case Case2
    thing RoleC filledBy RoleD
    thing RoleD
    thing Computed = RoleD >> binder RoleC

  case Case3
    thing RoleE
      property Prop1 (mandatory, Boolean)
    thing Computed = filter RoleE with Prop1

  case Case4
    thing RoleF filledBy RoleG
    thing RoleG
      property Prop1 (mandatory, Boolean)
    thing Computed = filter RoleF with (binding >> Prop1)

  case Case5
    thing RoleH filledBy RoleI
    thing RoleI
      property Prop1 (mandatory, Boolean)
    thing Computed = filter RoleH with not binding >> Prop1
