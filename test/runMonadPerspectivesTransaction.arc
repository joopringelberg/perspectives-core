domain: Test
  use: sys for model:System

  case: TestCaseContextDelta_context
    user: Self filledBy: sys:PerspectivesSystem$User
    thing: SourceRole filledBy: FillerRole
    thing: SomeRole filledBy: FillerRole
    thing: FillerRole
    bot: for Self
      perspective on: Self
        if not exists SomeRole then
          bind SourceRole >> binding to SomeRole
      perspective on: RoleToInspect
        if exists SomeRole then
          Flag = true
    thing: RoleToInspect
      property: Flag (not mandatory, functional, Boolean)

  case: TestCaseContextDelta_rol
    user: UberSelf filledBy: sys:PerspectivesSystem$User
    bot: for UberSelf
      perspective on: UberSelf
        if not (exists NestedContext) then
          bind SourceRole >> binding to NestedContext
    thing: AnotherRole
      property: Prop3 (mandatory, functional, Boolean)
    context: NestedContext filledBy: SubCase
    context: SourceRole filledBy: SubCase
    case: SubCase
      user: Self filledBy: sys:PerspectivesSystem$User
      bot: for Self
        perspective on: RoleToInspect
          -- geeft een leeg resultaat! Vermoedelijk leggen 'bind' en 'bind_' de omgekeerde administratie niet aan.
          if extern >> binder NestedContext >> context >> AnotherRole >> Prop3 then
            Flag = true
      thing: RoleToInspect
        property: Flag (not mandatory, functional, Boolean)

  case: TestCaseRoleDelta_binding
    user: Self filledBy: sys:PerspectivesSystem$User
    bot: for Self
      perspective on: Self
        if true then
          bind_ BindingRole to BinderRole
      perspective on: BinderRole
        if exists (BinderRole >> binding) then
          Flag = true
    thing: BindingRole (not mandatory, functional)
    thing: BinderRole (not mandatory, functional) filledBy: BindingRole
      property: Flag (not mandatory, functional, Boolean)

  case: TestCaseRoleDelta_binder
    user: UberSelf filledBy: sys:PerspectivesSystem$User
    bot: for UberSelf
      perspective on: UberSelf
        if (not exists NestedContext2 >> binding) and (exists SourceRole2 >> binding) then
          bind_ SourceRole2 >> binding to NestedContext2
    thing: AnotherRole
      property: Prop4 (mandatory, functional, Boolean)
    context: NestedContext2 (not mandatory, functional) filledBy: SubCase2
    context: SourceRole2 (not mandatory, functional) filledBy: SubCase2
    case: SubCase2
      user: Self filledBy: sys:PerspectivesSystem$User
      bot: for Self
        perspective on: RoleToInspect
          if extern >> binder NestedContext2 >> context >> AnotherRole >> Prop4 then
            Flag = true
      thing: RoleToInspect
        property: Flag (not mandatory, functional, Boolean)

  case: TestCasePropertyDelta
    user: Self filledBy: sys:PerspectivesSystem$User
    bot: for Self
      perspective on: ARole
        if not exists ARole >> Prop4 then
          Prop4 = true
      perspective on: RoleToInspect
        if ARole >> Prop4 then
          Flag = true
    thing: ARole (not mandatory, functional)
      property: Prop4 (not mandatory, functional, Boolean)
    thing: RoleToInspect
      property: Flag (not mandatory, functional, Boolean)

  case: TestCaseInCouchdb
    user: Self filledBy: sys:PerspectivesSystem$User
    thing: ARole
      -- In the test, we will load the example in Couchdb and then unload.
      -- After that, we will set Prop to true 'by hand'.
      -- This causes the instance usr:MyTestCase to be loaded, but not the subcontext     
      -- usr:MySubcase (to which the rule of SubCase3 applies)
      -- The PropertyDelta must 'finger' the trigger for the bot on AnotherRole
      -- in SubCase3, causing usr:MySubcase to be loaded in memory.
      property: Prop (not mandatory, functional, Boolean)
    context: NestedContext3 filledBy: SubCase3
    case: SubCase3
      user: Self filledBy: sys:PerspectivesSystem$User
      bot: for Self
        perspective on: RoleToInspect
          if extern >> binder NestedContext3 >> context >> ARole >> Prop then
            Flag = true
      thing: RoleToInspect
        property: Flag (not mandatory, functional, Boolean)
