domain: Test
  case: TestCase1
    user: Self
    bot: for Self
      perspective on: ARole
        if true then
          Prop1 = false
    thing: ARole
      property: Prop1 (mandatory, functional, Boolean)

  case: TestCase2
    user: Self
    bot: for Self
      perspective on: ARole
        if ARole >> Prop1 then
          Prop1 = false
    thing: ARole
      property: Prop1 (mandatory, functional, Boolean)
  case: TestCase3
    user: Self
    bot: for Self
      perspective on: ARole
        if NestedContext >> binding >> Prop2 then
          Prop1 = false
    thing: ARole
      property: Prop1 (mandatory, functional, Boolean)
    context: NestedContext filledBy: SubCase1
    case: SubCase1
      external:
        property: Prop2 (mandatory, functional, Boolean)

  case: TestCase4
    user: Self
    bot: for Self
      perspective on: ARole
        if NestedContext >> binding >> context >> SubCaseRole1 >> Prop2 then
          Prop1 = false
    thing: ARole
      property: Prop1 (mandatory, functional, Boolean)
    context: NestedContext filledBy: SubCase2
    case: SubCase2
      thing: SubCaseRole1
        property: Prop2 (mandatory, functional, Boolean)
        
  case: TestCase5
    context: NestedContext filledBy: SubCase3
    case: SubCase3
      external:
        property: Prop2 (mandatory, functional, Boolean)
      thing: SubCaseRole3
        property: Prop2 (mandatory, functional, Boolean)
      user: Self
      bot: for Self
        perspective on: ARole
          if extern >> Prop2 then
            Prop1 = false
      thing: ARole
        property: Prop1 (mandatory, functional, Boolean)
  case: TestCase6
    thing: AnotherRole
      property: Prop3 (mandatory, functional, Boolean)
    context: NestedContext6 filledBy: SubCase4
    case: SubCase4
      user: Self
      bot: for Self
        perspective on: ARole
          if extern >> binder NestedContext6 >> context >> AnotherRole >> Prop3 then
            Prop1 = false
      thing: ARole
        property: Prop1 (mandatory, functional, Boolean)
