domain: Test
  case: TestCase1
    user: Self
    bot: for Self
      perspective on: ARole
        if filter AnotherRole with Prop2 >> Prop3 then
          Prop4 = false
    thing: ARole
      property: Prop4 (mandatory, functional, Boolean)
    thing: AnotherRole
      property: Prop2 (mandatory, functional, Boolean)
      property: Prop3 (mandatory, functional, Boolean)
  case: TestCase2
    user: Self
    bot: for Self
      perspective on: ARole
        if (filter ARole with Prop1) >> binding >> context >> AnotherRole >> Prop3 then
          Prop4 = false
    context: ARole filledBy: NestedCase1
      property: Prop1 (mandatory, functional, Boolean)
      property: Prop4 (mandatory, functional, Boolean)
    case: NestedCase1
      thing: AnotherRole
        property: Prop2 (mandatory, functional, Boolean)
        property: Prop3 (mandatory, functional, Boolean)
  case: TestCase3
    user: Self
    bot: for Self
      perspective on: ARole
        if (filter ARole with Prop1) >> binding >> context >> filter AnotherRole with Prop2 >> Prop3 then
          Prop4 = false
    context: ARole filledBy: NestedCase2
      property: Prop1 (mandatory, functional, Boolean)
      property: Prop4 (mandatory, functional, Boolean)
    case: NestedCase2
      thing: AnotherRole
        property: Prop2 (mandatory, functional, Boolean)
        property: Prop3 (mandatory, functional, Boolean)
  case: TestCase4
    user: Self
    bot: for Self
      perspective on: ARole
        if filter ARole with (binding >> context >> YetAnotherRole >> Prop5) >> binding >> context >> filter AnotherRole with Prop2 >> Prop3 then
          Prop4 = false
    context: ARole filledBy: NestedCase3
      property: Prop1 (mandatory, functional, Boolean)
      property: Prop4 (mandatory, functional, Boolean)
    case: NestedCase3
      thing: AnotherRole
        property: Prop2 (mandatory, functional, Boolean)
        property: Prop3 (mandatory, functional, Boolean)
      thing: YetAnotherRole
        property: Prop5 (mandatory, functional, Boolean)

  case: TestCase5
    user: Self
    bot: for Self
      perspective on: ARole
        if ARole >> CalculatedCondition then
          Prop5 = false
    thing: ARole
      property: Prop5 (mandatory, functional, Boolean)
      property: Prop6 (mandatory, functional, Boolean)
      property: Prop7 (mandatory, functional, Boolean)
      property: CalculatedCondition = Prop6 and Prop7
