domain: Test
  use: sys for model:System
  case: TestCase1
    user: Self filledBy: sys:PerspectivesSystem$User
    thing: ARole
    bot: for Self
      perspective on: Self
        if true then
          remove ARole
  case: TestCase2
    user: Self filledBy: sys:PerspectivesSystem$User
    thing: ARole
    bot: for Self
      perspective on: Self
        if true then
          createRole ARole
  case: TestCase3
    user: Self filledBy: sys:PerspectivesSystem$User
      property: Prop1 (not mandatory, functional, Boolean)
    context: Nested1 filledBy: NestedContext
    context: Nested2 filledBy: NestedContext
    bot: for Self
      perspective on: Self
        if Self >> Prop1 == true then
          move Nested1 >> binding >> context >> ARole to Nested2 >> binding >> context
    case: NestedContext
      thing: ARole
  case: TestCaseBind
    user: Self filledBy: sys:PerspectivesSystem$User
    thing: ARole filledBy: AnotherRole
    thing: AnotherRole
    bot: for Self
      perspective on: Self
        if true then
          bind AnotherRole to ARole
