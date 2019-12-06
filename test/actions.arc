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
    context: Nested filledBy: NestedCase
    bot: for Self
      perspective on: Self
        if true then
          bind AnotherRole to ARole
          bind AnotherRole to ARole in Nested >> binding >> context
    case: NestedCase
      thing: ARole
  case: TestCaseBind_
    user: Self filledBy: sys:PerspectivesSystem$User
    thing: ARole4 filledBy: AnotherRole4
    thing: AnotherRole4
    context: Nested4 filledBy: NestedCase4
    bot: for Self
      perspective on: Self
        if true then
          bind_ AnotherRole4 to ARole4
          bind_ AnotherRole4 to Nested4 >> binding >> context >> ARole4
    case: NestedCase4
      thing: ARole4 filledBy: AnotherRole4
  case: TestCaseUnbind
    user: Self filledBy: sys:PerspectivesSystem$User
    thing: ARole5 filledBy: AnotherRole5
    thing: AnotherRole5
    context: Nested5 filledBy: NestedCase5
    bot: for Self
      perspective on: Self
        if true then
          unbind AnotherRole5
    case: NestedCase5
      thing: ARole5
