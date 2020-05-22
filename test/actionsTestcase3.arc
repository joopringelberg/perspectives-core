domain: Test

  use: sys for model:System

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
