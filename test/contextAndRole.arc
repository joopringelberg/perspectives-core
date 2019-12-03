domain: ContextAndRole
  use: sys for model:System
  case: TestCase
    user: Self
      property: Prop1 (mandatory, not functional, Number) filledBy: sys:PerspectivesSystem
    thing: SomeRole
    context: AContextRole filledBy: NestedCase
    bot: for Self
      perspective on: Self
        if true then
          createRole SomeRole
    case: NestedCase
