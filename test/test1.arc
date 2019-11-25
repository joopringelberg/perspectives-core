domain: Test
  case: TestCase
    user: Self
    thing: SomeRole
    context: AContextRole filledBy: NestedCase
    bot: for Self
      perspective on: Self
        if true then
          createRole SomeRole
    case: NestedCase
