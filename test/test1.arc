domain: Test
  case: TestCase
    user: Self
    thing: SomeRole
    thing: AnotherRole
    bot: for Self
      perspective on: Self
        if true then
          createRole SomeRole
