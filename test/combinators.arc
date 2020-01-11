domain: Combinators
  case: TestCase
    thing: Role1
    thing: SomeRole filledBy: Role1
      property: BindingAvailable = available binding
    thing: AnotherRole filledBy: SomeRole
      property: BindingAvailable = available binding
