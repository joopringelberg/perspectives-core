domain: Combinators
  case: TestCase
    thing: SomeRole
      property: BindingAvailable = available binding
    thing: AnotherRole filledBy: SomeRole
      property: BindingAvailable = available binding
