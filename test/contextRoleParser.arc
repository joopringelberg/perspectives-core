domain: Test
  case: TestCase
    user: Self
    context: AContextRole filledBy: NestedCase
    context: AContextRole2 filledBy: NestedCase2
    case: NestedCase
      user: NestedSelf filledBy: Self
    case: NestedCase2
      user: NestedSelf filledBy: Self
