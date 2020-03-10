domain: Test
  use: sys for model:System

  case: TestCase
    user: Self filledBy: sys:PerspectivesSystem$User
    user: Other filledBy: sys:PerspectivesSystem$User
      perspective on: ARole
    thing: ARole
      property: Prop1 (not mandatory, functional, Boolean)
