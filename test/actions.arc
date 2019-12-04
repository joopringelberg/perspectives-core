domain: Test
  use: sys for model:System
  case: TestCase1
    user: Self filledBy: sys:PerspectivesSystem
    thing: ARole
    bot: for Self
      perspective on: Self
        if true then
          remove ARole
  case: TestCase2
    user: Self filledBy: sys:PerspectivesSystem
    thing: ARole
    bot: for Self
      perspective on: Self
        if true then
          createRole ARole
