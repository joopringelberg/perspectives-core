-- Copyright Joop Ringelberg and Cor Baars 2019
domain: TestBotActie
  use: sys for model:System

  case: Tests
    user: Tester filledBy: sys:PerspectivesSystem$User
    context: TestInstances (not mandatory, not functional) filledBy: Test

  case: Test
    external:
      property: V1 (not mandatory, functional, String)
      property: V2 (not mandatory, functional, String)
      property: Trigger (not mandatory, functional, Boolean)
      property: PropsEqual = V1 == V2
    user: Tester filledBy: sys:PerspectivesSystem$User
    bot: for Tester
      perspective on: External
        if not extern >> PropsEqual and extern >> Trigger then V2 = extern >> V1
