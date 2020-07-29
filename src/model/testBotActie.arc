-- Copyright Joop Ringelberg and Cor Baars 2019
domain: TestBotActie
  use: sys for model:System

  case: Model
    external:
      aspect: sys:Model$External
    aspect: sys:Model

  case: Tests
    external:
      aspect: sys:RootContext$External
    aspect: sys:RootContext
    user: Tester filledBy: sys:PerspectivesSystem$User
      aspect: sys:RootContext$RootUser
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
        if (not extern >> PropsEqual) and extern >> Trigger then V2 = extern >> V1 for extern
