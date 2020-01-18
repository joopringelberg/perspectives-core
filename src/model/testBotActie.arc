-- Copyright Joop Ringelberg and Cor Baars 2019
domain: TestBotActie
  use: sys for model:System

  case: Test
    external:
      property: V1 (not mandatory, functional, String)
      property: V2 (not mandatory, functional, String)
      property: Trigger (not mandatory, functional, Boolean)
      property: PropsEqual = V1 == V2
    user: Gebruiker filledBy: sys:PerspectivesSystem$User
    bot: for Gebruiker
      perspective on: External
        if not extern >> PropsEqual and extern >> Trigger then V2 = extern >> V1
