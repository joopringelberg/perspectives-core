-- Copyright Joop Ringelberg and Cor Baars 2020
domain: Repository
  use: sys for model:System$PerspectivesSystem

  case: Repository
    external:
      property: Name (mandatory, functional, String)
      property: Url (mandatory, functional, String)

    user: Maintainer (mandatory, functional) filledBy: sys:User
      perspective on: AvailableModels
      perspective on: Customers
      perspective on: Maintainer
      perspective on: UsedModels

    user: Customers (not mandatory, not functional) filledBy: sys:User
      perspective on: AvailableModels
      perspective on: Customers -- should be SelfOnly

    thing: AvailableModels (not mandatory, not functional) filledBy: model:System$Model

    thing: UsedModels = Customers >> binding >> context >> ModelsInUse
