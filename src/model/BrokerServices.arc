-- Copyright Joop Ringelberg and Cor Baars, 2020
-- A model to maintain AMQP Broker Services.
domain: BrokerServices
  use: sys for model:System
  use: bs for model:BrokerServices

  -- The model description case.
  case: Model
    external:
      aspect: sys:Model$External
    aspect: sys:Model

  -- The entry point (the `application`), available as bs:MyBrokers.
  case: BrokerServices
    external:
      aspect: sys:RootContext$External
    aspect: sys:RootContext
    indexed: bs:MyBrokers

    -- The BrokerServices I manage.
    context: ManagedBrokers filledBy: BrokerService

    -- The BrokerServices I use (have a contract with).
    context: Contracts = sys:Me >> binder AccountHolder >> context

    user: Guest filledBy: sys:PerspectivesSystem$User
      perspective on: ManagedBrokers
      perspective on: Contracts

  -- A Managed service.
  case: BrokerService
    external:
      property: Name (mandatory, functional, String)
      property: Url (mandatory, functional, String)
      property: Exchange (mandatory, functional, String)

    user: Administrator filledBy sys:PerspectivesSystem$User
      property: RegistryTopic (not mandatory, functional, String)
      property: GuestRolePassword (not mandatory, functional, String)

      perspective on: Accounts

    context: Accounts filledBy: BrokerContract

  -- The contract between an end user and a BrokerService.
  case: BrokerContract
    external:
      property: Url = context >> Administrator >> binder model:BrokerServices$BrokerService$Administrator >> context >> extern >> Url
      property: Exchange = context >> Administrator >> binder model:BrokerServices$BrokerService$Administrator >> context >> extern >> Exchange
      property: Name = context >> Administrator >> binder model:BrokerServices$BrokerService$Administrator >> context >> extern >> Name

    user: AccountHolder filledBy: sys:PerspectivesSystem$User
      property: AccountName (mandatory, functional, String)
      property: AccountPassword (mandatory, functional, String)
      property: ConfirmationCode (not mandatory, functional, String)

      perspective on: External

    user: Administrator filledBy: bs:BrokerService$Administrator
      property: ConfirmationCode (not mandatory, functional, String)

      perspective on: AccountHolder
