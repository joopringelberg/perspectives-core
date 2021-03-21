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
    context: Contracts = sys:Me >> binder AccountHolder >> context >> extern

    user: Guest = sys:Me
      perspective on: ManagedBrokers
      perspective on: Contracts

  -- A Managed service.
  case: BrokerService
    external:
      property: Name (mandatory, functional, String)
      property: Url (mandatory, functional, String)
      property: Exchange (mandatory, functional, String)

    user: Administrator filledBy: sys:PerspectivesSystem$User
      property: RegistryTopic (not mandatory, functional, String)
      property: GuestRolePassword (not mandatory, functional, String)
      view: Admin (RegistryTopic, GuestRolePassword)

      perspective on: Accounts
      perspective on: Administrator
      perspective on: extern

    user: Guest = sys:Me
      perspective on: Administrator: Bind

    bot: for Guest
      perspective on: Administrator
        rule FillBrokerServiceAdministrator:
          if not exists object then
            bind sys:Me to Administrator

    context: Accounts filledBy: BrokerContract

  -- The contract between an end user and a BrokerService.
  case: BrokerContract
    aspect: sys:Invitation
    external:
      aspect: sys:Invitation$External
      property: Url = binder model:BrokerServices$BrokerService$Accounts >> context >> extern >> Url
      property: Exchange = binder model:BrokerServices$BrokerService$Accounts >> context >> extern >> Exchange
      property: Name = binder model:BrokerServices$BrokerService$Accounts >> context >> extern >> Name
      property: FirstNameOfAccountHolder = context >> AccountHolder >> Voornaam
      property: LastNameOfAccountHolder = context >> AccountHolder >> Achternaam

      view: ForAccountHolder (Url, Exchange)
      view: ForAdministrator (IWantToInviteAnUnconnectedUser, Message, SerialisedInvitation)
      view: Account (FirstNameOfAccountHolder, LastNameOfAccountHolder)

    user: AccountHolder filledBy: sys:PerspectivesSystem$User
      aspect: sys:Invitation$Invitee
      property: AccountName (mandatory, functional, String)
      property: AccountPassword (mandatory, functional, String)
      property: QueueName (mandatory, functional, String)
      property: ConfirmationCode (not mandatory, functional, String)

      view: ForAdministrator (AccountName, AccountPassword, QueueName)
      view: ForAccountHolder (AccountName, AccountPassword, QueueName, ConfirmationCode, Achternaam)

      perspective on: extern (ForAccountHolder)
      perspective on: AccountHolder

    user: Administrator filledBy: bs:BrokerService$Administrator
      aspect: sys:Invitation$Inviter
      property: ConfirmationCode (not mandatory, functional, String)
      view: Confirmation (ConfirmationCode)

      perspective on: AccountHolder
      perspective on: extern

    user: Guest = sys:Me
      perspective on: Administrator: Bind

    bot: for Guest
      perspective on: Administrator
        rule FillBrokerContractAdministrator:
          if not exists object then
            bind extern >> binder model:BrokerServices$BrokerService$Accounts >> context >> Administrator to Administrator

    bot: for Guest
      perspective on: AccountHolder
        rule CreateAccountHolder:
          if not exists object then
            createRole AccountHolder
