-- Copyright Joop Ringelberg and Cor Baars, 2020, 2021 -15
-- A model to maintain AMQP Broker Services.
domain BrokerServices
  use sys for model:System
  use bs for model:BrokerServices

  -- The model description case.
  case Model
    aspect sys:Model
    external
      aspect sys:Model$External

  -- The entry point (the `application`), available as bs:MyBrokers.
  case BrokerServices
    indexed bs:MyBrokers
    aspect sys:RootContext
    external
      aspect sys:RootContext$External

    -- The BrokerServices I manage.
    context ManagedBrokers (relational) filledBy BrokerService

    -- The BrokerServices I use (have a contract with).
    context Contracts = sys:Me >> binder AccountHolder >> context >> extern

    -- The BrokerServices I am the Administrator of.
    context MyBrokers = sys:Me >> binder BrokerService$Administrator >> context >> extern

    user Guest = sys:Me
      perspective on ManagedBrokers
        only (CreateAndFill, Remove)
        props (Name) verbs (Consult)
      perspective on Contracts
        view BrokerContract$External$ForAccountHolder verbs (Consult)
      perspective on MyBrokers
        all roleverbs
        props (Name) verbs (Consult)

  -- A Managed service.
  case BrokerService
    state NoAdministrator = not exists Administrator
      on entry
        do for BrokerService$Guest
          bind sys:Me to Administrator
    external
      property Name (mandatory, String)
      property Url (mandatory, String)
      property Exchange (mandatory, String)

    user Administrator filledBy sys:PerspectivesSystem$User
      property RegistryTopic (String)
      property GuestRolePassword (String)
      view Admin (RegistryTopic, GuestRolePassword)

      perspective on Accounts
        only (CreateAndFill, Remove)
        props(LastNameOfAccountHolder) verbs (Consult)
      perspective on Administrator
        defaults
      perspective on extern
        defaults
      -- Without this perspective we get a synchronization warning.
      perspective on Accounts >> binding >> context >> Administrator
        props (ConfirmationCode) verbs (Consult)

    user Guest = sys:Me
      perspective on Administrator
        only (Fill, Create)

    context Accounts (relational) filledBy BrokerContract

  -- The contract between an end user and a BrokerService.
  case BrokerContract
    aspect sys:Invitation
    state NoAdministrator = not exists Administrator
      on entry
        do for Guest
          bind extern >> binder model:BrokerServices$BrokerService$Accounts >> context >> Administrator to Administrator
    state NoAccountHolder = (exists Administrator) and (not exists AccountHolder)
      on entry
        do for BrokerContract$Administrator
          create role AccountHolder
    external
      aspect sys:Invitation$External
      property Url = binder model:BrokerServices$BrokerService$Accounts >> context >> extern >> Url
      property Exchange = binder model:BrokerServices$BrokerService$Accounts >> context >> extern >> Exchange
      property Name = binder model:BrokerServices$BrokerService$Accounts >> context >> extern >> Name
      property FirstNameOfAccountHolder = context >> AccountHolder >> FirstName
      property LastNameOfAccountHolder = context >> AccountHolder >> LastName

      view ForAccountHolder (Url, Exchange)
      view ForAdministrator (IWantToInviteAnUnconnectedUser, Message, SerialisedInvitation)
      view Account (FirstNameOfAccountHolder, LastNameOfAccountHolder)

    user AccountHolder filledBy sys:PerspectivesSystem$User
      aspect sys:Invitation$Invitee
      property AccountName (mandatory, String)
      property AccountPassword (mandatory, String)
      property QueueName (mandatory, String)
      property ConfirmationCode (String)

      view ForAccountHolder (AccountName, AccountPassword, QueueName, ConfirmationCode, LastName)

      perspective on extern
        view External$ForAccountHolder verbs (Consult)
      perspective on AccountHolder
        all roleverbs
        props (AccountPassword) verbs (SetPropertyValue)
        props (AccountName, QueueName) verbs (Consult)
      perspective on BrokerContract$Administrator
        props (LastName) verbs (Consult)

      screen "Broker Contract"
        column
          form "BrokerService" External
          form "Administrator" Administrator
          form "Account" AccountHolder

    user Administrator filledBy bs:BrokerService$Administrator
      aspect sys:Invitation$Inviter
      property ConfirmationCode (String)
      view Confirmation (ConfirmationCode)

      perspective on AccountHolder
        all roleverbs
        props (AccountName, QueueName, AccountPassword) verbs (Consult, SetPropertyValue)
        props (LastName, FirstName) verbs (Consult)
      perspective on extern
        view External$ForAdministrator verbs (Consult, SetPropertyValue)

    aspect user sys:Invitation$Guest

    user Guest = sys:Me
      perspective on Administrator
        only (Fill, Create)
      perspective on AccountHolder
        only (Fill)
