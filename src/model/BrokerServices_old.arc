-- Copyright Joop Ringelberg and Cor Baars, 2020, 2021 -15
-- A model to maintain AMQP Broker Services.
domain model://perspectives.domains#BrokerServices
  use sys for model://perspectives.domains#System
  use bs for model://perspectives.domains#BrokerServices

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- We must first create the context and then later bind it.
          -- If we try to create and bind it in a single statement, 
          -- we find that the Installer can just create RootContexts
          -- as they are the allowed binding of StartContexts.
          -- As a consequence, no context is created.
          app <- create context BrokerServices
          indexedcontext <- create role IndexedContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind app >> extern to StartContexts in sys:MySystem
          Name = "Broker Services App" for app >> extern
          bind_ app >> extern to indexedcontext
          IndexedContexts$Name = app >> indexedName for indexedcontext
  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with binds (bs:MyBrokers >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with binds (bs:MyBrokers >> extern)
      in
        remove context indexedcontext
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer

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
        props (FirstName, LastName) verbs (Consult)
      perspective on extern
        props (Name, Url, Exchange) verbs (SetPropertyValue)
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
          bind extern >> binder model://perspectives.domains#BrokerServices$BrokerService$Accounts >> context >> Administrator to Administrator
    state NoAccountHolder = (exists Administrator) and (not exists AccountHolder)
      on entry
        do for BrokerContract$Administrator
          create role AccountHolder
    external
      aspect sys:Invitation$External
      property Url = binder model://perspectives.domains#BrokerServices$BrokerService$Accounts >> context >> extern >> Url
      property Exchange = binder model://perspectives.domains#BrokerServices$BrokerService$Accounts >> context >> extern >> Exchange
      property Name = binder model://perspectives.domains#BrokerServices$BrokerService$Accounts >> context >> extern >> Name
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
