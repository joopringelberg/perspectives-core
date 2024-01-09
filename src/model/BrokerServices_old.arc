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
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (bs:MyBrokers >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (bs:MyBrokers >> extern)
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
      -- state NoAdministrator = (exists binding) and not exists binding >> context >> Administrator
      --   do for Guest
      --     bind sys:Me to Administrator in binding >> context

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
  -- PDRDEPENDENCY
  case BrokerService
    state NoAdministrator = not exists Administrator
      on entry
        do for BrokerService$Guest
          bind sys:Me to Administrator
    external
      property Name (mandatory, String)
      -- PDRDEPENDENCY
      property Url (mandatory, String)
      -- PDRDEPENDENCY
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

    user Guest = sys:Me
      perspective on Administrator
        only (Fill, Create)

    -- PDRDEPENDENCY
    context Accounts (relational) filledBy BrokerContract

  -- The contract between an end user and a BrokerService.
  -- PDRDEPENDENCY
  case BrokerContract
    aspect sys:Invitation
    state NoAdministrator = not exists Administrator
      on entry
        do for sys:Invitation$Guest
          bind extern >> binder model://perspectives.domains#BrokerServices$BrokerService$Accounts >> context >> Administrator >>= first to Administrator
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

    -- PDRDEPENDENCY
    user AccountHolder filledBy sys:PerspectivesSystem$User
      aspect sys:Invitation$Invitee
      -- PDRDEPENDENCY
      property AccountName (mandatory, String)
      -- PDRDEPENDENCY
      property AccountPassword (mandatory, String)
      -- PDRDEPENDENCY
      property QueueName (mandatory, String)

      perspective on extern
        props (Url, Name) verbs (Consult)
      -- perspective on Administrator is inherited as the perspective of Invitee on Inviter.
      perspective on AccountHolder
        all roleverbs
        props (AccountPassword) verbs (SetPropertyValue)
        props (AccountName, QueueName) verbs (Consult)

      screen "Broker Contract"
        column
          form "BrokerService" External
          form "Administrator" Administrator
          form "Account" AccountHolder

    user Administrator filledBy bs:BrokerService$Administrator
      aspect sys:Invitation$Inviter

      perspective on AccountHolder
        all roleverbs
        props (AccountName, QueueName, AccountPassword) verbs (SetPropertyValue)

      screen "Create Broker Contract"
        row 
          form External
            -- NOTE: the file control should preferrably not show the upload button in this case.
            props (Message) verbs (SetPropertyValue)
        row 
          form "Invitation" External
            props (SerialisedInvitation, ConfirmationCode, CompleteMessage) verbs (Consult)
        row
          form "AccountHolder" AccountHolder

    aspect user sys:Invitation$Guest
