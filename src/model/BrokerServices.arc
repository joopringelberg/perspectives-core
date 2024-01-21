-- Copyright Joop Ringelberg and Cor Baars, 2020, 2021 -15
-- A model to maintain AMQP Broker Services.
domain model://perspectives.domains#BrokerServices
  use sys for model://perspectives.domains#System
  use bs for model://perspectives.domains#BrokerServices
  use util for model://perspectives.domains#Utilities
  use rabbit for model://perspectives.domains#RabbitMQ

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

  -------------------------------------------------------------------------------
  ---- BROKER SERVICES
  -------------------------------------------------------------------------------

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
        pattern = "^wss://[^\\/]+:[0-9]+\\/ws$" "A url with the wss scheme, ending on a port followed by '/ws'"
      -- PDRDEPENDENCY
      property Exchange (mandatory, String)
      -- For mycontexts this is "https://mycontexts.com/rbmq/".
      property ManagementEndpoint (mandatory, String)

    user Administrator filledBy sys:PerspectivesSystem$User
      -- The credentials of Administrator for the remote RabbitMQ server.
      property AdminUserName (String)
      property AdminPassword (String)

      perspective on Accounts
        only (CreateAndFill, Remove)
        props(LastNameOfAccountHolder) verbs (Consult)
      perspective on Administrator
        props (FirstName, LastName) verbs (Consult)
        props (AdminUserName, AdminPassword) verbs (SetPropertyValue)
      perspective on extern
        props (Name, Url, ManagementEndpoint, Exchange) verbs (SetPropertyValue)

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
          -- Guest has a sufficient perspective on Administrator in state Invitation$NoInviter, which corresponds to this state NoAdministrator.
          bind extern >> binder model://perspectives.domains#BrokerServices$BrokerService$Accounts >> context >> Administrator >>= first to Administrator
    state NoAccountHolder = (exists Administrator) and (not exists AccountHolder)
      on entry
        do for BrokerContract$Administrator
          -- This is the role that the Invitee/AccountHolder will fill with himself if she accepts the BrokerContract.
          create role AccountHolder
    external
      aspect sys:Invitation$External
      property Url = binder model://perspectives.domains#BrokerServices$BrokerService$Accounts >> context >> extern >> Url
      property ManagementEndpoint = binder model://perspectives.domains#BrokerServices$BrokerService$Accounts >> context >> extern >> ManagementEndpoint
      property Exchange = binder model://perspectives.domains#BrokerServices$BrokerService$Accounts >> context >> extern >> Exchange
      property Name = binder model://perspectives.domains#BrokerServices$BrokerService$Accounts >> context >> extern >> Name
      property FirstNameOfAccountHolder = context >> AccountHolder >> FirstName
      property LastNameOfAccountHolder = context >> AccountHolder >> LastName

      view ForAccountHolder (Url, Exchange)
      view Account (FirstNameOfAccountHolder, LastNameOfAccountHolder)
    
    -- PDRDEPENDENCY
    user AccountHolder filledBy sys:PerspectivesSystem$User
      aspect sys:Invitation$Invitee
      -- PDRDEPENDENCY
      property AccountName (mandatory, String)
      -- PDRDEPENDENCY
      property AccountPassword (mandatory, String)
      -- PDRDEPENDENCY
      property QueueName (mandatory, String)

      -- Create an account on the RabbitMQ server. It is ready for the AccountHolder to listen to,
      -- but no other users can reach him yet.
      state PrepareAccount = not exists binding
        on entry
          do for BrokerContract$Administrator
            AccountName = callExternal util:GenSym() returns String
            AccountPassword = callExternal util:GenSym() returns String
            QueueName = callExternal util:GenSym() returns String
            callEffect rabbit:PrepareAMQPaccount(
              context >> extern >> ManagementEndpoint,
              context >> Administrator >> AdminUserName,
              context >> Administrator >> AdminPassword,
              AccountName,
              AccountPassword,
              QueueName)
      
      state StartService = exists binding
        on entry
          do for AccountHolder
            callEffect rabbit:StartListening()

      on exit
        do for BrokerContract$Administrator
          callEffect rabbit:DeleteAMQPaccount(
            context >> extern >> ManagementEndpoint,
            context >> Administrator >> AdminUserName,
            context >> Administrator >> AdminPassword,
            AccountName,
            QueueName)

      view ForAccountHolder (AccountName, AccountPassword, QueueName, LastName)

      perspective on extern
        view External$ForAccountHolder verbs (Consult)
      perspective on AccountHolder
        all roleverbs
        props (AccountName, AccountPassword, QueueName) verbs (SetPropertyValue)
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
