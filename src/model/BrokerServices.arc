-- Copyright Joop Ringelberg and Cor Baars, 2020, 2021 -15
-- A model to maintain AMQP Broker Services.
domain model://perspectives.domains#BrokerServices
  use sys for model://perspectives.domains#System
  use bs for model://perspectives.domains#BrokerServices
  use util for model://perspectives.domains#Utilities
  use rabbit for model://perspectives.domains#RabbitMQ
  use sensor for model://perspectives.domains#Sensor

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
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind app >> extern to StartContexts in sys:MySystem
          Name = "Broker Services App" for app >> extern
  
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
    -- PDRDEPENDENCY
    indexed bs:MyBrokers
    aspect sys:RootContext
    external
      aspect sys:RootContext$External

    -- The BrokerServices I manage.
    context ManagedBrokers (relational) filledBy BrokerService
      property StorageLocation (String)
        pattern = "^https://.*" "A url with the https scheme"

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
        pattern = "^https://[^\\/]+\\/rbmq\\/$" "A url with the https scheme, ending on '/rbmq/'"
      -- For mycontexts this is "https://mycontexts.com/rbsr/".
      property SelfRegisterEndpoint (mandatory, String)
        pattern = "^https://[^\\/]+\\/rbsr\\/$" "A url with the https scheme, ending on '/rbsr/'"
      property PublicUrl = binder ManagedBrokers >> StorageLocation
      property ContractPeriod (Day)
      property GracePeriod (Day)
      property TerminationPeriod (Day)
  
      
    user Administrator filledBy sys:PerspectivesSystem$User
      -- The credentials of Administrator for the remote RabbitMQ server.
      property AdminUserName (String)
      property AdminPassword (String)

      state WithCredentials = (exists AdminUserName) and (exists AdminPassword)
        perspective on Accounts
          only (CreateAndFill, Remove)
          props(LastNameOfAccountHolder) verbs (Consult)
      perspective on Administrator
        props (FirstName, LastName) verbs (Consult)
        props (AdminUserName, AdminPassword) verbs (SetPropertyValue)
      perspective on extern
        props (Name, Url, ManagementEndpoint, Exchange) verbs (Consult, SetPropertyValue)

    user Guest = sys:Me
      perspective on Administrator
        only (Fill, Create)

    public Visitor at (extern >> PublicUrl) = sys:Me
      perspective on extern
        props (Name, SelfRegisterEndpoint) verbs (Consult)
      perspective on Administrator
        props (FirstName, LastName) verbs (Consult)
      -- LET OP!! De volgende drie perspectieven zijn te ruim, want hierdoor krijgt elke bezoeker alle contracten te zien.
      perspective on Accounts
        only (Create, Fill, CreateAndFill)
      perspective on Accounts >> binding >> context >> AccountHolder
        only (Create, Fill)
      perspective on Accounts >> binding >> context >> Administrator
        only (Create, Fill)
      state NoAccount = not exists sys:Me >> binder AccountHolder
        action SignUp
          letA
            account <- create context BrokerContract bound to Accounts
          in
            bind sys:Me to AccountHolder in account >> binding >> context
            bind Administrator to Administrator in account >> binding >> context

    -- PDRDEPENDENCY
    context Accounts (relational, unlinked) filledBy BrokerContract

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
    state CanRegister = (exists AccountHolder) and exists Administrator
      on entry
        do for AccountHolder
          letA 
            queue <- create role Queues
            queueid <- callExternal util:GenSym() returns String
          in
            AccountName = callExternal util:GenSym() returns String for AccountHolder
            AccountPassword = callExternal util:GenSym() returns String for AccountHolder
            QueueName = queueid for queue
            Registered = callExternal rabbit:SelfRegisterWithRabbitMQ(
              extern >> ManagementEndpoint,
              AccountHolder >> AccountName,
              AccountHolder >> AccountPassword,
              queueid) returns Boolean for External
    state Active = extern >> Registered
      on entry
        do for AccountHolder
          letA 
            now <- callExternal sensor:ReadSensor("clock", "now") returns DateTime
          in 
            UseExpiresOn = now + Service >> ContractPeriod for External
            GracePeriodExpiresOn = extern > UseExpiresOn + Service >> GracePeriod for External
            TerminatesOn = extern > GracePeriodExpiresOn + Service >> TerminationPeriod for External
        notify AccountHolder
          "You now have an account at the BrokerService { extern >> Name }"
      state ExpiresSoon = callExternal sensor:ReadSensor("clock", "now") returns DateTime > extern >> UseExpiresOn
        on entry
          notify AccountHolder
            "Your lease of the BrokerService has ended. Within {Service >> extern >> GracePeriod} days, you will no longer be able to receive information from peers."

    external
      aspect sys:Invitation$External
    -- PDRDEPENDENCY
      property Url = binder model://perspectives.domains#BrokerServices$BrokerService$Accounts >> context >> extern >> Url
      property ManagementEndpoint = binder model://perspectives.domains#BrokerServices$BrokerService$Accounts >> context >> extern >> ManagementEndpoint
    -- PDRDEPENDENCY
      property Exchange = binder model://perspectives.domains#BrokerServices$BrokerService$Accounts >> context >> extern >> Exchange
      property Name = binder model://perspectives.domains#BrokerServices$BrokerService$Accounts >> context >> extern >> Name
      property FirstNameOfAccountHolder = context >> AccountHolder >> FirstName
      property LastNameOfAccountHolder = context >> AccountHolder >> LastName
      -- We use this on system startup.
      -- PDRDEPENDENCY
      property CurrentQueueName = sys:MySystem >> extern >> binder Queues >> QueueName
      property Registered (Boolean)
      property UseExpiresOn (DateTime)
      property GracePeriodExpiresOn (DateTime)
      property TerminatesOn (DateTime)

      view ForAccountHolder (Url, Exchange )
      view Account (FirstNameOfAccountHolder, LastNameOfAccountHolder)
    
    -- PDRDEPENDENCY
    user AccountHolder filledBy sys:PerspectivesSystem$User
      aspect sys:Invitation$Invitee
      -- PDRDEPENDENCY
      property AccountName (mandatory, String)
      -- PDRDEPENDENCY
      property AccountPassword (mandatory, String)
      -- PDRDEPENDENCY
      -- property QueueName (mandatory, String)

      -- Create an account on the RabbitMQ server. It is ready for the AccountHolder to listen to,
      -- but no other users can reach him yet.
      -- Notice that this state will never be active if the Visitor executes SignUp.
      state PrepareAccount = not exists binding
        on entry
          do for BrokerContract$Administrator
            letA 
              queue <- create role Queues
              queueid <- callExternal util:GenSym() returns String
            in
              AccountName = callExternal util:GenSym() returns String
              AccountPassword = callExternal util:GenSym() returns String
              QueueName = queueid for queue
              callEffect rabbit:PrepareAMQPaccount(
                context >> extern >> ManagementEndpoint,
                context >> Administrator >> AdminUserName,
                context >> Administrator >> AdminPassword,
                AccountName,
                AccountPassword,
                queueid)
      
      state StartService = exists binding
        on entry
          do for AccountHolder
            bind_ (sys:MySystem >> extern) to (context >> EmptyQueue)
            callEffect rabbit:StartListening()

      on exit
        do for BrokerContract$Administrator
          callEffect rabbit:DeleteAMQPaccount(
            context >> extern >> ManagementEndpoint,
            context >> Administrator >> AdminUserName,
            context >> Administrator >> AdminPassword,
            AccountName)

      view ForAccountHolder (AccountName, AccountPassword, LastName)

      perspective on extern
        props (Url, Exchange, CurrentQueueName) verbs (Consult)
      perspective on AccountHolder
        all roleverbs
        props (AccountName, AccountPassword) verbs (Consult, SetPropertyValue)
      
      perspective on Queues
        only (CreateAndFill)
        props (QueueName) verbs (SetPropertyValue, Consult)

      screen "Broker Contract"
        column
          row
            form "BrokerService" External
          row
            form "Administrator" Administrator
          row
            form "Account" AccountHolder
          row
            table Queues
              props (QueueName) verbs (Consult)


    context EmptyQueue (functional) = filter Queues with not exists binding

    user Administrator filledBy bs:BrokerService$Administrator
      aspect sys:Invitation$Inviter

      perspective on AccountHolder
        all roleverbs
        props (AccountName, AccountPassword) verbs (Consult, SetPropertyValue)
      
      perspective on Queues
        only (CreateAndFill)
        props (QueueName) verbs (Consult, SetPropertyValue)

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
        row 
          table "Queues" Queues

    aspect user sys:Invitation$Guest

    context Queues (relational) filledBy sys:PerspectivesSystem
      property QueueName (String)
      on exit
        do for BrokerContract$Administrator
          callEffect rabbit:DeleteQueue(
            context >> extern >> ManagementEndpoint,
            context >> Administrator >> AdminUserName,
            context >> Administrator >> AdminPassword,
            QueueName)

    context Service (functional) = extern >> binder Accounts >> context >> extern