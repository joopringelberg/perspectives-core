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

  context Broker (relational) filledBy BrokerService

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
      state HasStorageLocation = exists StorageLocation
        on entry
          do for Manager
            create_ context BrokerService bound to origin
    
    context PublicBrokers (relational) filledBy BrokerService
      state NoContract = not exists context >> filter PublicBrokers >> binding >> context >> Accounts >> binding >> context >> AccountHolder with filledBy sys:SocialMe -- gaat fout; origin kan wel.
    
    -- A contract for the BrokerService I use.
    context Contracts = sys:SocialMe >> binder AccountHolder >> context >> extern

    user Manager = sys:Me
      perspective on ManagedBrokers
        only (Create, Fill, CreateAndFill, Remove)
        props (Name) verbs (Consult)
        props (StorageLocation) verbs (Consult, SetPropertyValue)
      perspective on Contracts
        view BrokerContract$External$ForAccountHolder verbs (Consult)
      perspective on PublicBrokers
        props (Name) verbs (Consult)
        in object state NoContract
          perspective on PublicBrokers >> binding >> context >> Accounts
            only (CreateAndFill, Remove)
          perspective on PublicBrokers >> binding >> context >> Accounts >> binding >> context >> AccountHolder
            only (Create, Fill)
          perspective on PublicBrokers >> binding >> context >> Accounts >> binding >> context >> Administrator
            only (Create, Fill)
          action SignUp
            letA
              accountsinstance <- create context BrokerContract bound to Accounts in origin >> binding >> context
            in
              bind sys:SocialMe to AccountHolder in accountsinstance >> binding >> context
              bind accountsinstance >> context >> Administrator to Administrator in accountsinstance >> binding >> context
          action EndContract
            letA
              mycontract <- filter binding >> context >> Accounts with binding >> context >> AccountHolder filledBy sys:SocialMe
            in
              remove context mycontract

      
      screen "Managing Broker Services and contracts"
        tab "My Contracts"
          row
            table "My Contracts" Contracts
        tab "Brokers"
          row 
            table ManagedBrokers
              only (Create, Remove)
          row 
            table PublicBrokers


  -- A Managed service.
  -- PDRDEPENDENCY
  case BrokerService
    state NoAdministrator = not exists Administrator
      on entry
        do for BrokerService$Guest
          bind sys:SocialMe to Administrator
          PublicUrl = extern >> binder ManagedBrokers >> StorageLocation for extern
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
      property PublicUrl (String)
      property ContractPeriod (Day)
      property GracePeriod (Day)
      property TerminationPeriod (Day)
  
      
    user Administrator filledBy sys:TheWorld$PerspectivesUsers
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
        props (Name, Url, ManagementEndpoint, SelfRegisterEndpoint, Exchange) verbs (Consult, SetPropertyValue)
        props (PublicUrl) verbs (Consult)

    user Guest = sys:Me
      perspective on Administrator
        only (Fill, Create)
      perspective on extern
        props (PublicUrl) verbs (SetPropertyValue)

    public Visitor at (extern >> PublicUrl) = sys:Me
      perspective on extern
        props (Name, SelfRegisterEndpoint, PublicUrl, Url, Exchange) verbs (Consult)
      perspective on Administrator
        props (FirstName, LastName) verbs (Consult)
      perspective on Accounts
        only (Create, Fill, CreateAndFill)
      perspective on Accounts >> binding >> context >> AccountHolder
        only (Create, Fill)
      perspective on Accounts >> binding >> context >> Administrator
        only (Create, Fill)
      perspective on bs:MyBrokers >> PublicBrokers
        only (CreateAndFill)
      screen 
        row
          markdown <# Broker service
                    This is a 'broker service'. It is a program running on 
                    a webserver that programs use to exchange messages. MyContexts
                    installations use it to synchronize information.
                   >
        row
          markdown <## Do you want to add this service?
                    This service is not yet available in your installation.
                    You might want to add it so you can sign up to it afterwards in order
                    to connect to other people on MyContexts.
                    [[action: AddThisServer|Add the service]]
                   >
            when not exists bs:MyBrokers >> Contracts
        row
          form "This service" External
            props (Name) verbs (Consult)
        row 
          form "Service administrator" Administrator
      action AddThisServer
        bind extern to PublicBrokers in bs:MyBrokers

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
    state CanRegister = ((exists AccountHolder) and exists Administrator) and exists extern >> SelfRegisterEndpoint
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
              extern >> SelfRegisterEndpoint,
              AccountHolder >> AccountName,
              AccountHolder >> AccountPassword,
              queueid) returns Boolean for External
    state Active = extern >> Registered
      on entry
        do for AccountHolder
          letA 
            now <- callExternal sensor:ReadSensor("clock", "now") returns DateTime
          in 
            UseExpiresOn = (now + Service >> ContractPeriod) for External
            GracePeriodExpiresOn = (extern >> UseExpiresOn + Service >> GracePeriod) for External
            TerminatesOn = (extern >> GracePeriodExpiresOn + Service >> TerminationPeriod) for External
        notify AccountHolder
          "You now have an account at the BrokerService { extern >> Name }"
      state ExpiresSoon = callExternal sensor:ReadSensor("clock", "now") returns DateTime > extern >> UseExpiresOn
        on entry
          notify AccountHolder
            "Your lease of the BrokerService has ended. Within {Service >> GracePeriod} days, you will no longer be able to receive information from peers."

    -- TODO: On exiting a BrokerContract, we want to undo the subscription with RabbitMQ.
    -- on exit

    external
      aspect sys:Invitation$External
      -- PDRDEPENDENCY
      property Url = binder Accounts >> context >> extern >> Url
      property ManagementEndpoint = binder Accounts >> context >> extern >> ManagementEndpoint
      property SelfRegisterEndpoint = binder Accounts >> context >> extern >> SelfRegisterEndpoint
      -- PDRDEPENDENCY
      property Exchange = binder Accounts >> context >> extern >> Exchange
      property Name = binder Accounts >> context >> extern >> Name
      property FirstNameOfAccountHolder = context >> AccountHolder >> FirstName
      property LastNameOfAccountHolder = context >> AccountHolder >> LastName
      -- We use this on system startup.
      -- PDRDEPENDENCY
      property CurrentQueueName = sys:MySystem >> extern >> binder Queues >> QueueName
      property Registered (Boolean)
      property UseExpiresOn (DateTime)
      property GracePeriodExpiresOn (DateTime)
      property TerminatesOn (DateTime)

      view ForAccountHolder (Name, UseExpiresOn)
      view Account (FirstNameOfAccountHolder, LastNameOfAccountHolder)
    
    -- PDRDEPENDENCY
    user AccountHolder filledBy sys:TheWorld$PerspectivesUsers
      aspect sys:Invitation$Invitee
      aspect sys:ContextWithNotification$NotifiedUser
      -- PDRDEPENDENCY
      property AccountName (mandatory, String)
      -- PDRDEPENDENCY
      property AccountPassword (mandatory, String)
      -- PDRDEPENDENCY
      -- property QueueName (mandatory, String)

      -- Create an account on the RabbitMQ server. It is ready for the AccountHolder to listen to,
      -- but no other users can reach him yet.
      -- We'll know the BrokerContract has arrived at the Administrator after a peer executed Signup,
      -- when a Queue already exists.
      state PrepareAccount = (not exists binding) and not exists context >> Queues
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
      
      state StartService = (exists binding) and context >> extern >> Registered
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
      
      -- If this contract is due to self-signup, Administrator needs this perspective to know that this contract
      -- fills an Accounts role in the service
      perspective on extern
        props (Name) verbs (Consult)

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
    aspect thing sys:ContextWithNotification$Notifications

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