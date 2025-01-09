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
          start <- create role StartContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind_ app >> extern to start
          Name = "Broker Services App" for start
  
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
      state NoContract = not exists (filter binding >> context >> Accounts with binding >> context >> AccountHolder >> binding == sys:SocialMe >> binding)
    
    -- A contract for the BrokerService I use.
    context Contracts = sys:SocialMe >> binding >> binder AccountHolder >> context >> extern

    -- PDRDEPENDENCY
    -- Arbitrarily take the first registerd (=active) contract to be the one that is in use.
    context ContractInUse (functional) = filter Contracts with Registered >>= first

    user Manager = sys:Me
      perspective on ManagedBrokers
        only (Create, Fill, CreateAndFill, Remove)
        props (Name) verbs (Consult)
        props (StorageLocation) verbs (Consult, SetPropertyValue)
      perspective on Contracts
        view BrokerContract$External$ForAccountHolder verbs (Consult)
      perspective on ContractInUse
        view BrokerContract$External$ForAccountHolder verbs (Consult)
      perspective on PublicBrokers
        only (Create, Fill, Remove)
        props (Name) verbs (Consult)
        in object state NoContract
          perspective on PublicBrokers >> binding >> context >> Accounts
            only (CreateAndFill, Remove, RemoveContext)
          perspective on PublicBrokers >> binding >> context >> Accounts >> binding >> context >> AccountHolder
            only (Create, Fill)
          perspective on PublicBrokers >> binding >> context >> Accounts >> binding >> context >> Administrator
            only (Create, Fill)
          action SignUp
            letA
              accountsinstance <- create context BrokerContract bound to Accounts in origin >> binding >> context
            in
              bind sys:SocialMe >> binding to AccountHolder in accountsinstance >> binding >> context
              bind accountsinstance >> context >> Administrator to Administrator in accountsinstance >> binding >> context

      
      screen "Managing Broker Services and contracts"
        tab "My Contracts"
          row
            table "My Contracts" Contracts
          row 
            form "Contract in use" ContractInUse
        tab "Brokers" default
          row 
            table ManagedBrokers
              only (Create, Remove)
          row
            markdown <## Get connected
                      MyContexts is most useful when you connect to other people. 
                      This installation has not yet a means to connect to others. 
                      Move to the [[link:pub:https://perspectives.domains/cw_j4qovsczpm/#gobbccbstw$External|Perspectives Broker Service]] 
                      page to get online. You will read further instructions there.
                     >
              when not (exists bs:MyBrokers >> PublicBrokers)
            markdown <## Sign up to a service
                      You cannot yet connect to peers, but a service to do so is available below.
                      Select a row with a service (there is probably just one) by clicking it.
                      Then, in the toolbar below, click the bolt icon. A menu will drop down.
                      Click the "SignUp" item. 
                      
                      ***NOTE*** that by signing up, you will share your first- and last name 
                      with the manager of the service. He or she will be able to contact you 
                      through MyContexts.

                      A little later, an icon will appear in the top left part of your screen, 
                      indicating you can now connect with peers.
                      >
              when (exists bs:MyBrokers >> PublicBrokers) and (not exists bs:MyBrokers >> Contracts)
            markdown <## You can connect to peers
                      This installation is subscribed to a Broker Service and is able to exchange information 
                      automatically with peers you are in contact with (including the manager of this service).
                     >
              when exists bs:MyBrokers >> Contracts
          row 
            table PublicBrokers
              only (Remove)


  -- A Managed service.
  -- PDRDEPENDENCY
  case BrokerService
    state NoAdministrator = not exists Administrator
      on entry
        do for BrokerService$Guest
          bind sys:SocialMe >> binding to Administrator
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
      property ServiceDescription (MarkDown)
        minLength = 100  
      
    user Administrator filledBy sys:TheWorld$PerspectivesUsers
      -- The credentials of Administrator for the remote RabbitMQ server.
      property AdminUserName (String)
      property AdminPassword (String)

      state WithCredentials = (exists AdminUserName) and (exists AdminPassword)
        perspective on Accounts
          only (CreateAndFill, Remove, RemoveContext)
          props(LastNameOfAccountHolder) verbs (Consult)
      perspective on Administrator
        props (FirstName, LastName) verbs (Consult)
        props (AdminUserName, AdminPassword) verbs (SetPropertyValue)
      perspective on extern
        props (Name, Url, ManagementEndpoint, SelfRegisterEndpoint, Exchange, ServiceDescription, TerminationPeriod, GracePeriod, ContractPeriod) verbs (Consult, SetPropertyValue)
        props (PublicUrl) verbs (Consult)

    user Guest = sys:Me
      perspective on Administrator
        only (Fill, Create)
      perspective on extern
        props (PublicUrl, ServiceDescription) verbs (SetPropertyValue)

    public Visitor at (extern >> PublicUrl) = sys:Me
      perspective on extern
        props (Name, SelfRegisterEndpoint, PublicUrl, Url, Exchange, ServiceDescription, ContractPeriod, GracePeriod, TerminationPeriod) verbs (Consult)
      perspective on Administrator
        props (FirstName, LastName, HasKey) verbs (Consult)
      perspective on Accounts
        only (Create, Fill, CreateAndFill)
      -- TODO: waarom dit perspectief?
      perspective on Accounts >> binding >> context >> Administrator
        only (Create, Fill)
      perspective on bs:MyBrokers >> PublicBrokers
        only (CreateAndFill, Fill)
      screen 
        row
          markdown <# Broker service
                    This is a 'broker service'. It is a program running on 
                    a webserver that programs use to exchange messages. MyContexts
                    installations use it to synchronize information.
                   >
        row
          markdown External
            props (ServiceDescription) verbs (Consult)
        row
          markdown <## Do you want to add this service?
                    This service is not yet available in your installation.
                    You might want to add it so you can sign up to it afterwards in order
                    to connect to other people on MyContexts.
                    [[action: AddThisServer|Add the service]]
                   >
            when not (exists bs:MyBrokers >> PublicBrokers)
          markdown <## Service available, sign up!
                    This service is available in your installation. However, you have not signed up to it.
                    Move to the [[link:model://perspectives.domains#BrokerServices$MyBrokers|Broker Services Management]] 
                    page to read how to sign up.
                   >
            when (exists bs:MyBrokers >> PublicBrokers) and (not exists bs:MyBrokers >> Contracts)
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
    state CanRegister = (not exists Queues) and (exists AccountHolder) and (exists Administrator) and exists extern >> SelfRegisterEndpoint
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
        notify AccountHolder
          "You now have an account at the BrokerService { extern >> Name }"
      state ExpiresSoon = (extern >> Registered) and callExternal sensor:ReadSensor("clock", "now") returns DateTime > extern >> UseExpiresOn
        on entry
          notify AccountHolder
            "Your lease of the BrokerService has ended. Within {Service >> GracePeriod} days, you will no longer be able to receive information from peers."
      
    state Terminated = (extern >> Registered) and callExternal sensor:ReadSensor("clock", "now") returns DateTime > extern >> TerminatesOn or extern >> ContractTerminated
      on entry
        do for BrokerContract$Administrator
          callEffect rabbit:DeleteAMQPaccount(
            extern >> ManagementEndpoint,
            Administrator >> AdminUserName,
            Administrator >> AdminPassword,
            AccountHolder >> AccountName)
          delete context bound to Queues
          Registered = false for extern

    -- On exiting, both the queue and the account are deleted. See the exit states of AccountHolder and Queue respectively.
    
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

      property ContractTerminated (Boolean)

      view ForAccountHolder (Name, UseExpiresOn)
      view Account (FirstNameOfAccountHolder, LastNameOfAccountHolder)

      on entry
        do for AccountHolder
          letA 
            now <- callExternal sensor:ReadSensor("clock", "now") returns DateTime
          in 
            UseExpiresOn = (now + context >> Service >> ContractPeriod)
            GracePeriodExpiresOn = (UseExpiresOn + context >> Service >> GracePeriod)
            TerminatesOn = (GracePeriodExpiresOn + context >> Service >> TerminationPeriod)
    
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
        props (Registered, UseExpiresOn, GracePeriodExpiresOn, TerminatesOn, ContractTerminated) verbs (SetPropertyValue)
      perspective on AccountHolder
        all roleverbs
        props (AccountName, AccountPassword) verbs (Consult, SetPropertyValue)
      
      perspective on Queues
        only (Create, Fill)
        props (QueueName) verbs (SetPropertyValue, Consult)

      screen "Broker Contract"
        column
          row
            form "Contract" External
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

      perspective on Administrator
        props (AdminUserName, AdminPassword) verbs (Consult)

      perspective on AccountHolder
        all roleverbs
        props (AccountName, AccountPassword) verbs (Consult, SetPropertyValue)
      
      perspective on Queues
        only (Create, Fill, DeleteContext)
        props (QueueName) verbs (Consult, SetPropertyValue)
      
      -- If this contract is due to self-signup, Administrator needs this perspective to know that this contract
      -- fills an Accounts role in the service
      perspective on extern
        props (Name, ContractTerminated, ManagementEndpoint, TerminatesOn) verbs (Consult)
        props (Registered) verbs (SetPropertyValue)

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