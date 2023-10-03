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
          Name = "Broker services app" for app >> extern
          bind_ app >> extern to indexedcontext
          IndexedContexts$Name = app >> indexedName for indexedcontext

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
  case BrokerService
    state NoAdministrator = not exists Administrator
      on entry
        do for BrokerService$Guest
          bind sys:Me to Administrator
    external
      property Name (mandatory, String)
      property Url (mandatory, String)
        pattern = "^wss://[^\\/]+:[0-9]+\\/$" "A url with the wss scheme, ending on a port followd by a forward slash"
      property Exchange (mandatory, String)

    user Administrator filledBy sys:PerspectivesSystem$User
      property AdminUserName (String)
      property AdminPassword (String)
      -- property RegistryTopic (String)
      -- property GuestRolePassword (String)

      perspective on Accounts
        only (CreateAndFill, Remove)
        props(LastNameOfAccountHolder) verbs (Consult)
      perspective on Administrator
        props (AdminUserName, AdminPassword) verbs (SetPropertyValue)
      perspective on extern
        defaults
      -- Without this perspective we get a synchronization warning.
      perspective on Accounts >> binding >> context >> Administrator
        props (ConfirmationCode) verbs (Consult)
      perspective on Nodes
        only (Create, Remove)
        props (Name) verbs (SetPropertyValue)

    user Guest = sys:Me
      perspective on Administrator
        only (Fill, Create)

    context Accounts (relational) filledBy BrokerContract

    thing Nodes (relational)
      property Name (String)

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
    state DefaultNode = 1 == extern >> binder Nodes >>= count
      on entry
        do for BrokerContract$Administrator
          bind extern >> context >> Nodes to Node
    external
      aspect sys:Invitation$External
      property EndPoint = binder model:BrokerServices$BrokerService$Accounts >> context >> extern >> Url + "ws"
      property Exchange = binder model:BrokerServices$BrokerService$Accounts >> context >> extern >> Exchange
      property Name = binder model:BrokerServices$BrokerService$Accounts >> context >> extern >> Name
      property FirstNameOfAccountHolder = context >> AccountHolder >> FirstName
      property LastNameOfAccountHolder = context >> AccountHolder >> LastName
      property NodeName = context >> Node >> Name

      view ForAccountHolder (EndPoint, Exchange, NodeName)
      view ForAdministrator (IWantToInviteAnUnconnectedUser, Message, SerialisedInvitation)
      view Account (FirstNameOfAccountHolder, LastNameOfAccountHolder)
    
    thing Node filledBy Nodes

    context Invitations filledBy BrokerContract

    user AccountHolder filledBy sys:PerspectivesSystem$User
      aspect sys:Invitation$Invitee
      property AccountName (mandatory, String)
      property AccountPassword (mandatory, String)
      property QueueName (mandatory, String)

      -- Create an account on the RabbitMQ server. It is ready for the AccountHolder to listen to,
      -- but no other users can reach him yet.
      state PrepareAccount = not exists binding
        on entry
          do for BrokerContract$Administrator
            AccountName = callExternal util:GenSym() returns String
            QueueName = callExternal util:GenSym() returns String
            AccountPassword = callExternal rabbit:PrepareAMQPaccount(
              context >> extern >> binder model:BrokerServices$BrokerService$Accounts >> context >> extern >> Url,
              context >> extern >> NodeName,
              context >> Administrator >> AdminUserName,
              context >> Administrator >> AdminPassword,
              AccountName,
              QueueName
            ) returns String
             
      -- Now we know who the AccountHolder is, we bind his identity to his queue, so other users can reach him.
      state SetBinding = exists binding
        on entry
          do for BrokerContract$Administrator
            callEffect rabbit:SetBindingKey(
              context >> extern >> binder model:BrokerServices$BrokerService$Accounts >> context >> extern >> Url,
              context >> Administrator >> AdminUserName,
              context >> Administrator >> AdminPassword,
              QueueName,
              binding >> callExternal util:RoleIdentifier() returns String
            )
            IWantToInviteAnUnconnectedUser = true for context >> extern

      state IsPrepared = IWantToInviteAnUnconnectedUser

      on exit
          do for BrokerContract$Administrator
            callEffect rabbit:DeleteAMQPaccount(
              context >> extern >> binder model:BrokerServices$BrokerService$Accounts >> context >> extern >> Url,
              context >> extern >> NodeName,
              context >> Administrator >> AdminUserName,
              context >> Administrator >> AdminPassword,
              callExternal util:RoleIdentifier() returns String,
              AccountName
            )
      view ForAccountHolder (AccountName, AccountPassword, QueueName, LastName)

      perspective on extern
        view External$ForAccountHolder verbs (Consult)
      perspective on AccountHolder
        all roleverbs
        props (AccountName, AccountPassword, QueueName) verbs (SetPropertyValue)
        props (AccountName, QueueName) verbs (Consult)
      perspective on BrokerContract$Administrator
        props (LastName) verbs (Consult)
      perspective on Invitations
        only (CreateAndFill)
        props (FirstNameOfAccountHolder, LastNameOfAccountHolder) verbs (Consult)
        in object state IsPrepared
          props (Message) verbs (SetPropertyValue)
          props (SerialisedInvitation) verbs (Consult)

      action CreateInvitation
        letA
          inv <- create context BrokerContract bound to Invitations
        in
          bind inv to Accounts in extern >> binder model:BrokerServices$BrokerService$Accounts >> context
          bind Administrator to Administrator in inv
            
      screen "Broker Contract"
        column
          form "BrokerService" External
          form "Administrator" Administrator
          form "Account" AccountHolder
          table "Invitations" Invitations
            props (Message, SerialisedInvitation)

    user Administrator filledBy bs:BrokerService$Administrator
      aspect sys:Invitation$Inviter
      property ConfirmationCode (String)

      perspective on AccountHolder
        all roleverbs
        props (AccountName, QueueName, AccountPassword) verbs (SetPropertyValue)
        props (LastName, FirstName) verbs (Consult)
      perspective on extern
        view External$ForAdministrator verbs (Consult, SetPropertyValue)
      perspective on Invitations
        -- This is merely to make sure Administrator is sent all Invitations.
        props (Name) verbs (Consult)

    aspect user sys:Invitation$Guest

    user Guest = sys:Me
      perspective on Administrator
        only (Fill, Create)
      perspective on AccountHolder
        only (Fill)
