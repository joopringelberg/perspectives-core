-- Copyright Joop Ringelberg and Cor Baars 2019, 2020, 2021, 2023
domain model://perspectives.domains#SimpleChat
  use sys for model://perspectives.domains#System
  use cdb for model://perspectives.domains#Couchdb
  use cht for model://perspectives.domains#SimpleChat

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
          app <- create context ChatApp
          indexedcontext <- create role IndexedContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind app >> extern to StartContexts in sys:MySystem
          Name = "Simple Chat App" for app >> extern
          bind_ app >> extern to indexedcontext
          IndexedContexts$Name = app >> indexedName for indexedcontext
  
  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (cht:MyChats >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (cht:MyChats >> extern)
      in
        remove context indexedcontext
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer

  -- The entry point (the `application`), available as cht:MyChats.
  case ChatApp
    indexed cht:MyChats
    aspect sys:RootContext
    aspect sys:ContextWithNotification
    on exit
      do for Chatter
        delete context bound to Chats
    external
      aspect sys:RootContext$External
    context Chats (relational) filledBy Chat
    context IncomingChats = sys:Me >> binder Partner >> context >> extern
    user Chatter = sys:Me
      perspective on Chats
        only (CreateAndFill, Remove)
        props (Title) verbs (SetPropertyValue)
      perspective on IncomingChats
        props (Title) verbs (Consult)
        -- action RemoveThisChat
        --   remove context origin
      screen "Simple Chat"
        row 
          table "My Chats" Chats
        row 
          table "Chats started by others" IncomingChats

  case Chat
    aspect sys:Invitation
    aspect cht:WithText
    state NoInitiator = not exists Initiator
      perspective of Creator
        perspective on Initiator
          only (Create, Fill)
      on entry
        do for Creator
          bind sys:Me to Initiator
    external
      aspect sys:Invitation$External
      property Title (String)
      -- on exit
      --   notify Me
      --     "Chat '{Title}' has been removed."
      ---- THIS action can be used to test upstream state based notification.
      -- on entry of sys:Invitation$External$InviteUnconnectedUser
      --   notify Initiator
      --     "Your invitation is being prepared."

    user Initiator (mandatory) filledBy sys:PerspectivesSystem$User
      aspect sys:Invitation$Inviter
      aspect cht:WithText$TextWriter
      perspective on Partner
        view sys:PerspectivesSystem$User$VolledigeNaam verbs (Consult)
        props (MyText) verbs (Consult)
        only (Create, Fill)
      perspective on Initiator
        props (MyText) verbs (SetPropertyValue)
        props (FirstName) verbs (Consult)
      perspective on extern
        props (Title) verbs (SetPropertyValue)
      screen "Chat"
        row 
          form External
            props (Title, Message, SerialisedInvitation) verbs (SetPropertyValue)
        row 
          form "This chat's partner" Partner
        row
          form "Your message" Initiator
            props (MyText) verbs (SetPropertyValue)

    user Partner filledBy sys:PerspectivesSystem$User
      aspect sys:Invitation$Invitee
      aspect cht:WithText$TextWriter
      perspective on extern
        props (Title) verbs (Consult)
      perspective on Initiator
        view sys:PerspectivesSystem$User$VolledigeNaam verbs (Consult)
        props (MyText) verbs (Consult)
      perspective on Partner
        props (MyText) verbs (SetPropertyValue)
        props (FirstName) verbs (Consult)
      screen "Chat"
        row 
          form External
            props (Title, Message) verbs (Consult)
        row
          form "This Chat's Initiator" Initiator
        row
          form "Your message" Partner
            props (MyText) verbs (SetPropertyValue)

    user Creator = filter sys:Me with not exists currentcontext >> Initiator

    thing PotentialPartners = filter (callExternal cdb:RoleInstances( "model:System$PerspectivesSystem$User" ) returns sys:PerspectivesSystem$User) with not filledBy sys:Me

    user Me = filter (Initiator union Partner) with filledBy sys:Me
    user You = filter (Initiator union Partner) with not filledBy sys:Me

    aspect user sys:Invitation$Guest

  case WithText
    user TextWriter filledBy sys:PerspectivesSystem$User
      property MyText (String)
