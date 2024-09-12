-- Copyright Joop Ringelberg and Cor Baars 2019, 2020, 2021, 2023, 2024
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
          start <- create role StartContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind_ app >> extern to start
          Name = "Simple Chat App" for start
  
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
    context IncomingChats = sys:SocialMe >> binding >> binder Partner >> context >> extern
    user Chatter = sys:SocialMe
      perspective on Chats
        only (CreateAndFill, Remove, Delete, RemoveContext, DeleteContext)
        props (Title) verbs (SetPropertyValue)
      perspective on IncomingChats
        props (Title) verbs (Consult)
      screen "Simple Chat"
        row 
          table "My Chats" Chats
        row 
          table "Chats started by others" IncomingChats

  case Chat
    aspect cht:WithText
    state NoInitiator = not exists Initiator
      perspective of Creator
        perspective on Initiator
          only (Create, Fill)
      on entry
        do for Creator
          bind sys:SocialMe >> binding to Initiator
    external
      property Title (String)

    user Initiator (mandatory) filledBy sys:TheWorld$PerspectivesUsers
      aspect cht:WithText$TextWriter
      perspective on Partner
        props (FirstName, LastName, MyText) verbs (Consult)
        only (Create, Fill)
      perspective on Initiator
        props (MyText) verbs (SetPropertyValue)
        props (FirstName) verbs (Consult)
      perspective on extern
        props (Title) verbs (SetPropertyValue)
      screen "Chat"
        row 
          form External
            props (Title) verbs (SetPropertyValue)
        row 
          form "This chat's partner" Partner
        row
          form "Your message" Initiator
            props (MyText) verbs (SetPropertyValue)

    user Partner filledBy sys:TheWorld$PerspectivesUsers
      aspect cht:WithText$TextWriter
      perspective on extern
        props (Title) verbs (Consult)
      perspective on Initiator
        props (FirstName, LastName, MyText) verbs (Consult)
      perspective on Partner
        props (MyText) verbs (SetPropertyValue)
        props (FirstName) verbs (Consult)
      screen "Chat"
        row 
          form External
            props (Title) verbs (Consult)
        row
          form "This Chat's Initiator" Initiator
        row
          form "Your message" Partner
            props (MyText) verbs (SetPropertyValue)

    user Creator = sys:SocialMe

  case WithText
    user TextWriter filledBy sys:TheWorld$PerspectivesUsers
      property MyText (String)
