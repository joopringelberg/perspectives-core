-- Copyright Joop Ringelberg and Cor Baars 2019, 2020, 2021
domain SimpleChat
  use sys for model:System
  use cdb for model:Couchdb
  use cht for model:SimpleChat

  case Model
    aspect sys:Model
    external
      aspect sys:Model$External

  case ChatApp
    indexed cht:MyChats
    aspect sys:RootContext
    aspect sys:ContextWithNotification
    on exit
      do for Chatter
        delete context bound to Chats
    external
      aspect sys:RootContext$External
    context Chats (relational, unlinked) filledBy Chat
      on exit
        notify Chatter
          "Chat '{Title}' has been removed."
    user Chatter (mandatory) filledBy sys:PerspectivesSystem$User
      aspect sys:RootContext$RootUser
      perspective on Chats
        only (CreateAndFill, Remove, Delete)
        props (Title) verbs (Consult)
        action RemoveThisChat
          remove context origin

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
    state NotBound = (not exists extern >> binder Chats) and (exists Partner)
      on entry
        do for Partner
          bind extern to Chats in cht:MyChats
    external
      aspect sys:Invitation$External
      property Title (String)
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
        defaults
      perspective on extern
        defaults

    user Partner filledBy sys:PerspectivesSystem$User
      aspect sys:Invitation$Invitee
      aspect cht:WithText$TextWriter
      perspective on extern
        props (Title) verbs (Consult)
      perspective on Initiator
        view sys:PerspectivesSystem$User$VolledigeNaam verbs (Consult)
        props (MyText) verbs (Consult)
      perspective on Partner
        defaults

    user Creator = filter sys:Me with not exists currentcontext >> Initiator

    thing PotentialPartners = filter (callExternal cdb:RoleInstances( "model:System$PerspectivesSystem$User" ) returns sys:PerspectivesSystem$User) with not filledBy sys:Me

    user Me = filter (Initiator either Partner) with filledBy sys:Me
    user You = filter (Initiator either Partner) with not filledBy sys:Me

    aspect user sys:Invitation$Guest

  case WithText
    user TextWriter filledBy sys:PerspectivesSystem$User
      property MyText (String)
