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
    external
      aspect sys:RootContext$External
    --context Chats = callExternal cdb:RoleInstances( "modelSimpleChat$Chat$External" ) returns Chat$External
    context Chats (relational, unlinked) filledBy Chat
    user Chatter (mandatory) filledBy sys:PerspectivesSystem$User
      aspect sys:RootContext$RootUser
      perspective on Chats
        all roleverbs
      perspective on Chats >> binding >> context >> Initiator
        only (Create, Fill)

  case Chat
    aspect sys:Invitation
    aspect cht:WithText
    state NotBound = (exists Partner) and not exists extern >> binder Chats
      on entry
        do for Partner
          bind extern to Chats in cht:MyChats
    external
      aspect sys:Invitation$External
      property Title (String)

    user Initiator (mandatory) filledBy Chatter
      aspect sys:Invitation$Inviter
      aspect cht:WithText$TextWriter
      perspective on Partner
        defaults
      perspective on Initiator
        defaults
      perspective on extern
        defaults

    user Partner filledBy sys:PerspectivesSystem$User
      aspect sys:Invitation$Invitee
      aspect cht:WithText$TextWriter
      perspective on extern
        verbs (Consult)
      perspective on Initiator
        defaults
      perspective on Partner
        defaults

    thing PotentialPartners = filter (callExternal cdb:RoleInstances( "model:System$PerspectivesSystem$User" ) returns sys:PerspectivesSystem$User) with not binds sys:Me

    user Me = filter (Initiator either Partner) with binds sys:Me
    user You = filter (Initiator either Partner) with not binds sys:Me

  case WithText
    user TextWriter filledBy sys:PerspectivesSystem$User
      property MyText (String)
