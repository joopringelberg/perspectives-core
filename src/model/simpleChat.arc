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
    on exit
      do for Chatter
        delete context bound to Chats
    external
      aspect sys:RootContext$External
    context Chats (relational, unlinked) filledBy Chat
    user Chatter (mandatory) filledBy sys:PerspectivesSystem$User
      aspect sys:RootContext$RootUser
      perspective on Chats
        only (CreateAndFill, Remove, Delete)
        props (Title) verbs (Consult)
        action RemoveThisChat
          remove context origin
          remove role origin

  case Chat
    aspect sys:Invitation
    aspect cht:WithText
    -------------------------------------------------------------------------------
    ---- CLEANUP ON EXIT
    ---- The states IAmInitiator and IAmPartner explicitly unbind the role that
    ---- the current user fills, on exit. In this way, the other participant
    ---- will know that she has pulled out of this Chat when it is deleted.
    -------------------------------------------------------------------------------
    state IAmInitiator = Initiator binds sys:Me
      on exit
        do for Initiator
          unbind sys:Me from Initiator
    state IAmPartner = Partner binds sys:Me
      on exit
        do for Partner
          unbind sys:Me from Partner
    state NoInitiator = not exists Initiator
      perspective of Creator
        perspective on Initiator
          only (Create, Fill)
      on entry
        do for Creator
          bind sys:Me to Initiator
    state NotBound = not exists extern >> binder Chats
      on entry
        do for Partner
          bind extern to Chats in cht:MyChats
    external
      aspect sys:Invitation$External
      property Title (String)

    user Initiator (mandatory) filledBy sys:PerspectivesSystem$User
      aspect sys:Invitation$Inviter
      aspect cht:WithText$TextWriter
      perspective on Partner
        view sys:PerspectivesSystem$User$VolledigeNaam verbs (Consult)
        props (MyText) verbs (Consult)
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

    thing PotentialPartners = filter (callExternal cdb:RoleInstances( "model:System$PerspectivesSystem$User" ) returns sys:PerspectivesSystem$User) with not binds sys:Me

    user Me = filter (Initiator either Partner) with binds sys:Me
    user You = filter (Initiator either Partner) with not binds sys:Me

  case WithText
    user TextWriter filledBy sys:PerspectivesSystem$User
      property MyText (String)
