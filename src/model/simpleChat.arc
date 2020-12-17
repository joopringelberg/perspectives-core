-- Copyright Joop Ringelberg and Cor Baars 2019, 2020
domain: SimpleChat
  use: sys for model:System
  use: cdb for model:Couchdb
  use: cht for model:SimpleChat

  case: Model
    external:
      aspect: sys:Model$External
    aspect: sys:Model

  case: ChatApp
    external:
      aspect: sys:RootContext$External
    aspect: sys:RootContext
    indexed: cht:MyChats
    context: Chats = callExternal cdb:RoleInstances( "model:SimpleChat$Chat$External" ) returns: Chat$External
    user: Chatter (mandatory, functional) filledBy: sys:PerspectivesSystem$User
      aspect: sys:RootContext$RootUser
      perspective on: Chats
      perspective on: Chats >> context >> Initiator: Create, Bind

  case: Chat
    aspect: sys:Invitation
    aspect: cht:WithText
    external:
      aspect: sys:Invitation$External
      property: Title (not mandatory, functional, String)
      --property: WithPartner = You >> Voornaam

    user: Initiator (mandatory, functional) filledBy: Chatter
      aspect: sys:Invitation$Inviter
      aspect: cht:WithText$TextWriter
      perspective on: Partner
      perspective on: extern

    user: Partner (not mandatory, functional) filledBy: sys:PerspectivesSystem$User
      aspect: sys:Invitation$Invitee
      aspect: cht:WithText$TextWriter
      perspective on: extern Consult
      perspective on: Initiator
      perspective on: Partner

    thing: PotentialPartners = filter (callExternal cdb:RoleInstances( "model:System$PerspectivesSystem$User" ) returns: sys:PerspectivesSystem$User) with not binds sys:Me

    user: Me = filter (Initiator either Partner) with binds sys:Me
    user: You = filter (Initiator either Partner) with not binds sys:Me

  case: WithText
    user: TextWriter filledBy: sys:PerspectivesSystem$User
      property: MyText (not mandatory, functional, String)
