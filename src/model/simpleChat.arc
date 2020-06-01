-- Copyright Joop Ringelberg and Cor Baars 2019
domain: SimpleChat
  use: sys for model:System
  use: cdb for model:Couchdb

  case: Model
    external:
      aspect: sys:Model$External
    aspect: sys:Model

  case: ChatApp
    external:
      aspect: sys:NamedContext$External
    aspect: sys:NamedContext
    context: Chats = callExternal cdb:RoleInstances( "model:SimpleChat$Chat$External" ) returns: Chat$External
    thing: PotentialPartners = callExternal cdb:RoleInstances( "model:System$PerspectivesSystem$User" ) returns: sys:PerspectivesSystem$User
    user: Chatter (mandatory, functional) filledBy: sys:PerspectivesSystem$User
      perspective on: Chats

  case: Chat
    aspect: sys:Invitation
    external:
      aspect: sys:Invitation$External
      property: WithPartner = context >> Partner >> binding >> Voornaam
    user: Initiator (mandatory, functional) filledBy: Chatter
      aspect: sys:Invitation$Inviter
      property: MyText (not mandatory, functional, String)
      perspective on: Partner
    user: Partner (not mandatory, functional) filledBy: sys:PerspectivesSystem$User
      aspect: sys:Invitation$Invitee
      property: MyText (not mandatory, functional, String)
      perspective on: Initiator
