domain model:Celebration
  use: sys for model:System$System
  user: Invitee (not mandatory, not functional) filledBy: sys:User
    property: Accepts (not mandatory, functional, Boolean)
    view: ViewOnInvitee (Name, Accepts)
    view: AcceptInvitation (Accepts)
    perspective on : Invitee (ViewOnInvitee): Consult
      Change with AcceptInvitation
        if Invitee == Me
    perspective on : UnClaimedWishes (Names): Consult
  user: Organiser (mandatory, functional) filledBy: sys:User
    perspective on : Invitee (ViewOnInvitee) : Bind
    perspective on : WishList (Names)
  bot for Organiser
    perspective on : Invitee
      if Accepts then
        bind Invitee in PotentialGiver
  context: WishList (not mandatory, not functional) filledBy: Wish
    view: Names (Name)
  context: UnClaimedWishes = filter WishList with not Claimed
    view: Names (Name)
  thing: TheEvent (mandatory, functional) filledBy Party
  case: Party
    user: Guest (not mandatory, not functional) filledBy: Invitee
  case: Wish
    external:
      property: Claimed = exists Giver
      property: Name (mandatory, functional, String)
    user: Wisher (mandatory, functional) filledBy: Organiser
    user: Giver (not mandatory, functional) filledBy: Guest
    user: PotentialGiver (not mandatory, not functional) filledBy: Guest
      perspective on : PotentialGiver : Consult, Bind
