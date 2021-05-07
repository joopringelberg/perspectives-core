
user: InvitedParticipant
  property: Accepted (Boolean)
  states:
    state Participates = Accepted
  in state: Participates
    perspective on: ForParticipantsOnly
      only Create, remove
      Consult, SetPropertyValue : view <ident>
thing: ForParticipantsOnly

-- Het kan ook zo:

user: InvitedParticipant
  property: Accepted (Boolean)
  states:
    state Participates = Accepted
      perspective on: ForParticipantsOnly
        only Create, Remove
        Consult, SetPropertyValue : view <ident>
thing: ForParticipantsOnly

-- En zo:

user: InvitedParticipant
  property: Accepted (Boolean)
  states:
    state Participates = Accepted
thing: ForParticipantsOnly
  in state: Participates
    perspective of: InvitedParticipant
      excluding Delete, CreateAndFill, Fill, Unbind, RemoveFiller, Move
      Consult, SetPropertyValue : view <ident>
