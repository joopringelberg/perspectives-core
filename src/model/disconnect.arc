domain model://joopringelberg.nl#Disconnect
  use sys for model://perspectives.domains#System
  use dc for model://joopringelberg.nl#Disconnect

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context Disconnect
          start <- create role StartContexts in sys:MySystem
        in
          bind_ app >> extern to start
          Name = "Disconnect from peers" for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (dc:DisconnectApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer

  case Disconnect
    indexed dc:DisconnectApp
    aspect sys:RootContext

    state NoManager = not exists Manager
      on entry 
        do for Initializer
          bind sys:SocialMe >> binding to Manager

    user Initializer = sys:Me
      perspective on Manager
        only (Create, Fill)

    user Manager filledBy sys:TheWorld$PerspectivesUsers
      perspective on DisconnectedPeers
        all roleverbs
        props (Peer, Disconnected) verbs (Consult)
      perspective on DisconnectedPeers >> binding >> context >> Disconnecter
        all roleverbs
      perspective on IncomingDisconnections
        props (Peer) verbs (Consult)

    context DisconnectedPeers (relational) filledBy DisconnectedPeer
      state GiveMeARole = exists binding
        on entry
          do for Manager
            bind currentactor to Disconnecter in binding >> context
    
    -- All DisconnectedPeers that do not fill an instance of DisconnectedPeers in this context.
    context IncomingDisconnections = filter callExternal cdb:RoleInstances( "model://joopringelberg.nl#Disconnect$DisconnectedPeer$External" ) returns dc:DisconnectedPeer$External
      with not (binder DisconnectedPeers >> context >>= first) == dc:DisconnectApp
    -- context IncomingDisconnections = sys:SocialMe >> binder Disconnected >> context >> extern

  case DisconnectedPeer
    external
      property Peer = context >> Disconnected >> LastName
      property Disconnected = context >> Disconnected >> Cancelled

    user Disconnecter filledBy sys:TheWorld$PerspectivesUsers
      perspective on Disconnecter
        props (FirstName, LastName) verbs (Consult)
      perspective on Disconnected
        only (Create, Fill)
        props (FirstName, LastName) verbs (Consult)
        props (Cancelled, Reconnect) verbs (SetPropertyValue)
      action Disconnect
        Cancelled = true for Disconnected
      action Reconnect
        Cancelled = false for Disconnected
        Reconnect = true for Disconnected
    
    user Disconnected filledBy sys:TheWorld$PerspectivesUsers
      property Reconnect (Boolean)

      on entry
        do for Disconnected
          Cancelled = true for context >> Disconnecter
      state Reconnect = Reconnect
        on entry
          do for Disconnected
            Cancelled = false for context >> Disconnecter

      perspective on Disconnecter
        props (FirstName, LastName) verbs (Consult)
        props (Cancelled) verbs (SetPropertyValue)

      perspective on Disconnected
        props (FirstName, LastName, Reconnect) verbs (Consult)
        