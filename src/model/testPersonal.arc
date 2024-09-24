domain model://joopringelberg.nl#TestPersonal
  use sys for model://perspectives.domains#System
  use tp for model://joopringelberg.nl#TestPersonal

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context TestPersonal
          start <- create role StartContexts in sys:MySystem
        in
          bind_ app >> extern to start
          Name = "Test Personal" for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (tp:TestPersonalApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer

  case TestPersonal
    indexed tp:TestPersonalApp
    aspect sys:RootContext

    state NoManager = not exists Manager
      on entry 
        do for Initializer
          bind sys:SocialMe >> binding to Manager

    user Initializer = sys:Me
      perspective on Manager
        all roleverbs

    user Manager filledBy sys:TheWorld$PerspectivesUsers
      perspective on ClientCases
        all roleverbs
        props (Name) verbs (Consult)
      perspective on GroupSessions
        all roleverbs
        props (Name) verbs (Consult)

    context ClientCases (relational) filledBy ClientCase
      state RemoveIncoming = not exists binding
        on entry
          do for Manager
            remove role origin

    context GroupSessions (relational) filledBy GroupSession
      state RemoveIncoming = not exists binding
        on entry
          do for Manager
            remove role origin

  case ClientCase
    external
      property Name = context >> Client >> FirstName

      -- For this to be an aspect construct: move to the type level from this role; on the type level, get the Aspect binder (and then that binder MUST have that aspect!), get the context, get the indexed name and from that get the actual root context.
      -- Assuming that a Client receives a case from his/her psychiatrist, make that case available in the root context.
      state AddIncoming = not exists filter tp:TestPersonalApp >> ClientCases with filledBy origin
        on entry
          do for Client
            bind origin to ClientCases in tp:TestPersonalApp

    user Client (relational) filledBy sys:TheWorld$PerspectivesUsers
      property Report (String)
      perspective on Client
        selfonly
        props (Report) verbs (Consult)
      -- This perspective allows Client to create a ClientCases instance for an incoming CLientCase.
      -- Because it is authorOnly, it will never be used for synchronisation.
      perspective on extern >> binder ClientCases
        authoronly
        only (Create, Fill)

    user Psychiatrist (functional) = extern >> binder ClientCases >> context >> Manager
      perspective on Client
        all roleverbs
        props (FirstName, Report) verbs (SetPropertyValue)

  case GroupSession
    external
      property Name = context >> Client >> FirstName

      state AddIncoming = (not exists filter tp:TestPersonalApp >> GroupSessions with filledBy origin) and exists context >> Client
        on entry
          do for Client
            bind origin to GroupSessions in tp:TestPersonalApp

    user Client (relational) filledBy sys:TheWorld$PerspectivesUsers
      property Report (selfonly, String)
      
      perspective on Client
        props (Report) verbs (Consult)
      -- This perspective allows Client to create a ClientCases instance for an incoming CLientCase.
      -- Because it is authorOnly, it will never be used for synchronisation.
      perspective on extern >> binder GroupSessions
        authoronly
        only (Create, Fill)

    user Psychiatrist (functional) = extern >> binder GroupSessions >> context >> Manager
      perspective on Client
        all roleverbs
        props (FirstName, Report) verbs (SetPropertyValue)
