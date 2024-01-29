domain model://joopringelberg.nl#TestDuration
  use sys for model://perspectives.domains#System
  use td for model://joopringelberg.nl#TestDuration
  use sensor for model://perspectives.domains#Sensor

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
          app <- create context TestDurationApp
          indexedcontext <- create role IndexedContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind app >> extern to StartContexts in sys:MySystem
          Name = "Test Duration App" for app >> extern
          bind_ app >> extern to indexedcontext
          IndexedContexts$Name = app >> indexedName for indexedcontext

  -- This does not compile.
  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (td:MyTestDurationApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (td:MyTestDurationApp >> extern)
      in
        remove context indexedcontext
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  -- The INDEXED context td:MyCouchdbApp, that is the starting point containing all CouchdbServers.
  -- There is NO PUBLIC PERSPECTIVE on this case.
  case TestDurationApp
    indexed td:MyTestDurationApp
    aspect sys:RootContext
    external
      aspect sys:RootContext$External
      property Today = callExternal sensor:ReadSensor("clock", "now") returns DateTime
      property Tomorrow = Today + 1 day
      property DayAfterTomorrow = Today + 2 days
    
    user Manager = sys:Me
      perspective on extern
        props (Today, Tomorrow, DayAfterTomorrow) verbs (Consult)
