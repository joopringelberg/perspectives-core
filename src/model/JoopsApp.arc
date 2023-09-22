domain model://joopringelberg.nl#JoopsModel
  use sys for model://perspectives.domains#System
  use cdb for model://perspectives.domains#Couchdb
  use joop for model://joopringelberg.nl#JoopsModel

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
          app <- create context JoopsApp
          indexedcontext <- create role IndexedContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind app >> extern to StartContexts in sys:MySystem
          Name = "Joops App" for app >> extern
          bind_ app >> extern to indexedcontext
          IndexedContexts$Name = app >> indexedName for indexedcontext

  -- This does not compile.
  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (joop:MyVersionOfJoopsApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (joop:MyVersionOfJoopsApp >> extern)
      in
        remove context indexedcontext
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  -- The INDEXED context joop:MyCouchdbApp, that is the starting point containing all CouchdbServers.
  -- There is NO PUBLIC PERSPECTIVE on this case.
  case JoopsApp
    indexed joop:MyVersionOfJoopsApp
    aspect sys:RootContext
    external
      aspect sys:RootContext$External
    
    user Manager = sys:Me
      action MakeTestDatabase
        callEffect cdb:CreateCouchdbDatabase( "https://localhost:6984/", "testjoop" )
        callEffect cdb:MakeDatabasePublic( "https://localhost:6984/", "testjoop" )
        callEffect cdb:MakeDatabaseWriteProtected( "https://localhost:6984/", "testjoop" )
      
      action AddWritingMember
        callEffect cdb:MakeWritingMemberOf( "https://localhost:6984/", "testjoop", "def:#pen$User" )
      
      action RemoveTestDatabase
        callEffect cdb:RemoveAsWritingMemberOf( "https://localhost:6984/", "testjoop", "def:#pen$User" )
        callEffect cdb:DeleteCouchdbDatabase( "https://localhost:6984/", "testjoop" )
    
