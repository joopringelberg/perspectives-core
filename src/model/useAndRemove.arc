domain model://perspectives.domains#UseAndRemove
  use sys for model://perspectives.domains#System
  use uar for model://perspectives.domains#UseAndRemove

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
          app <- create context UseAndRemoveApp
          indexedcontext <- create role IndexedContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind app >> extern to StartContexts in sys:MySystem
          Name = "Use and Remove App" for app >> extern
          bind_ app >> extern to indexedcontext
          IndexedContexts$Name = app >> indexedName for indexedcontext

  -- This does not compile.
  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with binds (uar:UseAndRemoveApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with binds (uar:UseAndRemoveApp >> extern)
      in
        remove context indexedcontext
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  -- The INDEXED context uar:MyCouchdbApp, that is the starting point containing all CouchdbServers.
  -- There is NO PUBLIC PERSPECTIVE on this case.
  -- The end user user (playing Manager) should have a Server Admin account for each CouchdbServer that is added to the App.
  -- The credentials of each such Server Admin go into the CouchdbServer$Admin roles.
  case UseAndRemoveApp
    indexed uar:MyUseAndRemoveApp
    aspect sys:RootContext
    external
      aspect sys:RootContext$External
    
