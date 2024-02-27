domain model://joopringelberg.nl#MinimalModel
  use sys for model://perspectives.domains#System
  use joop for model://joopringelberg.nl#MinimalModel

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context MinimalApp
        in
          bind app >> extern to StartContexts in sys:MySystem
          Name = "Minimal App" for app >> extern

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (mm:MyMinimalApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case MinimalApp
    indexed mm:MyMinimalApp
    aspect sys:RootContext
    external
      aspect sys:RootContext$External
    
    -- Without a role this context could never be opened.
    user Manager = sys:Me
