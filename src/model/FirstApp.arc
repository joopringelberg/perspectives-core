domain model://joopringelberg.nl#CCC
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#CCC

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

  aspect user sys:PerspectivesSystem$Installer

  case MinimalApp
    indexed mm:MyMinimalApp
    aspect sys:RootContext
    external 
      aspect sys:RootContext$External
    user Manager = sys:Me
      perspective on Meeting
        all roleverbs
        defaults
    thing Meeting
      property B (Boolean)