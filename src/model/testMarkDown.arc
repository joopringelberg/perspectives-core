domain model://joopringelberg.nl#TestMarkDown
  use sys for model://perspectives.domains#System
  use md for model://joopringelberg.nl#TestMarkDown

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context MarkDownTest
        in
          bind app >> extern to StartContexts in sys:MySystem
          Name = "Markdown test" for app >> extern

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (md:MyMarkDownTest >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case MarkDownTest
    indexed md:MyMarkDownTest
    aspect sys:RootContext
    external
      aspect sys:RootContext$External
    
    thing WithMarkDown
      property MD (MarkDown)
    
    user Manager = sys:Me
      screen "MarkDown testing"
        row
          markdown "# Dit is Markdown!"
