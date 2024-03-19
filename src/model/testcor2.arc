domain model://joopringelberg.nl#Parser_1
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#Parser_1

  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          app <- create context MyFirstApp
        in
          bind app >> extern to StartContexts in sys:MySystem
          Name = "Minimal App" for app >> extern

  aspect user sys:PerspectivesSystem$Installer

  case MyFirstApp
    indexed mm:MinimalApp
    aspect sys:RootContext
    external
      aspect sys:RootContext$External
    user Manager = sys:Me
      perspective on Meeting
        defaults
    thing Meeting
      (relational)
      property B  (Boolean)
      property T  (String)
      property N  (Number)
      property D  (DateTime)
      property F  (File)
      view SomeProps (B, N, T)
    user Worker = sys:Me
      perspective on GenesteContext
        defaults
      perspective on Meeting
        view SomeProps verbs (Consult)
          defaults
    context GenesteContext filledBy GenesteContext
    case GenesteContext
      external
        property Datum  (DateTime)