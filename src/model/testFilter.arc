domain model://joopringelberg.nl#TestFilter
  use sys for model://perspectives.domains#System
  use ftest for model://joopringelberg.nl#TestFilter

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
          app <- create context FilterTestApp
          indexedcontext <- create role IndexedContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind app >> extern to StartContexts in sys:MySystem
          Name = "Filter Test App" for app >> extern
          bind_ app >> extern to indexedcontext
          IndexedContexts$Name = app >> indexedName for indexedcontext

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (ftest:MyFilterTest >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (ftest:MyFilterTest >> extern)
      in
        remove context indexedcontext
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- TOP CONTEXT
  -------------------------------------------------------------------------------
  case FilterTestApp
    indexed ftest:MyFilterTest
    aspect sys:RootContext
    external
      aspect sys:RootContext$External
    
    user Manager = sys:Me
      perspective on FilterTests
        all roleverbs
        props (Name) verbs (Consult, SetPropertyValue)
      perspective on FilterTests >> binding >> context >> ZietAlles
        all roleverbs
      perspective on IncomingFilterTests
        props (Name) verbs (Consult)

    context FilterTests (relational) filledBy FilterTest
      state ExistsContext = exists binding >> context
        on entry
          do for Manager
            bind currentactor to ZietAlles in origin >> binding >> context
    
    context IncomingFilterTests = sys:Me >> binder ZietMinder >> context >> extern

  case FilterTest
    external
      property Name (String)
    user ZietAlles filledBy sys:TheWorld$PerspectivesUsers
      perspective on AlleDingen
        all roleverbs
        props (Naam, Zichtbaar) verbs (Consult, SetPropertyValue)
      perspective on SommigeDingen
        props (Naam) verbs (Consult)
      perspective on ZietMinder
        only (Create, Fill)
        props (FirstName) verbs (Consult)

    user ZietMinder filledBy sys:TheWorld$PerspectivesUsers
      perspective on SommigeDingen
        props (Naam) verbs (Consult)
      perspective on ZietMinder
        props (FirstName) verbs (Consult)

    thing AlleDingen (relational)
      property Naam (String)
      property Zichtbaar (Boolean)

    thing SommigeDingen = filter AlleDingen with Zichtbaar