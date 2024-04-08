domain model://joopringelberg.nl#TestQueries
  use sys for model://perspectives.domains#System
  use tq for model://joopringelberg.nl#TestQueries

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context TestQueries
        in
          bind app >> extern to StartContexts in sys:MySystem
          Name = "Test Queries" for app >> extern

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (tq:TestQueryApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  

  case TestQueries
    indexed tq:TestQueryApp
    aspect sys:RootContext
    aspect sys:ContextWithNotification

    external
      aspect sys:RootContext$External

    -- This state has been seen to become active after creating TheEmbeddedContext.
    state State1 = exists TheEmbeddedContext
      on entry
        notify Manager
          "Entering State1"
      
    -- This state has been seen to become active after setting Prop1.
    state State2 = TheEmbeddedContext >> Prop1
      on entry
        notify Manager
          "Entering State2"
    
    -- This state has not seen to become active after creating Thing1.
    state State3 = exists TheEmbeddedContext >> binding >> context >> Thing1
      on entry
        notify Manager
          "Entering State3"
    
    state State4 = TheEmbeddedContext >> binding >> context >> Thing1 >> Prop2
      on entry
        notify Manager
          "Entering State4"

    -- Without a role this context could never be opened.
    user Manager = sys:Me
      perspective on TheEmbeddedContext
        defaults
      perspective on TheEmbeddedContext >> binding >> context >> Thing1
        defaults

    context TheEmbeddedContext filledBy EmbeddedContext

  case EmbeddedContext
    external 
      property Prop1 (Boolean)
    thing Thing1
      -- Manager cannot set Prop2
      property Prop2 (Boolean)