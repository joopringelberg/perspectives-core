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
      on exit
        notify Manager
          "Leaving State1"
      
    -- This state has been seen to become active after setting Prop1 to true.
    -- It has also been seen to become inactive after setting Prop1 to false.
    -- This state has been seen to become inactive after removing the filler of TheEmbeddedContext.
    -- And it has been seen to become active again by filling it with the external role of an EmbeddedContext instance.
    state State2 = TheEmbeddedContext >> Prop1
      on entry
        notify Manager
          "Entering State2"
      on exit
        notify Manager
          "Leaving State2"
    
    -- This state has been seen to become active after creating Thing1.
    -- This state has been seen to become inactive after removing the filler of TheEmbeddedContext.
    -- TODO. Removing Thing1 does not exit this state.
    state State3 = exists TheEmbeddedContext >> binding >> context >> Thing1
      on entry
        notify Manager
          "Entering State3"
      on exit
        notify Manager
          "Leaving State3"
    
    -- This state has been seen to become active after setting Prop2 to true.
    -- This state has been seen to become inactive after removing the filler of TheEmbeddedContext.
    -- TODO. Removing Thing1 does not exit this state.
    state State4 = TheEmbeddedContext >> binding >> context >> Thing1 >> Prop2
      on entry
        notify Manager
          "Entering State4"
      on exit
        notify Manager
          "Leaving State4"

    -- Without a role this context could never be opened.
    user Manager = sys:Me
      perspective on TheEmbeddedContext
        defaults
      perspective on TheEmbeddedContext >> binding >> context >> Thing1
        defaults
      perspective on AnotherRole
        defaults

    context TheEmbeddedContext filledBy EmbeddedContext
      property Prop4 (Boolean)

    thing AnotherRole
      property Prop3 (Boolean)

  case EmbeddedContext
    aspect sys:ContextWithNotification
    external 
      property Prop1 (Boolean)
    
    -- This state has been seen to become active after setting Prop4 to true.
    -- This state has been seen to become inactive after removing the filler of TheEmbeddedContext.
    state EmbeddedState1 = extern >> binder TheEmbeddedContext >> Prop4
      on entry
        notify EmbeddedUser
          "Entering EmbeddedState1"
      on exit
        notify EmbeddedUser
          "Leaving EmbeddedState1"
    
    -- This state has been seen to become active after setting Prop4 to true.
    -- It has also been seen to become inactive after setting Prop1 to false.
    -- This state has been seen to become inactive after removing the filler of TheEmbeddedContext.
    state EmbeddedState2 = extern >> binder TheEmbeddedContext >> context >> AnotherRole >> Prop3
      on entry
        notify EmbeddedUser
          "Entering EmbeddedState2"
      on exit
        notify EmbeddedUser
          "Leaving EmbeddedState2"

    thing Thing1
      property Prop2 (Boolean)
    
    user EmbeddedUser = sys:Me
      perspective on Thing1
        defaults
      perspective on extern
        defaults