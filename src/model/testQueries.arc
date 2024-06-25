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
  
  case TQAspect
    -- This state has been seen to become active after creating TheEmbeddedContext.
    -- This state has been seen to become inactive after removing TheEmbeddedContext.
    state AspectState1 = exists EC
      on entry
        notify AspectManager
          "Entering AspectState1"
      on exit
        notify AspectManager
          "Leaving AspectState1"
    
    -- This state has been seen to become active after setting Prop1 on the external role of the EmbeddedContext instance.
    -- This state has been seen to become inactive after setting Prop1 to false on the external role of the EmbeddedContext instance.
    state AspectState2 = EC >> AspectProp1
      on entry
        notify AspectManager
          "Entering AspectState2"
      on exit
        notify AspectManager
          "Leaving AspectState2"

    -- This state has been seen to become active after creating Thing1 in the EmbeddedContext.
    -- This state has been seen to become inactive after removing Thing1 in the EmbeddedContext.
    state AspectState3 = exists EC >> binding >> context >> AspectThing1
      on entry
        notify AspectManager
          "Entering AspectState3"
      on exit
        notify AspectManager
          "Leaving AspectState3"

    -- This state has been seen to become active after setting Prop2 on Thing1 in the EmbeddedContext.
    -- This state has been seen to become inactive after removing Prop2 (or setting it to false) in the EmbeddedContext.
    state AspectState4 = EC >> binding >> context >> AspectThing1 >> AspectProp2
      on entry
        notify AspectManager
          "Entering AspectState4"
      on exit
        notify AspectManager
          "Leaving AspectState4"

    context EC filledBy TQAspectEmbeddedContext

    user AspectManager

  case TQAspectEmbeddedContext
    external
      property AspectProp1 (Boolean)
    thing AspectThing1
      property AspectProp2 (Boolean)

  case TestQueries
    indexed tq:TestQueryApp
    aspect sys:RootContext
    aspect sys:ContextWithNotification
    aspect tq:TQAspect

    external
      aspect sys:RootContext$External
    
    state NoManager = not exists Manager
      on entry 
        do for Initializer
          bind sys:SocialMe to Manager

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
    state State3 = exists TheEmbeddedContext >> binding >> context >> Thing1
      on entry
        notify Manager
          "Entering State3"
      on exit
        notify Manager
          "Leaving State3"
    
    -- This state has been seen to become active after setting Prop2 to true.
    -- This state has been seen to become inactive after removing the filler of TheEmbeddedContext.
    state State4 = TheEmbeddedContext >> binding >> context >> Thing1 >> Prop2
      on entry
        notify Manager
          "Entering State4"
      on exit
        notify Manager
          "Leaving State4"
    
    aspect thing sys:ContextWithNotification$Notifications
    
    user Initializer = sys:Me
      perspective on Manager
        all roleverbs
      perspective on TheEmbeddedContext >> binding >> context >> EmbeddedUser
        all roleverbs


    -- Without a role this context could never be opened.
    user Manager filledBy sys:TheWorld$PerspectivesUsers
      aspect sys:ContextWithNotification$NotifiedUser
      aspect tq:TQAspect$AspectManager
      perspective on TheEmbeddedContext
        defaults
      perspective on Thing1InEmbeddedContext
        defaults
      perspective on AnotherRole
        defaults
      perspective on MarkDown
        all roleverbs
        props (MD, ShowIt) verbs (Consult, SetPropertyValue)
      
      screen "TestQueries"
        row
          form "TheEmbeddedContext" TheEmbeddedContext
        row
          form "Thing1 in EmbeddedContext" Thing1InEmbeddedContext
        row 
          form "AnotherRole" AnotherRole
        row
          -- MarkDownConstant:
          markdown <## Markdown! 
                    De bron van deze tekst is het model.
                   >
        -- Editing MarkDown in a form:
        row 
          column
            form "Very Simple MarkDown Editor" MarkDown
          -- MarkDownExpression
          column
            markdown MarkDown >> MD
              when MarkDown >> ShowIt
        -- MarkDownPerspective
        row
          column
            markdown MarkDown
              props (MD) verbs (Consult, SetPropertyValue)

    thing MarkDown
      property MD (MarkDown)
        minLength = 100
      property ShowIt (Boolean)

    context TheEmbeddedContext filledBy EmbeddedContext
      aspect tq:TQAspect$EC
      on entry
        do for Initializer
          bind sys:SocialMe to EmbeddedUser in binding >> context
      -- This state has been seen to become active after creating it and its filler.
      -- This state has been seen to become active after filling it.
      -- This state has been seen to become inactive after removing the filler.
      state TEC = exists binding
        on entry
          notify Manager
            "Entering state TEC"
        on exit
          notify Manager
            "Leaving state TEC"
      property Prop4 (Boolean)

    thing AnotherRole
      property Prop3 (Boolean)
    
    thing Thing1InEmbeddedContext = TheEmbeddedContext >> binding >> context >> Thing1

  case EmbeddedContext
    aspect sys:ContextWithNotification
    aspect tq:TQAspectEmbeddedContext 
    external 
      aspect tq:TQAspectEmbeddedContext$External
        where
          AspectProp1 is replaced by Prop1
      state ECE = (exists context >> EmbeddedUser) and exists binder TheEmbeddedContext
        on entry
          notify EmbeddedUser
            "Entering state ECE"
        on exit
          notify EmbeddedUser
            "Leaving state ECE"

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
    state EmbeddedState2 = (exists EmbeddedUser) and extern >> binder TheEmbeddedContext >> context >> AnotherRole >> Prop3 >>= first
      on entry
        notify EmbeddedUser
          "Entering EmbeddedState2"
      on exit
        notify EmbeddedUser
          "Leaving EmbeddedState2"

    aspect thing sys:ContextWithNotification$Notifications

    thing Thing1
      aspect tq:TQAspectEmbeddedContext$AspectThing1
        where 
          AspectProp2 is replaced by Prop2
      property Prop2 (Boolean)
    
    user EmbeddedUser filledBy sys:TheWorld$PerspectivesUsers
      aspect sys:ContextWithNotification$NotifiedUser
      perspective on Thing1
        defaults
      perspective on extern
        defaults