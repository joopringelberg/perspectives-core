domain model://joopringelberg.nl#TestActionPerspectives
  use sys for model://perspectives.domains#System
  use tap for model://joopringelberg.nl#TestActionPerspectives

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context TestActionPerspectivesApp
          start <- create role StartContexts in sys:MySystem
        in
          bind_ app >> extern to start
          Name = "Test perspectives for Actions App" for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (tap:MyTestActionPerspectivesApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case TestActionPerspectivesApp
    indexed tap:MyTestActionPerspectivesApp
    aspect sys:RootContext
    external
      aspect sys:RootContext$External
    
    user Manager = sys:Me
      perspective on ToBeRemoved
        -- Outcomment the next line to demonstrate the compiler checks ScrapRole, CreateRole, FillWithOrigin, UnBindm DeleteRole
        only (Remove, Create, Fill, RemoveFiller, Delete)
        action ScrapRole
          remove role origin
        action CreateRole
          create role ToBeRemoved
      perspective on ContextToBeRemoved
        -- Outcomment the next line to demonstrate the compiler check on ScrapContext an CreateContext
        only (Remove, CreateAndFill, Fill, DeleteContext)
        action ScrapContext
          remove role origin
      action CreateContext
        create context SomeContext bound to ContextToBeRemoved
      action CreateContext_
        create_ context SomeContext bound to ContextToBeRemoved
      action FillWithOrigin
        bind currentactor to ToBeRemoved
      perspective on ThingToBeFilled_
        only (Fill, RemoveFiller)
      action FillWithOrigin_
        bind currentactor to ThingToBeFilled_
      action UnBind
        unbind currentactor from ToBeRemoved
      action UnBind_
        unbind currentactor from ThingToBeFilled_
      action DeleteRole
        delete role ToBeRemoved
      action DeleteContext
        delete context bound to ContextToBeRemoved
      perspective on RoleWithProperties
        props (ToBeDeleted) verbs (DeleteProperty, SetPropertyValue)
        action DeleteProperty
          delete property ToBeDeleted
        action SetProperty 
          ToBeDeleted = "aap"

    thing ToBeRemoved

    thing ThingToBeFilled_

    context ContextToBeRemoved filledBy SomeContext

    thing RoleWithProperties
      property ToBeDeleted (String)
  
  case SomeContext
