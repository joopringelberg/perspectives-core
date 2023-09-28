-- Copyright Joop Ringelberg and Cor Baars 2019, 2020, 2021, 2023
domain model://perspectives.domains#Introduction
  use sys for model://perspectives.domains#System
  use cdb for model://perspectives.domains#Couchdb
  use intro for model://perspectives.domains#Introduction

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
          app <- create context IntroductionApp
          indexedcontext <- create role IndexedContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind app >> extern to StartContexts in sys:MySystem
          Name = "Introduction App" for app >> extern
          bind_ app >> extern to indexedcontext
          IndexedContexts$Name = app >> indexedName for indexedcontext
  
  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (intro:MyIntroductions >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (intro:MyIntroductions >> extern)
      in
        remove context indexedcontext
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer

  -- The entry point (the `application`), available as intro:MyIntroductions.
  case IntroductionApp
    indexed intro:MyIntroductions
    aspect sys:RootContext
    aspect sys:ContextWithNotification
    external
      aspect sys:RootContext$External

    context Introductions (relational) filledBy Introduction
    context IncomingIntroductions = sys:Me >> binder Introducee >> context >> extern
    user Manager = sys:Me
      perspective on Introductions
        only (CreateAndFill, Remove)
        props (Title) verbs (SetPropertyValue)
      perspective on IncomingIntroductions
        props (Title) verbs (Consult)
      screen "Introductions"
        row 
          table "My Introductions" Introductions
        row 
          table "Introductions started by others" IncomingIntroductions

  case Introduction 
    state NoIntroducer = not exists Introducer
      on entry
        do for Guest
          bind sys:Me to Introducer
    external
      property Title (String)
    user Guest = sys:Me
      perspective on Introducer
        only (Create, Fill)
    user Introducer filledBy sys:PerspectivesSystem$User
      perspective on Introducee
        only (Create, Fill, Remove, CreateAndFill)
        props (FirstName, LastName) verbs (Consult)

    user Introducee (relational) filledBy sys:PerspectivesSystem$User
      perspective on Introducer
        props (FirstName, LastName) verbs (Consult)
      perspective on Introducee
        props (FirstName, LastName) verbs (Consult)
      
