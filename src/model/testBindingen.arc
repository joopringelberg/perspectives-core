domain model://joopringelberg.nl#TestBindingen
  use sys for model://perspectives.domains#System
  use tb for model://joopringelberg.nl#TestBindingen

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context TestBindingen
          start <- create role StartContexts in sys:MySystem
        in
          bind_ app >> extern to start
          Name = "Test Bindingen" for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (tb:TestBindingenApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer

  thing TestFiller
    property Name (String)
  
  thing TestFilled
    property Verwachting (String)
    property VerwachtingIngelost (Boolean)
    property Observaties (String)
      minLength = 100

  case TestBindingen
    indexed tb:TestBindingenApp
    aspect sys:RootContext
    aspect sys:ContextWithNotification

    state NoManager = not exists Manager
      on entry 
        do for Initializer
          bind sys:SocialMe >> binding to Manager

    user Initializer = sys:Me
      perspective on Manager
        all roleverbs

    user Manager filledBy sys:TheWorld$PerspectivesUsers
      aspect sys:ContextWithNotification$NotifiedUser
      perspective on Tests
        all roleverbs
        props (TestNaam) verbs (Consult, SetPropertyValue)
        props (TestSucceeded) verbs (Consult)
      perspective on MarkDown
        all roleverbs
        props (MD) verbs (SetPropertyValue)
      
      screen "MarkDown testing"
        row
          -- MarkDownConstant
          markdown <## Dit is Markdown! 
                    En dit is de tweede paragraaf.
                   >
        row 
          form MarkDown

    context Tests (relational) filledBy Test

    thing MarkDown
      property MD (MarkDown)

  case Test
    external 
      property TestNaam (String)
      property TestSucceeded = context >> FilledRole >> VerwachtingIngelost
    thing FilledRole
      aspect tb:TestFilled
    
  case TestDirectFiller
    aspect tb:Test

    user Tester = extern >> binder Tests >> context >> Manager
      perspective on Filled1
        defaults
      perspective on Filler1
        defaults
      perspective on Filler2
        defaults

    -- We expect Filled1 to be fillable by Filler1 and not Filler2.
    thing Filled1 filledBy TestDirectFiller$Filler1
      aspect tb:Test$FilledRole

    thing Filler1
      aspect tb:TestFiller

    thing Filler2
      aspect tb:TestFiller

  case TestIndirectFiller
    aspect tb:Test

    -- We expect that Filled1 can be filled by Filler1 and Filler2 but not by Filler3
    user Tester = extern >> binder Tests >> context >> Manager
      perspective on Filled1
        defaults
      perspective on Filler1
        defaults
      perspective on Filler2
        defaults
      perspective on Filler3
        defaults

    thing Filled1 filledBy TestIndirectFiller$Filler2
      aspect tb:Test$FilledRole

    thing Filler1 filledBy TestIndirectFiller$Filler2
      aspect tb:TestFiller

    thing Filler2
      aspect tb:TestFiller
    
    thing Filler3
      aspect tb:TestFiller

  case TestAspectRestrictie
    aspect tb:Test

    user Tester = extern >> binder Tests >> context >> Manager
      perspective on Filled1
        defaults
      perspective on Filler1
        defaults
      perspective on Filler2
        defaults
      perspective on Filler3
        defaults

    -- We expect Filled1 to be fillable by Filler1 and Filler2 but not Filler3
    thing Filled1 filledBy TestAspectRestrictie$Filler2
      aspect tb:Test$FilledRole

    thing Filler1
      aspect tb:TestFiller
      aspect tb:TestAspectRestrictie$Filler2

    thing Filler2
      aspect tb:TestFiller

    thing Filler3
      aspect tb:TestFiller

  case TestFillerWithAspectRestrictie
    aspect tb:Test

    user Tester = extern >> binder Tests >> context >> Manager
      perspective on Filled1
        defaults
      perspective on Filler1
        defaults
      perspective on Filler2
        defaults
      perspective on Filler3
        defaults
      perspective on Filler4
        defaults

    -- We expect Filled1 to be fillable by Filler1, Filler2 and Filler3 but not Filler4
    thing Filled1 filledBy TestFillerWithAspectRestrictie$Filler3
      aspect tb:Test$FilledRole

    thing Filler1 filledBy TestFillerWithAspectRestrictie$Filler2
      aspect tb:TestFiller

    thing Filler2
      aspect tb:TestFiller
      aspect tb:TestFillerWithAspectRestrictie$Filler3

    thing Filler3
      aspect tb:TestFiller

    thing Filler4
      aspect tb:TestFiller

  case TestInsufficientFiller
    aspect tb:Test

    -- We expect that Filled1 can be filled by Filler1 but not by Filler2.
    user Tester = extern >> binder Tests >> context >> Manager
      perspective on Filled1
        defaults
      perspective on Filler1
        defaults
      perspective on Filler2
        defaults

    thing Filled1 filledBy TestInsufficientFiller$Filler1
      aspect tb:Test$FilledRole

    thing Filler1 filledBy TestInsufficientFiller$Filler2
      aspect tb:TestFiller

    thing Filler2
      aspect tb:TestFiller
