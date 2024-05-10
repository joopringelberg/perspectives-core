-- TestPublicRole - Copyright Joop Ringelberg and Cor Baars 2023

domain model://perspectives.domains#TestPublicRole
  use sys for model://perspectives.domains#System
  use tbr for model://perspectives.domains#TestPublicRole

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
          app <- create context PublicRoleApp
          indexedcontext <- create role IndexedContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind app >> extern to StartContexts in sys:MySystem
          Name = "An App to test the public role syntax" for app >> extern
          bind_ app >> extern to indexedcontext
          IndexedContexts$Name = app >> indexedName for indexedcontext

  aspect user sys:PerspectivesSystem$Installer
  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------

  case PublicRoleApp
    indexed tbr:MyPublicRoleTest
    aspect sys:RootContext
    external
      aspect sys:RootContext$External

    user Manager = sys:Me
      action MakeTeaRoom
        letA
          tearoom <- create context TeaRoom bound to TeaRooms
        in
          bind sys:Me to TeaRoomOperator in tearoom >> binding >> context

      perspective on TeaRooms
        only (CreateAndFill, Remove)
      perspective on TeaRooms >> binding >> context >> TeaRoomOperator
        only (Create, Fill)


    context TeaRooms (relational) filledBy TeaRoom

  case TeaRoom

    user TeaRoomOperator filledBy Manager
      perspective on Thee
        only (Create, Remove)
        props (Soort, Beschrijving) verbs (SetPropertyValue)
      
        action MakeArcFile
          create file "myModel.arc" as "text/arc" in Beschrijving for origin
            "domain model://perspectives.domains#MyModel"

      perspective on Contracts
        props (AccountName) verbs (Consult)

    thing Thee
      property Beschrijving (File)
        pattern = "text/arc" "Only .arc files (Perspectives Language source files) are allowed, so use `text//arc."
      property Soort (String)
    
    public Visitor at "https://perspectives.domains/cw_servers_and_repositories/" = sys:Me
      perspective on Thee
        props (Soort, Beschrijving) verbs (Consult)
      
      -- Hier hebben we een nieuwe versie van selfOnly nodig?
      perspective on Contracts
        only (CreateAndFill)
        props (AccountName) verbs (Consult)

      perspective on Contracts >> binding >> context >> Account
        only (Create, Fill)
      
      perspective on TeaRoomOperator
        props (FirstName) verbs (Consult)
      
      action MakeContract
        letA
          c <- create context Contract bound to Contracts
        in
          bind sys:Me to Account in c >> binding >> context
          bind TeaRoomOperator to Admin in c >> binding >> context
    
    context Contracts (relational, unlinked) filledBy Contract

  case Contract
    external
      property AccountName = context >> Account >> FirstName

    user Account filledBy sys:TheWorld$PerspectivesUsers
      perspective on Admin
      perspective on extern
        props (AccountName) verbs (Consult)

    user Admin filledBy TeaRoomOperator
      perspective on Account
        props (FirstName) verbs (Consult)
        