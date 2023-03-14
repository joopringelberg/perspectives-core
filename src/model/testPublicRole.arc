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
      perspective on Thee
        only (Create)
        props (Soort) verbs (SetPropertyValue)

    thing Thee
      property Soort (String)
    
    public Visitor at "https://perspectives.domains/cw_servers_and_repositories/" = sys:Me
      perspective on Thee
        props (Soort) verbs (Consult)
        