-- Copyright Joop Ringelberg and Cor Baars 2021
domain CouchdbManagement
  use sys for model:System
  use cdb for model:Couchdb
  use cm for model:CouchdbManagement

  -- The model description case.
  case Model
    aspect sys:Model
    external
      aspect sys:Model$External

  case CouchdbManagementApp
    indexed cm:MyCouchdbApp
    aspect sys:RootContext
    external
      aspect sys:RootContext$External
    user Manager filledBy sys:PerspectivesSystem$User
      perspective on CouchdbServers
        defaults

    context CouchdbServers filledBy CouchdbServer

  -- This case represents all models for this installation.
  -- This case should be stored in private space (the default).
  -- NOTE. Twee indexed contexts gaat misschien niet geod.
  case LocalModelsOverview
    indexed cm:MyModels
    aspect sys:RootContext
    user LocalUser = sys:Me
      perspective on LocalModels
        verbs (Consult)
        props (PerformUpdate, CurrentVersion) verbs (SetPropertyValue)
        all roleverbs

    context LocalModels filledBy ManagedModel
      state OutOfDate = LastVersion > CurrentVersion
        state Update = PerformUpdate
          on entry
            do for LocalUser
              -- For the implementer: notice that this effect is called with
              -- a context role instance!
              callEffect cdb:UpdateModel()
          on exit
            do for LocalUser
              CurrentVersion = LastVersion
              PerformUpdate = false
      property PerformUpdate (Boolean)
      property CurrentVersion (String)

  case CouchdbServer
    external
      property Url (mandatory, String)
      property Name (mandatory, String)

    user Admin filledBy sys:PerspectivesSystem$User
      perspective on Accounts
        defaults
      perspective on Repositories
        defaults

    user Accounts filledBy sys:PerspectivesSystem$User
      perspective on Accounts
        defaults
        selfonly
      perspective on Repositories
        verbs (Consult)

    context Repositories filledBy Repository

  -- This could be a public context (stored in publicly available space,
  -- albeit behind passwords).
  case Repository
    user Admin filledBy CouchdbServer$Admin
      perspective on Accounts
        defaults
      perspective on AvailableModels
        defaults
      perspective on Authors
        defaults

    -- This role should stored in private space.
    -- By making the role unlinked, there are no references from the context to
    -- Accounts role instances (but there are references the other way round).
    -- This allows us to create a screen to browse through Accounts without
    -- loading them all at once.
    user Accounts (unlinked) filledBy CouchdbServer$Accounts
      perspective on AvailableModels
        verbs (Consult)
      perspective on Accounts
        verbs (Consult)
        selfonly

    -- This role should be stored in private space.
    user Authors filledBy CouchdbServer$Accounts
      -- Like Accounts, Authors can see all models.
      perspective on AvailableModels
        verbs (Consult)

    -- This role should be stored in public space.
    context AvailableModels filledBy ManagedModel
      state AuthoredByMe = binding >> context >> Author >> binds sys:Me
        -- An Author can do anything with his own models.
        perspective of Authors
          defaults

  -- This context should be stored in public space.
  case ManagedModel
    external
      property Name (mandatory, String)
      property LastVersion = (context >> Versions >> VersionNumber) >>= maximum
      property LastVersionUrl = (filter context >> Versions with (VersionNumber == object >> LastVersion)) >> Url

    -- This role should be stored in private space by Guests.
    thing Versions
      property Name = context >> extern >> Name
      property Description (String)
      -- Eventually, we'll use semantic versioning.
      property VersionNumber (mandatory, String)
      property LastVersion = context >> extern >> LastVersion
      -- Url is used to fetch the compiled model.
      property Url (mandatory, String)

    user Author filledBy Repository$Authors
      perspective on Versions
        defaults

    user Guest = sys:Me
      perspective on Versions
        verbs (Consult)
      perspective on extern
