-- Copyright Joop Ringelberg and Cor Baars 2021
domain CouchdbManagement
  use sys for model:System
  use cm for model:CouchdbManagement
  use mod for model:Models
  use acc for model:BodiesWithAccounts

  -- The INDEXED context cm:MyCouchdbApp, that is the starting point containing all CouchdbServers.
  case CouchdbManagementApp
    indexed cm:MyCouchdbApp
    aspect sys:RootContext
    external
      aspect sys:RootContext$External
    -- Every user manages his own CouchdbServers.
    -- This manager is not necessarily the admin of a Couchdb installation!
    user Manager = sys:Me
      perspective on CouchdbServers
        defaults

    context CouchdbServers filledBy CouchdbServer

  -- PUBLIC
  -- This contexts implements the BodyWithAccounts pattern.
  case CouchdbServer
    aspect acc:Body
    --storage public
    external
      property Url (mandatory, String)
      property Name (mandatory, String)

    -- This role should be in private space.
    -- Admin in Couchdb of a particular server.
    user Admin filledBy sys:PerspectivesSystem$User
      aspect acc:Body$Admin

      property Username (mandatory, String)
      property Password (mandatory, String)
      perspective on Repositories
        defaults

    -- Note that the aspect acc:Body introduces a Guest role
    -- with a perspective that allows it to create an Account.

    -- This role should be in private space.
    user Accounts (unlinked) filledBy sys:PerspectivesSystem$User
      aspect acc:Body$Accounts
      property Username (mandatory, String)
      property Password (mandatory, String)
      perspective on filter Repositories with IsPublic
        verbs (Consult)

    -- This role should be in public space.
    context Repositories filledBy Repository
      --storage public

  -- PUBLIC
  -- This contexts implements the BodyWithAccounts pattern.
  case Repository
    aspect acc:Body
    --storage public
    external
      -- Only public repositories will be visible to Accounts of CouchdbServers.
      property IsPublic (mandatory, Boolean)
    user Admin filledBy CouchdbServer$Admin
      aspect acc:Body$Admin

      perspective on AvailableModels
        defaults
      perspective on Authors
        defaults

    -- This role should stored in private space.
    -- By making the role unlinked, there are no references from the context to
    -- Accounts role instances (but there are references the other way round).
    -- This allows us to create a screen to browse through Accounts without
    -- loading them all at once.
    -- No further credentials are needed to access a Repository.
    -- This is because, in Couchdb, access to a database can be determined
    -- through (Couchdb database) roles.
    user Accounts filledBy CouchdbServer$Accounts
      aspect acc:Body$Accounts
      in state Accepted
        -- An account that is accepted has a perspective on available models.
        perspective on AvailableModels
          verbs (Consult)

    -- Note that the aspect acc:Body introduces a Guest role
    -- with a perspective that allows it to create an Account.

    -- This role should be stored in private space.
    user Authors filledBy CouchdbServer$Accounts
      -- Like Accounts, Authors can see all models.
      perspective on AvailableModels
        verbs (Consult)

    -- This role should be stored in public space.
    context AvailableModels filledBy mod:ModelDescription
      --storage public
      state AuthoredByMe = binding >> context >> Author >> binds sys:Me
        -- An Author can do anything with his own models.
        perspective of Authors
          defaults
