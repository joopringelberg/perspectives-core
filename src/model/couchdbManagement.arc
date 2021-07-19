-- Copyright Joop Ringelberg and Cor Baars 2021
domain CouchdbManagement
  use sys for model:System
  use cdb for model:Couchdb
  use cm for model:CouchdbManagement

  -- A BOOTSTRAP PROBLEM TO SOLVE: where do instances of these types come from?
  --    *  cm:Model
  --    *  cm:CouchdbManagementApp
  --    *  cm:LocalModelsOverview
  -- model:CouchdbManagement, like model:System, should be added to a local installation when it is created.
  -- We probably need to load a file with instance serialisations (like CRL) for the above instances.

  -- The model description case.
  -- It should be stored in public space.
  -- This is where we provide actions to construct Indexed Contexts and Roles particular for this model.
  -- We'd love to make that state dependent, but public contexts have no states.
  -- Notice that the action given here serves as an example only, as we need to bootstrap an installation
  -- with instances before we can use the machinery defined below (see the bootstrap remarks above).
  case Model
    aspect ManagedModel
    --storage public
    external
      aspect cm:ManagedModel$External
      -- This does not need to be a perspective on the external role, but we cannot
      -- create a perspective otherwise.
      perspective of Guest
        action Initialize
          -- Generate an instance of cm:CouchdbManagementApp, with a generated name, and bind it:
          createContext_ cm:CouchdbManagementApp bound to filter IndexedContext with Name == "CouchdbManagement$MyCouchdbApp"
          -- Generate an instance of cm:LocalModelsOverview, with a generated name, and bind it:
          createContext_ cm:LocalModelsOverview bound to filter IndexedContext with Name == "CouchdbManagement$MyModels"

  case CouchdbManagementApp
    indexed cm:MyCouchdbApp
    aspect sys:RootContext
    external
      aspect sys:RootContext$External
    user Manager = sys:Me
      perspective on CouchdbServers
        defaults

    context CouchdbServers filledBy CouchdbServer

  -- This case represents all models for this installation.
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
              callEffect cdb:UpdateModel( LastVersionUrl, ModelIdentification)
              PerformUpdate = false
          on exit
            do for LocalUser
              CurrentVersion = LastVersion
      -- When an instance of LocalModels is created by the end user by dropping a
      -- ManagedModel, we install the model automatically.
      on entry
        do for LocalUser
          callEffect cdb:InstallModel( lastVersionUrl, ModelIdentification)

      property PerformUpdate (Boolean)
      property CurrentVersion (String)

  -- TODO. Add a Guest who can request an Account.
  -- This case should be in public space.
  case CouchdbServer
    --storage public
    external
      property Url (mandatory, String)
      property Name (mandatory, String)

    -- This role should be in private space.
    user Admin filledBy sys:PerspectivesSystem$User
      perspective on Accounts
        defaults
      perspective on Repositories
        defaults

    -- This role should be in private space.
    user Accounts filledBy sys:PerspectivesSystem$User
      perspective on Accounts
        defaults
        selfonly
      perspective on filter Repositories with IsPublic
        verbs (Consult)

    -- This role should be in public space.
    context Repositories filledBy Repository
      --storage public

  -- This could be a public context (stored in publicly available space,
  -- albeit behind passwords).
  case Repository
    --storage public
    state IAmNotAnAccount = not exists Accounts
    external
      -- Only public repositories will be visible to Accounts of CouchdbServers.
      property IsPublic (mandatory, Boolean)
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
      property IsAccepted (Boolean)
      property IsRejected (Boolean)
      state Root = true
        state Waiting = not IsRejected and not IsAccepted
        state Rejected = IsRejected
        state Accepted = IsAccepted
          -- An account that is accepted has a perspective on available models.
          perspective on AvailableModels
            verbs (Consult)
          perspective on Accounts
            only (RemoveFiller)
            verbs (Consult)
            selfonly

    -- Role Guest is available so a user with a Couchdb Account can request
    -- a Repository account.
    -- Because CouchdbServer$Accounts is selfonly, there will only ever be one
    -- instance of Guest for any sys:Me.
    user Guest = extern >> binder Repositories >> context >> Accounts
      -- Guest can request an Account as long as he does not have one.
      perspective on Accounts
        in state IAmNotAnAccount of context
          only (Create, Fill)
        in state Waiting of object
          -- Guest can see he is not yet rejected, but not accepted either.
          props (IsRejected) verbs (Consult)
        in state Rejected of object
          -- Guest can see his request for an account is rejected.
          props (IsRejected) verbs (Consult)

    -- This role should be stored in private space.
    user Authors filledBy CouchdbServer$Accounts
      -- Like Accounts, Authors can see all models.
      perspective on AvailableModels
        verbs (Consult)

    -- This role should be stored in public space.
    context AvailableModels filledBy ManagedModel
      --storage public
      state AuthoredByMe = binding >> context >> Author >> binds sys:Me
        -- An Author can do anything with his own models.
        perspective of Authors
          defaults

  -- This context is only used as Aspect. Contexts that specialise it should be stored in public space.
  case ManagedModel
    external
      property Name (mandatory, String)
      property ModelIdentification (mandatory, String)
      property LastVersion = (context >> Versions >> VersionNumber) >>= maximum
      property LastVersionUrl = (filter context >> Versions with (VersionNumber == object >> LastVersion)) >> Url

    -- This role should be stored in public space.
    thing Versions
      --storage public
      property Name = context >> extern >> Name
      property Description (String)
      -- Eventually, we'll use semantic versioning.
      property VersionNumber (mandatory, String)
      property LastVersion = context >> extern >> LastVersion
      -- Url is used to fetch the compiled model.
      property Url (mandatory, String)

    -- This role should be stored in public space.
    user Author filledBy Repository$Authors
      --storage public
      perspective on Versions
        defaults
      perspective on extern
        defaults

        -- The action BootstrapType should change the type of the context and external role to that of the
        -- specialised model type of the ManagedModel.
        -- That would allow the Author to turn a general ManagedModel into the public instance of
        -- the description of his model.
        -- Consequently, he could create its indexed contexts and roles, bootstrapping their creation.
        action BootstrapType

    user Guest = sys:Me
      perspective on Versions
        verbs (Consult)
      perspective on extern

    user User = extern >> binder LocalModels >> context >> LocalUser

    -- This should be bound to an instance of the locally specialised version of the model.
    context ModelDescription filledBy ManagedModel
      perspective of User
        -- The User visits the instance to execute the Initialize action.
        verbs (Consult)
      perspective of Author
        -- The Author creates this instance.
        defaults

    -- This role should be stored in private space.
    context IndexedContext (mandatory) filledBy sys:RootContext
      property Name (mandatory, String)

    -- This role should be stored in private space.
    thing IndexedRole (relational)
      property Name (mandatory, String)
