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

    context Repositories filledBy Repository

  case Repository
    user Admin filledBy CouchdbServer$Admin
      perspective on Accounts
        defaults
      perspective on AvailableModels
        defaults
      perspective on Authors
        defaults

    user Accounts filledBy CouchdbServer$Accounts
      perspective on AvailableModels

    user Authors filledBy CouchdbServer$Accounts
      -- verfijn zodat een Author alleen zijn eigen modellen ziet.
      perspective on AvailableModels
        defaults

    context AvailableModels filledBy ManagedModel

  case ManagedModel
    external
      property Name (mandatory, String)
      property LastVersion = (context >> Versions >> VersionNumber) >>= maximum

    thing Versions
      state OutOfDate = LastVersion > VersionNumber
      --state OutOfDate = true
        state Update = PerformUpdate
          on entry
            do for Author
              callEffect cdb:UpdateModel()
          on exit
            do for Author
              PerformUpdate = false
      property Name = context >> extern >> Name
      property Description (String)
      -- Eventually, we'll use semantic versioning.
      property VersionNumber (mandatory, String)
      property LastVersion = context >> extern >> LastVersion
      property Url (mandatory, String)
      property PerformUpdate (Boolean)

    user Author filledBy Repository$Authors
      perspective on Versions
        defaults

    user Guest = sys:Me
      perspective on Versions
        verbs (Consult)
      perspective on extern
