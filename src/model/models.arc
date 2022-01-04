-- Copyright Joop Ringelberg and Cor Baars 2021
domain Models
  use sys for model:System
  use cdb for model:Couchdb
  use mod for model:Models

  -- The model description case.
  -- REMOVE ONCE WE CREATE INSTANCES WITH AN ACTION
  case Model
    aspect sys:Model
    external
      aspect sys:Model$External

  -- The INDEXED context mod:ModelsOverview, a case holding all models available in this installation.
  case ModelsOverview
    indexed mod:MyModels
    aspect sys:RootContext
    user LocalUser = sys:Me
      perspective on LocalModels
        verbs (Consult)
        props (PerformUpdate, CurrentVersion) verbs (SetPropertyValue)
        all roleverbs

    context LocalModels filledBy ModelDescription
      state OutOfDate = LastVersion > CurrentVersion
        state Update = PerformUpdate
          on entry
            do for LocalUser
              -- For the implementer: notice that this effect is called with
              -- a context role instance!
              callEffect cdb:UpdateModel( LastVersionUrl, ModelIdentification, IncludingDependencies)
              PerformUpdate = false
          on exit
            do for LocalUser
              CurrentVersion = LastVersion
      property PerformUpdate (Boolean)
      property IncludingDependencies (Boolean)
      property CurrentVersion (String)

  -- The PUBLIC context type ModelDescription allows for a description of a model from the outside (not needing the model itself).
  case ModelDescription
    --storage public
    external
      property Name (mandatory, String)
      property ModelIdentification (mandatory, String)
      property LastVersion = (context >> Versions >> VersionNumber) >>= maximum
      property LastVersionUrl = (filter context >> Versions with (VersionNumber == origin >> LastVersion)) >> Url

    -- This role should be stored in public space.
    -- It needs no filler; we use it just for its properties.
    thing Versions
      --storage public
      property Name = context >> extern >> Name
      property Description (String)
      -- Semantic versioning.
      property VersionNumber (mandatory, String)
      property LastVersion = context >> extern >> LastVersion
      -- Url is used to fetch the compiled model, i.e. domeinfile.
      property Url (mandatory, String)

    -- This role should be stored in public space.
    user Author filledBy sys:PerspectivesSystem$User
      --storage public
      state Test = true
        on entry
          do
            bind sys:Me to Author
      perspective on Versions
        defaults
      perspective on extern
        defaults

    user Guest = sys:Me
      perspective on Versions
        verbs (Consult)
      perspective on extern
        props (Name, ModelIdentification, LastVersion, LastVersionUrl) verbs (Consult)
        in object state
          action Boot
            letA
              -- Create the indexed context:
              irole <- createContext ModelsOverview bound to IndexedContexts in sys:MySystem
            in
              -- Add the indexed name:
              Name = "model://perspect.it/Models$MyModels" for irole
              -- Add the model description to MyModels:
              -- TODO: gebruik de uitgecommentarieerde regel zodra we URLs als identifiers herkennen.
              -- Voor nu gebruiken we het type in plaats van de beschrijving.
              --bind https://cw.perspect.it/SimpleChat to LocalModels
              bind sys:Me to Author
