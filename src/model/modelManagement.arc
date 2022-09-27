-- Copyright Joop Ringelberg and Cor Baars, 2020, 2021
domain ModelManagement
  use sys for model:System
  use mm for model:ModelManagement
  use p for model:Parsing

  -- The model description case.
  case Model
    aspect sys:Model
    external
      aspect sys:Model$External

  case ModelManagementApp
    indexed mm:MyManagedModels
    aspect sys:RootContext
    external
      aspect sys:RootContext$External

    context Models (relational) filledBy ManagedModel

    thing Repository (mandatory, relational)
      property Name (mandatory, String)
      property Url (mandatory, String)
      property Description (String)

    user Manager filledBy sys:PerspectivesSystem$User
      perspective on Models
        action RecompileAll
          SourcesChanged = true for context >> Models
        only (CreateAndFill, Remove)
        props (Name) verbs (Consult)
        props (SourcesChanged) verbs (Consult, SetPropertyValue)
      perspective on Repository
        defaults
        action CompileRepositoryModels
          callEffect p:CompileRepositoryModels( Url )

  case ManagedModel
    aspect sys:ContextWithNotification
    state UploadToRepository = extern >> (ArcOK and CrlOK and SourcesChanged)
      on entry
        do for Author
          callEffect p:UploadToRepository( extern >> ArcSource, extern >> CrlSource, Repository >> Url )
          SourcesChanged = false for extern
        notify Author
          "Model {ModelDescription >> ModelIdentification} has been uploaded to the repository {Repository >> Name}."
    external
      state ReadyToCompile = (exists ArcSource) and exists CrlSource
        perspective of Author
          action RestoreState
            ArcFeedback = "Explicitly restoring state"
            CrlFeedback = "Explicitly restoring state"
          action CompileArc
            delete property ArcFeedback
          action CompileCrl
            delete property CrlFeedback
      state ProcessArc = (exists ArcSource) and not exists ArcFeedback
        on entry
          do for Author
            ArcFeedback = callExternal p:ParseAndCompileArc( ArcSource ) returns String
            SourcesChanged = true
      state ProcessCrl = (exists CrlSource) and not exists CrlFeedback
        on entry
          do for Author
            CrlFeedback = callExternal p:ParseAndCompileCrl( CrlSource ) returns String
            SourcesChanged = true
      property ArcUrl (mandatory, String)
      property ArcSource (mandatory, String)
      property ArcFeedback (mandatory, String)
      property ArcOK = ArcFeedback matches regexp "^OK.*"
      property CrlUrl (mandatory, String)
      property CrlSource (mandatory, String)
      property CrlFeedback (mandatory, String)
      property CrlOK = CrlFeedback == "OK"
      property SourcesChanged (Boolean)
      property Name = context >> ModelDescription >> Name

      view Paths (ArcUrl, CrlUrl)
      view Feedback (ArcFeedback, CrlFeedback)

    thing Repository (mandatory) filledBy ModelManagementApp$Repository

    context ModelDescription filledBy sys:Model

    user Author = extern >> binder Models >> context >> Manager
      perspective on extern
        defaults
      perspective on ModelDescription
        defaults
      perspective on Repository
        defaults
