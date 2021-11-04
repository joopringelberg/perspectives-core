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

    context Models filledBy ManagedModel

    thing Repository (mandatory)
      property Name (mandatory, String)
      property Url (mandatory, String)
      property Description (String)

    user Manager filledBy sys:PerspectivesSystem$User
      perspective on Models
      perspective on Repository

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
      state Root = true
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
      property ArcOK = ArcFeedback == "OK"
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

    user Author (mandatory) filledBy Manager
      perspective on extern

      perspective on ModelDescription
      perspective on Repository
