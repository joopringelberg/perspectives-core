domain model://joopringelberg.nl#MinimalModel
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#MinimalModel

  case MinimalApp
    indexed mm:MyMinimalApp
    aspect sys:RootContext
    
    -- Without a role this context could never be opened.
    user Manager = sys:Me
