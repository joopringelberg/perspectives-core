-- Copyright Joop Ringelberg and Cor Baars 2022
-- A generic model of a top level context with an indexed name
-- and subcontexts with a user role filled by sys:User.

domain TopContexts
  use sys for model:System
  use tc for model:TopContexts

  case Model
    aspect sys:Model
    external
      aspect sys:Model$External

  case TopContext
    ---- NOTE: We cannot make this generic, as the indexed name has to be specific for 
    ---- the specialised context.
    ---- Instead, copy the line below to the specialised context and replace 
    ---- the indexed name.
    -- indexed tc:MyTopContext

    user Manager = sys:Me
      perspective on SubContexts
        only (CreateAndFill, Remove, Delete)
        props (Name) verbs (SetPropertyValue)
      perspective on SubContexts >> binding >> context >> CreatingUser
        only (Create, Fill)

    context SubContexts (relational) filledBy SubContext
      on entry
        do for Manager
          bind sys:Me to CreatingUser in binding >> context

  case SubContext
    external 
      property Name (String)

      -- On being invited to a SubContext, OtherUser puts it in 
      -- the list of SubContexts in his indexed instance of TopContext.
      state NotInTopContext = not exists binder SubContexts
        ---- NOTE: We cannot make this generic, as the indexed name has to be specific for 
        ---- the specialised context.
        ---- Instead, copy the two lines below to the specialised context and replace 
        ---- the indexed name.
        -- do for OtherUser
        --   bind origin to SubContexts in tc:MyTopContext 
    
    user CreatingUser filledBy sys:PerspectivesSystem$User

    -- No real need for this role; don't use it as an aspect.
    -- It is here because it is referenced in the outcommented lines above.
    user OtherUser
