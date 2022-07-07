-- TopContexts. Copyright Joop Ringelberg and Cor Baars 2022

-- A generic pattern of a top level context with an indexed name
-- and subcontexts with a user role automatically filled by sys:User.
-- When a peer makes a SubContext instance and puts you in the role MeInSubContext in it,
-- the new SubContext is automatically added to your TopContext.
--
-- NOTE: We cannot make this generic, as the indexed name has to be specific for 
-- the specialised context. Instead, 
--    * use tc:TopContext as aspect context
--    * use tc:TopContext$Manager as aspect role
--    * use tc:SubContext as aspect context
--    * use tc:SubContext$MeInSubContext as aspect role
--    * add to the specialisation of SubContext:
--
--       user SubManager = tc:MyTopContext >> Manager
--       external
--         in state NotInTopContext
--           do for SubManager
--             bind origin to SubContexts in <the indexed name of your specialisation of TopContext> 

domain TopContexts
  use sys for model:System
  use tc for model:TopContexts

  case Model
    aspect sys:Model
    external
      aspect sys:Model$External

  case TopContext
    indexed tc:MyTopContext

    user Manager = sys:Me

      -- Manager can create a new instance of SubContexts and fill it with a new instance of SubContext.
      perspective on SubContexts
        only (CreateAndFill, Remove, Delete)
        props (Name) verbs (SetPropertyValue)

      -- Manager can create a role for himself in a SubContext instance.
      perspective on SubContexts >> binding >> context >> MeInSubContext
        only (Create, Fill)

    context SubContexts (relational) filledBy SubContext
      on entry
        do for Manager
          bind sys:Me to MeInSubContext in binding >> context

  case SubContext
    external 
      property Name (String)

      -- When a peer creates a SubContext and puts 'me' into role InvitedUser,
      -- 'I' need to add it to MyTopContext.
      -- The computed role SubContext$Manager can do just that.
      state NotInTopContext = not exists binder SubContexts
        -- Copy the next two lines, replace tc:MyTopContext with the IndexedName of your
        -- TopContext specialisation.
        do for SubManager
          bind origin to SubContexts in tc:MyTopContext
          -- Sketch of type level query:
          -- bind origin to SubContexts in type >> filled in TopContext >> context >> indexedContext

    -- Copy the next line, replace tc:MyTopContext with the IndexedName of your
    -- TopContext specialisation.
    user SubManager = tc:MyTopContext >> Manager
    -- Sketch of type level query:
    -- user SubManager = type >> extern >> filled in TopContext >> context >> indexedContext >> Manager

    user MeInSubContext filledBy sys:PerspectivesSystem$User
