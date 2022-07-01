-- Copyright Joop Ringelberg and Cor Baars 2022
-- A generic model of board games for two players, 
-- with strict turn taking.

domain TwoPlayerBoardGame
  use sys for model:System
  use tpbg for model:TwoPlayerBoardGame
  use tc for model:TopContexts

  case Model
    aspect sys:Model
    external
      aspect sys:Model$External

  -- A top level role used as aspect in this model.
  user Player filledBy sys:PerspectivesSystem$User
    property BoardState (String)
    state CanPlay = not exists BoardState

  case GamesApp
    aspect sys:RootContext
    aspect tc:TopContext

    indexed tpbg:MyGames

    aspect user tc:TopContext$Manager

    context Games (relational) filledBy Game
      aspect tc:TopContext$SubContexts

  case Game
    aspect tc:SubContext
    on entry
      -- By setting an empty BoardState for the OtherPlayer
      -- InitiatingPlayer claims the initiative.
      do for InitiatingPlayer
        letA 
          other = create OtherPlayer
        in
          BoardState = "" for other

    -- Copied and modified from model:TopContexts
    -- Guarantees that a receiving player can see the game in his indexed version of MyGames.
    in state tc:SubContext$NotInTopContext
      do for OtherPlayer
        bind origin to Games in tpbg:MyGames

    external 
      property GameState (String)
    
    user InitingPlayer
      aspect tc:SubContext$CreatingUser
      aspect Player

      -- Player can see the state of the game.
      perspective on extern
        props (GameState) verbs (Consult)
      
      -- Player can choose an OtherPlayer.
      perspective on OtherPlayer
        only (Fill)
        props (Name) verbs Consult

      in state CanPlay
        -- InitingPlayer can now add his own move to the GameState, storing it
        -- in his own BoardState. Immediately afterwards...
        perspective on InitingPlayer
          props (BoardState) verbs (SetPropertyValue)
        -- he should delete the others BoardState, thereby effectively yielding control
        -- to OtherPlayer.
        perspective on OtherPlayer
          props (BoardState) verbs (Delete)
        -- Having the right to set GameState, we will have InitingPlayer assign it
        -- as new value the current value of BoardState, and then hand over control
        -- to OtherPlayer by deleting his value of BoardState.
        perspective on extern
          props (GameState) verbs (SetPropertyValue)
        
      -- InitingPlayer will terminate this state by setting BoardState.
      on exit of CanPlay
        do 
          GameState = BoardState in context >> extern
          delete property BoardState for context >> OtherPlayer


    user OtherPlayer
      aspect Player

      -- OtherPlayer can see the state of the game.
      perspective on extern
        props (GameState) verbs (Consult)
      
      -- OtherPlayer can see InitingPlayer
      perspective on InitingPlayer
        props (Name) verbs Consult

      in state CanPlay
        perspective on OtherPlayer
          props (BoardState) verbs (SetPropertyValue)
        perspective on InitingPlayer
          props (BoardState) verbs (Delete)
        perspective on extern
          props (GameState) verbs (SetPropertyValue)

      on exit of CanPlay
        do 
          GameState = BoardState in context >> extern
          delete property BoardState for context >> InitingPlayer

  ------------------------------------------------------------------------------
  ---- ALTNERNATIVE MODEL
  ------------------------------------------------------------------------------

  case GenericGame
    aspect tc:SubContext

    external 
      property GameState (String)

    user Self = filter GamePlayer with filledBy sys:Me
    user Other = filter GamePlayer with not filledBy sys:Me

    user GamePlayer filledBy sys:PerspectivesSystem$User
      aspect tc:SubContext$CreatingUser
      property BoardState (String)
      
      -- Player can see the state of the game.
      perspective on extern
        props (GameState) verbs (Consult)
      
      perspective on GamePlayer
        props (Name) verbs Consult

      state CanPlay = not exists BoardState

        -- InitingPlayer can now add his own move to the GameState, storing it
        -- in his own BoardState. Immediately afterwards...
        perspective on Self
          props (BoardState) verbs (SetPropertyValue)
        -- he should delete the others BoardState, thereby effectively yielding control
        -- to OtherPlayer.
        perspective on Other
          props (BoardState) verbs (Delete)
        -- Having the right to set GameState, we will have InitingPlayer assign it
        -- as new value the current value of BoardState, and then hand over control
        -- to OtherPlayer by deleting his value of BoardState.
        perspective on extern
          props (GameState) verbs (SetPropertyValue)
        
      -- GamePlayer will terminate this state by setting BoardState.
      on exit of CanPlay
        do 
          GameState = BoardState in context >> extern
          delete property BoardState for context >> Other

  case Game
    aspect GenericGame

    on entry
      -- By setting an empty BoardState for the OtherPlayer
      -- InitiatingPlayer claims the initiative.
      do for InitiatingPlayer
        letA 
          other = create OtherPlayer
        in
          BoardState = "" for other

    user InitingPlayer
      aspect tc:GenericGame$GamePlayer

      -- InitingPlayer can choose an OtherPlayer.
      perspective on OtherPlayer
        only (Fill)

    user OtherPlayer
      aspect tc:GenericGame$GamePlayer