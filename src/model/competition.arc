-- Copyright Joop Ringelberg and Cor Baars, 2021
-- A model of a simple sports competition.
-- This model was created to test the selfonly modifier
domain Competition
  use sys for model:System
  use com for model:Competition

  -- The model description case.
  case Model
    aspect sys:Model
    external
      aspect sys:Model$External

  -- The entry point (the `application`), available as com:TheCompetition.
  case CompetitionApp
    indexed com:TheCompetition
    aspect sys:RootContext
    external
      aspect sys:RootContext$External

    -- The Manager can manage all Competitions. Each system user is manager in his own CompetitionApp!
    -- The manager role is constructed in the .crl file.
    user Manager filledBy sys:TheWorld$PerspectivesUsers
      indexed com:CompetitionManager
      perspective on Competitions
        defaults

    -- The Competitions I manage.
    context Competitions (relational) filledBy Competition

  case Competition
    state NotBound = not exists extern >> binder Competitions
      on entry
        do for Captains
          bind extern to Competitions in com:TheCompetition
    external
      property Name (String)
    -- The Manager can manage the Competition. But the end user cannot be the Manager of a competition
    -- if he is also a Captain in a Team. This is a weak way of ensuring that one cannot manage a competition
    -- created by another.
    user Manager = filter com:CompetitionManager with not exists filter currentcontext >> Captains with filledBy sys:Me
    -- if not exists Captains then com:CompetitionManager
      perspective on SubCompetitions
        defaults
      perspective on Captains
        props (LastName, Budget) verbs (Consult)
      perspective on extern

    -- The SubCompetitions I manage.
    context SubCompetitions (relational) filledBy SubCompetition

    -- Captains come from the Teams in the SubCompetitions. A Captain can only see himself and the Manager.
    user Captains = SubCompetitions >> binding >> context >> Teams >> binding >> context >> Captain
      perspective on Captains
        props (LastName, Budget) verbs (Consult)
        selfonly
      perspective on Manager
        props (LastName) verbs (Consult)
      perspective on extern
        props (Name) verbs (Consult)

  case SubCompetition
    state NoTeam = not exists Teams
      on entry
        do for Manager
          create context Team bound to Teams
    external
      property Name (String)
    user Manager = com:CompetitionManager
      perspective on Teams
        defaults

    context Teams (relational) filledBy Team

  case Team
    state NoCaptain = not exists Captain
      on entry
        do for Manager
          create role Captain

    external
      property Name (String)
    user Manager = com:CompetitionManager
      perspective on Captain
        defaults

    user Captain filledBy sys:TheWorld$PerspectivesUsers
      property Budget (Number)
