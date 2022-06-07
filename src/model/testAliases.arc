-- Copyright Joop Ringelberg and Cor Baars, 2022
-- A model to test the contextualisation of queries.

domain TestAliases
  use sys for model:System
  use ta for model:TestAliases

  -- The model description case.
  case Model
    aspect sys:Model
    external
      aspect sys:Model$External
