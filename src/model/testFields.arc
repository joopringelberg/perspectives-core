-- Copyright Joop Ringelberg and Cor Baars, 2021
-- A model to test smart field controls.

domain TestFields
  use sys for model:System
  use tf for model:TestFields

  -- The model description case.
  case Model
    aspect sys:Model
    external
      aspect sys:Model$External

  -- The entry point (the `application`), available as tf:TheTestFields.
  case TestFieldsApp
    indexed tf:TheTestFields
    aspect sys:RootContext
    external
      aspect sys:RootContext$External

    user Tester filledBy sys:PerspectivesSystem$User
      indexed com:Tester
      perspective on TestRole
        defaults
      perspective on TestTable
        defaults

    thing TestRole
      property Text (mandatory, String)
        minLength = 100
        maxLength = 200
      property Bool (Boolean)
      property ADateTime (DateTime)
      property ANumber (Number)
      property AnEmail (Email)

    thing TestTable (relational)
      property Text (mandatory, String)
      property Bool (Boolean)
      property ADateTime (DateTime)
      property ANumber (mandatory, Number)
      property AnEmail (Email)
