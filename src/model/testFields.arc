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
      screen "Test screen"
        tab "Test"
          row
            column
              form TestRole
                props (Text, WeekDay) verbs (Consult)
              table "MyTable" TestTable
                view Limited verbs (Consult)
                only (Remove)
            --graph TestTable
              --y = ANumber
          --row
            --masterSlave TestTable
              --master = ViewX
              --slave = ViewY

    thing TestRole
      property Text (mandatory, String)
        minLength = 100
        maxLength = 200
      property Bool (Boolean)
      property ADateTime (DateTime)
        minInclusive = '2022-04-15'
      property ANumber (Number)
        minInclusive = 10
        maxInclusive = 80
      property AnEmail (Email)
        minLength = 10
      property WeekDay (String)
        enumeration = ("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
      property Appel (String)
        pattern = /.*appel.*/ "Any word with the string `appel` in it."

    thing TestTable (relational)
      property Text (mandatory, String)
      property Bool (Boolean)
      property ADateTime (DateTime)
      property ANumber (mandatory, Number)
      property AnEmail (Email)
      view Limited (Text, ADateTime)
