-- Copyright Joop Ringelberg and Cor Baars, 2021
-- A model to test smart field controls.

domain model://joopringelberg.nl#TestFields --@1.0.0
  use sys for model://perspectives.domains#System
  use tf for model://joopringelberg.nl#TestFields

  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- We must first create the context and then later bind it.
          -- If we try to create and bind it in a single statement, 
          -- we find that the Installer can just create RootContexts
          -- as they are the allowed binding of StartContexts.
          -- As a consequence, no context is created.
          app <- create context TestFieldsApp
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind app >> extern to StartContexts in sys:MySystem
          Name = "TestFields Management" for app >> extern

  aspect user sys:PerspectivesSystem$Installer

  -- The entry point (the `application`), available as tf:TheTestFields.
  case TestFieldsApp
    indexed tf:TheTestFields
    aspect sys:RootContext

    external
      aspect sys:RootContext$External
    
    user Tester = sys:Me
      perspective on TestRole
        props (Text, Bool, ADate, ANumber, AnEmail, WeekDay, Appel) verbs (Consult, SetPropertyValue)
        only (Remove, Create)
      perspective on TestTable
        defaults
      screen "Test screen"
        tab "Test"
          row
            form TestRole
              props (Text, WeekDay, ADate) verbs (SetPropertyValue)
              only (Create)
          row
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
      property ADate (Date)
        minInclusive = '2022-04-15'
      property ANumber (Number)
        minInclusive = 10
        maxInclusive = 80
      property AnEmail (Email)
        minLength = 10
      property WeekDay (String)
        enumeration = ("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
      property Appel (String)
        pattern = ".*appel.*" "Any word with the string `appel` in it."

    thing TestTable (relational)
      property Text (mandatory, String)
      property Bool (Boolean)
      property ADate (DateTime)
      property ANumber (mandatory, Number)
      property AnEmail (Email)
      view Limited (Text, ADate)
