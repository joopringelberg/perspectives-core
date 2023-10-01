-- Copyright Joop Ringelberg and Cor Baars, 2020, 2021 -15
-- A model to evaluate property expressions interactively.
domain model://perspectives.domains#ExpressionEvaluator
  use sys for model://perspectives.domains#System
  use util for model://perspectives.domains#Utilities
  use eval for model://perspectives.domains#ExpressionEvaluator

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- We must first create the context and then later bind it.
          -- If we try to create and bind it in a single statement, 
          -- we find that the Installer can just create RootContexts
          -- as they are the allowed binding of StartContexts.
          -- As a consequence, no context is created.
          app <- create context EvaluatorApp
          indexedcontext <- create role IndexedContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind app >> extern to StartContexts in sys:MySystem
          Name = "Expression Evaluator App" for app >> extern
          bind_ app >> extern to indexedcontext
          IndexedContexts$Name = app >> indexedName for indexedcontext
  
  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (eval:MyEvaluations >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (eval:MyEvaluations >> extern)
      in
        remove context indexedcontext
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer

  -- The entry point (the `application`), available as eval:MyEvaluations.
  case EvaluatorApp
    indexed eval:MyEvaluations
    aspect sys:RootContext
    external
      aspect sys:RootContext$External
    
    user Evaluator = sys:Me
      perspective on Evaluations
        only (CreateAndFill, Remove, Delete)
        props (Description) verbs (Consult)
    
    context Evaluations (relational) filledBy Evaluation
  
  case Evaluation
      external 
        property Description (String)
        property Expression (String)
        property EvaluationResult (String)
        property ErrorMessages = EvaluationResult >> callExternal util:SelectR( "^error#(.*)" ) returns String
        property Result = EvaluationResult >> callExternal util:SelectR( "^result#(.*)" ) returns String
        property Numeric1 (Number)
        property Numeric2 (Number)
        property Boolean1 (Boolean)
        property Boolean2 (Boolean)
        property String1 (String)
        property String2 (String)
      
      user Evaluator = sys:Me
        perspective on External
          props (Description, Expression) verbs (SetPropertyValue, Consult)
          props (Numeric1, Numeric2, Boolean1, Boolean2, String1, String2) verbs (SetPropertyValue)
          props (ErrorMessages, Result) verbs (Consult)
          action Evaluate
            EvaluationResult = callExternal util:EvalExpression( Expression ) returns String
        
        screen "Evaluation Test"
          row 
            column
              form "This test" External
                props (Description) verbs (SetPropertyValue)
            column
              form "Inputs" External
                props (Numeric1, Numeric2, Boolean1, Boolean2, String1, String2) verbs (SetPropertyValue)
          row
            form "Expression" External
              props (Expression) verbs (SetPropertyValue)
          row
            form "Evaluation Results" External
              props (Result, ErrorMessages) verbs (Consult)

