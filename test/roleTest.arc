domain model://joopringelberg.nl#TestModel
  use mm for model://joopringelberg.nl#TestModel

  thing Filler1 filledBy Filler2
    property Filler1Prop1 (String)
  
  thing Filler2
    property Prop1 (String)

  thing Filler3
    property Prop1 (String)

  thing Aspect1
    property Prop1 (String)

  case Root
    external
      property RootProperty (String)

  case Test1
    thing Role1 filledBy Filler1
      aspect mm:Aspect1
      property Prop1 (String)

  -- Test the test op fillers by checking whether Filler1 may fill Test2$Role1
  -- generalisesRoleType
  case Test2
    thing Role1 filledBy Filler2
    thing Role2 filledBy Aspect1
  
  -- Two cases to test context role filling based on aspect.
  case Test3
    aspect mm:Root
    external
  
  case Test4
    context Roots filledBy Root

  case Test5
    thing Role1 filledBy (Filler1, Filler3)
  
  case Test6
    thing Role1 filledBy Filler2
      aspect mm:Filler1
    thing Role2 filledBy Filler3
      aspect mm:Filler1
