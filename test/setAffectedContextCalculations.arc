domain Test
  case TestCase1
    user Self
      on entry
        do
          Prop1 = false for ARole
    thing ARole
      property Prop1 (mandatory, Boolean)

  case TestCase2
    user Self
      state SomeState = context >> ARole >> Prop1
        on entry
          do
            Prop1 = false for ARole
    thing ARole
      property Prop1 (mandatory, Boolean)

  case TestCase3
    user Self
      state SomeState = context >> NestedContext >> binding >> Prop2
        on entry
          do
            Prop1 = false for ARole
    thing ARole
      property Prop1 (mandatory, Boolean)
    context NestedContext filledBy SubCase1
    case SubCase1
      external
        property Prop2 (mandatory, Boolean)

  case TestCase4
    user Self
      state SomeState = context >> NestedContext >> binding >> context >> SubCaseRole1 >> Prop2
        on entry
          do
            Prop1 = false for ARole
    thing ARole
      property Prop1 (mandatory, Boolean)
    context NestedContext filledBy SubCase2
    case SubCase2
      thing SubCaseRole1
        property Prop2 (mandatory, Boolean)

  case TestCase5
    context NestedContext filledBy SubCase3
    case SubCase3
      external
        property Prop2 (mandatory, Boolean)
      thing SubCaseRole3
        property Prop2 (mandatory, Boolean)
      user Self
        state SomeState = context >> extern >> Prop2
          on entry
            do
              Prop1 = false for ARole
      thing ARole
        property Prop1 (mandatory, Boolean)

  case TestCase6
    thing AnotherRole
      property Prop3 (mandatory, Boolean)
    context NestedContext6 filledBy SubCase4
    case SubCase4
      user Self
        state SomeState = context >> extern >> binder NestedContext6 >> context >> AnotherRole >> Prop3
          on entry
            do
              Prop1 = false for ARole
      thing ARole
        property Prop1 (mandatory, Boolean)
