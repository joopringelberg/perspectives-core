domain Test

  use sys: for modelSystem
  case TestCase1
    user Self filledBy sys:PerspectivesSystem$User
      state SomeState = true
        on entry
          do
            remove ARole
    thing ARole

  case TestCase2
    user Self filledBy sys:PerspectivesSystem$User
    state SomeState = true
      on entry
        do
        createRole ARole
    thing ARole

  case TestCase3
    user Self filledBy sys:PerspectivesSystem$User
      property Prop1 (Boolean)
      state SomeState = Prop1 == true
        on entry
          do
            move Nested1 >> binding >> context >> ARole to Nested2 >> binding >> context
    context Nested1 filledBy NestedContext
    context Nested2 filledBy NestedContext
    case NestedContext
      thing ARole

  case TestCaseBind
    user Self filledBy sys:PerspectivesSystem$User
      state SomeState = true
        on entry
          do
            bind AnotherRole to ARole
            bind AnotherRole to ARole in Nested >> binding >> context
    thing ARole filledBy AnotherRole
    thing AnotherRole
    context Nested filledBy NestedCase
    case NestedCase
      thing ARole

  case TestCaseBind_
    user Self filledBy sys:PerspectivesSystem$User
      state SomeState = true
        on entry
          do
            bind_ AnotherRole4 to ARole4
            bind_ AnotherRole4 to Nested4 >> binding >> context >> ARole4
    thing ARole4 filledBy AnotherRole4
    thing AnotherRole4
    context Nested4 filledBy NestedCase4
    case NestedCase4
      thing ARole4 filledBy AnotherRole4

  case TestCaseUnbind
    user Self filledBy sys:PerspectivesSystem$User
      state SomeState = true
        on entry
          do
            unbind AnotherRole5
    thing ARole5 filledBy AnotherRole5
    thing AnotherRole5
    context Nested5 filledBy NestedCase5
    case NestedCase5
      thing ARole5

  case TestCaseUnbindQualified
    user Self filledBy sys:PerspectivesSystem$User
      state SomeState = true
        on entry
          do
            unbind AnotherRole6 from TestCaseUnbindQualified$ARole6
    thing ARole6 filledBy AnotherRole6
    thing AnotherRole6
    context Nested6 filledBy NestedCase6
    case NestedCase6
      thing ARole6

  case TestCaseUnbind_
    user Self filledBy sys:PerspectivesSystem$User
    state SomeState = true
      on entry
        do
          unbind_ AnotherRole7 from Nested7 >> binding >> context >> ARole7
    thing ARole7 filledBy AnotherRole7
    thing AnotherRole7
    context Nested7 filledBy NestedCase7
    case NestedCase7
      thing ARole7

  case TestCaseDeleteProp
    user Self filledBy sys:PerspectivesSystem$User
    state SomeState = true
      on entry
        do
          delete property Prop1 from ARole8
    thing ARole8
      property Prop1 (relational, String)

  case TestCaseRemoveProp
    user Self filledBy sys:PerspectivesSystem$User
    state SomeState = true
      on entry
        do
          Prop2 =- "aap" from ARole9
    thing ARole9
      property Prop2 (relational, String)

  case TestCaseAddProp
    user Self filledBy sys:PerspectivesSystem$User
    state SomeState = true
      on entry
        do
          Prop3 =+ "mies" from ARole10
    thing ARole10
      property Prop3 (relational, String)

  case TestCaseSetProp
    user Self filledBy sys:PerspectivesSystem$User
    state SomeState = true
      on entry
        do
          Prop4 = "mies" from ARole11
    thing ARole11
      property Prop4 (relational, String)
