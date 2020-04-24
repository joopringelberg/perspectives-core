domain: Test

  use: sys for model:System
  case: TestCase1
    user: Self filledBy: sys:PerspectivesSystem$User
    thing: ARole
    bot: for Self
      perspective on: Self
        if true then
          remove ARole
          
  case: TestCase2
    user: Self filledBy: sys:PerspectivesSystem$User
    thing: ARole
    bot: for Self
      perspective on: Self
        if true then
          createRole ARole

  case: TestCase3
    user: Self filledBy: sys:PerspectivesSystem$User
      property: Prop1 (not mandatory, functional, Boolean)
    context: Nested1 filledBy: NestedContext
    context: Nested2 filledBy: NestedContext
    bot: for Self
      perspective on: Self
        if Self >> Prop1 == true then
          move Nested1 >> binding >> context >> ARole to Nested2 >> binding >> context
    case: NestedContext
      thing: ARole

  case: TestCaseBind
    user: Self filledBy: sys:PerspectivesSystem$User
    thing: ARole filledBy: AnotherRole
    thing: AnotherRole
    context: Nested filledBy: NestedCase
    bot: for Self
      perspective on: Self
        if true then
          bind AnotherRole to ARole
          bind AnotherRole to ARole in Nested >> binding >> context
    case: NestedCase
      thing: ARole
  case: TestCaseBind_
    user: Self filledBy: sys:PerspectivesSystem$User
    thing: ARole4 filledBy: AnotherRole4
    thing: AnotherRole4
    context: Nested4 filledBy: NestedCase4
    bot: for Self
      perspective on: Self
        if true then
          bind_ AnotherRole4 to ARole4
          bind_ AnotherRole4 to Nested4 >> binding >> context >> ARole4
    case: NestedCase4
      thing: ARole4 filledBy: AnotherRole4
  case: TestCaseUnbind
    user: Self filledBy: sys:PerspectivesSystem$User
    thing: ARole5 filledBy: AnotherRole5
    thing: AnotherRole5
    context: Nested5 filledBy: NestedCase5
    bot: for Self
      perspective on: Self
        if true then
          unbind AnotherRole5
    case: NestedCase5
      thing: ARole5
  case: TestCaseUnbindQualified
    user: Self filledBy: sys:PerspectivesSystem$User
    thing: ARole6 filledBy: AnotherRole6
    thing: AnotherRole6
    context: Nested6 filledBy: NestedCase6
    bot: for Self
      perspective on: Self
        if true then
          unbind AnotherRole6 from TestCaseUnbindQualified$ARole6
    case: NestedCase6
      thing: ARole6

  case: TestCaseUnbind_
    user: Self filledBy: sys:PerspectivesSystem$User
    thing: ARole7 filledBy: AnotherRole7
    thing: AnotherRole7
    context: Nested7 filledBy: NestedCase7
    bot: for Self
      perspective on: Self
        if true then
          unbind_ AnotherRole7 from Nested7 >> binding >> context >> ARole7
    case: NestedCase7
      thing: ARole7

  case: TestCaseDeleteProp
    user: Self filledBy: sys:PerspectivesSystem$User
    thing: ARole8
      property: Prop1 (not mandatory, not functional, String)
    bot: for Self
      perspective on: ARole8
        if true then
          delete property Prop1
  case: TestCaseRemoveProp
    user: Self filledBy: sys:PerspectivesSystem$User
    thing: ARole9
      property: Prop2 (not mandatory, not functional, String)
    bot: for Self
      perspective on: ARole9
        if true then
          Prop2 =- "aap"
  case: TestCaseAddProp
    user: Self filledBy: sys:PerspectivesSystem$User
    thing: ARole10
      property: Prop3 (not mandatory, not functional, String)
    bot: for Self
      perspective on: ARole10
        if true then
          Prop3 =+ "mies"
  case: TestCaseSetProp
    user: Self filledBy: sys:PerspectivesSystem$User
    thing: ARole11
      property: Prop4 (not mandatory, not functional, String)
    bot: for Self
      perspective on: ARole11
        if true then
          Prop4 = "mies"
