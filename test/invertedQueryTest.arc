domain Test

  use sys for model:System

  case TestCase
    user Self filledBy sys:PerspectivesSystem$User
      perspective on Role2
    context Role1 filledBy SubCase
    context Role2 = Role1 >> binding >> context >> SubCaseRole

  case SubCase
    thing SubCaseRole
