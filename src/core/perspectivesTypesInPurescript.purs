module Perspectives.PerspectivesTypesInPurescript where


-- PRIMARY REPRESENTATION
newtype Context = Context String
newtype Rol = Rol String
newtype Val = Val String

-- MODEL:PERSPECTIVES
newtype SimpleValue = SimpleValue String

newtype PBool = PBool String
newtype PString = PString String
newtype PDate = PDate String
newtype PNumber = PNumber String

newtype ContextDef = ContextDef String
newtype RolDef = RolDef String
newtype PropertyDef = PropertyDef String

newtype SysteemBot = SysteemBot String

newtype View = View String
newtype Actie = Actie String

newtype Zaak = Zaak String

newtype Function = Function String
newtype ElkType = ElkType String

newtype Systeem = Systeem String

newtype TrustedCluster = TrustedCluster String

newtype AssignToRol = AssignToRol String

newtype AssignToProperty = AssignToProperty String

newtype EffectFullFunction = EffectFullFunction String

-- MODEL:QUERYAST
newtype DataTypeGetter = DataTypeGetter String
newtype PropertyGetter = PropertyGetter String
newtype RolGetter = RolGetter String
newtype ComputedRolGetter = ComputedRolGetter String
newtype ComputedPropertyGetter = ComputedPropertyGetter String
newtype UnaryCombinator = UnaryCombinator String
newtype NAryCombinator = NAryCombinator String

newtype Filter = Filter String
newtype RolesOf = RolesOf String
newtype Contains = Contains String
newtype Constant = Constant String
newtype Variable = Variable String
newtype SetVariable = SetVariable String
