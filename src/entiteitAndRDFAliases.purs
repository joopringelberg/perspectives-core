module Perspectives.EntiteitAndRDFAliases where

{-
When talking about Deltas or Triples, we use these terms:
	<Subject, Predicate, Object(s)>
To be precise: a Delta has a (nullOrUndefined Object), while a Triple is modelled as an (Array Object) - where Object is an alias for String.
All three positions can be occupied with identifiers of Resources (in terms of RDF an URI), though the Object position can be filled with a value-expression, too (in terms of RDF a Literal)

When talking about Contexts and Roles, we use the terms:
	<ContextID, RolName, RolID>
	<RolID, PropertyName, Value>

All six positions are occupied by identifiers of contexts and roles (in CRL), except for the Value position that is filled with the String representation of simple values (Boolean, String, Number, Date).

If we generalize over Context and Role, we use the following terms:
  - ID for ContextID and RolID
  - MemberName for RolName and PropertyName
  - Value for RolID and Value.
-}
-----------------------------------------------------------
-- RDF
-----------------------------------------------------------

type Subject = String
type Predicate = String
type Object = String
type Objects = Array String

-----------------------------------------------------------
-- CONTEXT AND ROL
-----------------------------------------------------------
type ID = String
type ContextID = String
type RolID = String
type PropertyName = String
type RolName = String
type MemberName = String
type Comment = String
type Value = String
type Values = Array String
