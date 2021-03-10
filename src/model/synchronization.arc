-- Copyright Joop Ringelberg and Cor Baars, 2021
domain: Synchronization
  use: sys for model:System
  use: sync for model:Synchronization
  use: rep for model:Replication

  case: Model
    external:
      aspect: sys:Model$External
    aspect: sys:Model

  case: Synchronization
    indexed: sync:MySynchronization
    aspect: sys:RootContext
    user: Synchronizer (mandatory, functional) filledBy: sys:PerspectivesSystem$User
      aspect: sys:RootContext$RootUser

    thing: PersistenceAccount (not mandatory, not functional)
      property: Url (mandatory, functional, String)
      property: UserName (mandatory, functional, String)
      property: Password (mandatory, functional, String)
      property: Name

    thing: DatabaseSync (not mandatory, not functional) filledBy: PersistenceAccount
      property: DatabaseName (mandatory, functional, String)
      -- When true, triggers a bot to activate the synchronizaton. Once only action!
      property: Activate (not mandatory, functional, Boolean)
      -- When true, triggers another bot to deactivate the synchronizaton. Once only action!
      property: Deactivate (not mandatory, functional, Boolean)

    bot: for Synchronizer
      perspective on: DatabaseSync
        if Activate then
          callExternal rep:Replicate(
            (filter sys:MySystem$Databases with Name == object >> DatabaseName) >> Identifier,
            $object >> Url,
            $object >> UserName,
            $object >> Password
            )

    bot: for Synchronizer
      perspective on: DatabaseSync
        if Deactivate then
          callExternal rep:EndReplication(
            (filter sys:MySystem$Databases with Name == object >> DatabaseName) >> Identifier,
            $object >> Url,
            $object >> UserName,
            $object >> Password
            )
