-- Copyright Joop Ringelberg and Cor Baars 2021
-- In this file we model financial transactions with Witnesses.

domain WitCoin
  use sys for model:System
  use wit for model:WitCoin

  -- The model description case.
  -- REMOVE ONCE WE CREATE INSTANCES WITH AN ACTION
  case Model
    aspect sys:Model
    external
      aspect sys:Model$External

  user AccountHolder filledBy sys:PerspectivesSystem$User
    property Prior (Number)
    property Post (Number)

-------------------------------------------------------------------------------
---- WITCOINAPP
-------------------------------------------------------------------------------
  case WitCoinApp
    indexed wit:MyWitCoinApp
    aspect sys:RootContext
    external
      aspect sys:RootContext$External

    -------------------------------------------------------------------------------
    ---- BOOT BY CREATING FIRST TRANSACTION
    ---- Note that this is just for demonstration purposes.
    ---- In reality, the user has to top up his account by transferring money
    ---- into the Perspectives Universe.
    -------------------------------------------------------------------------------
    state NoTransactions = (not exists Transactions) and exists Witnesses
      on entry
        do for Manager
          letA
            trans <- create context Transaction bound to Transactions
            cred <- create role Creditor in trans >> binding >> context
          in
            bind_ sys:Me to cred
            Prior = 0 for cred
            Post = 100 for cred
            -- Here we create a role instance for LastTransaction.
            bind trans >> binding to LastTransaction
            -- And we add a Witness.
            bind Witnesses >> binding to Witness in trans >> binding >> context

      perspective of Manager
        perspective on Transactions >> binding >> context >> Creditor
          only (Create, Fill)
          props (Prior, Post) verbs (SetPropertyValue)

    state AtLeastOneTransaction = exists Transactions
    -------------------------------------------------------------------------------
    ---- MANAGER
    -------------------------------------------------------------------------------
    user Manager = sys:Me
      in state AtLeastOneTransaction
        perspective on Transactions
          only (CreateAndFill, Delete, Remove)
          props (TransactionPartnerName, MyTransferSum, MyResultingSaldo) verbs (Consult)

          -------------------------------------------------------------------------------
          ---- ADD A TRANSACTION AS PAYMENT
          -------------------------------------------------------------------------------
          action NewPayment
            letA
              trans <- create context Transaction bound to Transactions
            in
              bind sys:Me to Debitor in trans >> binding >> context
              bind sys:Me to Initiator in trans >> binding >> context

          -------------------------------------------------------------------------------
          ---- ADD A TRANSACTION AS PAYMENT REQUEST
          -------------------------------------------------------------------------------
          action RequestPayment
            letA
              trans <- create context Transaction bound to Transactions
            in
              bind sys:Me to Creditor in trans >> binding >> context
              bind sys:Me to Initiator in trans >> binding >> context

      perspective on Transactions >> binding >> context >> Debitor
        only (Create, Fill)

      perspective on Transactions >> binding >> context >> Creditor
        only (Create, Fill)

      perspective on Witnesses
        only (Create, Fill)
        view sys:PerspectivesSystem$User$SyncId verbs (Consult)

      perspective on LastTransaction
        only (Create, Fill)

    -------------------------------------------------------------------------------
    ---- WITNESSES
    ---- These are the Witnesses that service me in Transactions.
    ---- We really want many witnesses, but for now we'll do with just one.
    -------------------------------------------------------------------------------
    user Witnesses filledBy sys:PerspectivesSystem$User

    -------------------------------------------------------------------------------
    ---- TRANSACTIONS
    -------------------------------------------------------------------------------
    context Transactions (relational) filledBy Transaction

    -------------------------------------------------------------------------------
    ---- LASTTRANSACTION
    -------------------------------------------------------------------------------
    -- Note that this role is in effect private to each end user, because no user
    -- role has a perspective on it.
    context LastTransaction filledBy Transaction

    -------------------------------------------------------------------------------
    ---- CLEANING UP ON EXIT
    -------------------------------------------------------------------------------
    on exit
      do for Manager
        delete context bound to Transactions

-------------------------------------------------------------------------------
---- TRANSACTION
-------------------------------------------------------------------------------
  case Transaction
    external
      property TransferSum (Number)
      property Committed (Boolean)

      -------------------------------------------------------------------------------
      ---- PROPERTIES RELATIVE TO THE OWN USER
      -------------------------------------------------------------------------------
      property TransactionPartnerName = context >> TransactionPartner >> Achternaam
      property MyTransferSum = context >> MeInTransaction >> (Post - Prior)
      property MyResultingSaldo = context >> MeInTransaction >> Post
      property IAmDebitor = context >> Debitor binds sys:Me
      property IAmCreditor = context >> Creditor binds sys:Me
      property IAmDebitorInLastTransaction = wit:MyWitCoinApp >> LastTransaction >> binding >> context >> extern >> IAmDebitor
      property IAmCreditorInLastTransaction = wit:MyWitCoinApp >> LastTransaction >> binding >> context >> extern >> IAmCreditor
      property IAmInitiator = context >> Initiator binds sys:Me

      perspective of Witness
        props (TransferSum, Committed) verbs (Consult)

      state DebitorCanSetTransferSum = IAmInitiator and IAmDebitor
        perspective of Debitor
          props (TransferSum, Committed) verbs (SetPropertyValue)

      perspective of Debitor
        props (Committed, TransferSum) verbs (Consult)

      state CreditorCanSetTransferSum = IAmInitiator and IAmCreditor
        perspective of Creditor
          props (TransferSum) verbs (SetPropertyValue)

      perspective of Creditor
        props (TransferSum, Committed) verbs (Consult)

      -------------------------------------------------------------------------------
      ---- TRANSACTIONCOMPLETE (STATES OF THE EXTERNAL ROLE OF TRANSACTION)
      -------------------------------------------------------------------------------
      state TransactionComplete = (exists context >> Debitor) and (exists context >> Creditor) and (exists TransferSum)
        state AcceptOrReject = IAmDebitor
          state DebitorInLastTransaction = IAmDebitorInLastTransaction
            perspective of Debitor
              action CommitPayment
                bind origin to NextDeb in wit:MyWitCoinApp >> LastTransaction >> binding >> context
                -- Here we bind this Transaction to the existing role instance of LastTransaction.
                bind_ origin to wit:MyWitCoinApp >> LastTransaction
                Committed = true
            state Committed = exists Committed
          state CreditorInLastTransaction = IAmCreditorInLastTransaction
            perspective of Debitor
              action CommitPayment
                bind origin to NextCred in wit:MyWitCoinApp >> LastTransaction >> binding >> context
                -- Here we bind this Transaction to the existing role instance of LastTransaction.
                bind_ origin to wit:MyWitCoinApp >> LastTransaction
                Committed = true
            state Committed = exists Committed
        state WaitForCommitment = IAmCreditor
          state DebitorInLastTransaction = IAmDebitorInLastTransaction
            state Committed = exists Committed
              on entry
                do for Creditor
                  bind origin to NextDeb in wit:MyWitCoinApp >> LastTransaction >> binding >> context
                  -- Here we bind this Transaction to the existing role instance of LastTransaction.
                  bind_ origin to wit:MyWitCoinApp >> LastTransaction
                  -- Add to Transactions, so Creditor can see it.
                  bind origin to Transactions in wit:MyWitCoinApp
          state CreditorInLastTransaction = IAmCreditorInLastTransaction
            state Committed = exists Committed
              on entry
                do for Creditor
                  bind origin to NextCred in wit:MyWitCoinApp >> LastTransaction >> binding >> context
                  -- Here we bind this Transaction to the existing role instance of LastTransaction.
                  bind_ origin to wit:MyWitCoinApp >> LastTransaction
                  -- Add to Transactions, so Creditor can see it.
                  bind origin to Transactions in wit:MyWitCoinApp

    state TransferFunds = (exists Debitor >> Prior) and exists Creditor >> Prior
      on entry
        do for Witness
          Post = Debitor >> Prior - extern >> TransferSum for Debitor
          Post = Creditor >> Prior + extern >> TransferSum for Creditor

    -------------------------------------------------------------------------------
    ---- DEBITOR
    -------------------------------------------------------------------------------
    user Debitor filledBy sys:PerspectivesSystem$User
      aspect wit:AccountHolder

      state PostCanBeComputed = (exists Prior) and exists context >> extern >> TransferSum

      perspective on Creditor
        only (Create, Fill)
        view sys:PerspectivesSystem$User$VolledigeNaam verbs (Consult)

      perspective on Witness
        view sys:PerspectivesSystem$User$SyncId verbs (Consult)

      perspective on NextDeb
        only (Create, Fill)

      perspective on NextCred
        only (Create, Fill)

      perspective on extern >> binder Transactions >> context >> LastTransaction
        only (Create, Fill)

      perspective on Debitor
        props (Prior, Post) verbs (Consult)
        -- This is to make sure the binding of Debitor gets transferred to himself by others.
        view sys:PerspectivesSystem$User$SyncId verbs (Consult)

    -------------------------------------------------------------------------------
    ---- CREDITOR
    -------------------------------------------------------------------------------
    user Creditor filledBy sys:PerspectivesSystem$User
      aspect wit:AccountHolder

      perspective on Debitor
        only (Create, Fill)
        view sys:PerspectivesSystem$User$VolledigeNaam verbs (Consult)

      perspective on Witness
        view sys:PerspectivesSystem$User$VolledigeNaam verbs (Consult)

      perspective on NextDeb
        only (Create, Fill)

      perspective on NextCred
        only (Create, Fill)

      perspective on extern >> binder Transactions >> context >> LastTransaction
        only (Create, Fill)

      perspective on Creditor
        props (Prior, Post) verbs (Consult)
        -- This is to make sure the binding of Creditor gets transferred to himself by others.
        view sys:PerspectivesSystem$User$SyncId verbs (Consult)

    -------------------------------------------------------------------------------
    ---- INITIATOR
    -------------------------------------------------------------------------------
    user Initiator filledBy sys:PerspectivesSystem$User

    -------------------------------------------------------------------------------
    ---- WITNESS
    -------------------------------------------------------------------------------
    user Witness filledBy sys:PerspectivesSystem$User
      perspective on Debitor
        props (Prior, Post) verbs (SetPropertyValue)
        -- This is to make sure Witness gets to see the User behind Debitor.
        view sys:PerspectivesSystem$User$SyncId verbs (Consult)
        on entry of object state PostCanBeComputed
          do
            Post = Prior - context >> extern >> TransferSum

      perspective on Creditor
        props (Prior, Post) verbs (SetPropertyValue)
        -- This is to make sure Witness gets to see the User behind Creditor.
        view sys:PerspectivesSystem$User$SyncId verbs (Consult)

      --perspective on NextDebRole
        --props (Prior) verbs (SetPropertyValue)

      --perspective on NextCredRole
        --props (Prior) verbs (SetPropertyValue)

      perspective on NextDebWitness
        only (Create, Fill)

      perspective on NextCredWitness
        only (Create, Fill)

      perspective on NextDeb >> binding
        on entry of object state TransactionComplete$AcceptOrReject$DebitorInLastTransaction$Committed
          do
            -- Set Prior of Debitor in the next transaction, as represented by the object.
            Prior = currentcontext >> Debitor >> Post for context >> Debitor
            bind sys:Me to Witness in currentcontext >> NextCred >> binding >> context

        on entry of object state TransactionComplete$WaitForCommitment$DebitorInLastTransaction$Committed
          do
            -- Set Prior of Creditor in the next transaction, as represented by the object.
            Prior = currentcontext >> Debitor >> Post for context >> Creditor

      perspective on NextCred >> binding
        on entry of object state TransactionComplete$AcceptOrReject$CreditorInLastTransaction$Committed
          do
            -- Set Prior of Debitor in the next transaction, as represented by the object.
            Prior = currentcontext >> Debitor >> Post for context >> Debitor
            bind sys:Me to Witness in currentcontext >> NextCred >> binding >> context

        on entry of object state TransactionComplete$WaitForCommitment$CreditorInLastTransaction$Committed
          do
            -- Set Prior of Creditor in the next transaction, as represented by the object.
            Prior = currentcontext >> Creditor >> Post for context >> Creditor

    -------------------------------------------------------------------------------
    ---- ACCOUNTHOLDER
    -------------------------------------------------------------------------------
    user Accountholder = sys:Me
      perspective on Debitor
        only (Create, Fill)
        props (Achternaam, Prior, Post) verbs (Consult)
      perspective on Creditor
        only (Create, Fill)
        props (Achternaam, Prior, Post) verbs (Consult)

    -------------------------------------------------------------------------------
    ---- NEXTDEB
    -------------------------------------------------------------------------------
    context NextDeb filledBy Transaction
      perspective of Debitor
        only (CreateAndFill, Remove)

    -------------------------------------------------------------------------------
    ---- NEXTCRED
    -------------------------------------------------------------------------------
    context NextCred filledBy Transaction
      perspective of Creditor
        only (CreateAndFill, Remove)

    -------------------------------------------------------------------------------
    ---- NEXTDEBROLE
    -------------------------------------------------------------------------------
    -- The role the Debitor of this Transaction has in his next Transaction.
    --user NextDebRole = filter (NextDeb >> binding >> context >> Debitor either NextDeb >> binding >> context >> Creditor) with binds origin >> Debitor >> binding

    -------------------------------------------------------------------------------
    ---- NEXTCREDROLE
    -------------------------------------------------------------------------------
    -- The role the Creditor of this Transaction has in his next Transaction.
    --user NextCredRole = filter (NextCred >> binding >> context >> Debitor either NextCred >> binding >> context >> Creditor) with binds origin >> Creditor >> binding

    -------------------------------------------------------------------------------
    ---- NEXTDEBWITNESS
    -------------------------------------------------------------------------------
    user NextDebWitness = NextDeb >> binding >> context >> Witness

    -------------------------------------------------------------------------------
    ---- NEXTCREDWITNESS
    -------------------------------------------------------------------------------
    user NextCredWitness = NextCred >> binding >> context >> Witness

    -------------------------------------------------------------------------------
    ---- TRANSACTIONPARTNER
    -------------------------------------------------------------------------------
    user TransactionPartner = filter (Debitor either Creditor) with not binds sys:Me

    -------------------------------------------------------------------------------
    ---- MEINTRANSACTION
    -------------------------------------------------------------------------------
    user MeInTransaction = filter (Debitor either Creditor) with binds sys:Me
