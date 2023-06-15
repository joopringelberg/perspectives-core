-- Copyright Joop Ringelberg and Cor Baars 2021-2022.
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
    property IsInitiator (Boolean)

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
    state NoTransactions = (not exists Transactions) and exists Witnesses >> binding
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
            -- And we add Witnesses.
            bind Witnesses >> binding to DebitorWitness in trans >> binding >> context
            bind Witnesses >> binding to CreditorWitness in trans >> binding >> context

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
              IsInitiator = true for trans >> binding >> context >> Debitor

          -------------------------------------------------------------------------------
          ---- ADD A TRANSACTION AS PAYMENT REQUEST
          -------------------------------------------------------------------------------
          action RequestPayment
            letA
              trans <- create context Transaction bound to Transactions
            in
              bind sys:Me to Creditor in trans >> binding >> context
              IsInitiator = true for trans >> binding >> context >> Creditor

      perspective on Transactions >> binding >> context >> Debitor
        only (Create, Fill)
        props (IsInitiator) verbs (SetPropertyValue)

      perspective on Transactions >> binding >> context >> Creditor
        only (Create, Fill)
        props (IsInitiator) verbs (SetPropertyValue)

      perspective on Witnesses
        only (Create, Fill, Remove)
        props (LastName) verbs (Consult)

      perspective on LastTransaction
        only (Create, Fill)
        props (TransactionPartnerName, MyTransferSum, MyResultingSaldo) verbs (Consult)

      screen "Witcoin Transactions"
        tab "Witnesses"
          form Witnesses
        tab "Transactions"
          form "Last Transaction" LastTransaction
          table Transactions

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
      property TransactionPartnerName = context >> TransactionPartner >> LastName
      property MyTransferSum = context >> MeInTransaction >> (Post - Prior)
      property MyResultingSaldo = context >> MeInTransaction >> Post
      property IAmDebitor = context >> Debitor filledBy sys:Me
      property IAmCreditor = context >> Creditor filledBy sys:Me
      property IAmDebitorInLastTransaction = wit:MyWitCoinApp >> LastTransaction >> binding >> context >> extern >> IAmDebitor
      property IAmCreditorInLastTransaction = wit:MyWitCoinApp >> LastTransaction >> binding >> context >> extern >> IAmCreditor
      property IAmInitiator = (IAmDebitor and context >> Debitor >> IsInitiator) or (IAmCreditor and context >> Creditor >> IsInitiator)

      perspective of DebitorWitness
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
              -- As I am Debitor in LastTransaction, we need to fill the NextDeb role of that Transaction.
              -- We then make the current Transaction the LastTransaction.
              action CommitPayment
                bind origin to NextDeb in wit:MyWitCoinApp >> LastTransaction >> binding >> context
                -- Create an empty DebitorWitness role. The Witness in LastTransaction sees and fills it.
                create role DebitorWitness
                bind_ origin to wit:MyWitCoinApp >> LastTransaction
                Committed = true
            state Committed = exists Committed
          state CreditorInLastTransaction = IAmCreditorInLastTransaction
            perspective of Debitor
              -- As I am Creditor in LastTransaction, we need to fill the NextCred role of that Transaction.
              -- We then make the current Transaction the LastTransaction.
              action CommitPayment
                bind origin to NextCred in wit:MyWitCoinApp >> LastTransaction >> binding >> context
                -- Create an empty DebitorWitness role. The Witness in LastTransaction sees and fills it.
                create role DebitorWitness
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

    -------------------------------------------------------------------------------
    ---- DEBITOR
    -------------------------------------------------------------------------------
    user Debitor filledBy sys:PerspectivesSystem$User
      aspect wit:AccountHolder

      -- In state PostCanBeComputed, it is guaranteed that there is a TransferSum, too.
      state PostCanBeComputed = (exists Prior) and context >> extern >> Committed

      perspective on Creditor
        only (Create, Fill)
        view sys:PerspectivesSystem$User$VolledigeNaam verbs (Consult)

      perspective on DebitorWitness
        only (Create)
        view sys:PerspectivesSystem$User$SyncId verbs (Consult)

      perspective on CreditorWitness
        only (Create)
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

      -- In state PostCanBeComputed, it is guaranteed that there is a TransferSum, too.
      state PostCanBeComputed = (exists Prior) and context >> extern >> Committed

      perspective on Debitor
        only (Create, Fill)
        view sys:PerspectivesSystem$User$VolledigeNaam verbs (Consult)

      perspective on DebitorWitness
        only (Create)
        view sys:PerspectivesSystem$User$SyncId verbs (Consult)

      perspective on CreditorWitness
        only (Create)
        view sys:PerspectivesSystem$User$SyncId verbs (Consult)

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
    ---- CREDITORWITNESS
    ---- The Witness set by Witness of the previous Transaction of the Creditor.
    -------------------------------------------------------------------------------
    user CreditorWitness filledBy sys:PerspectivesSystem$User
      state Unfilled = not exists binding

      -------------------------------------------------------------------------------
      ---- LINKING TO THE NEXT TRANSACTION(S)
      ---- As a Witness I have a perspective on several roles in transactions
      ---- that are chained to the current one.
      -------------------------------------------------------------------------------
      -- As soon as this transaction has a successor transaction in NextDeb
      -- with an (unfilled) Witness role, I as Witness will fill it with myself.
      perspective on NextDeb >> binding >> context >> CreditorWitness
        only (Fill)
        on entry of object state Unfilled
          do
            bind_ origin to sys:Me

      -- As soon as this transaction has a successor transaction in NextCred
      -- with an (unfilled) Witness role, I as Witness will fill it with myself.
      perspective on NextCred >> binding >> context >> CreditorWitness
        only (Fill)
          on entry of object state Unfilled
            do
              bind_ origin to sys:Me

      -- I as CreditorWitness need to see the Debitor, DebitorWitness and
      -- Creditor in the next transactions, in order to be able to tell them
      -- that I have set myself as the Debitor Witness in those transactions, too.
      perspective on NextDeb >> binding >> context >> Creditor
        view sys:PerspectivesSystem$User$SyncId verbs (Consult)

      perspective on NextDeb >> binding >> context >> Debitor
        view sys:PerspectivesSystem$User$SyncId verbs (Consult)

      perspective on NextDeb >> binding >> context >> DebitorWitness
        view sys:PerspectivesSystem$User$SyncId verbs (Consult)

      perspective on NextCred >> binding >> context >> Creditor
        view sys:PerspectivesSystem$User$SyncId verbs (Consult)

      perspective on NextCred >> binding >> context >> Debitor
        view sys:PerspectivesSystem$User$SyncId verbs (Consult)

      perspective on NextCred >> binding >> context >> DebitorWitness
        view sys:PerspectivesSystem$User$SyncId verbs (Consult)

      -------------------------------------------------------------------------------
      ---- COMPUTING PRIOR VALUES
      -------------------------------------------------------------------------------
      -- When I am the CreditorWitness in the next Transaction,
      -- I must set the Prior value of the Creditor in that Transaction.

      -- In this case, it must be equal to the Post value of the Debitor in this Transaction
      -- (because the next Transaction is stored in NextDeb).
      perspective on NextDeb >> binding >> context >> Creditor
        props (Prior) verbs (SetPropertyValue)
        on entry of object state
          do
            Prior = currentcontext >> Debitor >> Post

      -- In this case, it must be equal to the Post value of the Creditor in this Transaction
      -- (because the next Transaction is stored in NextCred).
      perspective on NextCred >> binding >> context >> Creditor
        props (Prior) verbs (SetPropertyValue)
        on entry of object state
          do
            Prior = currentcontext >> Creditor >> Post

    -------------------------------------------------------------------------------
    ---- DEBITORWITNESS
    ---- The Witness set by Witness of the previous Transaction of the Debitor.

    ---- The Witness of a Transaction has always been set by the Witness of the
    ---- previous Transaction of the Debitor, as only the Debitor can commit a
    ---- Transaction.
    -------------------------------------------------------------------------------
    user DebitorWitness filledBy sys:PerspectivesSystem$User
      state Unfilled = not exists binding

      -------------------------------------------------------------------------------
      ---- LINKING TO THE NEXT TRANSACTION(S)
      ---- As a Witness I have a perspective on several roles in transactions
      ---- that are chained to the current one.
      -------------------------------------------------------------------------------
      -- As soon as this transaction has a successor transaction in NextDeb
      -- with an (unfilled) Witness role, I as Witness will fill it with myself.
      perspective on NextDeb >> binding >> context >> DebitorWitness
        only (Fill)
        on entry of object state Unfilled
          do
            bind_ origin to sys:Me

      -- As soon as this transaction has a successor transaction in NextCred
      -- with an (unfilled) Witness role, I as Witness will fill it with myself.
      perspective on NextCred >> binding >> context >> DebitorWitness
        only (Fill)
          on entry of object state Unfilled
            do
              bind_ origin to sys:Me

      -- I as DebitorWitness need to see the Creditor, CreditorWitness and
      -- Debitor in the next transactions, in order to be able to tell them
      --  that I have set myself as the Debitor Witness in those transactions, too.
      perspective on NextDeb >> binding >> context >> Creditor
        view sys:PerspectivesSystem$User$SyncId verbs (Consult)

      perspective on NextDeb >> binding >> context >> Debitor
        view sys:PerspectivesSystem$User$SyncId verbs (Consult)

      perspective on NextDeb >> binding >> context >> CreditorWitness
        view sys:PerspectivesSystem$User$SyncId verbs (Consult)

      perspective on NextCred >> binding >> context >> Creditor
        view sys:PerspectivesSystem$User$SyncId verbs (Consult)

      perspective on NextCred >> binding >> context >> Debitor
        view sys:PerspectivesSystem$User$SyncId verbs (Consult)

      perspective on NextCred >> binding >> context >> CreditorWitness
        view sys:PerspectivesSystem$User$SyncId verbs (Consult)

      -------------------------------------------------------------------------------
      ---- COMPUTING PRIOR VALUES
      -------------------------------------------------------------------------------
      -- When I am the DebitorWitness in the next Transaction,
      -- I must set the Prior value of the Debitor.
      -- This is because this Witness is always filled from the Debitor side of the transaction.
      -- So I have access to the previous Transaction of the Debitor.

      -- In this case, it must be equal to the Post value of the Debitor in this Transaction
      -- (because the next Transaction is stored in NextDeb).
      perspective on NextDeb >> binding >> context >> Debitor
        props (Prior) verbs (SetPropertyValue)
        on entry of object state
          do
            Prior = currentcontext >> Debitor >> Post

      -- In this case, it must be equal to the Post value of the Creditor in this Transaction
      -- (because the next Transaction is stored in NextCred).
      perspective on NextCred >> binding >> context >> Debitor
        props (Prior) verbs (SetPropertyValue)
        on entry of object state
          do
            Prior = currentcontext >> Creditor >> Post

      -------------------------------------------------------------------------------
      ---- COMPUTING POST VALUES
      ---- Once I am DebitorWitness in this Transaction, and I have all the necessary data,
      ---- I compute the amount in the account after this Transaction (the Post value).
      -------------------------------------------------------------------------------
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
        on entry of object state PostCanBeComputed
          do
            Post = Prior + context >> extern >> TransferSum

    -------------------------------------------------------------------------------
    ---- NEXTDEB
    ---- This is the next Transaction for the Debitor in the current Transaction.
    -------------------------------------------------------------------------------
    context NextDeb filledBy Transaction
      perspective of Debitor
        only (CreateAndFill, Remove)

    -------------------------------------------------------------------------------
    ---- NEXTCRED
    ---- This is the next Transaction for the Creditor in the current Transaction.
    -------------------------------------------------------------------------------
    context NextCred filledBy Transaction
      perspective of Creditor
        only (CreateAndFill, Remove)

    -------------------------------------------------------------------------------
    ---- TRANSACTIONPARTNER
    -------------------------------------------------------------------------------
    user TransactionPartner = filter (Debitor union Creditor) with not filledBy sys:Me

    -------------------------------------------------------------------------------
    ---- MEINTRANSACTION
    -------------------------------------------------------------------------------
    user MeInTransaction = filter (Debitor union Creditor) with filledBy sys:Me
