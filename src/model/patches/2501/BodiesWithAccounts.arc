-- Bodies With Accounts. Copyright Joop Ringelberg and Cor Baars 2021
-- This model provides a pattern for bodies that have accounts that can be applied for.
-- The Accounts role has been kept sparse. Obvious extensions would be:
--  * a date marking the beginning of the Account;
--  * a date marking the end of the Account;
--  * credentials like username and password to provide access to resources of the Body.
--  * a fee due for Accountship;
-- Because types in this model are only used as Aspects, we do not create an entry for it in StartContexts.

domain model://perspectives.domains#BodiesWithAccounts
  use sys for model://perspectives.domains#System
  use bwa for model://perspectives.domains#BodiesWithAccounts
  use util for model://perspectives.domains#Utilities

  case BodiesWithAccountsApp
    aspect sys:RootContext
    external
      aspect sys:RootContext$External

  case Body

    state NoAccounts = not exists Accounts

    -- Admin can always create and fill Accounts and see the UserName.
    user Admin filledBy sys:TheWorld$PerspectivesUsers
      aspect sys:WithCredentials

      perspective on Accounts
        only (Create, Fill, Remove, RemoveFiller)
        props (FirstName, LastName) verbs (SetPropertyValue)
        props (UserName) verbs (Consult)

        -- We limit visibility of the Password to the (initial) situation that it
        -- does not exist.
        -- When Accounts reset their password, Admin is duly not informed.
        in object state IsFilled$NoPassword
          props (Password) verbs (SetPropertyValue)

      perspective on Admin
        props (FirstName, LastName) verbs (Consult)
        props (Password) verbs (SetPropertyValue)
        props (UserName) verbs (Consult)

    -- Role Guest is available so any user can request an Account.
    -- Guest is superceded by Accounts as soon as it exists.
    -- This role is useful when a Body is a public context.
    -- Notice that because Accounts is unlinked and this perspective is selfonly, the existence of other accounts 
    -- plays no role.
    user Guest = sys:SocialMe
      in state NoAccounts
        -- Guest can request an Account.
        -- The only Guest instance that a PDR can ever compute, is by construction
        -- the user himself. A change to Accounts will therefore never lead to synchronization with
        -- the Guest role. That is OK,
        -- as this perspective should only be used to create an Accounts
        -- instance.
        perspective on Accounts
          only (Create, Fill)
          props (FirstName, LastName) verbs (Consult)
          -- selfonly -- This has no effect; selfOnly only applies to self-perspectives.
          action RequestAccount
            bind currentactor to Accounts
        -- Guest needs a perspective on Admin in order to keep him up to date of the new account
        -- created by RequestAccount
        perspective on Admin
          props (FirstName, LastName) verbs (Consult)

    -- User Accounts should stored in private space.
    -- By making the role unlinked, there are no references from the context to
    -- Accounts role instances (but there are references the other way round).
    -- This allows us to create a screen to browse through Accounts without
    -- loading all references at once with the context.
    -- Specialisation may restrict their fillers.
    user Accounts (unlinked, relational) filledBy sys:TheWorld$PerspectivesUsers
      aspect sys:WithCredentials
      property IsAccepted (Boolean)
      property IsRejected (Boolean)

      -- We add this perspective to enable synchronisation between
      -- Admin and Accounts.
      perspective on Admin
        props (FirstName, LastName) verbs (Consult)

      state IsFilled = exists binding
        perspective on Accounts
          props (FirstName, LastName) verbs (Consult)

        -- Use this state to inform the applicant that his case is in
        -- consideration.
        state Waiting = (not IsRejected) and not IsAccepted
          perspective on Accounts
            -- Account can see he is not yet rejected,
            -- but not accepted either.
            props (IsRejected, IsAccepted) verbs (Consult)
            selfonly

        -- Use this state to inform the applicant that he will not become
        -- a member.
        state Rejected = IsRejected
          perspective on Accounts
            -- Guest can see his request for an account is rejected.
            props (IsRejected, IsAccepted) verbs (Consult)
            selfonly

        -- Use this state to provide perspectives on the various
        -- things that are hidden for non-members.
        -- This has to be done in the specialisations of Accounts.
        state Accepted = IsAccepted
          -- An Account can see just himself and can decide to be no longer
          -- a member, by removing himself from the role.
          perspective on Accounts
            only (RemoveFiller)
            props (IsAccepted, UserName, Password) verbs (Consult)
            selfonly

        state NoPassword = not exists Password
