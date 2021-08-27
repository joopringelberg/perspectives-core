-- Copyright Joop Ringelberg and Cor Baars 2021
-- This model provides a pattern for bodies that have accounts that can be applied for.
-- The Accounts role has been kept sparse. Obvious extensions would be:
--  * a date marking the beginning of the Account;
--  * a date marking the end of the Account;
--  * credentials like username and password to provide access to resources of the Body.
--  * a fee due for Accountship;

domain BodiesWithAccounts
  use sys for model:System

  case Body

    -- Admin has a full perspective on Accounts.
    user Admin filledBy CouchdbServer$Admin
      perspective on Accounts
        defaults

    -- Role Guest is available so any user can request an Account.
    -- Guest is superceded by Accounts as soon as it exists.
    user Guest = sys:Me
      -- Guest can request an Account.
      -- Because Guest is calculated, the PDR will make no effort
      -- to keep it up to date with the Accounts role. That is OK,
      -- as this perspective should only be used to create an Accounts
      -- instance.
      perspective on Accounts
        only (Create, Fill)

    -- User Accounts should stored in private space.
    -- By making the role unlinked, there are no references from the context to
    -- Accounts role instances (but there are references the other way round).
    -- This allows us to create a screen to browse through Accounts without
    -- loading all references at once with the context.
    -- Specialisation may restrict their fillers.
    user Accounts (unlinked)
      property IsAccepted (Boolean)
      property IsRejected (Boolean)
      state Root = true
        -- Use this state to inform the applicant that his case is in
        -- consideration.
        state Waiting = not IsRejected and not IsAccepted
          perspective on Accounts
            -- Account can see he is not yet rejected,
            -- but not accepted either.
            props (IsRejected, isAccepted) verbs (Consult)
            selfOnly

        -- Use this state to inform the applicant that he will not become
        -- a member.
        state Rejected = IsRejected
          perspective on Accounts
            -- Guest can see his request for an account is rejected.
            props (IsRejected, isAccepted) verbs (Consult)
            selfOnly

        -- Use this state to provide perspectives on the various
        -- things that are hidden for non-members.
        -- This has to be done in the specialisations of this User role.
        state Accepted = IsAccepted
          -- An Account can see just himself and can decide to be no longer
          -- a member, by removing himself from the role.
          perspective on Accounts
            only (RemoveFiller)
            verbs (Consult)
            selfonly
