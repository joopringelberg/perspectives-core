domain model://joopringelberg.nl#HyperContext
  use sys for model://perspectives.domains#System
  use ht for model://joopringelberg.nl#HyperContext

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context HyperTexts
        in
          bind app >> extern to StartContexts in sys:MySystem
          Name = "Hypertext types" for app >> extern

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (ht:HyperTextApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer
  
  case HyperTexts
    indexed ht:HyperTextApp
    aspect sys:RootContext

    external
      aspect sys:RootContext$External
    
    state NoManager = not exists Manager
      on entry 
        do for Initializer
          bind sys:SocialMe to Manager

    user Initializer = sys:Me
      perspective on Manager
        all roleverbs

    user Manager filledBy sys:TheWorld$PerspectivesUsers
      perspective on Pages
        defaults
      perspective on PublicPageCollections
        defaults

    context Pages (relational) filledBy Page
      on entry
        do for Manager
          bind currentactor to Author in binding >> context

    context PublicPageCollections (relational) filledBy PublicPageCollection

  case Page
    external 
      property Title (String)

    user Author filledBy sys:TheWorld$PerspectivesUsers
      perspective on TextBlocks
        only (Create, Remove)
        props (MD, Condition, Title) verbs (Consult, SetPropertyValue)
        props (RawConditionResult, ShowBlock) verbs (Consult)
      perspective on LinkedPages
        all roleverbs
        props (Title) verbs (Consult, SetPropertyValue)

      screen "Page Editor"
        row
          column
            table "Text blocks" TextBlocks
              props (Title, Condition) verbs (Consult, SetPropertyValue)
          column
            table "Conditions" TextBlocks
              props (RawConditionResult) verbs (Consult)
        row
          table "Links" LinkedPages
        row
          -- editor
          column
            markdown TextBlocks
              props (MD, Condition) verbs (Consult, SetPropertyValue) 
          -- preview
          column
            markdown TextBlocks
              props (MD) verbs (Consult) 
              -- LET OP: na de verandering is alleen ShowBlock nog nodig.
              when ShowBlock

    thing TextBlocks (relational)
      property Title (String)
      property MD (MarkDown)
        minLength = 100
      property Condition (String)
      property RawConditionResult = callExternal util:EvalExpression( Condition ) returns String
      property ConditionResult = RawConditionResult >> callExternal util:SelectR( "^result#(.*)" ) returns String
      property ShowBlock = (exists ConditionResult) and ConditionResult == "true"
    
    context LinkedPages (relational) filledBy (Page, PublicPage)

    user Reader (relational) filledBy sys:TheWorld$PerspectivesUsers
      perspective on TextBlocks
        props (MD, Condition, RawConditionResult, ConditionResult, ShowBlock) verbs (Consult)
      screen
        row 
          -- Conditional read only view of blocks.
          markdown TextBlocks
            props (MD) verbs (Consult)
            when ShowBlock

  -- A collection of pages published at the same location.
  -- The location is determined by the Author in her capacity of sys:WithCredentials.
  case PublicPageCollection
    external 
      property Name (String)
    
    user Author filledBy (sys:TheWorld$PerspectivesUsers + sys:WithCredentials)
      perspective on extern
        props (Name) verbs (Consult, SetPropertyValue)
      perspective on EntryPoint
        all roleverbs
        props (Title) verbs (Consult, SetPropertyValue)
      perspective on PublicPages
        all roleverbs
        props (Title) verbs (Consult, SetPropertyValue)
      screen "Public Page Collection"
        row 
          form External
        row 
          form EntryPoint
        row 
          table PublicPages
    
    context PublicPages (relational) filledBy PublicPage

    context EntryPoint filledBy PublicPage

  case PublicPage
    aspect ht:Page

    context LinkedPages (relational) filledBy PublicPage

    public Visitor at extern >> binder PublicPages >> context >> Author >> AuthorizedDomain = sys:SocialMe
      perspective on ht:Page$TextBlocks
        props (MD, Condition, RawConditionResult, ConditionResult, ShowBlock) verbs (Consult)
      -- perspective on Author
      --   props (FirstName, LastName) verbs (Consult)
      screen
        row 
          -- Conditional read only view of blocks.
          markdown ht:Page$TextBlocks
            props (MD) verbs (Consult)
            when ShowBlock

    user Author
      aspect ht:Page$Author
      -- Notice that these are PublicPage$LinkedPages, as opposed to 
      -- Page$LinkedPages!
      perspective on LinkedPages
        all roleverbs
        props (Title) verbs (Consult, SetPropertyValue)

    aspect thing ht:Page$TextBlocks

