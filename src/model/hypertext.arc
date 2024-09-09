domain model://perspectives.domains#HyperContext
  use sys for model://perspectives.domains#System
  use ht for model://perspectives.domains#HyperContext
  use cm for model://perspectives.domains#CouchdbManagement

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context HyperTexts
          start <- create role StartContexts in sys:MySystem
        in
          bind_ app >> extern to start
          Name = "Hypertext types" for start

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
          bind sys:SocialMe >> binding to Manager

    user Initializer = sys:Me
      perspective on Manager
        all roleverbs

    user Manager filledBy sys:TheWorld$PerspectivesUsers
      perspective on Pages
        all roleverbs
        props (Title) verbs (Consult, SetPropertyValue)
      perspective on Pages >> binding >> context >> Author
        only (Create, Fill)
      perspective on PublicPageCollections
        defaults
      screen
        row
          table Pages
        row
          table PublicPageCollections

    context Pages (relational) filledBy Page
      state PageAvailable = exists binding
        on entry
          do for Manager
            bind currentactor to Author in origin >> binding >> context

    context PublicPageCollections (relational) filledBy PublicPageCollection

  case Page
    external 
      property Title (String)
      property ShowAllBlocks (Boolean)

    user Author filledBy sys:TheWorld$PerspectivesUsers
      perspective on extern
        props (Title, ShowAllBlocks) verbs (Consult, SetPropertyValue)
      perspective on TextBlocks
        only (Create, Remove)
        props (MD, Condition, Title) verbs (Consult, SetPropertyValue)
        props (RawConditionResult, ShowBlock, ShowToAuthor) verbs (Consult)
      perspective on LinkedPages
        all roleverbs
        props (Title) verbs (Consult, SetPropertyValue)
      perspective on LinkedPages >> binding >> context >> Author
        only (Create, Fill)

      screen "Page Editor"
        row
          form External
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
              when ShowToAuthor

    thing TextBlocks (relational)
      property Title (String)
      property MD (MarkDown)
        minLength = 100
      property Condition (String)
      property RawConditionResult = callExternal util:EvalExpression( Condition ) returns String
      property ConditionResult = RawConditionResult >> callExternal util:SelectR( "^result#(.*)" ) returns String
      property ShowBlock = (exists ConditionResult) and ConditionResult == "true"
      property ShowToAuthor = ShowBlock or context >> extern >> ShowAllBlocks
    
    context LinkedPages (relational) filledBy (Page, PublicPage)
      state PageAvailable = exists binding
        on entry
          do for Author
            bind currentactor >> binding to Author in origin >> binding >> context

    user Reader (relational) filledBy sys:TheWorld$PerspectivesUsers
      perspective on TextBlocks
        props (MD, Condition, RawConditionResult, ConditionResult, ShowBlock) verbs (Consult)
      screen
        row 
          -- Conditional read only view of blocks.
          markdown TextBlocks
            props (MD) verbs (Consult)
            when ShowBlock
    
    user TestReader = sys:SocialMe
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
    
    user Creator = sys:SocialMe
      perspective on Author
        only (Create, Fill)
        props (FirstName, LastName) verbs (Consult)
    
    user Author filledBy (sys:TheWorld$PerspectivesUsers + cm:BespokeDatabase$Owner)
      perspective on extern
        props (Name) verbs (Consult, SetPropertyValue)
      perspective on EntryPoint
        all roleverbs
        props (Title) verbs (Consult, SetPropertyValue)
      perspective on PublicPages
        all roleverbs
        props (Title) verbs (Consult, SetPropertyValue)
      perspective on PublicPages >> binding >> context >> Author
        only (Create, Fill)
      screen "Public Page Collection"
        row 
          form External
        row 
          form "Entrypoint" EntryPoint
        row 
          table "Public Pages" PublicPages
    
    context PublicPages (relational) filledBy PublicPage
      state PageAvailable = exists binding
        on entry
          do for Author
            bind currentactor to Author in origin >> binding >> context

    context EntryPoint filledBy PublicPage

  case PublicPage
    aspect ht:Page

    context LinkedPages (relational) filledBy PublicPage
      aspect ht:Page$LinkedPages

    public Visitor at extern >> binder PublicPages >> context >> Author >> BespokeDatabaseUrl = sys:SocialMe
      perspective on ht:Page$TextBlocks
        props (MD, Condition, RawConditionResult, ConditionResult, ShowBlock) verbs (Consult)
      perspective on LinkedPages
        props (Title) verbs (Consult)
      -- perspective on Author
      --   props (FirstName, LastName) verbs (Consult)
      screen
        row 
          -- Conditional read only view of blocks.
          markdown ht:Page$TextBlocks
            props (MD) verbs (Consult)
            when ShowBlock

    aspect user ht:Page$Author

    aspect thing ht:Page$TextBlocks

