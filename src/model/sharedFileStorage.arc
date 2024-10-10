-- Copyright Joop Ringelberg and Cor Baars, 2024
-- A model to maintain a Shared Fileserver.
domain model://perspectives.domains#SharedFileServices
  use sys for model://perspectives.domains#System
  use sfs for model://perspectives.domains#SharedFileServices

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- We must first create the context and then later bind it.
          -- If we try to create and bind it in a single statement, 
          -- we find that the Installer can just create RootContexts
          -- as they are the allowed binding of StartContexts.
          -- As a consequence, no context is created.
          app <- create context SharedFileServices
          start <- create role StartContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind_ app >> extern to start
          Name = "Shared File Services App" for start
  
  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (sfs:MySharedFileServices >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (sfs:MySharedFileServices >> extern)
      in
        remove context indexedcontext
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer

  thing SharedFileService
    aspect sys:RoleWithId
    property AccountName (String)
    property Password (String)
    property StorageType (String)
    -- PDRDEPENDENCY
    property FileShareCredentials = "{\"accountName\": \"" + AccountName + "\", \"password\": \"" + Password + "\", \"storageType\": \"" + StorageType + "\", \"sharedStorageId\": \"" + Id + "\"}"

  -------------------------------------------------------------------------------
  ---- SHARED FILE SERVICES
  -------------------------------------------------------------------------------
  -- The entry point (the `application`), available as sfs:MySharedFileServices.
  case SharedFileServices
    -- PDRDEPENDENCY
    indexed sfs:MySharedFileServices
    aspect sys:RootContext
    external
      aspect sys:RootContext$External
    
    on entry 
      do for Manager
        letA
          defaultserver <- create role DefaultFileServer
        in
          -- For the ppstorage type (perspectives-sharedfilestorage) we have no need for the AccountName. 
          -- Also, we overload `Password` with the unique personal SharedFileServerKey that allows limited uploading of files.
          AccountName = "ignored" for defaultserver
          Password = sys:SocialMe >> binding >> SharedFileServerKey for defaultserver
          StorageType = "ppstorage" for defaultserver
          NrOfUploadedFiles = 0 for defaultserver
    
    user Manager = sys:Me
      perspective on MySharedFileService 
        only (Create, Remove)
        props (AccountName, Password, StorageType) verbs (Consult, SetPropertyValue)
      
      perspective on ActualSharedFileServer
        props (AccountName, Password, StorageType, FileShareCredentials) verbs (Consult)
      
      perspective on DefaultFileServer
        only (Create)
        props (AccountName, Password, StorageType, Disabled, NrOfUploadedFiles) verbs (SetPropertyValue)
            
      screen "Shared File Servers"
        row
          markdown <## Configure file sharing
                    Media files are too large to be sent directly to your peers. Consequently, MyContexts puts them in a place that you have write rights to, and from which your peers can read.

                    As MyContexts is totally distributed, we cannot provide you with a central place to put the media files that all MyContexts users want to share. 
                    However, you can easily set up your own storage. Currently we support the cloud service Mega. The company offers you a free account with 20Gb of storage. 
                    Sign up at the [Mega website](https://mega.nz/). Write down your username and password and enter them below. Write "mega" in the field 'StorageType' and you are all set. 
                    Rest assured: these credentials will never leave your computer (except to authenticate with Mega)! Such is the inherent safety of a truly distributed program such as MyContexts.
                    >
            when not (exists MySharedFileService)
        row 
          form "Your own shared file service" MySharedFileService
        row
          form "The actual shared file service in use" ActualSharedFileServer

    thing MySharedFileService 
      aspect sfs:SharedFileService

    -- PDRDEPENDENCY
    thing ActualSharedFileServer (functional) = MySharedFileService orElse (filter DefaultFileServer with not Disabled)

    thing DefaultFileServer
      aspect sfs:SharedFileService
      property Disabled (Boolean)
      property NrOfUploadedFiles (Number)
