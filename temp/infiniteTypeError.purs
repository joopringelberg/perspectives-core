
    retrieveDomeinResourceDefinition :: forall e.
      ResourceId
      -> Namespace
      -> (AsyncDomeinFile e (Either String PropDefs))
    retrieveDomeinResourceDefinition id ns = do
      -- df :: (Either String DomeinFile)
      df <- retrieveDomeinFile (namespaceToDomeinFileName ns)
      case df of
        e@(Left err) -> pure $ e
        (Right f) -> case lookup id f of
          Nothing -> pure $ Left ("retrieveDomeinResourceDefinition: cannot find definition of " <> id <> " in DomeinFile for " <> ns)
          (Just propDefs) -> pure (Right propDefs)

    retrieveDomeinFile :: forall e. Namespace -> AsyncDomeinFile e (Either String DomeinFile)

    newtype PropDefs = PropDefs (StrMap Json)
