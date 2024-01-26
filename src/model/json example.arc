domain model://perspectives.domaind/jsonexample

case JSONExample
  user A
    perspective on B
      all roleverbs
      -- defaults betekent: alle properties, alle property verbs
      defaults
    perspective on SubContexts
      all roleverbs
      props (P) verbs (Consult)
  thing B
    property X (Boolean)
  
  context SubContexts filledBy SubContext
  
  case SubContext
    extern 
      Property P (String)
    user SubUser filledBy A
      perspective on C
        all roleverbs
        props (Y) verbs (Consult) 
    thing C
      property Y (Number)