-- Copyright Joop Ringelberg and Cor Baars 2021
domain TestStandardVars

  case C1
    thing R1
      property SomeProperty (Boolean)
    context C2S filledBy C2

  case C2
      thing R2
      user U
        property Name (String)
        perspective on extern >> binder C2S >> context >> R1
          on entry of object state
            do
              SomeProperty = true
              Name = "Joop" for currentactor
              createRole R2 in currentcontext
