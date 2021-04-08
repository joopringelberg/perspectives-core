-- Copyright Joop Ringelberg and Cor Baars, 2021
domain: Publishing
  use: sys for model:System
  use: pub for model:Publishing

  case: Publishing
    state Available = Book >> Published
    user: Author filledBy sys:User
      perspective on: Book
    user: Reader filledBy sys:User
      perspective on: Book
        in state Available only Consult
        onentry Available alert
    thing: Book
      property: Text (mandatory, functional, String)
      property: Published (mandatory, functional, Boolean)
