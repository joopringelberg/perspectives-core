module Test.Record where

type Person = {voornaam :: String, achternaam :: String}

joop :: Person
joop = {voornaam: "Joop", achternaam: "Ringelberg"}

anders :: Person
anders = joop {voornaam = "Jan"}
