(ns ca-recipe.recipe
  (:require [schema.core :as s]))

(s/defrecord Ingredient
    [name :- s/Str
     quantity :- s/Int
     unit :- s/Keywort
     ])

(s/defrecord Recipe
    [name :- s/Str
     author :- s/Str
     description :- s/Str
     ingredients :- [Ingredient]
     steps :- [s/Str]
     servings :- s/Int
     ])

(s/defrecord Person
    [fname :- s/Str
     lname :- s/Str
     ])


