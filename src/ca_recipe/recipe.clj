(defrecord Recipe
    [name
     author
     description
     ingredients
     steps
     servings
     ])

(defrecord Ingredient
    [name
     quantity
     unit
     ])

(defrecord Person
    [fname
     lname])


