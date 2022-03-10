-- Copyright Joop Ringelberg and Cor Baars 2021
-- A model to plan meals.

domain MenuPlanning
  use sys for model:System
  use mn for model:MenuPlanning

  -- The model description case.
  case Model
    aspect sys:Model
    external
      aspect sys:Model$External

  -- This model-level thing role returns in several cases.
  thing Ingredient (relational)
    -- None of these properties is required.
    -- Use them to indicate how much of the ingredient is needed
    -- to cook the meal.
    property Weight
    property Number
    property Volume

  case MenuPlanner
    thing Weeks filledBy WeekMenu

    user Shopper filledBy User

  case WeekMenu
    external
      property StartsOn (DateTime)

    -- The week has seven days and (at most) seven meals.
    context MealOfTheDay filledBy Recipe
      property Day (DateTime)

    thing Ingredients = MealOfTheDay >> binding >> Ingredient

    user Shopper filledBy MenuPlanner$Shopper
      perspective on Ingredients

  case ShoppingList
    external
      property OrderOn (DateTime)

    thing Ingredients filledBy mn:Ingredient

    user Shopper
      perspective on Ingredients

  case Meal

    -- (Dinner) Guests sit at the table and consume the meal. Their interest is to
    -- know the menu, beforehand.
    user Guest (relational) filledBy User

    user Cook filledBy User

    -- The Shopper takes care of the groceries to cook the meal.
    user Shopper filledBy MenuPlanner$Shopper

    thing Recipe filledBy Recipe

  -- A Recipe is a manual for cooking a meal.
  case Recipe
    external
      property CookingTime (Number)
      property Instructions (String)

    thing Ingredient filledBy mn:Ingredient

      thing Item (relational) filledBy mn:Ingredient

        -- Use these properties in any combination to indicate how much
        -- should be ordered. Notice that these properties are separate from
        -- their counterparts in mn:Ingredient (which are used to indicate
        -- what is needed for a particular meal).
        property Weight
        property Number
        property Volume

    -- This case is an inventory of provisions to be kept in stock.
    case ItemsInStock
      thing Item (relational) filledBy mn:Ingredient
