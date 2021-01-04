package day21

import day21.Day21._
import helper.Helper._

object part1 {

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).getLines.toList

    val solution = solve(input)

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  def solve(lines: List[String]): Long = {

    val recipes = Day21.parse(lines)

    val allergenMap = findAllergenMap(recipes)

    val allergenFreeIngredients = allergenMap.filter(_._2.isEmpty).keySet
    val numberOfAppearances = recipes.map(_.ingredients.toSet.intersect(allergenFreeIngredients).size).sum
    numberOfAppearances
  }

}

object part2 {

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).getLines.toList

    val solution = solve(input)

    println(s"Solution for ${getCallingMainClass} is: '$solution'")
  }

  def solve(lines: List[String]): String = {

    val recipes = Day21.parse(lines)

    val allergenMap = findAllergenMap(recipes)

    val allergenFreeIngredients = allergenMap.toList
      .collect { case (ing, Some(allergen)) =>
        ing -> allergen
      }
      .sortBy(_._2.name)

    val answer = allergenFreeIngredients.map(_._1.name).mkString(",")
    answer
  }

}

object Day21 {

  case class Ingredient(name: String) extends AnyVal
  case class Allergen(name: String) extends AnyVal
  case class Recipe(id: Int, ingredients: List[Ingredient], allergens: List[Allergen])
  def parse(lines: List[String]): List[Recipe] = {

    val recipes = lines.zipWithIndex
      .map { case (line, idx) =>
        val List(ingredientsString, allergensString) = line.split(" \\(contains ").toList
        val ingredients = ingredientsString.split(" ").map(Ingredient).toList
        val allergens = allergensString.replace(")", "").split(", ").map(Allergen).toList

        Recipe(idx, ingredients, allergens)
      }

    recipes
  }

  def findAllergenMap(recipes: List[Recipe]): Map[Ingredient, Option[Allergen]] = {

    // IMPORTANT:
    // Allergens aren't always marked; when they're listed (as in (contains nuts, shellfish) after an ingredients list),
    // the ingredient that contains each listed allergen will be somewhere in the corresponding ingredients list.

    // for each allergen, all the missing ingredients are definitely not it

    val allAllergens = recipes.flatMap(_.allergens).toSet
    val allIngredients = recipes.flatMap(_.ingredients).toSet

    val couldBeMap: Map[Ingredient, Set[Allergen]] =
      findPotentialAllergensPerIngredient(recipes, allIngredients, allIngredients.map(ing => ing -> allAllergens).toMap)

    val allergenMap = reduceAllergensPerIngredient(couldBeMap)

    allergenMap
  }

  def findPotentialAllergensPerIngredient(
      recipes: List[Recipe],
      allIngredients: Set[Ingredient],
      couldBeMap: Map[Ingredient, Set[Allergen]]
  ): Map[Ingredient, Set[Allergen]] = {

    recipes match {
      case Nil =>
        couldBeMap
      case ::(head, tail) =>
        val notUsedIngredients = allIngredients.diff(head.ingredients.toSet)

        //None of the ingredients, that are not used in this recipe, can contain any of the allergens listed in this recipe.
        val updatedMap = notUsedIngredients.foldLeft(couldBeMap) { case (map, ing) =>
          map.updated(ing, map(ing).removedAll(head.allergens))
        }
        findPotentialAllergensPerIngredient(tail, allIngredients, updatedMap)
    }
  }

  def reduceAllergensPerIngredient(
      couldBeMap: Map[Ingredient, Set[Allergen]],
      result: Map[Ingredient, Option[Allergen]] = Map.empty
  ): Map[Ingredient, Option[Allergen]] = {
    //now, we have a map, that lists the potential allergens of each ingredient.
    // mxmxvkd --> Set(dairy, fish)
    // sqjhc --> Set(fish, soy)
    // fvjkl --> Set(soy)
    // trh --> Set())

    if (couldBeMap.isEmpty) {
      result
    } else {

      val (ing, allergens) = couldBeMap.toList.minBy(_._2.size)

      allergens.toList match {
        case Nil =>
          //this ingredient has no allergens
          reduceAllergensPerIngredient(couldBeMap.removed(ing), result.updated(ing, None))

        case a @ List(allergen) =>
          //this ingredient is definitively the one that contains this allergen

          //remove this allergen from all candidates (and filter out the empty entries)
          val updatedMap = couldBeMap
            .map { case (ing, all) => ing -> all.removedAll(a) }
            .filterNot(_._2.isEmpty)

          //add the mapping
          val updatedResult = result.updated(ing, Some(allergen))

          reduceAllergensPerIngredient(updatedMap, updatedResult)

        case a :: rest =>
          throw new RuntimeException("broken")
      }
    }

  }
}

/*
ingredients

fvjkl
kfcds
mxmxvkd
nhms
sbzzf
sqjhc


allergens
dairy
fish
soy

trh */
