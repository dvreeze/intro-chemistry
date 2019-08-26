

// Run amm in scripts folder
// In amm session, use command "import $exec.eu.cdevreeze.introchemistry.scripts.InitSession"

import $ivy.`eu.cdevreeze.introchemistry::introchemistry:0.1.0-SNAPSHOT`

import eu.cdevreeze.introchemistry.periodictable.jvm._, eu.cdevreeze.introchemistry.periodictable._, eu.cdevreeze.introchemistry.stoichiometry._
import eu.cdevreeze.introchemistry.internal._, eu.cdevreeze.introchemistry.api._

val periodicTable = PeriodicTableLoader.newInstance().loadPeriodicTable()

val queryApi = new SimpleQueryApi(periodicTable)

import queryApi._
import ElementSymbol._
import scala.util.chaining._

// Now we can make formulas/reactions and do stoichiometry calculations etc.

