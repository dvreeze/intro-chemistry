

// Run amm in scripts folder
// In amm session, use command "import $exec.eu.cdevreeze.introchemistry.scripts.InitSession"

import $ivy.`eu.cdevreeze.introchemistry::introchemistry:0.1.0-SNAPSHOT`

import eu.cdevreeze.introchemistry.periodictable.jvm._, eu.cdevreeze.introchemistry.periodictable._, eu.cdevreeze.introchemistry.stoichiometry._, eu.cdevreeze.introchemistry.internal._

val periodicTable = PeriodicTableLoader.newInstance().loadPeriodicTable()

val stoichiometrySupport = new StoichiometrySupport(periodicTable)

import stoichiometrySupport._
import ElementSymbol._

// Now we can make formulas/formula units and do stoichiometry calculations

