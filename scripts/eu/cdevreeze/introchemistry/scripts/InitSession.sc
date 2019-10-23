

// Run amm in scripts folder
// In amm session, use command "import $exec.eu.cdevreeze.introchemistry.scripts.InitSession"

import $ivy.`eu.cdevreeze.introchemistry::introchemistry:0.1.0-SNAPSHOT`

import eu.cdevreeze.introchemistry.periodictable.load._, eu.cdevreeze.introchemistry.periodictable._, eu.cdevreeze.introchemistry.stoichiometry._
import eu.cdevreeze.introchemistry.internal._, eu.cdevreeze.introchemistry.api._
import eu.cdevreeze.introchemistry.particlesandwaves._, eu.cdevreeze.introchemistry.orbitals._
import eu.cdevreeze.introchemistry.lewis._, eu.cdevreeze.introchemistry.thermochemistry._
import eu.cdevreeze.introchemistry.typeclasses.instances.ShowChemicalEquations._
import eu.cdevreeze.introchemistry.typeclasses.instances.ShowElectronConfigs._
import eu.cdevreeze.introchemistry.typeclasses.instances.ShowLewisStructures._
import eu.cdevreeze.introchemistry.typeclasses.show.ShowSyntax._

val periodicTable = PeriodicTableLoader.newInstance().loadPeriodicTable()

val queryApi = new SimpleQueryApi(periodicTable)

import SimpleQueryApi._
import queryApi._
import ElementSymbol._
import scala.util.chaining._
import LewisStructure._

// Now we can make formulas/reactions and do stoichiometry calculations etc.

