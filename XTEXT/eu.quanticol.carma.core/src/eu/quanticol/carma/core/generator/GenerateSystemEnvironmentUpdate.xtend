package eu.quanticol.carma.core.generator

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.EnvironmentUpdate
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.System
import eu.quanticol.carma.core.generator.carmaVariable.CarmaVariableManager
import eu.quanticol.carma.core.generator.predicates.Predicates
import eu.quanticol.carma.core.utils.LabelUtil

import static extension org.eclipse.xtext.EcoreUtil2.*

class GenerateSystemEnvironmentUpdate {
	
	@Inject extension LabelUtil
	@Inject extension Predicates
	
	def String defineEnvironmentUpdatePredicates(System system, CarmaVariableManager cvm){
		
		var casts = system.getContainerOfType(Model).eAllOfType(EnvironmentUpdate)
		'''
		«FOR cast : casts»
		«cast.convertToPredicateName.getEvolutionRulePredicate("rstore","sstore",cast.guard.booleanExpression,cvm)»
		«ENDFOR»
		'''
		
	}
}