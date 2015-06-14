package eu.quanticol.carma.core.generator

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.System
import eu.quanticol.carma.core.generator.carmaVariable.CarmaVariableManager
import eu.quanticol.carma.core.generator.predicates.Predicates
import eu.quanticol.carma.core.utils.LabelUtil

import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.core.carma.Rate
import eu.quanticol.carma.core.carma.RateBlock
import eu.quanticol.carma.core.carma.EnvironmentOperation

class GenerateSystemRate {
	
	@Inject extension LabelUtil
	@Inject extension Predicates
	@Inject extension GeneratorUtils
	
	def String defineEnvironmentRatePredicates(System system, CarmaVariableManager cvm){
		
		var casts = system.getContainerOfType(Model).eAllOfType(Rate)
		'''
		«FOR cast : casts»
		«cast.convertToPredicateName.getEvolutionRulePredicate("rstore","sstore",cast.guard.booleanExpression,cvm)»
		«ENDFOR»
		'''
		
	}
	
	def String defineBroadcastRate(){
		'''
		@Override
		public double broadcastRate(CarmaStore sender, int action){
			return 1.0;
		}
		'''
	}
	
	def String defineUnicastRate(){
		'''
		@Override
		public double unicastRate(CarmaStore sender, int action){
			return 1.0;
		}
		'''
	}
	
	def String defineRates(System system){
		if(system.eAllOfType(RateBlock).size > 0){
			system.defineEnvironmentOperation(system.eAllOfType(RateBlock).get(0).eAllOfType(EnvironmentOperation),
			"double",
			"Rate(CarmaStore sender, int action)",
			"Rate(CarmaStore sender, int action)",
			"return 1.0;")
		} else {
			'''
			@Override
			public double unicastRate(CarmaStore sender, int action){
				return 1.0;
			}
			@Override
			public double broadcastRate(CarmaStore sender, int action){
				return 1.0;
			}'''
		}
	}
}