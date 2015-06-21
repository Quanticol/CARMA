package eu.quanticol.carma.core.generator

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.Rate
import eu.quanticol.carma.core.carma.System
import eu.quanticol.carma.core.generator.carmavariable.CarmaVariableManager
import eu.quanticol.carma.core.generator.predicates.Predicates
import eu.quanticol.carma.core.utils.LabelUtil
import eu.quanticol.carma.core.utils.Util

import static extension org.eclipse.xtext.EcoreUtil2.*
import java.util.ArrayList
import eu.quanticol.carma.core.carma.EnvironmentOperation
import eu.quanticol.carma.core.generator.actions.ActionManager

class GenerateSystemRate {
	
	@Inject extension LabelUtil
	@Inject extension Predicates
	@Inject extension ExpressionHandler
	@Inject extension Util
	
	def String defineEnvironmentRatePredicates(System system, CarmaVariableManager cvm){
		
		var casts = system.getContainerOfType(Model).eAllOfType(Rate)
		'''
		«FOR cast : casts»
		«cast.convertToPredicateName.getEvolutionRulePredicateRate("sstore",cast.guard.booleanExpression,cvm)»
		«ENDFOR»
		'''
		
	}
	
	def String defineEnvironmentRateRules(System system, CarmaVariableManager cvm){
		'''
		«defineBroadcastRate(system,cvm)»
		«defineUnicastRate(system,cvm)»
		'''
	}
	
	def String defineBroadcastRate(System system, CarmaVariableManager cvm){
		var updates = system.getContainerOfType(Model).eAllOfType(Rate)
		var casts = new ArrayList<EnvironmentOperation>()
		
		for(eu : updates)
			if(eu.stub.isBroadcast)
				casts.add(eu)
		
		'''
		@Override
		public double broadcastRate(CarmaStore sstore, int action){
			«casts.defineConditions(system,cvm)»
			return 1.0;
		}
		'''
	}
	
	def String defineUnicastRate(System system, CarmaVariableManager cvm){
		var updates = system.getContainerOfType(Model).eAllOfType(Rate)
		var casts = new ArrayList<EnvironmentOperation>()
		
		for(eu : updates)
			if(!eu.stub.isBroadcast)
				casts.add(eu)
		
		'''
		@Override
		public double unicastRate(CarmaStore sstore, int action){
			«casts.defineConditions(system,cvm)»
			return 1.0;
		}
		'''
	}
	
	def String defineConditions(ArrayList<EnvironmentOperation> casts, System system, CarmaVariableManager cvm){
		var prefix = cvm.definitionsPrefix
		'''
		«FOR cast : casts»
		if(
		action == «prefix»«cast.stub.label.toUpperCase.replace("*","")»
		&&
		«cast.convertToPredicateName»().satisfy(sstore)
		){
			return «(cast as Rate).expression.evaluateExpression»;
		}
		«ENDFOR»
		'''
		
	}
}