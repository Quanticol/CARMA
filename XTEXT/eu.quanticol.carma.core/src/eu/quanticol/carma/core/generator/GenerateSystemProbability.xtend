package eu.quanticol.carma.core.generator

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.Probability
import eu.quanticol.carma.core.carma.System
import eu.quanticol.carma.core.generator.carmavariable.CarmaVariableManager
import eu.quanticol.carma.core.generator.predicates.Predicates
import eu.quanticol.carma.core.utils.LabelUtil

import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.core.carma.EnvironmentOperation
import eu.quanticol.carma.core.generator.actions.ActionManager
import java.util.ArrayList
import eu.quanticol.carma.core.utils.Util

class GenerateSystemProbability {
	
	@Inject extension LabelUtil
	@Inject extension Predicates
	@Inject extension ExpressionHandler
	@Inject extension Util
		
	def String defineEnvironmentProbPredicates(System system, CarmaVariableManager cvm){
		
		var casts = system.getContainerOfType(Model).eAllOfType(Probability)
		'''
		«FOR cast : casts»
		«cast.convertToPredicateName.getEvolutionRulePredicate("rstore","sstore",cast.guard.booleanExpression,cvm)»
		«ENDFOR»
		'''
	}
	
	def String defineEnvironmentProbRules(System system, CarmaVariableManager cvm){
		'''
		«defineBroadcastProb(system,cvm)»
		«defineUnicastProb(system,cvm)»
		'''
	}
	
	def String defineBroadcastProb(System system, CarmaVariableManager cvm){
		var updates = system.getContainerOfType(Model).eAllOfType(Probability)
		var casts = new ArrayList<EnvironmentOperation>()
		
		for(eu : updates)
			if(eu.stub.isBroadcast)
				casts.add(eu)
		
		'''
		@Override
		public double broadcastProbability( CarmaStore sstore , CarmaStore rstore , int action ){
			«casts.defineConditions(system,cvm)»
			return 1.0;
		}
		'''
	}
	
	def String defineUnicastProb(System system, CarmaVariableManager cvm){
		var updates = system.getContainerOfType(Model).eAllOfType(Probability)
		var casts = new ArrayList<EnvironmentOperation>()
		
		for(eu : updates)
			if(!eu.stub.isBroadcast)
				casts.add(eu)
		
		'''
		@Override
		public double unicastProbability( CarmaStore sstore , CarmaStore rstore , int action ){
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
		«cast.convertToPredicateName»(rstore).satisfy(sstore)
		){
			return «(cast as Probability).expression.evaluateExpression»;
		}
		«ENDFOR»
		'''
		
	}
}