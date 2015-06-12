package eu.quanticol.carma.core.generator.actions

import com.google.inject.Inject
import eu.quanticol.carma.core.generator.predicates.Predicates
import eu.quanticol.carma.core.carma.BooleanExpressions
import eu.quanticol.carma.core.generator.carmaVariable.CarmaVariableManager
import eu.quanticol.carma.core.carma.Action

class Actions {
	
	@Inject extension Predicates
	@Inject extension Updates
	@Inject extension Values
	
	def String getAction(String name, Action action, CarmaVariableManager cvm){
		'''
		
		'''
	}
	
	def String getActionOutput(String action_name, 
		String action_enum, 
		String isBroadcast, 
		BooleanExpressions satisfyBlock, 
		String updateBlock,
		String valueBlock
	){
		'''
		CarmaOutput «action_name» = new CarmaOutput( «action_enum», «isBroadcast» ) {
			
			«getOutputActionPredicate(satisfyBlock)»
		
			«getOutputUpdate(updateBlock)»
		
			«getValues(valueBlock)»
		};
		'''
	}
	
	def String getActionInput(String action_name, 
		String action_enum, 
		String isBroadcast, 
		BooleanExpressions satisfyBlock, 
		Object value,
		String updateBlock
	){
		'''
		CarmaInput «action_name» = new CarmaInput( «action_enum», «isBroadcast» ) {
			
			«getInputActionPredicate(satisfyBlock, value)»
		
			«getInputUpdate(updateBlock)»
		
		};
		'''
	}
	
}