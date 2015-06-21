package eu.quanticol.carma.core.generator.actions

import com.google.inject.Inject
import eu.quanticol.carma.core.generator.predicates.Predicates
import eu.quanticol.carma.core.carma.BooleanExpressions
import eu.quanticol.carma.core.generator.carmavariable.CarmaVariableManager
import eu.quanticol.carma.core.carma.Action
import eu.quanticol.carma.core.typing.TypeProvider
import eu.quanticol.carma.core.utils.LabelUtil
import eu.quanticol.carma.core.carma.Update
import eu.quanticol.carma.core.carma.OutputActionArguments
import eu.quanticol.carma.core.carma.InputActionArguments

class Actions {
	
	@Inject extension Predicates
	@Inject extension Updates
	@Inject extension Values
	@Inject extension TypeProvider
	@Inject extension LabelUtil
	
	def String getAction(String name, Action action, CarmaVariableManager cvm, ActionManager am){
		var av = am.get(action.name.label)		
		'''
		«IF(action.type.set.equals("outputAction"))»
		«name.getActionOutput(av.staticName,
			av.isBroadcast,
			av.getPredicate(action.hashCode), 
			av.getUpdate(action.hashCode),
			av.getOutputActionArguments(action.hashCode),
			cvm
		)»
		«ELSE»
		«name.getActionInput(av.staticName,
			av.isBroadcast,
			av.getPredicate(action.hashCode), 
			av.getUpdate(action.hashCode),
			av.getInputActionArguments(action.hashCode),
			cvm
		)»
		«ENDIF»
		'''
	}
	
	def String getActionOutput(String action_name, 
		String action_enum, 
		boolean isBroadcast, 
		BooleanExpressions satisfyBlock, 
		Update updateBlock,
		OutputActionArguments valueBlock,
		CarmaVariableManager cvm
	){
		'''
		CarmaOutput «action_name» = new CarmaOutput( «action_enum», «isBroadcast» ) {
			
			«getOutputActionPredicate(satisfyBlock,cvm)»
		
			«getOutputUpdate(updateBlock,cvm)»
		
			«getValues(valueBlock,cvm)»
		};
		'''
	}
	
	def String getActionInput(String action_name, 
		String action_enum, 
		boolean isBroadcast, 
		BooleanExpressions satisfyBlock, 
		Update updateBlock,
		InputActionArguments valueBlock,
		CarmaVariableManager cvm
	){
		'''
		CarmaInput «action_name» = new CarmaInput( «action_enum», «isBroadcast» ) {
			
			«getInputActionPredicate(satisfyBlock, valueBlock, cvm)»
		
			«getInputUpdate(updateBlock,valueBlock, cvm)»
		
		};
		'''
	}
	
}