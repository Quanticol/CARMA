package eu.quanticol.carma.core.generator.actions

import eu.quanticol.carma.core.carma.Update
import eu.quanticol.carma.core.carma.UpdateExpressions
import eu.quanticol.carma.core.generator.carmavariable.CarmaVariableManager
import eu.quanticol.carma.core.carma.VariableReference
import static extension org.eclipse.xtext.EcoreUtil2.*
import com.google.inject.Inject
import eu.quanticol.carma.core.utils.LabelUtil
import eu.quanticol.carma.core.generator.GeneratorUtils
import eu.quanticol.carma.core.generator.ExpressionHandler
import java.util.HashMap
import eu.quanticol.carma.core.carma.UpdateAssignment
import eu.quanticol.carma.core.carma.InputActionArguments
import java.util.ArrayList
import eu.quanticol.carma.core.carma.VariableName

class Updates {
	
	@Inject extension LabelUtil
	@Inject extension GeneratorUtils
	@Inject extension ExpressionHandler
		
	def String getOutputUpdate(Update updateBlock, CarmaVariableManager manager){
		'''
		@Override
		protected CarmaStoreUpdate getUpdate() {
			return new CarmaStoreUpdate() {
				
				@Override
				public void update(RandomGenerator r, CarmaStore store) {
					«updateBlock.outputUpdateBlock(manager)»
				}
			};
		}
		'''
	}
	
	def String outputUpdateBlock(Update updateBlock, CarmaVariableManager manager){
		switch(updateBlock){
			NullUpdate: ''''''
			default: '''
		boolean hasAttributes = true;
		«FOR ua : updateBlock.eAllOfType(UpdateAssignment)»
		«defineOutputUpdateBlock(ua, manager)»
		«ENDFOR»
		'''
		}
	}
	
	def String defineOutputUpdateBlock(UpdateAssignment updateAssignment, CarmaVariableManager manager){
		var vrs = getAll(updateAssignment.expression,manager)
		var assign = updateAssignment.storeReference
		var expression = updateAssignment.expression
		'''
		«FOR item : manager.declareAll(vrs,"")»
		«item»
		«ENDFOR»
		«manager.setupStores(vrs,"")»
		«expression.express(manager,assign,"")»
		'''
	}
	
	def HashMap<String,VariableReference> getAll(UpdateExpressions expressions, CarmaVariableManager manager){
		var HashMap<String,VariableReference> vrs 	= new HashMap<String,VariableReference>()
		for(vr : expressions.eAllOfType(VariableReference)){
			vrs.put(manager.cleanName(vr.asFullJava),vr);
		}	
		return vrs
	}
	
	def String setupStores(CarmaVariableManager manager, HashMap<String,VariableReference> vrs, String modifier){
		'''
		«FOR key : vrs.keySet»
		«var vr = vrs.get(key)»
		«manager.getCarmaName(key,vr).getStore("Integer.class",manager.getJavaAssign(key,vr,modifier),modifier+"store","hasAttributes")»
		«ENDFOR»
		'''
	}
	
	def String express(UpdateExpressions expressions, CarmaVariableManager manager, VariableReference vr, String modifier){
		var key = manager.cleanName(vr.asFullJava)
		'''
		if(hasAttributes){
			«manager.getCarmaName(key,vr).setStore("Integer.class",manager.getJavaAssign(key,vr,modifier),modifier+"store",expressions.evaluateExpression)»
		}
		'''
	}
	
	def String getInputUpdate(Update updateBlock, InputActionArguments value, CarmaVariableManager manager){
		'''
		@Override
		protected CarmaStoreUpdate getUpdate(final Object value) {
			
			return new CarmaStoreUpdate() {
				@Override
				public void update(RandomGenerator r, CarmaStore store) {
					if (value instanceof int[]){
						«updateBlock.inputUpdateBlock(value, manager)»
					};
				};
			
			};
		};
		'''
	}
	
	
	def String inputUpdateBlock(Update updateBlock, InputActionArguments value, CarmaVariableManager manager){
		switch(updateBlock){
			NullUpdate: '''return true;'''
			default: '''
		boolean hasAttributes = true;
		«FOR ua : updateBlock.eAllOfType(UpdateAssignment)»
		«defineInputUpdateBlock(ua, value, manager)»
		«ENDFOR»
		'''
		}
	}
	
	def String defineInputUpdateBlock(UpdateAssignment updateAssignment, InputActionArguments value, CarmaVariableManager manager){
		var vrs = getAllInput(updateAssignment.expression,manager)
		var assign = updateAssignment.storeReference
		var expression = updateAssignment.expression
		'''
		«setupInputArguments(value)»
		«FOR item : manager.declareAll(vrs,"")»
		«item»
		«ENDFOR»
		«manager.setupStores(vrs,"")»
		«expression.express(manager,assign,"")»
		'''
	}
	
	def HashMap<String,VariableReference> getAllInput(UpdateExpressions expressions, CarmaVariableManager manager){
		var HashMap<String,VariableReference> vrs 	= new HashMap<String,VariableReference>()
		
		for(vr : expressions.eAllOfType(VariableReference)){
			if(manager.contains(manager.cleanName(vr.asJava)))
				vrs.put(manager.cleanName(vr.asFullJava),vr);
		}
		return vrs
	}
	
	def String setupInputArguments(InputActionArguments value){
		var ArrayList<VariableName> vns = new ArrayList<VariableName>(value.eAllOfType(VariableName))
		'''
		«FOR vn : vns»
		int «vn.label» = ((int[]) value)[«vns.indexOf(vn)»];
		«ENDFOR»
		'''	
	}
	
}