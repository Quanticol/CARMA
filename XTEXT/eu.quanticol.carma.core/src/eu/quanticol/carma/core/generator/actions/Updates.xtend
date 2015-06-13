package eu.quanticol.carma.core.generator.actions

import eu.quanticol.carma.core.carma.Update
import eu.quanticol.carma.core.carma.UpdateExpressions
import eu.quanticol.carma.core.generator.carmaVariable.CarmaVariableManager
import eu.quanticol.carma.core.carma.VariableReference
import static extension org.eclipse.xtext.EcoreUtil2.*
import com.google.inject.Inject
import eu.quanticol.carma.core.utils.LabelUtil
import eu.quanticol.carma.core.generator.GeneratorUtils
import eu.quanticol.carma.core.generator.ExpressionHandler

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
	
	def String getInputUpdate(Update updateBlock, CarmaVariableManager manager){
		'''
		@Override
		protected CarmaStoreUpdate getUpdate(final Object value) {
			
			return new CarmaStoreUpdate() {
				@Override
				public void update(RandomGenerator r, CarmaStore store) {
					if (value instanceof int[]){
						«updateBlock.inputUpdateBlock(manager)»
					};
				};
			
			};
		};
		'''
	}
	
	
	def String outputUpdateBlock(Update updateBlock, CarmaVariableManager manager){
		switch(updateBlock){
			NullUpdate: '''return true;'''
			default: '''
		boolean hasAttributes = true;
		«updateBlock.declareAll(manager)»
		'''
		}
	}
	
	def String inputUpdateBlock(Update updateBlock, CarmaVariableManager manager){
		switch(updateBlock){
			NullUpdate: '''return true;'''
			default: '''
		boolean hasAttributes = true;
		«updateBlock.declareAll(manager)»
		'''
		}
	}
	
	def String declareAll(Update update, CarmaVariableManager manager){
		var vrs = update.eAllOfType(VariableReference)
		'''
		«FOR vr : vrs»
		«var name = manager.cleanName(vr.label)»
		«manager.getJavaDeclaration(name,vr,"")» = 0;
		«ENDFOR»
		'''
	}
	
}