package eu.quanticol.carma.core.generator.predicates

import eu.quanticol.carma.core.carma.BooleanExpressions
import com.google.inject.Inject
import eu.quanticol.carma.core.generator.carmaVariable.CarmaVariableManager
import eu.quanticol.carma.core.carma.InputActionArguments

class Predicates {
	
	@Inject extension SatisfyBlock
	
	def String getInputActionPredicate(BooleanExpressions bes, InputActionArguments value, CarmaVariableManager cvm){
		'''
		@Override
		protected CarmaPredicate getPredicate(CarmaStore outputStore, final Object value) {
			if (value instanceof int[]){
				return new CarmaPredicate() {
					@Override
					public boolean satisfy(CarmaStore inputStore) {
						«bes.getInputSatisfyBlock(value,cvm)»
					}
				};
			}
			return null;
		}
		'''
	}
	
	def String getOutputActionPredicate(BooleanExpressions bes, CarmaVariableManager cvm){
		'''
		@Override
		protected CarmaPredicate getPredicate(CarmaStore outputStore) {
			return new CarmaPredicate() {
				@Override
				public boolean satisfy(CarmaStore inputStore) {
					«bes.getOutputSatisfyBlock(cvm)»
				}
			};
		}
		'''
	}
	
	def String getGuardPredicate(String name, BooleanExpressions bes, CarmaVariableManager cvm){
		'''
		CarmaPredicate «name» = new CarmaPredicate() {
			@Override
			public boolean satisfy(CarmaStore store) {
				«bes.getGuardSatisfyBlock(cvm)»
			}
		};
		'''
	}
	
}