package eu.quanticol.carma.core.generator.predicates

import eu.quanticol.carma.core.carma.BooleanExpressions
import com.google.inject.Inject
import eu.quanticol.carma.core.generator.carmaVariable.CarmaVariableManager

class Predicates {
	
	@Inject extension SatisfyBlock
	
	def String getInputActionPredicate(BooleanExpressions bes, Object value){
		'''
		@Override
		protected CarmaPredicate getPredicate(CarmaStore outputStore, final Object value) {
			if (value instanceof int[]){
				return new CarmaPredicate() {
					@Override
					public boolean satisfy(CarmaStore inputStore) {
						«bes.getSatisfyBlock(value)»
					}
				};
			}
			return null;
		}
		'''
	}
	
	def String getOutputActionPredicate(BooleanExpressions bes){
		'''
		@Override
		protected CarmaPredicate getPredicate(CarmaStore outputStore) {
			return new CarmaPredicate() {
				@Override
				public boolean satisfy(CarmaStore inputStore) {
					«bes.getSatisfyBlock»
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