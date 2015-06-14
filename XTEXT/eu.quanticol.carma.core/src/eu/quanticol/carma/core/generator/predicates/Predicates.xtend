package eu.quanticol.carma.core.generator.predicates

import eu.quanticol.carma.core.carma.BooleanExpressions
import com.google.inject.Inject
import eu.quanticol.carma.core.generator.carmaVariable.CarmaVariableManager
import eu.quanticol.carma.core.carma.InputActionArguments
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressions
import eu.quanticol.carma.core.generator.ExpressionHandler

class Predicates {
	
	@Inject extension SatisfyBlock
	@Inject extension ExpressionHandler
	
	def String getInputActionPredicate(BooleanExpressions bes, InputActionArguments value, CarmaVariableManager cvm){
		'''
		@Override
		protected CarmaPredicate getPredicate(CarmaStore istore, final Object value) {
			if (value instanceof int[]){
				return new CarmaPredicate() {
					@Override
					public boolean satisfy(CarmaStore ostore) {
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
		protected CarmaPredicate getPredicate(CarmaStore ostore) {
			return new CarmaPredicate() {
				@Override
				public boolean satisfy(CarmaStore istore) {
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
	
	def String getMeasureStatePredicate(String name, EnvironmentMacroExpressions eme){
		'''
		public static CarmaProcessPredicate getMeasure«name»_State_Predicate(){
			return new CarmaProcessPredicate() {
				
				@Override
				public boolean eval(CarmaProcess p) {
					return «eme.evaluateExpression»
				}
			};
		}
		'''
	}
	
	def String getMeasureBooleanExpressionPredicate(String name, BooleanExpressions bes, String inArgs, String outArgs, CarmaVariableManager cvm){
		'''
		protected static CarmaPredicate getPredicate«name»(«inArgs») {
			return new CarmaPredicate() {
				@Override
				public boolean satisfy(CarmaStore store) {
					«bes.getMeasureSatisfyBlock(cvm)»
				}
			};
		}
		
		public static ComponentPredicate getMeasure«name»_BooleanExpression_Predicate(«inArgs»){
			return new ComponentPredicate() {
				
				@Override
				public boolean eval(CarmaComponent c){
					return getPredicate«name»(«outArgs»).satisfy(c.getStore()) && (c.isRunning(getMeasure«name»_State_Predicate()));
				}
			};
		}
		'''
	}
	
	def String defineGetMeasureMethod(String name, BooleanExpressions bes, String inArgs, String outArgs, CarmaVariableManager cvm){
		'''
		public static Measure<CarmaSystem> getMeasure«name»(«inArgs»){
			
			return new Measure<CarmaSystem>(){
			
				ComponentPredicate predicate = getMeasure«name»_BooleanExpression_Predicate(«outArgs»);
			
				@Override
				public double measure(CarmaSystem t){
					return t.measure(predicate);
			
				};
			
				@Override
				public String getName() {
					return "«name» with «inArgs»";
				}
			};
		}
		'''
	}
	
	def String defineGetEnvMeasureMethod(String name, BooleanExpressions bes, String inArgs, String outArgs, CarmaVariableManager cvm){
		'''
		public static Measure<CarmaSystem> get«name»(«inArgs»){
			
			return new Measure<CarmaSystem>(){
			
				ComponentPredicate predicate = getMeasure«name»_BooleanExpression_Predicate(«outArgs»);
			
				@Override
				public double measure(CarmaSystem t){
					return t.measure(predicate);
			
				};
			
				@Override
				public String getName() {
					return "«name» with «inArgs»";
				}
			};
		}
		'''
	}
	
	def String getEvolutionRulePredicate(String predicateName, 
		String rStore, 
		String sStore,
		BooleanExpressions bes,
		CarmaVariableManager cvm
	){
		'''
		public static CarmaPredicate «predicateName»(CarmaStore «rStore»){
			return new CarmaPredicate() {
				@Override
				public boolean satisfy(CarmaStore «sStore») {
					«bes.getCastSatisfyBlock(cvm)»
				}
			};
		}
		'''
		
	}
	
}