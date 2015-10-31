package eu.quanticol.carma.core.generator.ms.measure

import com.google.inject.Inject
import eu.quanticol.carma.core.utils.Util
import eu.quanticol.carma.core.carma.MeasureDefinition
import eu.quanticol.carma.core.generator.ms.expression.ExpressionHandler
import eu.quanticol.carma.core.carma.Expression
import eu.quanticol.carma.core.carma.Range
import eu.quanticol.carma.core.carma.MeasureVariableDeclaration
import eu.quanticol.carma.core.generator.ms.attribute.AttributeHandler
import eu.quanticol.carma.core.utils.ReferenceContext

class MeasureHandler {

	@Inject extension Util	
	@Inject extension ExpressionHandler
	@Inject extension AttributeHandler
	
	public static final String SetUpMeasureMethodName = "setUpMeasures()"
	
	def measureBuilderMethod( String name ) {
		'''buildMeasure«name»( )'''
	}

	def collectMeasures( Iterable<MeasureDefinition> measures ) {

		'''
		
		private HashMap<String,Measure<CarmaSystem>> measures;
				
		public String[] getMeasures() {
			TreeSet<String> sortedSet = new TreeSet<String>( measures.keySet() );
			return sortedSet.toArray( new String[ sortedSet.size() ] );
		}
		
		public Measure<CarmaSystem> getMeasure( String name ) {
			return measures.get( name );
		}

		protected void «SetUpMeasureMethodName» {
			measures = new HashMap<String,Measure<CarmaSystem>>();
			«FOR m:measures»
			«m.name.measureBuilderMethod»;
			«ENDFOR»
		}	
		
		«FOR m:measures»
		«m.measureToJava»
		«ENDFOR»
		
		'''
	}

	def CharSequence measureBuilderBody( String name , Iterable<Pair<Integer,MeasureVariableDeclaration>> vars , int idx ) {
		if (vars.size>idx) {
			var v = vars.get(idx)
			var value = v.value.assign
			if (value instanceof Range) {
				'''
					for( 
						int v«idx» = «value.min.expressionToJava» ; 
						v«idx» <= «value.max.expressionToJava» ; 
						«IF value.step != null»
						v«idx» = v«idx» + «value.step.expressionToJava»
						«ELSE»
						v«idx»++
						«ENDIF»
					) {
						«name.measureBuilderBody(vars,idx+1)»
					}
				'''
			} else {
				'''
				int v«idx» = «value.expressionToJava»;
				«name.measureBuilderBody(vars,idx+1)»
				'''				
			}
		} else {
			'''measures.put( 
				"«name»«IF vars.size>0»[«ENDIF»"«FOR v:vars»+v«v.key»«ENDFOR»«IF vars.size>0»+"]"«ENDIF» ,
				getMeasure«name»( «FOR v:vars SEPARATOR ','»v«v.key»«ENDFOR»)
			);'''
		}
	}

	def measureToJava( MeasureDefinition m ) {
		'''
		
		private void «m.name.measureBuilderMethod» {
			«m.name.measureBuilderBody( m.variables.indexed , 0 )»
		}
		
		
		private Measure<CarmaSystem> getMeasure«m.name»( 
			«FOR v:m.variables SEPARATOR ','»final Integer «v.name.variableName»«ENDFOR»
		) {

			return new Measure<CarmaSystem>() {
			
				//@Override
				public double measure(final CarmaSystem system) {
					final CarmaStore global = system.getGlobalStore();
					final double now = system.now();
					«FOR a:m.measure.globalAttributes»
					«a.attributeTemporaryVariableDeclaration(ReferenceContext::GLOBAL,"global")»
					«ENDFOR»
					return «m.measure.expressionToJava»;
				}

				//@Override
				public String getName() {
					return "«m.name»"«FOR v:m.variables»+"_"+«v.name.variableName»«ENDFOR»;
				}
			
			};
			
		}
		
		'''
	}	
	
}