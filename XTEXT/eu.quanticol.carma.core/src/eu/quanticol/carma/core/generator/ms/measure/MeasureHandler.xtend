package eu.quanticol.carma.core.generator.ms.measure

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.MeasureDefinition
import eu.quanticol.carma.core.generator.ms.attribute.AttributeHandler
import eu.quanticol.carma.core.generator.ms.expression.ExpressionHandler
import eu.quanticol.carma.core.utils.ReferenceContext
import eu.quanticol.carma.core.utils.Util

class MeasureHandler {

	@Inject extension Util	
	@Inject extension ExpressionHandler
	@Inject extension AttributeHandler
	
	public static final String SetUpMeasureMethodName = "setUpMeasures()"
	
	def collectMeasures( Iterable<MeasureDefinition> measures ) {

		'''
		
		public String[] getMeasures() {
			TreeSet<String> sortedSet = new TreeSet<String>( );
			«FOR m:measures»
			sortedSet.add( "«m.name»" );
			«ENDFOR»
			return sortedSet.toArray( new String[ sortedSet.size() ] );
		}
		
		public Measure<CarmaSystem> getMeasure( String name , Map<String,Object> parameters ) {
			«FOR m:measures»
			if ("«m.name»".equals( name ) ) {
				return getMeasure«m.name»( parameters );
			}
			«ENDFOR»			
			return null;
		}

		public String[] getMeasureParameters( String name ) {
			«FOR m:measures»
			if ("«m.name»".equals( name ) ) {
				return new String[] { «FOR v:m.variables SEPARATOR ','»"«v.name»"«ENDFOR»};
			}
			«ENDFOR»			
			return new String[] {};
		}
		
		public Map<String,Class<?>> getParametersType( String name ) {
			«FOR m:measures»
			if ("«m.name»".equals( name ) ) {
				HashMap<String,Class<?>> toReturn = new HashMap<>();
				«FOR v:m.variables»
				toReturn.put( "«v.name»" , «v.type.toJavaType».class );
				«ENDFOR»
				return toReturn;
			}
			«ENDFOR»			
			return new HashMap<>();
		}
		
	
		«FOR m:measures»
		«m.measureToJava»
		«ENDFOR»
		
		'''
	}

	def measureToJava( MeasureDefinition m ) {
		'''		
		private static double «m.name.measureName»( CarmaSystem system «FOR v:m.variables » , «v.type.toJavaType» «v.name.variableName» «ENDFOR») {
			final CarmaStore global = system.getGlobalStore();
			final double now = system.now();
			final CarmaSystem sys = system;
			«FOR a:m.measure.globalAttributes»
			«a.attributeTemporaryVariableDeclaration(ReferenceContext::GLOBAL,"global")»
			«ENDFOR»
			return «m.measure.expressionToJava»;
		}
		
		
		private Measure<CarmaSystem> getMeasure«m.name»( 
			Map<String,Object> parameters
		) {
			
			«FOR v:m.variables»
			final «v.type.toJavaType» «v.name.variableName» = («v.type.toJavaType») parameters.get("«v.name»");
			«ENDFOR»

			return new Measure<CarmaSystem>() {
			
				//@Override
				public double measure(final CarmaSystem system) {
					return «m.name.measureName»( system «FOR v:m.variables», «v.name.variableName» «ENDFOR»);
				}

				//@Override
				public String getName() {
					return "«m.name»"«IF m.variables.size>0»+"["
							+«FOR v:m.variables SEPARATOR '+'»"«v.name»="+«v.name.variableName»«ENDFOR»
						+"]"«ENDIF»;
				}
			
			};
			
		}
		
		'''
					/*
					final CarmaStore global = system.getGlobalStore();
					final double now = system.now();
					«FOR a:m.measure.globalAttributes»
					«a.attributeTemporaryVariableDeclaration(ReferenceContext::GLOBAL,"global")»
					«ENDFOR»
					return «m.measure.expressionToJava»;
					*/
	}	
	
}