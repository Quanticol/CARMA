package eu.quanticol.carma.core.generator.ms.system

import eu.quanticol.carma.core.carma.SystemDefinition
import com.google.inject.Inject
import eu.quanticol.carma.core.utils.Util
import eu.quanticol.carma.core.generator.ms.collective.CollectiveHandler
import eu.quanticol.carma.core.generator.ms.environment.EnvironmentHandler
import eu.quanticol.carma.core.generator.ms.expression.ExpressionHandler
import eu.quanticol.carma.core.carma.CollectiveReference
import eu.quanticol.carma.core.carma.CollectiveBlock

class SystemHandler {
	
	@Inject extension Util
	@Inject extension CollectiveHandler
	@Inject extension EnvironmentHandler
	@Inject extension ExpressionHandler
	
	def systemsCollector( Iterable<SystemDefinition> systems ) {
		'''

		public String[] getSystems() {
			return new String[] {
				«FOR s:systems.map[it.name].sort SEPARATOR ','»
				"«s»"
				«ENDFOR»
			};	
		}
		
		public SimulationFactory<CarmaSystem> getFactory( String name ) {
			«FOR s:systems»
			if ("«s.name»".equals( name )) {
				return getFactorySystem«s.name»();
			}
			«ENDFOR»
			return null;
		}
				
		'''
	}
	
	def systemFactory( SystemDefinition sys ) {
		'''
		public class «sys.name.systemName» extends CarmaSystem {
			
			public «sys.name.systemName»( ) {
				super( «IF sys.space != null» «sys.space.spaceName» ( «FOR e:sys.args SEPARATOR ','»«e.expressionToJava»«ENDFOR») «ENDIF»);
				«IF (sys.environment != null)&&(sys.environment.store != null)»
				«FOR a:sys.environment.store.attributes»
				setGLobalAttribute( "«a.name»" , «a.value.expressionToJava» );
				«ENDFOR»
				«ENDIF»
				CarmaSystem system = this;
				CarmaSystem sys = this;
				«sys.collective.generateCollective»		
			}
			
			«IF (sys.environment != null)&&(sys.environment.probabilityBlock!=null)»
			«sys.environment.probabilityBlock.handleProbabilityBlock»
			«ELSE»
			@Override
			public double broadcastProbability( CarmaStore sender , CarmaStore receiver , int action ) {
				return 1.0;
			}
			«ENDIF»

			«IF (sys.environment != null)&&(sys.environment.weightBlock!=null)»
			«sys.environment.weightBlock.handleWeightBlock»
			«ELSE»
			@Override
			public double unicastProbability( CarmaStore sender , CarmaStore receiver , int action ) {
				return 1.0;
			}
			«ENDIF»
			
			«IF (sys.environment != null)&&(sys.environment.rateBlock!=null)»
			«sys.environment.rateBlock.handleRateBlock»			
			«ELSE»
			@Override
			public double broadcastRate( CarmaStore sender , int action ) {
				return 1.0;
			}

			@Override
			public double unicastRate( CarmaStore sender , int action ) {
				return 1.0;
			}				
			«ENDIF»
			
			«IF (sys.environment != null)&&(sys.environment.updateBlock!=null)»
			«sys.environment.updateBlock.handleUpdateBlock»
			«ELSE»
			@Override
			public void broadcastUpdate( RandomGenerator random , CarmaStore sender , int action , Object value ) {					
			}

			@Override
			public void unicastUpdate( RandomGenerator random , CarmaStore sender , CarmaStore receiver, int action , Object value ) {
			}			
			«ENDIF»
		}
		
		
		public SimulationFactory<CarmaSystem> getFactorySystem«sys.name»() {
			return new SimulationFactory<CarmaSystem>() {

				//@Override
				public CarmaSystem getModel() {
					return new «sys.name.systemName»();
				}
			
				//@Override
				public Measure<CarmaSystem> getMeasure(String name) {
					// TODO Auto-generated method stub
					//FIXME!!!!
					return null;
				}
			
			};
			
		}
		'''	
	}
	
	
	def dispatch CharSequence generateCollective( CollectiveReference ref ) {
		ref.reference.block.generateCollective
	}
	
	def dispatch CharSequence generateCollective( CollectiveBlock block ) {
		'''
		«FOR c:block.collective»
		«c.instantiationCode»
		«ENDFOR»		
		'''
	}
	
}