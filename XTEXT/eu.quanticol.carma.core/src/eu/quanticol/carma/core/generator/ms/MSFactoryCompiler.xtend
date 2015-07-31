package eu.quanticol.carma.core.generator.ms

import eu.quanticol.carma.core.carma.Model
import java.util.HashMap
import eu.quanticol.carma.core.carma.SystemDefinition

/**
	 * 	Factory
	 * 		contructor
	 * 		getModel()
	 */

class MSFactoryCompiler {
	
	def void extractFactory(SystemDefinition system, Model model, HashMap<String,String> output){
		
		var toReturn = 
		'''
		'''

//		import org.cmg.ml.sam.sim.*;
//		import eu.quanticol.carma.simulator.*;
//		public class «(system as BlockSystem).name.name»ModelFactory implements SimulationFactory<CarmaSystem> {
//		
//			public «(system as BlockSystem).name.name»ModelFactory() {
//				
//			}
//			
//			@Override
//			public CarmaSystem getModel() {
//				return new «(system as BlockSystem).name.name»();
//			}
//		
//		}
		
		output.put('''«system.name»ModelFactory.java''',toReturn)
	}
}