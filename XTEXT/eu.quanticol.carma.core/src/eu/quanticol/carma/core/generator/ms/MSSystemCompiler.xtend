package eu.quanticol.carma.core.generator.ms

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.SetComp
import eu.quanticol.carma.core.generator.ms.collective.CollectiveHandler
import eu.quanticol.carma.core.generator.ms.environment.EnvironmentHandler
import eu.quanticol.carma.core.generator.ms.function.FunctionHandler
import eu.quanticol.carma.core.generator.ms.main.MainHandler
import eu.quanticol.carma.core.generator.ms.measure.MeasureHandler
import eu.quanticol.carma.core.generator.ms.record.RecordHandler
import java.util.ArrayList
import java.util.HashMap

import static extension org.eclipse.xtext.EcoreUtil2.*

class MSSystemCompiler {
	
	
//	@Inject extension FunctionHandler
//	@Inject extension RecordHandler
//	@Inject extension CollectiveHandler
//	@Inject extension PredefinedFunctions
//	@Inject extension MeasureHandler
//	@Inject extension EnvironmentHandler
//	@Inject extension MainHandler
//	
//	public static var SYSTEMNAME = ""
//	
//	def void extractSystem(System system, Model model, HashMap<String,String> output){
//		MSSystemCompiler.SYSTEMNAME = (system as BlockSystem).name.name
//		var toReturn = 
//			'''
//			import org.apache.commons.math3.random.RandomGenerator;
//			import org.cmg.ml.sam.sim.SimulationEnvironment;
//			import eu.quanticol.carma.simulator.*;
//			import java.util.ArrayList;
//			import java.util.Arrays;
//			import java.util.HashMap;
//			import org.cmg.ml.sam.sim.sampling.Measure;
//			import org.cmg.ml.sam.sim.sampling.SamplingCollection;
//			import org.cmg.ml.sam.sim.sampling.StatisticSampling;
//			import org.cmg.ml.sam.sim.DefaultRandomGenerator;
//			
//			public class «MSSystemCompiler.SYSTEMNAME» extends CarmaSystem {
//				
//				«getUniform»
//				
//				«model.functions.printFunctions»
//				«model.records.records»
//				«IF (system as BlockSystem).environment != null»
//				«(system as BlockSystem).collective.constructor((system as BlockSystem).environment.stores)»
//				«ELSE»
//				«(system as BlockSystem).collective.constructor(null)»
//				«ENDIF»
//				«(model.components as BlockStyle).components»
//				
//				«(model.components as BlockStyle).processes.processes.createProcess("Global")»
//				
//				«FOR c:(model.components as BlockStyle).definitions»
//				«c.componentBlock.processes.processes.createProcess(c.componentSignature.name.name)»
//				«ENDFOR»
//				
//				«new ArrayList<SetComp>(model.eAllOfType(SetComp)).getMeasures»
//				«system.getEnvironment(model)»
//				«model.getMain(system)»
//			}
//			'''
//		output.put(MSSystemCompiler.SYSTEMNAME+".java",toReturn)
//	}
	
}