package eu.quanticol.carma.core.generator

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.System
import eu.quanticol.carma.core.generator.actions.ActionManager
import eu.quanticol.carma.core.generator.carmavariable.CarmaVariableManager
import eu.quanticol.carma.core.generator.components.ComponentDefine
import eu.quanticol.carma.core.generator.components.ComponentManager
import eu.quanticol.carma.core.generator.components.ComponentNew
import eu.quanticol.carma.core.generator.components.DeclareGlobalStore
import eu.quanticol.carma.core.generator.main.Main
import eu.quanticol.carma.core.generator.measures.MeasureManager
import eu.quanticol.carma.core.generator.predicates.Predicates
import eu.quanticol.carma.core.utils.LabelUtil

import static extension org.eclipse.xtext.EcoreUtil2.*

class GenerateSystems {
	
	
	@Inject extension LabelUtil
	@Inject extension GenerateSystemEnvironmentUpdate
	@Inject extension GenerateSystemRate
	@Inject extension GenerateSystemProbability
	@Inject extension Main
	@Inject extension ComponentNew
	@Inject extension DeclareGlobalStore
	@Inject extension ComponentDefine
	@Inject extension Predicates
	
	def String compileSystem(System system, 
		String packageName,
		CarmaVariableManager variableManager, 
		ActionManager actionManager,
		ComponentManager componentManager,
		MeasureManager measureManager
		){
		var modelName = system.getContainerOfType(Model).label
		var systemName = system.label
		'''
		«packageName»;
		
		import java.util.ArrayList;
		import org.apache.commons.math3.random.RandomGenerator;
		import org.cmg.ml.sam.sim.SimulationEnvironment;
		import eu.quanticol.carma.simulator.*;
		import org.cmg.ml.sam.sim.sampling.Measure;
		import org.cmg.ml.sam.sim.sampling.SamplingCollection;
		import org.cmg.ml.sam.sim.sampling.StatisticSampling;
		public class «systemName» extends CarmaSystem {
			
			//constructor
			public «systemName»(){
				«componentManager.setupComponents»
				«variableManager.setGlobalStore(system)»
			}
			
			//define components
			«componentManager.defineComponents»
			
			//predicates
			«system.defineEnvironmentRatePredicates(variableManager)»
			«system.defineEnvironmentProbPredicates(variableManager)»
			«system.defineEnvironmentUpdatePredicates(variableManager)»
			
			//evol rules
			«system.defineEnvironmentRateRules(variableManager)»
			«system.defineEnvironmentProbRules(variableManager)»
			«system.defineEnvironmentUpdateRules(variableManager,componentManager)»
			
			//measures
			«measureManager.defineMeasures(systemName)»
			
			//main
			«modelName.getMain(measureManager)»
			
		}
		'''
	}
	
	def String defineMeasures(MeasureManager mm, String system){
		var measures = mm.getMeasures(system)
		if(measures.size > 0)
			'''
			«FOR key : measures»
			«getMeasureStatePredicate(key,mm.getEnvMeasures.get(key).getEME)»
			«getMeasureBooleanExpressionPredicate(key, 
				mm.getEnvMeasures.get(key).getBES, 
				mm.getEnvMeasures.get(key).getInArgs, 
				mm.getEnvMeasures.get(key).getOutArgs, 
				mm.getCVM
			)»
			«defineGetEnvMeasureMethod(key,
				mm.getEnvMeasures.get(key).getBES, 
				mm.getEnvMeasures.get(key).getInArgs, 
				mm.getEnvMeasures.get(key).getOutArgs, 
				mm.getCVM
			)»
			«ENDFOR»
			'''
	}
}