package eu.quanticol.carma.core.generator

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.BlockSpawn
import eu.quanticol.carma.core.carma.ComponentBlockNewDeclarationSpawn
import eu.quanticol.carma.core.carma.EnvironmentMeasure
import eu.quanticol.carma.core.carma.LineSpawn
import eu.quanticol.carma.core.carma.Measure
import eu.quanticol.carma.core.carma.MeasureBlock
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.NCA
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnDeclare
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnMethod
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnPrimitive
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnReference
import eu.quanticol.carma.core.carma.Range
import eu.quanticol.carma.core.carma.RecordDeclaration
import eu.quanticol.carma.core.carma.System
import eu.quanticol.carma.core.carma.VariableDeclaration
import eu.quanticol.carma.core.generator.actions.ActionManager
import eu.quanticol.carma.core.generator.carmaVariable.CarmaVariableManager
import eu.quanticol.carma.core.generator.components.ComponentDefine
import eu.quanticol.carma.core.generator.components.ComponentManager
import eu.quanticol.carma.core.generator.components.ComponentNew
import eu.quanticol.carma.core.generator.components.DeclareGlobalStore
import eu.quanticol.carma.core.generator.main.Main
import eu.quanticol.carma.core.generator.measures.MeasureManager
import eu.quanticol.carma.core.generator.predicates.Predicates
import eu.quanticol.carma.core.typing.TypeProvider
import eu.quanticol.carma.core.utils.LabelUtil
import java.util.ArrayList

import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.core.generator.actions.Actions

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