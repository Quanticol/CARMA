package eu.quanticol.carma.core.generator

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.generator.actions.ActionManager
import eu.quanticol.carma.core.generator.actions.Actions
import eu.quanticol.carma.core.generator.carmaVariable.CarmaVariableManager
import eu.quanticol.carma.core.generator.components.ComponentManager
import eu.quanticol.carma.core.generator.measures.MeasureManager
import eu.quanticol.carma.core.generator.predicates.Predicates
import eu.quanticol.carma.core.typing.TypeProvider
import eu.quanticol.carma.core.utils.LabelUtil
import eu.quanticol.carma.core.utils.Util

class GenerateDefinitions {
	
		@Inject extension LabelUtil
		@Inject extension Predicates
		@Inject extension Actions
	
		def String compileDefinitions(Model model, 
			String packageName, 
			CarmaVariableManager variableManager, 
			ActionManager actionManager,
			ComponentManager componentManager,
			MeasureManager measureManager
		){
		'''
		«packageName»;
		
		import org.apache.commons.math3.random.RandomGenerator;
		import org.cmg.ml.sam.sim.SimulationEnvironment;
		import org.cmg.ml.sam.sim.sampling.*;
		import eu.quanticol.carma.simulator.*;
		
		public class «model.label»Definition {
			
			«variableManager.declareAllAttributesAndTypes»
			«actionManager.declareAllActionsAndRates»
			«componentManager.declareAllCarmaProcessAutomatons»
			«componentManager.defineProcessDefinitions»
			«measureManager.defineMeasures»
		}
		'''
	}
	
	def String defineProcessDefinitions(ComponentManager componentManager){
		'''
		«FOR component_name : componentManager.getComponentGenerators().keySet»
			
			private static CarmaProcessAutomaton create«component_name»Process() {
				
				CarmaProcessAutomaton toReturn = new CarmaProcessAutomaton("«component_name»");
				
				«FOR statement : componentManager.getComponentGenerators().get(component_name).declareStates»
				«statement»
				«ENDFOR»
				«var actionExpression = componentManager.getComponentGenerators().get(component_name).declareActions»
				«FOR key : actionExpression.keySet»
				«key.getAction(actionExpression.get(key),componentManager.getCVM, componentManager.getAM)»
				«ENDFOR»
				«var guardExpressions = componentManager.getComponentGenerators().get(component_name).declareGuards»
				«FOR key : guardExpressions.keySet»
				«key.getGuardPredicate(guardExpressions.get(key),componentManager.getCVM)»
				«ENDFOR»
				«FOR transition : componentManager.getComponentGenerators().get(component_name).declareTransitions»
				«transition»
				«ENDFOR»
				return toReturn;
			}
		«ENDFOR»
		'''
	}

	def String defineMeasures(MeasureManager mm){
		'''
		«FOR key : mm.getMeasures.keySet»
		«getMeasureStatePredicate(key,mm.measures.get(key).getEME)»
		«getMeasureBooleanExpressionPredicate(key, 
			mm.measures.get(key).getBES, 
			mm.measures.get(key).getInArgs, 
			mm.measures.get(key).getOutArgs, 
			mm.getCVM
		)»
		«defineGetMeasureMethod(key,
			mm.measures.get(key).getBES, 
			mm.measures.get(key).getInArgs, 
			mm.measures.get(key).getOutArgs, 
			mm.getCVM
		)»
		«ENDFOR»
		«FOR key : mm.getEnvMeasures.keySet»
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