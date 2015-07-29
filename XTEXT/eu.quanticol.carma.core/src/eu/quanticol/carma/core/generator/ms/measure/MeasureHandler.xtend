package eu.quanticol.carma.core.generator.ms.measure

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.AComponentAState
import eu.quanticol.carma.core.carma.AComponentAllStates
import eu.quanticol.carma.core.carma.AllComponents
import eu.quanticol.carma.core.carma.ComponentComprehension
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.ParallelComponentComprehension
import eu.quanticol.carma.core.carma.SetComp
import java.util.ArrayList
import java.util.HashMap
import java.util.HashSet

import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.core.carma.ProcessComposition
import eu.quanticol.carma.core.utils.Util
import eu.quanticol.carma.core.carma.MeasureDefinition

class MeasureHandler {
	
		
//	def String getMeasures(Iterable<MeasureDefinition> measures){
//		'''
//		«FOR m: measures»
//		«setcomp.measureFunction»
//		«setcomp.measurePredicatePredicate»
//		«setcomp.measureVariablePredicate»
//		«ENDFOR»
//		'''
//	}
//	
//	
//	/**
//	 * sender. receiver. global. and 'pure' should become parameters
//	 * my. should be the store of the Component under evaluation IE found in the predicate, and compared against the input arguments
//	 */
//	def String getMeasureFunction(SetComp setcomp){
//		var String name = ""
//		if(setcomp.getContainerOfType(Measure) != null){
//			name = setcomp.getContainerOfType(Measure).name.name
//		}
//		'''
//		public static Measure<CarmaSystem> getMeasure«setcomp.javanise»(final String name, «setcomp.predicate.disarmParameters»){
//			
//			return new Measure<CarmaSystem>(){
//			
//				ComponentPredicate predicate = getMeasure«setcomp.javanise»_predicate_Predicate(«setcomp.predicate.disarmOut»);
//				String myName = setName(name+«setcomp.javanise»,«setcomp.predicate.disarmOut»);
//				
//				public String setName(String name, «setcomp.predicate.disarmParameters»){
//					return ""+name+" : "+«setcomp.predicate.disarmString»;
//				}
//			
//				@Override
//				public double measure(CarmaSystem t){
//					return t.measure(predicate);
//			
//				};
//			
//				@Override
//				public String getName() {
//					return myName;
//				}
//			};
//		}
//		'''
//	}
//	
//	def String getMeasurePredicatePredicate(SetComp setcomp){
//		'''
//		protected static CarmaPredicate getPredicate«setcomp.javanise»(«setcomp.predicate.disarmParameters») {
//			return new CarmaPredicate() {
//				@Override
//				public boolean satisfy(CarmaStore store) {
//					«setcomp.getMeasureSatisfyBlock»
//				}
//			};
//		}
//		
//		public static ComponentPredicate getMeasure«setcomp.javanise»_predicate_Predicate(«setcomp.predicate.disarmParameters»){
//			return new ComponentPredicate() {
//				
//				@Override
//				public boolean eval(CarmaComponent c){
//					return getPredicate«setcomp.javanise»(«setcomp.predicate.disarmOut»).satisfy(c.getStore()) && (c.isRunning(getMeasure«setcomp.javanise»_Variable_Predicate()));
//				}
//			};
//		}
//		'''
//	}
//	
//	def String getMeasureSatisfyBlock(SetComp setcomp){
//		var vrs =  new ArrayList<VariableReference>(setcomp.predicate.eAllOfType(VariableReference))
//		vrs = vrs.reverseClean
//		var vrsh = new HashMap<String,VariableReference>()
//		for(vr : vrs){
//			vrsh.put(vr.name.name,vr)
//		}
//		'''
//		HashMap<String,Class> my_variables = new HashMap<String,Class>();
//		«FOR key : vrsh.keySet»
//		«vrsh.get(key).checkStorePredicate»
//		«ENDFOR»
//		boolean hasAttributes = true;
//		if(my_variables != null)
//			for(String key : my_variables.keySet()){
//				hasAttributes = store.has(key,my_variables.get(key)) && hasAttributes;
//			}
//		if(hasAttributes){
//			«FOR key : vrsh.keySet»
//			«vrsh.get(key).storePredicate»
//			«ENDFOR»
//			return «setcomp.predicate.express»;
//		} else {
//			return false;
//		}
//		'''
//	}
//	
//	def String checkStorePredicate(VariableReference vr){
//		switch (vr) {
//			VariableReferenceMy: 		'''my_variables.put("«vr.name.name»",«vr.name.type.storeExpress»);'''
//			RecordReferenceMy: 			'''my_variables.put("«vr.name.name»",«vr.name.type.storeExpress»);'''
//		}
//	}
//	
//	def String getStorePredicate(VariableReference vr){
//		switch (vr) {
//			VariableReferenceMy: 		'''«vr.name.type.express» «vr.name.name» = store.get("«vr.name.name»",«vr.name.type.storeExpress»);'''
//			RecordReferenceMy: 			'''«vr.name.type.express» «vr.name.name» = store.get("«vr.name.name»",«vr.name.type.storeExpress»);'''
//		}
//	}
//	
//	def String getMeasureVariablePredicate(SetComp setcomp){
//		'''
//		public static CarmaProcessPredicate getMeasure«setcomp.javanise»_Variable_Predicate(){
//			return new CarmaProcessPredicate() {
//				
//				@Override
//				public boolean eval(CarmaProcess p) {
//					return «setcomp.variable.evaluateExpression»
//				}
//			};
//		}
//		'''
//	}
//	
//	def String evaluateExpression(ComponentComprehension ems){
//		var componentState = new HashMap<String,ArrayList<String>>()
//		ems.getComponentState(componentState)
//		var output = ""
//		if(componentState.keySet.size > 0){
//			output = output + '''
//			( 
//			(((CarmaSequentialProcess) p).automaton().getName().equals(«componentState.keySet.get(0)».getName())) && (
//			'''
//			for(state : componentState.get(componentState.keySet.get(0))){
//				output = output + '''
//				(((CarmaSequentialProcess) p).automaton().getState("«state»") != null ) ||
//				'''
//			}
//			output = output + '''
//			(((CarmaSequentialProcess) p).getState() !=  null))
//			)
//			'''
//			for(var int i = 1; i < componentState.keySet.size; i++){
//				output = output + '''
//				||( 
//				(((CarmaSequentialProcess) p).automaton().getName().equals(«componentState.keySet.get(i)».getName())) && (
//				'''
//				for(state : componentState.get(componentState.keySet.get(i))){
//					output = output + '''
//					(((CarmaSequentialProcess) p).automaton().getState("«state»") != null ) ||
//					'''
//				}
//				
//				output = output + '''
//				(((CarmaSequentialProcess) p).getState() !=  null))
//				)
//				'''
//			}
//		}
//		return output + ";"
//	}
//	
//	def void getComponentState(ComponentComprehension comp, HashMap<String,ArrayList<String>> componentState){
//		
//		switch(comp){
//			ParallelComponentComprehension: {comp.left.getComponentState(componentState)  comp.right.getComponentState(componentState)}  
//			AllComponents:					comp.allComponents(componentState)
//			AComponentAllStates:			comp.comp.getContainerOfType(ComponentBlockDefinition).aComponentAllStates(componentState)
//			AComponentAState:				comp.comp.getContainerOfType(ComponentBlockDefinition).aComponentAState(componentState,new ArrayList<ProcessComposition>(comp.state.eAllOfType(ProcessComposition)))
//		}
//	}
//	
//	def void allComponents(ComponentComprehension comp, HashMap<String,ArrayList<String>> componentState){
//		var components = comp.getContainerOfType(Model).eAllOfType(ComponentBlockDefinition)
//		for(component : components){
//			componentState.put('''create«component.componentSignature.name.name.toFirstUpper»Process()''',component.allStates)
//		}
//	}
//	
//	def void aComponentAllStates(ComponentBlockDefinition component, HashMap<String,ArrayList<String>> componentState){
//		componentState.put('''create«component.componentSignature.name.name.toFirstUpper»Process()''',component.allStates)
//	}
//	
//	def void aComponentAState(ComponentBlockDefinition component, HashMap<String,ArrayList<String>> componentState, ArrayList<ProcessComposition> states){
//		var allStates = allStates(component)
//		var ArrayList<String> output = new ArrayList<String>()
//		for(state : states){
//			for(s : allStates){
//				if(s.equals("state_"+ state.javanise)){
//					output.add(s)
//				}			
//			}
//		}
//		componentState.put('''create«component.componentSignature.name.name.toFirstUpper»Process()''',output)
//	}
//	
//	def ArrayList<String> allStates(ComponentBlockDefinition component){
//		var output = new ArrayList<String>()
//		//FIXME!!!
////		var HashSet<String> states = new HashSet<String>()
////		var tree = component.getCBND.getTree
////		tree.getStates(states)
////		for(state : states)
////			output.add(state)
//		return output
//	}
//	
//	def CBND getCBND(ComponentBlockDefinition component){
//		var CBND toReturn = null
//		for(cbnd : component.getContainerOfType(Model).eAllOfType(CBND)){
//			if(cbnd.name.sameName(component.componentSignature.name))
//				toReturn = cbnd			
//		}
//		return toReturn
//	}
	
}