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
import eu.quanticol.carma.core.carma.ComponentBlockDefinition
import eu.quanticol.carma.core.carma.ProcessComposition
import eu.quanticol.carma.core.utils.Util
import eu.quanticol.carma.core.carma.CBND
import eu.quanticol.carma.core.carma.VariableReference
import eu.quanticol.carma.core.carma.VariableReferenceMy
import eu.quanticol.carma.core.carma.RecordReferenceMy
import eu.quanticol.carma.core.typing.TypeProvider
import eu.quanticol.carma.core.carma.Measure
import eu.quanticol.carma.core.generator.ms.SharedJavaniser
import eu.quanticol.carma.core.carma.ParallelComposition
import eu.quanticol.carma.core.carma.ProcessReference

class MeasureHandler {
	
	@Inject extension Util
	@Inject extension TypeProvider
	@Inject extension SharedJavaniser
		
	def String getMeasures(ArrayList<SetComp> setcomps){
		'''
		«FOR setcomp : setcomps»
		«setcomp.measureFunction»
		«setcomp.measurePredicatePredicate»
		«setcomp.measureVariablePredicate»
		«ENDFOR»
		'''
	}
	
	
	/**
	 * sender. receiver. global. and 'pure' should become parameters
	 * my. should be the store of the Component under evaluation IE found in the predicate, and compared against the input arguments
	 */
	def String getMeasureFunction(SetComp setcomp){
		var String name = ""
		if(setcomp.getContainerOfType(Measure) != null){
			name = setcomp.getContainerOfType(Measure).name.name
		}
		'''
		public Measure<CarmaSystem> getMeasure«setcomp.javanise»(final String name, «setcomp.predicate.disarmParameters»){
			
			return new Measure<CarmaSystem>(){
			
				ComponentPredicate predicate = getMeasure«setcomp.javanise»_predicate_Predicate(«setcomp.predicate.disarmOut»);
				String myName = setName(name+«setcomp.javanise»,«setcomp.predicate.disarmOut»);
				
				public String setName(String name, «setcomp.predicate.disarmParameters»){
					return ""+name+" : "+«setcomp.predicate.disarmString»;
				}
			
				@Override
				public double measure(CarmaSystem t){
					return t.measure(predicate);
			
				};
			
				@Override
				public String getName() {
					return myName;
				}
			};
		}
		'''
	}
	
	def String getMeasurePredicatePredicate(SetComp setcomp){
		'''
		protected CarmaPredicate getPredicate«setcomp.javanise»(«setcomp.predicate.disarmParameters») {
			return new CarmaPredicate() {
				@Override
				public boolean satisfy(CarmaStore store) {
					«setcomp.getMeasureSatisfyBlock»
				}
			};
		}
		
		public ComponentPredicate getMeasure«setcomp.javanise»_predicate_Predicate(«setcomp.predicate.disarmParameters»){
			return new ComponentPredicate() {
				
				@Override
				public boolean eval(CarmaComponent c){
					return getPredicate«setcomp.javanise»(«setcomp.predicate.disarmOut»).satisfy(c.getStore()) && (c.isRunning(getMeasure«setcomp.javanise»_Variable_Predicate()));
				}
			};
		}
		'''
	}
	
	def String getMeasureSatisfyBlock(SetComp setcomp){
		var vrs =  new ArrayList<VariableReference>(setcomp.predicate.eAllOfType(VariableReference))
		vrs = vrs.reverseClean
		var vrsh = new HashMap<String,VariableReference>()
		for(vr : vrs){
			vrsh.put(vr.name.name,vr)
		}
		'''
		try{
			«FOR key : vrsh.keySet»
			«vrsh.get(key).storePredicate»
			«ENDFOR»
			return «setcomp.predicate.javanise»;
		} catch (NullPointerException exception) {
			return false;
		}
		'''
	}
	
	def String checkStorePredicate(VariableReference vr){
		switch (vr) {
			VariableReferenceMy: 		'''my_variables.put("«vr.name.name»",«vr.name.type.classJavanise»);'''
			RecordReferenceMy: 			'''my_variables.put("«vr.name.name»",«vr.name.type.classJavanise»);'''
		}
	}
	
	def String getStorePredicate(VariableReference vr){
		switch (vr) {
			VariableReferenceMy: 		'''«vr.name.type.javanise» my_«vr.name.name» = store.get("«vr.name.name»",«vr.name.type.classJavanise»);'''
			RecordReferenceMy: 			'''«vr.name.type.javanise» my_«vr.name.name» = store.get("«vr.name.name»",«vr.name.type.classJavanise»);'''
		}
	}
	
	def String getMeasureVariablePredicate(SetComp setcomp){
		'''
		public CarmaProcessPredicate getMeasure«setcomp.javanise»_Variable_Predicate(){
			return new CarmaProcessPredicate() {
				
				@Override
				public boolean eval(CarmaProcess p) {
					return «setcomp.variable.expressComponentComprehension»
				}
			};
		}
		'''
	}
	
	def String expressComponentComprehension(ComponentComprehension cc){
		//sanity
		if(cc == null){
			return '''false;'''
		} 
		
		return '''«cc.componentState»;'''
	}

	def String getComponentState(ComponentComprehension comp){
		switch(comp){
			ParallelComponentComprehension: '''(«comp.left.getComponentState» || «comp.right.getComponentState»)''' 
			AllComponents:					'''true'''
			AComponentAllStates:			'''(«comp.aComponentAllStates»)'''
			AComponentAState:				'''(«comp.aComponentAState»)'''
		}
	}

	def String aComponentAllStates(AComponentAllStates comp){
		var name = comp.comp.getContainerOfType(ComponentBlockDefinition).componentSignature.name.name
		var component = comp.comp.getContainerOfType(ComponentBlockDefinition)
		'''((CarmaSequentialProcess) p).automaton().getName().equals(create«name»Process().getName())
		&& («FOR state : component.allStates SEPARATOR '||'» «state» «ENDFOR»)'''
	}
	
	def ArrayList<String> allStates(ComponentBlockDefinition component){
		var output = new ArrayList<String>()
		var HashSet<String> states = new HashSet<String>()
		var tree = component.getCBND.getTree
		tree.getStates(states)
		for(state : states)
			if(state.equals("null")){
				output.add('''(((CarmaSequentialProcess) p).getState() ==  null)''')
			} else {
				output.add('''((((CarmaSequentialProcess) p).getState() !=  null) && 
				((CarmaSequentialProcess) p).getState().getName().equals("«state»"))''')
			}
			
		return output
	}
	
	def String aComponentAState(AComponentAState comp){
		var name = comp.comp.getContainerOfType(ComponentBlockDefinition).componentSignature.name.name
		var processComposition = comp.state
		'''((CarmaSequentialProcess) p).automaton().getName().equals(create«name»Process().getName()) && 
		«processComposition.processCompositionState»'''
	}
	
	def String getProcessCompositionState(ProcessComposition process){
		switch(process){
			ParallelComposition: '''(«process.left.getProcessCompositionState» || «process.right.getProcessCompositionState»)'''  
			ProcessReference:	 '''(((CarmaSequentialProcess) p).getState() !=  null) && 
			(((CarmaSequentialProcess) p).getState().getName().equals("state_«(process as ProcessReference).expression.name»"))'''
		}
	}
	
	def CBND getCBND(ComponentBlockDefinition component){
		var CBND toReturn = null
		for(cbnd : component.getContainerOfType(Model).eAllOfType(CBND)){
			if(cbnd.name.sameName(component.componentSignature.name))
				toReturn = cbnd			
		}
		return toReturn
	}
	
}