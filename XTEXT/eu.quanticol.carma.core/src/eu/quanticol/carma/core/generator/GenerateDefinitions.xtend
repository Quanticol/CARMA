package eu.quanticol.carma.core.generator

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.Action
import eu.quanticol.carma.core.carma.BooleanExpression
import eu.quanticol.carma.core.carma.Component
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressionAll
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressionComponentAState
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressionComponentAllStates
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressionParallel
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressions
import eu.quanticol.carma.core.carma.EnvironmentMeasure
import eu.quanticol.carma.core.carma.Guard
import eu.quanticol.carma.core.carma.InputActionArguments
import eu.quanticol.carma.core.carma.MacroExpressionReference
import eu.quanticol.carma.core.carma.Measure
import eu.quanticol.carma.core.carma.MeasureBlock
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.OutputActionArgument
import eu.quanticol.carma.core.carma.OutputActionArgumentVR
import eu.quanticol.carma.core.carma.Range
import eu.quanticol.carma.core.carma.UpdateAssignment
import eu.quanticol.carma.core.carma.VariableDeclaration
import eu.quanticol.carma.core.carma.VariableName
import eu.quanticol.carma.core.carma.VariableReference
import eu.quanticol.carma.core.generator.actions.ActionManager
import eu.quanticol.carma.core.generator.carmaVariable.CarmaVariableManager
import eu.quanticol.carma.core.generator.components.ComponentManager
import eu.quanticol.carma.core.typing.TypeProvider
import eu.quanticol.carma.core.utils.LabelUtil
import eu.quanticol.carma.core.utils.Tree
import eu.quanticol.carma.core.utils.Util
import java.util.ArrayList
import java.util.HashMap
import java.util.HashSet

import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.core.generator.predicates.Predicates

class GenerateDefinitions {
	
		@Inject extension TypeProvider
		@Inject extension LabelUtil
		@Inject extension Util
		@Inject extension GeneratorUtils
		@Inject extension Predicates
	
		def String compileDefinitions(Model model, 
			String packageName, 
			CarmaVariableManager variableManager, 
			ActionManager actionManager,
			ComponentManager componentManager
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
			«model.defineMeasures»
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
				«key.getAction(actionExpression(key),componentManager.getCVM)»
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
	
	//inputStore - the store of the Component doing the input Action
	//outputStore - the store of the Component doing the output Action
	//value is the values from that Component
	def String outputActionPredicate(Action action){
		var hasPredicate = action.eAllOfType(BooleanExpression).size > 0
		if(hasPredicate){
			var be = action.eAllOfType(BooleanExpression).get(0)
			if(be.label.equals("True") || be.label.equals("true")){
				return 
				'''
				@Override
				protected CarmaPredicate getPredicate(CarmaStore outputStore) {
					return CarmaPredicate.TRUE;
				}
				'''
			} else if(be.label.equals("False") || be.label.equals("false")){
				return 
				'''
				@Override
				protected CarmaPredicate getPredicate(CarmaStore outputStore) {
					return CarmaPredicate.FALSE;
				}
				'''
			} else {
				return
				'''
				@Override
				protected CarmaPredicate getPredicate(CarmaStore outputStore) {
					return new CarmaPredicate() {
						@Override
						public boolean satisfy(CarmaStore inputStore) {
							boolean hasAttributes = true;
							«be.getAllVariablesOutputAction»
							if(hasAttributes)
								return «be.convertToJavaOutputAction»;
							else
								return false;
						}
					};
				}
				'''
			}
		} else {
			return
			'''
			@Override
			protected CarmaPredicate getPredicate(CarmaStore outputStore) {
				return CarmaPredicate.TRUE;
			}
			'''
		}
	}


	def String outputAction(Action action, String name){
		'''
		CarmaOutput «name» = new CarmaOutput( «action.name.label.toUpperCase», «(action.type.toString.equals("broad") || action.type.toString.equals("spont"))» ) {
			
			«action.outputActionPredicate»

			«defineOutputUpdates(new ArrayList<UpdateAssignment>(action.eAllOfType(UpdateAssignment)))»

			«action.defineValues»
		};
		'''
	}
	
	def String defineOutputUpdates(ArrayList<UpdateAssignment> updates){
		var output = ""
		if(updates.size >0){
			for(update : updates){
				var vd = update.storeReference.variableDeclaration
				output = output + vd.getStore
			}
			
			for(update : updates){
				var vd = update.storeReference.variableDeclaration
				output = output + vd.setStore(update.expression)
			}
		}
		'''
		@Override
		protected CarmaStoreUpdate getUpdate() {
			return new CarmaStoreUpdate() {
				
				@Override
				public void update(RandomGenerator r, CarmaStore store) {
					boolean hasAttributes = true;
					«output»
				}
			};
		}
		'''
	}
	
	def String defineValues(Action action){
		var outputArgs = action.eAllOfType(OutputActionArgument);
		if(outputArgs.size == 0){
			return 
			'''
			@Override
			protected Object getValue(CarmaStore store) {
			return new Object();
			}
			'''
		}
		var ArrayList<OutputActionArgumentVR> vrs = new ArrayList<OutputActionArgumentVR>()
		
		for(arg : outputArgs)
			vrs.addAll(arg.eAllOfType(OutputActionArgumentVR))
			
		var output = '''int[] output = new int[«outputArgs.size»];'''
		
		for(vr : vrs){
			vr.ref.declareVariable
		}
		
		for(var i = 0; i < outputArgs.size; i++){
			output = output + "\n" + '''output[«i»] = «outputArgs.get(0).label»;'''
		}
		
		output = output + "\n" + "return output;"
		
		'''
		@Override
		protected Object getValue(CarmaStore store) {
			«output»
		}
		'''
	}
	
	//inputStore - the store of the Component doing the input Action
	//outputStore - the store of the Component doing the output Action
	//value is the values from that Component
	def String inputActionPredicate(Action action){
		var hasPredicate = action.eAllOfType(BooleanExpression).size > 0
		if(hasPredicate){
			var be = action.eAllOfType(BooleanExpression).get(0)
			if(be.label.equals("True") || be.label.equals("true")){
				return 
				'''
				@Override
				protected CarmaPredicate getPredicate(CarmaStore outputStore, final Object value) {
					return CarmaPredicate.TRUE;
				}
				'''
			} else if(be.label.equals("False") || be.label.equals("false")){
				return 
				'''
				@Override
				protected CarmaPredicate getPredicate(CarmaStore outputStore, final Object value) {
					return CarmaPredicate.FALSE;
				}
				'''
			} else {
				return
				'''
				@Override
				protected CarmaPredicate getPredicate(CarmaStore outputStore, final Object value) {
					if (value instanceof int[]){
						return new CarmaPredicate() {
							@Override
							public boolean satisfy(CarmaStore inputStore) {
								boolean hasAttributes = true;
								«getAndSetInputArgumentsBool(new ArrayList<VariableName>(action.eAllOfType(InputActionArguments).get(0).eAllOfType(VariableName)))»
								«be.getAllVariablesInputAction»
								if(hasAttributes)
									return «be.convertToJavaInputAction»;
								else
									return false;
							}
						};
					}
					return null;
				}
				'''
			}
		} else {
			return
			'''
			@Override
			protected CarmaPredicate getPredicate(CarmaStore outputStore, final Object value) {
				return CarmaPredicate.TRUE;
			}
			'''
		}
	}
	
	def String getAndSetInputArgumentsBool(ArrayList<VariableName> vns){
		'''
		«FOR vn : vns»
		int «vn.label»_i = ((int[]) value)[«vns.indexOf(vn)»];
		«ENDFOR»
		'''		
	}
	
	def String inputUpdate(Action action){
		'''
		@Override
		protected CarmaStoreUpdate getUpdate(final Object value) {
			
			return new CarmaStoreUpdate() {
				@Override
				public void update(RandomGenerator r, CarmaStore store) {
					if (value instanceof int[]){
						boolean hasAttributes = true;
						«getAndSetInputArguments(new ArrayList<VariableName>(action.eAllOfType(InputActionArguments).get(0).eAllOfType(VariableName)))»
						«defineInputUpdates(new ArrayList<UpdateAssignment>(action.eAllOfType(UpdateAssignment)))»
					};
				};
			
			};
		};
		'''
	}
	
	def String getAndSetInputArguments(ArrayList<VariableName> vns){
		'''
		«FOR vn : vns»
		int «vn.label» = ((int[]) value)[«vns.indexOf(vn)»];
		«ENDFOR»
		'''		
	}
	
	def String defineInputUpdates(ArrayList<UpdateAssignment> updates){
		var output = ""
		if(updates.size >0){
			for(update : updates){
				var vd = update.storeReference.variableDeclaration
				output = output + vd.getStore
			}
			
			for(update : updates){
				var vd = update.storeReference.variableDeclaration
				output = output + vd.setStore(update.expression)
			}
		}
		return output
	}
	
	def String inputAction(Action action, String name){
		'''
		CarmaInput «name» = new CarmaInput( «action.name.label.toUpperCase», «(action.type.toString.equals("broad") || action.type.toString.equals("spont"))» ) {
			
			«action.inputActionPredicate»
			
			«action.inputUpdate»
		};
		'''
	}
	

	
	def String defineMeasures(Model model){
		var measuresBlock = model.eAllOfType(MeasureBlock)
		var measures = new ArrayList<Measure>()
		for(m : measuresBlock){
			measures.addAll(m.eAllOfType(Measure))
		}
		'''
		«FOR m : measures»
		«var measureName = m.name.getLabel.toFirstUpper»
		«var stateName = (m.measure as EnvironmentMeasure).componentReference.getLabel.toFirstUpper»
		//predicate states get_MeasureName_State(ProcessName_ProcessName... || All)Predicate()
		«m.defineGetBooleanExpressionStateMeasure(measureName,stateName)»
		//predicate for boolean expression get_MeasureName_BooleanExpression_Predicate()
		«m.defineGetBooleanExpressionPredicateMeasure(measureName,stateName)»
		//getMethod
		«m.defineGetMeasureMethod(measureName,stateName)»
		«ENDFOR»
		'''
	}
	
	def String defineGetBooleanExpressionStateMeasure(Measure measure, String measureName, String stateName){
		
		var ems = ((measure.measure as EnvironmentMeasure).componentReference as EnvironmentMacroExpressions)
		
		
		'''
		public static CarmaProcessPredicate getMeasure«measureName»_«stateName»_State_Predicate(){
			return new CarmaProcessPredicate() {
				
				@Override
				public boolean eval(CarmaProcess p) {
					return «ems.booleanExpressionFromEMS»
				}
			};
		}
		'''
	}
	
	def String getBooleanExpressionFromEMS(EnvironmentMacroExpressions ems){
		var componentState = new HashMap<String,ArrayList<String>>()
		ems.emsExpression(componentState)
		var output = ""
		if(componentState.keySet.size > 0){
			output = output + '''
			( 
			(((CarmaSequentialProcess) p).automaton() ==  «componentState.keySet.get(0)») && (
			'''
			for(state : componentState.get(componentState.keySet.get(0))){
				output = output + '''
				(((CarmaSequentialProcess) p).automaton().getState("«state»") != null ) ||
				'''
			}
			output = output + '''
			(((CarmaSequentialProcess) p).getState() !=  null))
			)
			'''
			for(var int i = 1; i < componentState.keySet.size; i++){
				output = output + '''
				||( 
				(((CarmaSequentialProcess) p).automaton() ==  «componentState.keySet.get(i)») && (
				'''
				for(state : componentState.get(componentState.keySet.get(i))){
					output = output + '''
					(((CarmaSequentialProcess) p).automaton().getState("«state»") != null ) ||
					'''
				}
				
				output = output + '''
				(((CarmaSequentialProcess) p).getState() !=  null))
				)
				'''
			}
		}
		return output + ";"
	}
	
	def void emsExpression(EnvironmentMacroExpressions ems, HashMap<String,ArrayList<String>> componentState){
		
		switch(ems){
			EnvironmentMacroExpressionParallel: 			{ems.left.emsExpression(componentState)  ems.right.emsExpression(componentState)}  
			EnvironmentMacroExpressionAll:					ems.allComponents(componentState)
			EnvironmentMacroExpressionComponentAllStates:	ems.comp.getContainerOfType(Component).aComponentAllStates(componentState)
			EnvironmentMacroExpressionComponentAState:		ems.comp.getContainerOfType(Component).aComponentAState(componentState,new ArrayList<MacroExpressionReference>(ems.state.eAllOfType(MacroExpressionReference)))
		}
	}
	
	def void allComponents(EnvironmentMacroExpressions ems, HashMap<String,ArrayList<String>> componentState){
		var components = ems.getContainerOfType(Model).eAllOfType(Component)
		for(component : components){
			componentState.put('''«component.label»Process''',component.allStates)
		}
	}
	
	def void aComponentAllStates(Component component, HashMap<String,ArrayList<String>> componentState){
		componentState.put('''«component.label»Process''',component.allStates)
	}
	
	def void aComponentAState(Component component, HashMap<String,ArrayList<String>> componentState, ArrayList<MacroExpressionReference> states){
		var allStates = allStates(component)
		var ArrayList<String> output = new ArrayList<String>()
		for(state : states){
			for(s : allStates){
				if(s.equals("state_"+state.label)){
					output.add(s)
				}			
			}
		}
		componentState.put('''«component.label»Process''',output)
	}
	
	def ArrayList<String> allStates(Component component){
		var output = new ArrayList<String>()
		var HashSet<String> states = new HashSet<String>()
		var tree = component.getTree
		tree.getStates(states)
		for(state : states)
			output.add(state)
		return output
	}
	
	
	def String defineGetBooleanExpressionPredicateMeasure(Measure measure, String measureName, String stateName){
		var exp = (measure.measure as EnvironmentMeasure).booleanExpression
		var parameters = measure.parameters
		var argsi = new ArrayList<String>()
		var args = new ArrayList<String>()
		for(vd : parameters.eAllOfType(VariableDeclaration)){
			if(vd.eAllOfType(Range).size > 0){
				args.add('''final «vd.convertToJava.split(" ").get(0) + " " + vd.convertToJava.split(" ").get(1).substring(0,vd.convertToJava.split(" ").get(1).length - 3)»''')
				argsi.add('''«vd.convertToJava.split(" ").get(1).substring(0,vd.convertToJava.split(" ").get(1).length - 3)»''')
			} else {
				args.add('''final «vd.convertToJava.split(" ").get(0) + " " + vd.convertToJava.split(" ").get(1)»''')
				argsi.add('''«vd.convertToJava.split(" ").get(1)»''')
			}
			
		}
		'''
		protected static CarmaPredicate getPredicate«measureName»_«stateName»(«args.generateArgs») {
			return new CarmaPredicate() {
				@Override
				public boolean satisfy(CarmaStore store) {
					boolean hasAttributes = true;
					«exp.getAllVariablesMeasure»
					if(hasAttributes)
						return «exp.convertToJava»;
					else
						return false;
				}
			};
		}
		
		
		public static ComponentPredicate getMeasure«measureName»_«stateName»_BooleanExpression_Predicate(«args.generateArgs»){
			return new ComponentPredicate() {
				
				@Override
				public boolean eval(CarmaComponent c){
					return getPredicate«measureName»_«stateName»(«argsi.generateArgs»).satisfy(c.getStore()) && (c.isRunning(getMeasure«measureName»_«stateName»_State_Predicate()));
				}
			};
		}
		'''
	}
	
	def String generateArgs(ArrayList<String> args){
		var output = ""
		if(args.size > 0){
			output = args.get(0)
			for(var i = 1; i < args.size; i++){
				output = output +", "+ args.get(i)
			}
		}
		
		output
	}
	
	def String defineGetMeasureMethod(Measure measure, String measureName, String stateName){
		
		var exp = measure.parameters
		var argsi = new ArrayList<String>()
		var args = new ArrayList<String>()
		for(vd : exp.eAllOfType(VariableDeclaration)){
			args.add('''final «vd.convertToJava.split(" ").get(0) + " " + vd.convertToJava.split(" ").get(1)»''')
			argsi.add('''«vd.convertToJava.split(" ").get(1)»''')
		}
		'''
		public static Measure<CarmaSystem> getMeasure«measureName»_«stateName»(«args.generateArgs»){
			
			return new Measure<CarmaSystem>(){
			
				ComponentPredicate predicate = getMeasure«measureName»_«stateName»_BooleanExpression_Predicate(«argsi.generateArgs»);
			
				@Override
				public double measure(CarmaSystem t){
					//TODO
				
					return t.measure(predicate);
			
				};
			
				@Override
				public String getName() {
					return "«measureName»_«stateName»";
				}
			};
		}
		'''
	}
	

	
}