package eu.quanticol.carma.core.generator

import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.System
import com.google.inject.Inject
import eu.quanticol.carma.core.typing.TypeProvider
import eu.quanticol.carma.core.utils.LabelUtil
import eu.quanticol.carma.core.utils.Util
import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.core.carma.Component
import java.util.HashMap
import eu.quanticol.carma.core.carma.VariableDeclaration
import eu.quanticol.carma.core.carma.ActionName
import eu.quanticol.carma.core.carma.ActionStub
import eu.quanticol.carma.core.carma.Rate
import eu.quanticol.carma.core.carma.Action
import eu.quanticol.carma.core.carma.UpdateAssignment
import java.util.HashSet
import java.util.ArrayList
import eu.quanticol.carma.core.utils.Tree
import eu.quanticol.carma.core.carma.Guard
import eu.quanticol.carma.core.carma.OutputActionArgument
import eu.quanticol.carma.core.carma.OutputActionArgumentVR
import eu.quanticol.carma.core.carma.VariableDeclarationEnum
import eu.quanticol.carma.core.carma.VariableDeclarationRecord
import eu.quanticol.carma.core.carma.RecordDeclaration
import eu.quanticol.carma.core.carma.VariableReference
import eu.quanticol.carma.core.carma.InputActionArguments
import eu.quanticol.carma.core.carma.VariableName

class GenerateDefinitions {
	
		@Inject extension TypeProvider
		@Inject extension LabelUtil
		@Inject extension Util
	
		def String compileDefinitions(Model model, String packageName, String className){
		'''
		«packageName»;
		
		import org.apache.commons.math3.random.RandomGenerator;
		import org.cmg.ml.sam.carma.*;
		import org.cmg.ml.sam.sim.SimulationEnvironment;
		
		public class «className»Definition {
			
			/*METHOD VARIABLES*/
			/*COMPONENT ATTRIBUTES*/
			«model.componentAttributes»
			/*INPUT ARGUMENTS*/
			/*ENVIRONMENT ATTRIBUTES*/
			/*ACTION*/
			«model.actionDefinitions»
			/*RATES*/
			«model.rateDefinitions»
			/*PROCESS*/
			«model.processDefinitions»
			/*MEASURES*/
		}
		'''
	}
	
	def String componentAttributes(Model model){
		
		var cat = model.getComponentAttributeType
		var HashMap<String,VariableDeclaration> vds = new HashMap<String,VariableDeclaration>()
		for(component : cat.keySet){
			for(vd : cat.get(component)){
				switch(vd){
					VariableDeclarationEnum:	vds.put(vd.name.label,vd)
					VariableDeclarationRecord:	{
						var rds = vd.eAllOfType(RecordDeclaration)
						for(rd : rds)
							vds.put(vd.name.label+"_"+rd.name.label,vd)
					}
				}
				
			}
		}
			
		'''
			«FOR vdName : vds.keySet»
				public static final String «vdName.toUpperCase»_ATTRIBUTE = "«vdName»";
				public static final Class<«vds.get(vdName).convertType»> «vdName.toUpperCase»_ATTRIBUTE_TYPE = «vds.get(vdName).convertType».class;
			«ENDFOR»
		'''
	}
	
	def String actionDefinitions(Model model){
		
		//public static final int PRODUCE = 0;
		
		var actions = model.eAllOfType(ActionName)
		var seen = new HashSet<String>()
		var int count = 0;
		
		'''
			«FOR action : actions»
			«IF seen.add(action.label)»
			public static final int «action.label.toUpperCase» = «count++»;
			«ENDIF»
			«ENDFOR»
		'''
		
	}
	
	def String rateDefinitions(Model model){
		
		//public static final double PRODUCE_RATE = 1;
		
		var actionStubs = model.eAllOfType(ActionStub)
		
		'''
			«FOR actionStub : actionStubs»
				«IF actionStub.getContainerOfType(Rate) != null»
					public static final double «actionStub.labelName.toUpperCase»_RATE_«actionStub.labelInOut» = «actionStub.getRates»;
				«ENDIF»
			«ENDFOR»
		'''
		
	}
	
	def String getRates(ActionStub actionStub){
		actionStub.getContainerOfType(Rate).expression.label
	}
	
	def String processDefinitions(Model model){
		
		/*ProcessAutomaton*/
		//create#COMPONENTNAME#Process
		//public static final CarmaProcessAutomaton ProducerProcess = createProducerProcess();
		
		//private static CarmaProcessAutomaton createProducerProcess() {
		
		var components = model.eAllOfType(Component)
		
		'''
		«FOR component : components»
			public static final CarmaProcessAutomaton «component.label»Process = create«component.label»Process();
			
			private static CarmaProcessAutomaton create«component.label»Process() {
				
				CarmaProcessAutomaton toReturn = new CarmaProcessAutomaton("«component.label»");
				
				«var tree = component.getTree»
				
				«tree.declareStates»
				
				«tree.declareActions»
				
				«tree.declareGuards»
				
				«tree.declareTransitions»
				
				return toReturn;
			}
		«ENDFOR»
		'''
	}
	
	def String declareStates(Tree tree){
		var HashSet<String> states = new HashSet<String>()
		tree.getStates(states)
		'''
		//create the states in the automata 
		«FOR state : states»
			CarmaProcessAutomaton.State «state» = toReturn.newState("«state»");
		«ENDFOR»
		'''
	}
	
	def String declareActions(Tree tree){
		var HashMap<String,Action> actions = new HashMap<String,Action>()
		tree.getActions(actions)
		'''
		«FOR key : actions.keySet»
			«IF(actions.get(key).type.set.equals("outputAction"))»
			«actions.get(key).outputAction(key)»
			«ELSE»
			«actions.get(key).inputAction(key)»
			«ENDIF»
		«ENDFOR»
		'''
	}
	
	def String declareGuards(Tree tree){
		var HashMap<String,Guard> guards = new HashMap<String,Guard>()
		tree.getGuards(guards)
		'''
		«FOR key : guards.keySet»
		CarmaPredicate «key» = new CarmaPredicate() {
			@Override
			public boolean satisfy(CarmaStore store) {
				«FOR vr : guards.get(key).eAllOfType(VariableReference)»
				«var vd = vr.getVariableDeclaration»
				«switch(vd){
					VariableDeclarationEnum:	'''«vd.convertPrimitiveType» «vd.name.label» = store.get("«vd.name.label»" , «vd.convertType».class );'''
					VariableDeclarationRecord:	{
						var rds = vd.eAllOfType(RecordDeclaration)
						'''
						«FOR rd : rds»
							«vd.convertPrimitiveType» «vd.name.label»_«rd.name.label» = store.get("«vd.name.label»_«rd.name.label»" , «vd.convertType».class );
						«ENDFOR»
						'''
					}
				}»
				«ENDFOR»
				return «guards.get(key).booleanExpression.labelJava»;
			}
		};
		«ENDFOR»
		'''
	}
	
	def String defineOutputUpdates(ArrayList<UpdateAssignment> updates){
		var output = ""
		if(updates.size >0){
			for(update : updates){
				var vd = update.storeReference.variableDeclaration
				switch(vd){
					VariableDeclarationEnum: {
						output = '''«vd.convertPrimitiveType» «vd.name.label» = store.get("«vd.name.label»" , «vd.convertType».class );'''+"\n"+
						'''store.set("«vd.name.label»",«update.expression.label»);'''+"\n"
					}
					VariableDeclarationRecord: {
							var rds = vd.eAllOfType(RecordDeclaration)
							for(rd : rds){
								output = output + '''«vd.convertPrimitiveType» «vd.name.label»_«rd.name.label» = store.get("«vd.name.label»_«rd.name.label»" , «vd.convertType».class );'''+"\n"+
								'''store.set("«vd.name.label»_«rd.name.label»",«update.expression.label»);'''+"\n"
							}
					}	
				}
				
			}
		}
		return output
	}

	def String outputAction(Action action, String name){
		'''
		CarmaOutput «name» = new CarmaOutput( «action.name.label.toUpperCase», «(action.type.toString.equals("broad") || action.type.toString.equals("spont"))» ) {
			
			@Override
			protected CarmaPredicate getPredicate(CarmaStore store) {
				return CarmaPredicate.FALSE;
			}

			@Override
			protected CarmaStoreUpdate getUpdate() {
				return new CarmaStoreUpdate() {
					
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
						«defineOutputUpdates(new ArrayList<UpdateAssignment>(action.eAllOfType(UpdateAssignment)))»
					
					}
				};
			}

			@Override
			protected Object getValue(CarmaStore store) {
				«action.getValue»
			}
		};
		'''
	}
	
	def String getValue(Action action){
		var outputArgs = action.eAllOfType(OutputActionArgument);
		if(outputArgs.size == 0){
			return '''return new Object();'''
		}
		var ArrayList<OutputActionArgumentVR> vrs = new ArrayList<OutputActionArgumentVR>()
		
		for(arg : outputArgs)
			vrs.addAll(arg.eAllOfType(OutputActionArgumentVR))
			
		var output = '''int[] output = new int[«outputArgs.size»];'''
		
		for(vr : vrs){
			var vd = vr.ref.getVariableDeclaration
			switch(vd){
				VariableDeclarationEnum: {
					output = output + '''«vd.convertPrimitiveType» «vd.name.label» = store.get("«vd.name.label»" , «vd.convertType».class );''' + "\n"
				}
				VariableDeclarationRecord: {
					var rds = vd.eAllOfType(RecordDeclaration)
					for(rd : rds){
						output = output + '''«vd.convertPrimitiveType» «vd.name.label»_«rd.name.label» = store.get("«vd.name.label»_«rd.name.label»" , «vd.convertType».class );''' + "\n"
					}	
				}
			}
		}
		
		for(var i = 0; i < outputArgs.size; i++){
			output = output + "\n" + '''output[«i»] = «outputArgs.get(0).label»;'''
		}
		
		output = output + "\n" + "return output;"
		
		return output
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
				switch(vd){
					VariableDeclarationEnum: {
						output = '''«vd.convertPrimitiveType» «vd.name.label» = store.get("«vd.name.label»" , «vd.convertType».class );'''+"\n"+
						'''store.set("«vd.name.label»",«update.expression.label»);'''
					}
					VariableDeclarationRecord: {
						var rds = vd.eAllOfType(RecordDeclaration)
						for(rd : rds){
							output = output + '''«vd.convertPrimitiveType» «vd.name.label»_«rd.name.label» = store.get("«vd.name.label»_«rd.name.label»" , «vd.convertType».class );'''+"\n"+
							'''store.set("«vd.name.label»_«rd.name.label»",«update.expression.label»);'''
						}
					}
				}
			}
		}
		return output
	}
	
	def String inputAction(Action action, String name){
		'''
		CarmaInput «name» = new CarmaInput( «action.name.label.toUpperCase», «(action.type.toString.equals("broad") || action.type.toString.equals("spont"))» ) {
			
			@Override
			protected CarmaPredicate getPredicate(CarmaStore store, Object value) {
				return CarmaPredicate.TRUE;
			}
			
			@Override
			protected CarmaStoreUpdate getUpdate(final Object value) {
				
				return new CarmaStoreUpdate() {
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
						if (value instanceof int[]){
					
							«getAndSetInputArguments(new ArrayList<VariableName>(action.eAllOfType(InputActionArguments).get(0).eAllOfType(VariableName)))»
							«defineInputUpdates(new ArrayList<UpdateAssignment>(action.eAllOfType(UpdateAssignment)))»
						
						};
					};
				
				};
			};
		};
		'''
		
	}
	
	def String declareTransitions(Tree tree){
		var ArrayList<String> transitions = new ArrayList<String>()
		tree.getTransitions(transitions)
		'''
		//create the transitions between states
		«FOR transition : transitions»
			«transition»
		«ENDFOR»
		'''
	}
	
}