package eu.quanticol.carma.core.generator.ms.collective

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.Action
import eu.quanticol.carma.core.carma.ActionGuard
import eu.quanticol.carma.core.carma.AttribParameter
import eu.quanticol.carma.core.carma.AttribType
import eu.quanticol.carma.core.carma.AttribVariableDeclaration
import eu.quanticol.carma.core.carma.BlockCollective
import eu.quanticol.carma.core.carma.BlockStyle
import eu.quanticol.carma.core.carma.BooleanExpression
import eu.quanticol.carma.core.carma.CBND
import eu.quanticol.carma.core.carma.ComponentAssignment
import eu.quanticol.carma.core.carma.ComponentBlockArguments
import eu.quanticol.carma.core.carma.ComponentBlockDeclaration
import eu.quanticol.carma.core.carma.ComponentBlockDefinition
import eu.quanticol.carma.core.carma.ComponentBlockForStatement
import eu.quanticol.carma.core.carma.ComponentBlockNew
import eu.quanticol.carma.core.carma.ComponentForVariableDeclaration
import eu.quanticol.carma.core.carma.Declaration
import eu.quanticol.carma.core.carma.DoubleParameter
import eu.quanticol.carma.core.carma.DoubleType
import eu.quanticol.carma.core.carma.GlobalStoreBlock
import eu.quanticol.carma.core.carma.InputActionParameters
import eu.quanticol.carma.core.carma.IntgerParameter
import eu.quanticol.carma.core.carma.IntgerType
import eu.quanticol.carma.core.carma.OutputActionArgument
import eu.quanticol.carma.core.carma.OutputActionArguments
import eu.quanticol.carma.core.carma.Parameter
import eu.quanticol.carma.core.carma.ProcessComposition
import eu.quanticol.carma.core.carma.ProcessParameter
import eu.quanticol.carma.core.carma.Processes
import eu.quanticol.carma.core.carma.RecordDeclaration
import eu.quanticol.carma.core.carma.RecordParameter
import eu.quanticol.carma.core.carma.RecordReferenceGlobal
import eu.quanticol.carma.core.carma.RecordReferenceMy
import eu.quanticol.carma.core.carma.RecordReferencePure
import eu.quanticol.carma.core.carma.RecordReferenceReceiver
import eu.quanticol.carma.core.carma.RecordReferenceSender
import eu.quanticol.carma.core.carma.RecordType
import eu.quanticol.carma.core.carma.Type
import eu.quanticol.carma.core.carma.Update
import eu.quanticol.carma.core.carma.UpdateAssignment
import eu.quanticol.carma.core.carma.VariableName
import eu.quanticol.carma.core.carma.VariableReference
import eu.quanticol.carma.core.carma.VariableReferenceGlobal
import eu.quanticol.carma.core.carma.VariableReferenceMy
import eu.quanticol.carma.core.carma.VariableReferencePure
import eu.quanticol.carma.core.carma.VariableReferenceReceiver
import eu.quanticol.carma.core.carma.VariableReferenceSender
import eu.quanticol.carma.core.generator.ms.MSSystemCompiler
import eu.quanticol.carma.core.generator.ms.SharedJavaniser
import eu.quanticol.carma.core.typing.TypeProvider
import eu.quanticol.carma.core.utils.Tree
import eu.quanticol.carma.core.utils.Util
import java.util.ArrayList
import java.util.HashMap
import java.util.HashSet

import static extension org.eclipse.xtext.EcoreUtil2.*

class CollectiveHandler {

	@Inject extension SharedJavaniser
	@Inject extension Util
	@Inject extension TypeProvider

	def String constructor(BlockCollective collective, GlobalStoreBlock globalStoreBlock) {
		var declarations = collective.declarations
		'''
			public «MSSystemCompiler.SYSTEMNAME»(){
				«FOR declaration : declarations»
					«declaration.addComponent»
				«ENDFOR»
				«IF globalStoreBlock != null»
					«FOR attribute : globalStoreBlock.attributes»
					«attribute.addGlobalStores»
					«ENDFOR»
				«ENDIF»
			}
		'''
	}

	def String addComponent(ComponentBlockDeclaration componentBlockDeclaration) {
		switch (componentBlockDeclaration) {
			ComponentBlockNew: addComponent(componentBlockDeclaration)
			ComponentBlockForStatement: addComponent(componentBlockDeclaration)
		}
	}

	def String addComponent(ComponentBlockNew componentBlockDeclaration) {
		var products = new ArrayList<ArrayList<String>>()
		(componentBlockDeclaration.arguments as ComponentBlockArguments).product.cartesianProduct(products)
		var name = (componentBlockDeclaration as ComponentBlockNew).name.name
		'''
			«IF products.size > 0»
			«FOR args : products»
				addComponent(get«name.toFirstUpper»(«args.asArguments»));
			«ENDFOR»
			«ELSE»
				addComponent(get«name.toFirstUpper»());
			«ENDIF»
		'''
	}

	def String asArguments(ArrayList<String> args) {
		var String toReturn = ""
		if (args.size > 0) {
			toReturn = args.get(0)
			for (var i = 1; i < args.size; i++)
				toReturn = toReturn + ", " + args.get(i)
		}
		return toReturn
	}

	def String addComponent(ComponentBlockForStatement componentBlockDeclaration) {
		'''for(«(componentBlockDeclaration.variable as ComponentForVariableDeclaration).cjavanise» ; «componentBlockDeclaration.expression.express» ; «(componentBlockDeclaration.afterThought.componentAssignment as ComponentAssignment).javanise»){
	«componentBlockDeclaration.componentBlockForBlock.component.addComponent»			
}'''
	}
	
	def String cjavanise(ComponentForVariableDeclaration componentForVariableDeclaration){
		'''«(componentForVariableDeclaration.type as Type).javanise» attrib_«componentForVariableDeclaration.name.name» = «componentForVariableDeclaration.assign.express»'''
	}

	def String addGlobalStores(Declaration storeDeclaration) {
		'''global_store.set(«storeDeclaration.setStore»);'''
	}

	def String getComponents(BlockStyle blockStyle) {
		'''
			«FOR definition : blockStyle.definitions»
				«(definition as ComponentBlockDefinition).getComponent»
			«ENDFOR»
		'''
	}

	def String getComponent(ComponentBlockDefinition componentBlockDefinition) {
		var String componentName = componentBlockDefinition.componentSignature.name.name
		var ArrayList<Parameter> parameters = new ArrayList<Parameter>(
			componentBlockDefinition.componentSignature.componentParameters.eAllOfType(Parameter))
		var boolean hasBehaviour = componentBlockDefinition.componentSignature.componentParameters.eAllOfType(
			ProcessParameter).size > 0
		var String behaviour = ""
		if (hasBehaviour) {
			behaviour = componentBlockDefinition.componentSignature.componentParameters.eAllOfType(ProcessParameter).
				get(0).name.name
		}
		var attributes = componentBlockDefinition.componentBlock.store.attributes
		'''
			private CarmaComponent get«componentName»( «parameters.getCParameters» ){
				CarmaComponent component_«componentName» = new CarmaComponent();
				«FOR attribute : attributes»
					«attribute.setStores(componentName)»
				«ENDFOR»
				«setBehaviour(behaviour, componentBlockDefinition.componentBlock.initBlock.init, componentName)»
				return component_«componentName»;
			}
			
		'''
	}

	def  String getCParameters(ArrayList<Parameter> parameters){
		var String toReturn = ""
		if(parameters.size > 0){
			toReturn = parameters.get(0).getCParameter
			for(var i = 1; i < parameters.size; i++){
				toReturn = toReturn + ", " + parameters.get(i).getCParameter
			}
		}
		return toReturn
	}
	
	def  String getCParameter(Parameter parameter){
		switch(parameter){
			AttribParameter: '''«(parameter.type as AttribType).javanise» attrib_«parameter.name.name»'''
			RecordParameter: '''«(parameter.type as RecordType).javanise» attrib_«parameter.name.name»'''
			DoubleParameter: '''«(parameter.type as DoubleType).javanise» attrib_«parameter.name.name»'''
			IntgerParameter: '''«(parameter.type as IntgerType).javanise» attrib_«parameter.name.name»'''
			ProcessParameter: '''ArrayList<String> behaviour'''
		}
	}

	def String setStores(Declaration storeDeclaration, String name) {
		'''component_«name».set(«storeDeclaration.setStore»);'''
	}
	
	def String setStore(Declaration declaration){
		switch(declaration){
			AttribVariableDeclaration	: declaration.setStore
			RecordDeclaration			: declaration.setStore
		}
	}
	
	def  String setStore(AttribVariableDeclaration attribVariableDeclaration){
		'''"«attribVariableDeclaration.name.name»", «attribVariableDeclaration.assign.javanise»'''
	}
	
	def  String setStore(RecordDeclaration recordDeclaration){
		'''"«recordDeclaration.name.name»", «recordDeclaration.assign.javanise»'''
	}

	def String setBehaviour(String behaviour, ProcessComposition processComposition, String componentName) {
		var ArrayList<String> states = new ArrayList<String>()
		processComposition.array(states)
		var boolean hasBehaviour = states.contains(behaviour)
		if (hasBehaviour) {
			states.remove(behaviour)
		}
		'''
			«IF hasBehaviour»
				ArrayList<String> processes = new ArrayList<String>(Arrays.asList( «states.javanise» ));
				processes.addAll(behaviour);
			«ELSE»
				ArrayList<String> processes = new ArrayList<String>(Arrays.asList( «processComposition.javanise» ));
			«ENDIF»
			for(int i = 0; i < processes.size(); i++){
				component_«componentName».addAgent( new CarmaSequentialProcess(component_«componentName»,create«componentName.toFirstUpper»Process(),create«componentName.toFirstUpper»Process().getState("state_"+processes.get(i))));
			}
		'''
	}

	def String createProcesses(BlockStyle blockStyle) {
		var Processes processes = blockStyle.processes
		var cbnds = blockStyle.eAllOfType(CBND)
		var HashSet<String> toReturn = new HashSet<String>()
		for (cbnd : cbnds)
			toReturn.add(cbnd.createProcess(processes))
		'''
			«FOR item : toReturn»
				«item»
			«ENDFOR»
		'''
	}

	def String createProcess(CBND declaration, Processes processes) {
		var String componentName = declaration.name.name
		var Tree tree = declaration.getTree
		'''
			private static CarmaProcessAutomaton create«componentName»Process() {
				CarmaProcessAutomaton toReturn = new CarmaProcessAutomaton("«componentName»");
				«tree.createStates»
				«tree.createActions»
				«tree.createGuards»
				«tree.createTransitions»
				return toReturn;
			}
		'''
	}

	def String createStates(Tree tree) {
		var HashSet<String> states = new HashSet<String>();
		tree.getStates(states)
		'''
			«FOR state : states»
				«IF !state.equals("null")»
					CarmaProcessAutomaton.State «state» = toReturn.newState("«state»");
				«ENDIF»
			«ENDFOR»
		'''
	}

	def String createActions(Tree tree) {
		var HashMap<String, Action> actions = new HashMap<String, Action>()
		tree.getActions(actions)
		'''
			«FOR key : actions.keySet»
				«key.getAction(actions.get(key))»
			«ENDFOR»
		'''
	}

	def String getAction(String name, Action action) {
		'''
			«IF (action.type.parent.equals("output"))»
				«name.getActionOutput(action.type.me.equals("broad"),action)»
			«ELSE»
				«name.getActionInput(action.type.me.equals("broad"),action)»
			«ENDIF»
		'''
	}

	def String getActionOutput(String actionName, boolean isBroadcast, Action action) {

		'''
			CarmaOutput «actionName» = new CarmaOutput( «action.actionName», «isBroadcast» ) {
				«action.outputActionPredicate»
				«action.getOutputUpdate»
				«action.getValues»
			};
		'''
	}

	def String getOutputActionPredicate(Action action) {
		if (action.eAllOfType(ActionGuard).size > 0) {
			'''
				@Override
				protected CarmaPredicate getPredicate(final CarmaStore their_store) {
					return new CarmaPredicate() {
						@Override
						public boolean satisfy(CarmaStore my_store) {
							«getOutputSatisfyBlock(action.eAllOfType(ActionGuard).get(0).booleanExpression)»
						}
					};
				}
			'''
		} else {
			'''
				@Override
				protected CarmaPredicate getPredicate(final CarmaStore their_store) {
					return new CarmaPredicate() {
						@Override
						public boolean satisfy(CarmaStore my_store) {
							return true;
						}
					};
				}
			'''
		}
	}

	def String getOutputSatisfyBlock(BooleanExpression bes) {
		var vrs = bes.eAllOfType(VariableReference)
		var vrsh = new HashMap<String, VariableReference>()
		for (vr : vrs) {
			vrsh.put(vr.name.name, vr)
		}
		'''
			HashMap<String,Class> my_variables = new HashMap<String,Class>();
			HashMap<String,Class> their_variables = new HashMap<String,Class>();
			«FOR key : vrsh.keySet»
				«vrsh.get(key).checkStoreOutputPredicate»
			«ENDFOR»
			boolean hasAttributes = true;
			if(my_variables != null)
				for(String key : my_variables.keySet()){
					hasAttributes = my_store.has(key,my_variables.get(key)) && hasAttributes;
				}
			if(their_variables != null)
				for(String key : their_variables.keySet()){
					hasAttributes = their_store.has(key,their_variables.get(key)) && hasAttributes;
				}
			if(hasAttributes){
				«FOR key : vrsh.keySet»
					«vrsh.get(key).storeOutputPredicate»
				«ENDFOR»
				return «bes.express»;
			} else {
				return false;
			}
		'''
	}

	def String checkStoreOutputPredicate(VariableReference vr) {
		switch (vr) {
			VariableReferencePure: '''their_variables.put("«vr.name.name»",«vr.type.storeExpress»);'''
			VariableReferenceMy: '''my_variables.put("«vr.name.name»",«vr.type.storeExpress»);'''
			VariableReferenceReceiver:
				"receiver_store."
			VariableReferenceSender:
				"sender_store."
			VariableReferenceGlobal:
				"global_store."
			RecordReferencePure: '''their_variables.put("«vr.name.name»",«vr.type.storeExpress»);'''
			RecordReferenceMy: '''my_variables.put("«vr.name.name»",«vr.type.storeExpress»);'''
			RecordReferenceReceiver:
				"receiver_store."
			RecordReferenceSender:
				"sender_store."
			RecordReferenceGlobal:
				"global_store."
		}
	}

	def String getStoreOutputPredicate(VariableReference vr) {
		switch (vr) {
			VariableReferencePure: '''«vr.name.type.express» attrib_«vr.name.name» = their_store.get("«vr.name.name»",«vr.name.type.storeExpress»);'''
			VariableReferenceMy: '''«vr.name.type.express» my_«vr.name.name» = my_store.get("«vr.name.name»",«vr.name.type.storeExpress»);'''
			VariableReferenceReceiver:
				"receiver_store."
			VariableReferenceSender:
				"sender_store."
			VariableReferenceGlobal:
				"global_store."
			RecordReferencePure: '''«vr.name.type.express» attrib_«vr.name.name» = their_store.get("«vr.name.name»",«vr.name.type.storeExpress»);'''
			RecordReferenceMy: '''«vr.name.type.express» my_«vr.name.name» = my_store.get("«vr.name.name»",«vr.name.type.storeExpress»);'''
			RecordReferenceReceiver:
				"receiver_store."
			RecordReferenceSender:
				"sender_store."
			RecordReferenceGlobal:
				"global_store."
		}
	}

	def String getOutputUpdate(Action action) {
		if (action.eAllOfType(Update).size > 0) {
			'''
				@Override
				protected CarmaStoreUpdate getUpdate() {
					return new CarmaStoreUpdate() {
						
						@Override
						public void update(RandomGenerator r, CarmaStore my_store) {
							«action.outputUpdateBlock»
						}
					};
				}
			'''
		} else {
			'''
				@Override
				protected CarmaStoreUpdate getUpdate() {
					return null;
				}
			'''
		}
	}

	def String outputUpdateBlock(Action action) {
		var update = action.eAllOfType(Update).get(0)
		var updateAssignments = update.eAllOfType(UpdateAssignment)
		var vrs = new HashMap<String, VariableReference>()
		for (updateAssignment : updateAssignments)
			for (vr : updateAssignment.eAllOfType(VariableReference))
				vrs.put(vr.name.name, vr)

		'''
			HashMap<String,Class> my_variables = new HashMap<String,Class>();
			«FOR key : vrs.keySet»
				«vrs.get(key).checkStoreOutput»
			«ENDFOR»
			boolean hasAttributes = true;
			if(my_variables != null)
				for(String key : my_variables.keySet()){
					hasAttributes = my_store.has(key,my_variables.get(key)) && hasAttributes;
				}
			if(hasAttributes){
				«FOR key : vrs.keySet»
					«vrs.get(key).storeOutput»
				«ENDFOR»
				«FOR updateAssignment : updateAssignments»
					attrib_«updateAssignment.reference.name.name» = «updateAssignment.expression.express»;
				«ENDFOR»
				«FOR updateAssignment : updateAssignments»
					my_store.set("«updateAssignment.reference.name.name»",attrib_«updateAssignment.reference.name.name»);
				«ENDFOR»
			}
		'''
	}

	def String checkStoreOutput(VariableReference vr) {
		switch (vr) {
			VariableReferencePure: '''my_variables.put("«vr.name.name»",«vr.type.storeExpress»);'''
			VariableReferenceMy: '''my_variables.put("«vr.name.name»",«vr.type.storeExpress»);'''
			VariableReferenceReceiver:
				"receiver_store."
			VariableReferenceSender:
				"sender_store."
			VariableReferenceGlobal:
				"global_store."
			RecordReferencePure: '''my_variables.put("«vr.name.name»",«vr.type.storeExpress»);'''
			RecordReferenceMy: '''my_variables.put("«vr.name.name»",«vr.type.storeExpress»);'''
			RecordReferenceReceiver:
				"receiver_store."
			RecordReferenceSender:
				"sender_store."
			RecordReferenceGlobal:
				"global_store."
		}
	}

	def String getStoreOutput(VariableReference vr) {
		switch (vr) {
			VariableReferencePure: '''«vr.name.type.express» attrib_«vr.name.name» = my_store.get("«vr.name.name»",«vr.type.storeExpress»);'''
			VariableReferenceMy: '''«vr.name.type.express» my_«vr.name.name» = my_store.get("«vr.name.name»",«vr.type.storeExpress»);'''
			VariableReferenceReceiver:
				"receiver_store."
			VariableReferenceSender:
				"sender_store."
			VariableReferenceGlobal:
				"global_store."
			RecordReferencePure: '''«vr.name.type.express» attrib_«vr.name.name» = my_store.get("«vr.name.name»",«vr.type.storeExpress»);'''
			RecordReferenceMy: '''«vr.name.type.express» my_«vr.name.name» = my_store.get("«vr.name.name»",«vr.type.storeExpress»);'''
			RecordReferenceReceiver:
				"receiver_store."
			RecordReferenceSender:
				"sender_store."
			RecordReferenceGlobal:
				"global_store."
		}
	}

	def String getValues(Action action) {
		if (action.eAllOfType(OutputActionArguments).size > 0) {
			'''
				@Override
				protected Object getValue(CarmaStore my_store) {
					«action.eAllOfType(OutputActionArguments).get(0).defineValueBlock»
				}
			'''
		} else {
			'''
				@Override
				protected Object getValue(CarmaStore my_store) {
					return new Object();
				}
			'''
		}
	}

	def String defineValueBlock(OutputActionArguments arguments) {
		var ArrayList<OutputActionArgument> args = new ArrayList<OutputActionArgument>(
			arguments.eAllOfType(OutputActionArgument))
		var count = 0
		'''
			int[] output = new int[«args.size»];
			HashMap<String,Class> my_variables = new HashMap<String,Class>();
			«FOR arg : args»
				«arg.checkStoreOutput»
			«ENDFOR»
			boolean hasAttributes = true;
			if(my_variables != null)
				for(String key : my_variables.keySet()){
					hasAttributes = my_store.has(key,my_variables.get(key)) && hasAttributes;
				}
			if(hasAttributes){
				«FOR arg : args»
					«arg.storeOutput»
				«ENDFOR»
				«FOR arg : args»
					output[«count++»] = «arg.javanise»;
				«ENDFOR»
				return output;
			} else {
				return new Object();
			}
		'''
	}

	def String checkStoreOutput(OutputActionArgument oaa) {
		switch (oaa.value) {
			VariableReferenceMy: '''my_variables.put("«(oaa.value as VariableReference).name.name»",«(oaa.value as VariableReference).type.storeExpress»);'''
			RecordReferenceMy: '''my_variables.put("«(oaa.value as VariableReference).name.name»",«(oaa.value as VariableReference).type.storeExpress»);'''
		}
	}

	def String getStoreOutput(OutputActionArgument oaa) {
		switch (oaa.value) {
			VariableReferenceMy: '''«(oaa.value as VariableReference).name.type.express» «(oaa.value as VariableReference).name.name» = my_store.get("«(oaa.value as VariableReference).name.name»",«(oaa.value as VariableReference).type.storeExpress»);'''
			RecordReferenceMy: '''«(oaa.value as VariableReference).name.type.express» «(oaa.value as VariableReference).name.name» = my_store.get("«(oaa.value as VariableReference).name.name»",«(oaa.value as VariableReference).type.storeExpress»);'''
		}
	}

	def String getActionInput(String actionName, boolean isBroadcast, Action action) {
		'''
			CarmaInput «actionName» = new CarmaInput( «action.actionName», «isBroadcast» ) {
				«getInputActionPredicate(action)»
				«getInputUpdate(action)»
			};
		'''
	}

	def String getInputActionPredicate(Action action) {
		if (action.eAllOfType(ActionGuard).size > 0) {
			'''
				@Override
				protected CarmaPredicate getPredicate(final CarmaStore my_store, final Object value) {
					if (value instanceof int[]){
						return new CarmaPredicate() {
							@Override
							public boolean satisfy(CarmaStore their_store) {
								«getInputSatisfyBlock(action)»
							}
						};
					}
					return null;
				}
			'''
		} else {
			'''
				@Override
				protected CarmaPredicate getPredicate(final CarmaStore my_store, final Object value) {
					if (value instanceof int[]){
						return new CarmaPredicate() {
							@Override
							public boolean satisfy(CarmaStore their_store) {
								return true;
							}
						};
					}
					return null;
				}
			'''
		}
	}

	def String getInputSatisfyBlock(Action action) {
		var BooleanExpression bes = action.eAllOfType(ActionGuard).get(0).booleanExpression
		var vrs = bes.eAllOfType(VariableReference)
		'''
			HashMap<String,Class> my_variables = new HashMap<String,Class>();
			«FOR vr : vrs»
				«vr.checkStoreInput»
			«ENDFOR»
			«setupInputArguments(action.eAllOfType(InputActionParameters).get(0))»
			boolean hasAttributes = true;
			if(my_variables != null)
				for(String key : my_variables.keySet()){
					hasAttributes = my_store.has(key,my_variables.get(key)) && hasAttributes;
				}
			if(hasAttributes){
				«FOR vr : vrs»
					«vr.storeInput»
				«ENDFOR»
				return «bes.express»;
			} else {
				return false;
			}
		'''
	}

	def String checkStoreInput(VariableReference vr) {
		switch (vr) {
			VariableReferencePure: ''''''
			VariableReferenceMy: '''my_variables.put("«vr.name.name»",«vr.type.storeExpress»);'''
			VariableReferenceReceiver:
				"receiver_store."
			VariableReferenceSender:
				"sender_store."
			VariableReferenceGlobal:
				"global_store."
			RecordReferencePure: ''''''
			RecordReferenceMy: '''my_variables.put("«vr.name.name»",«vr.type.storeExpress»);'''
			RecordReferenceReceiver:
				"receiver_store."
			RecordReferenceSender:
				"sender_store."
			RecordReferenceGlobal:
				"global_store."
		}
	}

	def String getStoreInput(VariableReference vr) {
		switch (vr) {
			VariableReferencePure: {
				if (vr.getContainerOfType(InputActionParameters) !=
					null
				) '''«vr.name.type.express» attrib_«vr.name.name» = my_store.get("«vr.name.name»",«vr.type.storeExpress»);''' else ''''''
			}
			VariableReferenceMy: '''«vr.name.type.express» my_«vr.name.name» = my_store.get("«vr.name.name»",«vr.type.storeExpress»);'''
			VariableReferenceReceiver:
				"receiver_store."
			VariableReferenceSender:
				"sender_store."
			VariableReferenceGlobal:
				"global_store."
			RecordReferencePure: {
				if (vr.getContainerOfType(InputActionParameters) !=
					null
				) '''«vr.name.type.express» attrib_«vr.name.name» = my_store.get("«vr.name.name»",«vr.type.storeExpress»);''' else ''''''
			}
			RecordReferenceMy: '''«vr.name.type.express» my_«vr.name.name» = my_store.get("«vr.name.name»",«vr.type.storeExpress»);'''
			RecordReferenceReceiver:
				"receiver_store."
			RecordReferenceSender:
				"sender_store."
			RecordReferenceGlobal:
				"global_store."
		}
	}

	def String getInputUpdate(Action action) {
		if (action.eAllOfType(Update).size > 0) {
			'''
				@Override
				protected CarmaStoreUpdate getUpdate(final Object value) {
					
					return new CarmaStoreUpdate() {
						@Override
						public void update(RandomGenerator r, CarmaStore my_store) {
							if (value instanceof int[]){
								«action.inputUpdateBlock»
							};
						};
					
					};
				};
			'''
		} else {
			'''
				@Override
				protected CarmaStoreUpdate getUpdate(final Object value) {
					
					return new CarmaStoreUpdate() {
						@Override
						public void update(RandomGenerator r, CarmaStore my_store) {
							return null;
						};
					
					};
				};
			'''
		}
	}

	def String inputUpdateBlock(Action action) {
		var update = action.eAllOfType(Update).get(0)
		var updateAssignments = update.eAllOfType(UpdateAssignment)
		var vrs = new HashMap<String, VariableReference>()
		for (updateAssignment : updateAssignments)
			for (vr : updateAssignment.eAllOfType(VariableReference))
				vrs.put(vr.name.name, vr)
		'''
			HashMap<String,Class> my_variables = new HashMap<String,Class>();
			«FOR key : vrs.keySet»
				«vrs.get(key).checkStoreInput»
			«ENDFOR»
			«setupInputArguments(action.eAllOfType(InputActionParameters).get(0))»
			boolean hasAttributes = true;
			if(my_variables != null)
				for(String key : my_variables.keySet()){
					hasAttributes = my_store.has(key,my_variables.get(key)) && hasAttributes;
				}
			if(hasAttributes){
				«FOR key : vrs.keySet»
					«vrs.get(key).storeInput»
				«ENDFOR»
				«FOR updateAssignment : updateAssignments»
					«updateAssignment.reference.type.express» attrib_«updateAssignment.reference.name.name» = my_store.get("«updateAssignment.reference.name.name»",«updateAssignment.reference.type.storeExpress»);
					attrib_«updateAssignment.reference.name.name» = «updateAssignment.expression.express»;
				«ENDFOR»
				«FOR updateAssignment : updateAssignments»
					my_store.set("«updateAssignment.reference.name.name»",attrib_«updateAssignment.reference.name.name»);
				«ENDFOR»
			}
		'''
	}

	def String setupInputArguments(InputActionParameters parameters) {
		var ArrayList<VariableName> vns = new ArrayList<VariableName>(parameters.eAllOfType(VariableName))
		'''
			«FOR vn : vns»
				int attrib_«vn.name» = ((int[]) value)[«vns.indexOf(vn)»];
			«ENDFOR»
		'''
	}

	def String createGuards(Tree tree) {
		var HashMap<String, BooleanExpression> guardExpressions = new HashMap<String, BooleanExpression>();
		tree.getGuards(guardExpressions);
		'''
			«FOR key : guardExpressions.keySet»
				«key.getGuardPredicate(guardExpressions.get(key))»
			«ENDFOR»
		'''
	}

	def String getGuardPredicate(String name, BooleanExpression bes) {
		'''
			CarmaPredicate «name» = new CarmaPredicate() {
				@Override
				public boolean satisfy(CarmaStore my_store) {
					«bes.getGuardSatisfyBlock»
				}
			};
		'''
	}

	def String getGuardSatisfyBlock(BooleanExpression bes) {
		var vrs = newHashMap()
		for (vr : bes.eAllOfType(VariableReference)) {
			vrs.put(vr.name.name, vr)
		}
		'''
			HashMap<String,Class> my_variables = new HashMap<String,Class>();
			«FOR key : vrs.keySet»
				«vrs.get(key).checkStoreOutput»
			«ENDFOR»
			boolean hasAttributes = true;
			if(my_variables != null)
				for(String key : my_variables.keySet()){
					hasAttributes = my_store.has(key,my_variables.get(key)) && hasAttributes;
				}
			if(hasAttributes){
				«FOR key : vrs.keySet»
					«vrs.get(key).storeOutput»
				«ENDFOR»
				return «bes.express»;
			} else {
				return false;
			}
		'''
	}

	def String createTransitions(Tree tree) {
		var ArrayList<String> transitions = new ArrayList<String>()
		tree.getTransitions(transitions)
		'''
			«FOR transition : transitions»
				«transition»
			«ENDFOR»
		'''
	}

}