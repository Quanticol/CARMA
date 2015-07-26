package eu.quanticol.carma.core.generator.ms.collective

import static extension eu.quanticol.carma.core.utils.Util.*
import static extension eu.quanticol.carma.core.generator.ms.expression.ExpressionHandler.*

import eu.quanticol.carma.core.carma.UpdateAssignment
import org.eclipse.emf.common.util.EList
import eu.quanticol.carma.core.carma.ProcessExpressionAction
import java.util.List
import eu.quanticol.carma.core.carma.Process
import eu.quanticol.carma.core.carma.ProcessExpressionNext
import eu.quanticol.carma.core.carma.ProcessExpressionNil
import eu.quanticol.carma.core.carma.ProcessExpressionKill
import eu.quanticol.carma.core.carma.ProcessExpressionReference
import eu.quanticol.carma.core.carma.ComponentDefinition
import eu.quanticol.carma.core.carma.Expression
import eu.quanticol.carma.core.carma.UntypedVariable
import eu.quanticol.carma.core.carma.InputAction
import eu.quanticol.carma.core.carma.OutputAction

class CollectiveHandler {

	def static CharSequence componentBehaviour( ComponentDefinition comp , Iterable<Process> global ) {
		'''
		/* DEFINITIONS OF «comp.name» PROCESSES */
		«FOR p:global»
		public final CarmaProcessAutomaton.State «p.name.stateName(comp.name)»;		
		«ENDFOR»
		
		«FOR p:comp.processes.processes»
		public final CarmaProcessAutomaton.State «p.name.stateName(comp.name)»;		
		«ENDFOR»

		private void generate«comp.name»Behaviour( ) {
			CarmaProcessAutomaton componentBehaviour = new CarmaProcessAutomaton("Info");
			«FOR p:global»
			«p.name.stateName(comp.name)» = componentBehaviour.newState("«p.name»");
			«ENDFOR»
			
			«FOR p:comp.processes.processes»
			«p.name.stateName(comp.name)» = componentBehaviour.newState("«p.name»");		
			«ENDFOR»
			
			«FOR p:global»
			«p.name.stateName(comp.name)» = componentBehaviour.newState("«p.name»");
			«ENDFOR»
			
			«FOR p:comp.processes.processes»
			«p.name.stateName(comp.name)» = componentBehaviour.newState("«p.name»");		
			«ENDFOR»
			
		}
		
		'''
	}
	
	def static CharSequence componentTransitions( ProcessExpressionAction p , String state , String component , List<String> guards , int guardCounter ) {
		'''
		{
			«p.action.actionCode»
			
			«IF guards.empty»			
			componentBehaviour.addTransition( «state.stateName(component)» , action , «p.next.nextCode(component)» );			
			«ELSE»
			componentBehaviour.addTransition( «state.stateName(component)» , new CarmaPredicate.Conjunction( «FOR s:guards SEPARATOR ','» «s» «ENDFOR» ) , action , «p.next.nextCode(component)» );			
			«ENDIF»
		}
		'''
	}
	
	def static nextCode( ProcessExpressionNext next , String component ) {
		switch next {
			ProcessExpressionNil: '''null'''
			ProcessExpressionKill: '''null'''
			ProcessExpressionReference: '''«next.expression.name.stateName(component)»'''
		}
	}
	
	def static dispatch CharSequence actionCode( InputAction act ) {
		var t = act.activity.reference.types
		var idxVar = act.parameters.indexed
		'''
		CarmaAction action = new CarmaInput( 
			«act.activity.reference.name.actionName» , «act.activity.isIsBroadacst»  		
		) {
			
			@Override
			protected CarmaStoreUpdate getUpdate(final Object value) {
				Iterable<Object> message = (Iterable<Object>) value;
				«FOR idv:idxVar»
				final «t.get(idv.key).toJavaType» «idv.value.name.variableName» = («t.get(idv.key).toJavaType») message.get(«idv.key»);
				«ENDFOR»
				return new CarmaStoreUpdate() {
					
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
						//FIXME!	
					}
				};
			}	
			
			@Override
			protected CarmaPredicate getPredicate(CarmaStore store, Object value) {
				return CarmaPredicate.TRUE;//FIXME!
			}
						
		};		
		'''		
	}

	def static dispatch CharSequence actionCode( OutputAction act ) {
		'''
		CarmaAction action = new CarmaOutput(
			«act.activity.reference.name.actionName» , «act.activity.isIsBroadacst»  		
		) {
			
			@Override
			protected Object getValue(CarmaStore store) {
				LinkedList<Object> toReturn = new LinkedList();
				«FOR e:act.outputArguments»
				toReturn.add( «e.expressionToJava» );
				«ENDFOR»
				return toReturn;
			}
			
			@Override
			protected CarmaStoreUpdate getUpdate() {
				return new CarmaStoreUpdate() {
					
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
						//FIXME!!!
					}
				};
			}
			
			@Override
			protected CarmaPredicate getPredicate(final CarmaStore taxiStore) {
				return new CarmaPredicate() {

					@Override
					public boolean satisfy(CarmaStore store) {
						return false;//FIXME!!!
					}
					
				};
			}
		};		
		'''
	}
	

//	def String constructor(BlockCollective collective, GlobalStoreBlock globalStoreBlock) {
//		var declarations = collective.declarations
//		'''
//			public «MSSystemCompiler.SYSTEMNAME»(){
//				«FOR declaration : declarations»
//					«declaration.addComponent»
//				«ENDFOR»
//				«IF globalStoreBlock != null»
//					«FOR attribute : globalStoreBlock.attributes»
//					«attribute.addGlobalStores»
//					«ENDFOR»
//				«ENDIF»
//			}
//		'''
//	}
//
//	def String addComponent(ComponentBlockDeclaration componentBlockDeclaration) {
//		switch (componentBlockDeclaration) {
//			ComponentBlockNew: addComponent(componentBlockDeclaration)
//			ComponentBlockForStatement: addComponent(componentBlockDeclaration)
//		}
//	}
//
//	def String addComponent(ComponentBlockNew componentBlockDeclaration) {
//		var products = new ArrayList<ArrayList<String>>()
//		(componentBlockDeclaration.arguments as ComponentBlockArguments).product.cartesianProduct(products)
//		var name = (componentBlockDeclaration as ComponentBlockNew).name.name
//		'''
//			«IF products.size > 0»
//			«FOR args : products»
//				addComponent(get«name.toFirstUpper»(«args.asArguments»));
//			«ENDFOR»
//			«ELSE»
//				addComponent(get«name.toFirstUpper»());
//			«ENDIF»
//		'''
//	}
//
//	def String asArguments(ArrayList<String> args) {
//		var String toReturn = ""
//		if (args.size > 0) {
//			toReturn = args.get(0)
//			for (var i = 1; i < args.size; i++)
//				toReturn = toReturn + ", " + args.get(i)
//		}
//		return toReturn
//	}
//
//	def String addComponent(ComponentBlockForStatement componentBlockDeclaration) {
//		'''for(«(componentBlockDeclaration.variable as ComponentForVariableDeclaration).javanise» ; «componentBlockDeclaration.expression.javanise» ; «(componentBlockDeclaration.afterThought.componentAssignment as ComponentAssignment).javanise»){
//	«componentBlockDeclaration.componentBlockForBlock.component.addComponent»			
//}'''
//	}
//
//	def String addGlobalStores(Declaration storeDeclaration) {
//		'''global_store.set(«storeDeclaration.setStore»);'''
//	}
//
//	def String getComponents(BlockStyle blockStyle) {
//		'''
//			«FOR definition : blockStyle.definitions»
//				«(definition as ComponentBlockDefinition).getComponent»
//			«ENDFOR»
//		'''
//	}
//
//	def String getComponent(ComponentBlockDefinition componentBlockDefinition) {
//		var String componentName = componentBlockDefinition.componentSignature.name.name
//		var ArrayList<Parameter> parameters = new ArrayList<Parameter>(
//			componentBlockDefinition.componentSignature.componentParameters.eAllOfType(Parameter))
//		var boolean hasBehaviour = componentBlockDefinition.componentSignature.componentParameters.eAllOfType(
//			ProcessParameter).size > 0
//		var String behaviour = ""
//		if (hasBehaviour) {
//			behaviour = componentBlockDefinition.componentSignature.componentParameters.eAllOfType(ProcessParameter).
//				get(0).name.name
//		}
//		var attributes = componentBlockDefinition.componentBlock.store.attributes
//		'''
//			private CarmaComponent get«componentName»( «parameters.getParameters» ){
//				CarmaComponent c4rm4 = new CarmaComponent();
//				«FOR attribute : attributes»
//					«attribute.setStores»
//				«ENDFOR»
//				«setBehaviour(behaviour, componentBlockDefinition.componentBlock.initBlock.init, componentName)»
//				return c4rm4;
//			}
//			
//		'''
//	}
//
//	def String setStores(Declaration storeDeclaration) {
//		'''c4rm4.set(«storeDeclaration.setStore»);'''
//	}
//
//	def String setBehaviour(String behaviour, ProcessComposition processComposition, String componentName) {
//		var ArrayList<String> states = new ArrayList<String>()
//		processComposition.array(states)
//		var boolean hasBehaviour = states.contains(behaviour)
//		if (hasBehaviour) {
//			states.remove(behaviour)
//		}
//		'''
//			«IF hasBehaviour»
//				ArrayList<String> processes = new ArrayList<String>(Arrays.asList( «states.javanise» ));
//				processes.addAll(behaviour);
//			«ELSE»
//				ArrayList<String> processes = new ArrayList<String>(Arrays.asList( «processComposition.javanise» ));
//			«ENDIF»
//			for(int i = 0; i < processes.size(); i++){
//				c4rm4.addAgent( new CarmaSequentialProcess(c4rm4,create«componentName.toFirstUpper»Process(),create«componentName.toFirstUpper»Process().getState("state_"+processes.get(i))));
//			}
//		'''
//	}
//
////	def String createProcesses(BlockStyle blockStyle) {
////		var Processes processes = blockStyle.processes
////		var cbnds = blockStyle.eAllOfType(CBND)
////		var HashSet<String> toReturn = new HashSet<String>()
////		for (cbnd : cbnds)
////			toReturn.add(cbnd.createProcess(processes))
////		'''
////			«FOR item : toReturn»
////				«item»
////			«ENDFOR»
////		'''
////	}
//
//	def String createComponentProcessAutomaton(EList<Process> processes,String name) {
//		'''
//		
//			
//		
//			private static CarmaProcessAutomaton create«name»Process() {
//				CarmaProcessAutomaton toReturn = new CarmaProcessAutomaton("«name»");
//				«FOR p: processes»
//				CarmaProcessAutomaton.State «p.name.stateName» = toReturn.newState("«p.name»");
//				«ENDFOR»
//				«FOR p: processes»
//				«FOR t: p.processExpression.normaliseProcessExpression»
//				{
//					«IF !t.key.empty»
//					CarmaPredicate guard = new CarmaPredicate.Conjunction(
//					«FOR g:t.key SEPARATOR ','»
//						new CarmaPredicate() {
//							@Override
//							public boolean satisfy(CarmaStore store) {
//							«g.booleanExpression.getGuardSatisfyBlock»;
//						}
//						}
//						);
//					«ENDFOR»
//					«ENDIF»
//					CarmaAction action = «t.value.action»	;
//					toReturn.addTransition(_STATE_«p.name.name»«IF !t.key.empty»,guard«ENDIF»,action,«t.value.next»);
//				}
//				«ENDFOR»
//				«ENDFOR»
//				return toReturn;
//			}
//		'''
//	}
//	
//	def createProcessExpressionNextCode( ProcessExpressionNext e ) {
//		switch e {
//			ProcessExpressionNil: '''null'''
//			ProcessExpressionKill: '''null''' //TODO: FIXME!!!
//			ProcessExpressionReference: '''_STATE_«e.expression.name»'''
//		}
//	}
//	
//	def Iterable<Pair<LinkedList<Guard>,ProcessExpressionAction>> normaliseProcessExpression(  ProcessExpression e ) {
//
//		switch e {
//			ProcessExpressionAction: newLinkedList( newLinkedList( ) -> e )
//			ProcessExpressionGuard: e.expression.normaliseProcessExpression.map[ p | p.key.addFirst(e.guard) p.key -> p.value ]
//			ProcessExpressionChoice: e.left.normaliseProcessExpression + e.right.normaliseProcessExpression
//		}
//	}
//
////	def String createProcess(CBND declaration, Processes processes) {
////		var String componentName = declaration.name.name
////		var Tree tree = declaration.getTree
////		'''
////			private static CarmaProcessAutomaton create«componentName»Process() {
////				CarmaProcessAutomaton toReturn = new CarmaProcessAutomaton("«componentName»");
////				«tree.createStates»
////				«tree.createActions»
////				«tree.createGuards»
////				«tree.createTransitions»
////				return toReturn;
////			}
////		'''
////	}
////
////	def String createStates(Tree tree) {
////		var HashSet<String> states = new HashSet<String>();
////		tree.getStates(states)
////		'''
////			«FOR state : states»
////				«IF !state.equals("null")»
////					CarmaProcessAutomaton.State «state» = toReturn.newState("«state»");
////				«ENDIF»
////			«ENDFOR»
////		'''
////	}
////
////	def String createActions(Tree tree) {
////		var HashMap<String, Action> actions = new HashMap<String, Action>()
////		tree.getActions(actions)
////		'''
////			«FOR key : actions.keySet»
////				«key.getAction(actions.get(key))»
////			«ENDFOR»
////		'''
////	}
//
//	def String getAction(String name, Action action) {
//		'''
//			«IF (action.type.parent.equals("output"))»
//				«name.getActionOutput(action.type.me.equals("broad"),action)»
//			«ELSE»
//				«name.getActionInput(action.type.me.equals("broad"),action)»
//			«ENDIF»
//		'''
//	}
//
//	def String getActionOutput(String actionName, boolean isBroadcast, Action action) {
//
//		'''
//			CarmaOutput «actionName» = new CarmaOutput( «action.actionName», «isBroadcast» ) {
//				«action.outputActionPredicate»
//				«action.getOutputUpdate»
//				«action.getValues»
//			};
//		'''
//	}
//
//	def String getOutputActionPredicate(Action action) {
//		if (action.eAllOfType(ActionGuard).size > 0) {
//			'''
//				@Override
//				protected CarmaPredicate getPredicate(final CarmaStore their_store) {
//					return new CarmaPredicate() {
//						@Override
//						public boolean satisfy(CarmaStore my_store) {
//							«getOutputSatisfyBlock(action.eAllOfType(ActionGuard).get(0).booleanExpression)»
//						}
//					};
//				}
//			'''
//		} else {
//			'''
//				@Override
//				protected CarmaPredicate getPredicate(final CarmaStore their_store) {
//					return new CarmaPredicate() {
//						@Override
//						public boolean satisfy(CarmaStore my_store) {
//							return true;
//						}
//					};
//				}
//			'''
//		}
//	}
//
//	def String getOutputSatisfyBlock(BooleanExpression bes) {
//		var vrs = bes.eAllOfType(VariableReference)
//		var vrsh = new HashMap<String, VariableReference>()
//		for (vr : vrs) {
//			vrsh.put(vr.name.name, vr)
//		}
//		'''
//			HashMap<String,Class> my_variables = new HashMap<String,Class>();
//			HashMap<String,Class> their_variables = new HashMap<String,Class>();
//			«FOR key : vrsh.keySet»
//				«vrsh.get(key).checkStoreOutputPredicate»
//			«ENDFOR»
//			boolean hasAttributes = true;
//			if(my_variables != null)
//				for(String key : my_variables.keySet()){
//					hasAttributes = my_store.has(key,my_variables.get(key)) && hasAttributes;
//				}
//			if(their_variables != null)
//				for(String key : their_variables.keySet()){
//					hasAttributes = their_store.has(key,their_variables.get(key)) && hasAttributes;
//				}
//			if(hasAttributes){
//				«FOR key : vrsh.keySet»
//					«vrsh.get(key).storeOutputPredicate»
//				«ENDFOR»
//				return «bes.express»;
//			} else {
//				return false;
//			}
//		'''
//	}
//
//	def String checkStoreOutputPredicate(VariableReference vr) {
//		switch (vr) {
//			VariableReferencePure: '''their_variables.put("«vr.name.name»",«vr.type.storeExpress»);'''
//			VariableReferenceMy: '''my_variables.put("«vr.name.name»",«vr.type.storeExpress»);'''
//			VariableReferenceReceiver:
//				"receiver_store."
//			VariableReferenceSender:
//				"sender_store."
//			VariableReferenceGlobal:
//				"global_store."
//			RecordReferencePure: '''their_variables.put("«vr.name.name»",«vr.type.storeExpress»);'''
//			RecordReferenceMy: '''my_variables.put("«vr.name.name»",«vr.type.storeExpress»);'''
//			RecordReferenceReceiver:
//				"receiver_store."
//			RecordReferenceSender:
//				"sender_store."
//			RecordReferenceGlobal:
//				"global_store."
//		}
//	}
//
//	def String getStoreOutputPredicate(VariableReference vr) {
//		switch (vr) {
//			VariableReferencePure: '''«vr.name.type.express» «vr.name.name» = their_store.get("«vr.name.name»",«vr.name.type.storeExpress»);'''
//			VariableReferenceMy: '''«vr.name.type.express» «vr.name.name» = my_store.get("«vr.name.name»",«vr.name.type.storeExpress»);'''
//			VariableReferenceReceiver:
//				"receiver_store."
//			VariableReferenceSender:
//				"sender_store."
//			VariableReferenceGlobal:
//				"global_store."
//			RecordReferencePure: '''«vr.name.type.express» «vr.name.name» = their_store.get("«vr.name.name»",«vr.name.type.storeExpress»);'''
//			RecordReferenceMy: '''«vr.name.type.express» «vr.name.name» = my_store.get("«vr.name.name»",«vr.name.type.storeExpress»);'''
//			RecordReferenceReceiver:
//				"receiver_store."
//			RecordReferenceSender:
//				"sender_store."
//			RecordReferenceGlobal:
//				"global_store."
//		}
//	}
//
//	def String getOutputUpdate(Action action) {
//		if (action.eAllOfType(Update).size > 0) {
//			'''
//				@Override
//				protected CarmaStoreUpdate getUpdate() {
//					return new CarmaStoreUpdate() {
//						
//						@Override
//						public void update(RandomGenerator r, CarmaStore my_store) {
//							«action.outputUpdateBlock»
//						}
//					};
//				}
//			'''
//		} else {
//			'''
//				@Override
//				protected CarmaStoreUpdate getUpdate() {
//					return null;
//				}
//			'''
//		}
//	}
//
//	def String outputUpdateBlock(Action action) {
//		var update = action.eAllOfType(Update).get(0)
//		var updateAssignments = update.eAllOfType(UpdateAssignment)
//		var vrs = new HashMap<String, VariableReference>()
//		for (updateAssignment : updateAssignments)
//			for (vr : updateAssignment.eAllOfType(VariableReference))
//				vrs.put(vr.name.name, vr)
//
//		'''
//			HashMap<String,Class> my_variables = new HashMap<String,Class>();
//			«FOR key : vrs.keySet»
//				«vrs.get(key).checkStoreOutput»
//			«ENDFOR»
//			boolean hasAttributes = true;
//			if(my_variables != null)
//				for(String key : my_variables.keySet()){
//					hasAttributes = my_store.has(key,my_variables.get(key)) && hasAttributes;
//				}
//			if(hasAttributes){
//				«FOR key : vrs.keySet»
//					«vrs.get(key).storeOutput»
//				«ENDFOR»
//				«FOR updateAssignment : updateAssignments»
//					«updateAssignment.reference.name.name» = «updateAssignment.expression.express»;
//				«ENDFOR»
//				«FOR updateAssignment : updateAssignments»
//					my_store.set("«updateAssignment.reference.name.name»",«updateAssignment.reference.name.name»);
//				«ENDFOR»
//			}
//		'''
//	}
//
//	def String checkStoreOutput(VariableReference vr) {
//		switch (vr) {
//			VariableReferencePure: '''my_variables.put("«vr.name.name»",«vr.type.storeExpress»);'''
//			VariableReferenceMy: '''my_variables.put("«vr.name.name»",«vr.type.storeExpress»);'''
//			VariableReferenceReceiver:
//				"receiver_store."
//			VariableReferenceSender:
//				"sender_store."
//			VariableReferenceGlobal:
//				"global_store."
//			RecordReferencePure: '''my_variables.put("«vr.name.name»",«vr.type.storeExpress»);'''
//			RecordReferenceMy: '''my_variables.put("«vr.name.name»",«vr.type.storeExpress»);'''
//			RecordReferenceReceiver:
//				"receiver_store."
//			RecordReferenceSender:
//				"sender_store."
//			RecordReferenceGlobal:
//				"global_store."
//		}
//	}
//
//	def String getStoreOutput(VariableReference vr) {
//		switch (vr) {
//			VariableReferencePure: '''«vr.name.type.express» «vr.name.name» = my_store.get("«vr.name.name»",«vr.type.storeExpress»);'''
//			VariableReferenceMy: '''«vr.name.type.express» «vr.name.name» = my_store.get("«vr.name.name»",«vr.type.storeExpress»);'''
//			VariableReferenceReceiver:
//				"receiver_store."
//			VariableReferenceSender:
//				"sender_store."
//			VariableReferenceGlobal:
//				"global_store."
//			RecordReferencePure: '''«vr.name.type.express» «vr.name.name» = my_store.get("«vr.name.name»",«vr.type.storeExpress»);'''
//			RecordReferenceMy: '''«vr.name.type.express» «vr.name.name» = my_store.get("«vr.name.name»",«vr.type.storeExpress»);'''
//			RecordReferenceReceiver:
//				"receiver_store."
//			RecordReferenceSender:
//				"sender_store."
//			RecordReferenceGlobal:
//				"global_store."
//		}
//	}
//
//	def String getValues(Action action) {
//		''''''
////		if (action.eAllOfType(OutputActionArguments).size > 0) {
////			'''
////				@Override
////				protected Object getValue(CarmaStore my_store) {
////					«action.eAllOfType(OutputActionArguments).get(0).defineValueBlock»
////				}
////			'''
////		} else {
////			'''
////				@Override
////				protected Object getValue(CarmaStore my_store) {
////					return new Object();
////				}
////			'''
////		}
//	}
//
////	def String defineValueBlock(OutputActionArguments arguments) {
////		var ArrayList<OutputActionArgument> args = new ArrayList<OutputActionArgument>(
////			arguments.eAllOfType(OutputActionArgument))
////		var count = 0
////		'''
////			int[] output = new int[«args.size»];
////			HashMap<String,Class> my_variables = new HashMap<String,Class>();
////			«FOR arg : args»
////				«arg.checkStoreOutput»
////			«ENDFOR»
////			boolean hasAttributes = true;
////			if(my_variables != null)
////				for(String key : my_variables.keySet()){
////					hasAttributes = my_store.has(key,my_variables.get(key)) && hasAttributes;
////				}
////			if(hasAttributes){
////				«FOR arg : args»
////					«arg.storeOutput»
////				«ENDFOR»
////				«FOR arg : args»
////					output[«count++»] = «arg.javanise»;
////				«ENDFOR»
////				return output;
////			} else {
////				return new Object();
////			}
////		'''
////	}
//
//	def String checkStoreOutput(OutputActionArgument oaa) {
//		switch (oaa.value) {
//			VariableReferenceMy: '''my_variables.put("«(oaa.value as VariableReference).name.name»",«(oaa.value as VariableReference).type.storeExpress»);'''
//			RecordReferenceMy: '''my_variables.put("«(oaa.value as VariableReference).name.name»",«(oaa.value as VariableReference).type.storeExpress»);'''
//		}
//	}
//
//	def String getStoreOutput(OutputActionArgument oaa) {
//		switch (oaa.value) {
//			VariableReferenceMy: '''«(oaa.value as VariableReference).name.type.express» «(oaa.value as VariableReference).name.name» = my_store.get("«(oaa.value as VariableReference).name.name»",«(oaa.value as VariableReference).type.storeExpress»);'''
//			RecordReferenceMy: '''«(oaa.value as VariableReference).name.type.express» «(oaa.value as VariableReference).name.name» = my_store.get("«(oaa.value as VariableReference).name.name»",«(oaa.value as VariableReference).type.storeExpress»);'''
//		}
//	}
//
//	def String getActionInput(String actionName, boolean isBroadcast, Action action) {
//		'''
//			CarmaInput «actionName» = new CarmaInput( «action.actionName», «isBroadcast» ) {
//				«getInputActionPredicate(action)»
//				«getInputUpdate(action)»
//			};
//		'''
//	}
//
//	def String getInputActionPredicate(Action action) {
//		if (action.eAllOfType(ActionGuard).size > 0) {
//			'''
//				@Override
//				protected CarmaPredicate getPredicate(final CarmaStore my_store, final Object value) {
//					if (value instanceof int[]){
//						return new CarmaPredicate() {
//							@Override
//							public boolean satisfy(CarmaStore their_store) {
//								«getInputSatisfyBlock(action)»
//							}
//						};
//					}
//					return null;
//				}
//			'''
//		} else {
//			'''
//				@Override
//				protected CarmaPredicate getPredicate(final CarmaStore my_store, final Object value) {
//					if (value instanceof int[]){
//						return new CarmaPredicate() {
//							@Override
//							public boolean satisfy(CarmaStore their_store) {
//								return true;
//							}
//						};
//					}
//					return null;
//				}
//			'''
//		}
//	}
//
//	def String getInputSatisfyBlock(Action action) {
//		''''''
////		var BooleanExpression bes = action.eAllOfType(ActionGuard).get(0).booleanExpression
////		var vrs = bes.eAllOfType(VariableReference)
////		'''
////			HashMap<String,Class> my_variables = new HashMap<String,Class>();
////			«FOR vr : vrs»
////				«vr.checkStoreInput»
////			«ENDFOR»
////			«setupInputArguments(action.eAllOfType(InputActionParameters).get(0))»
////			boolean hasAttributes = true;
////			if(my_variables != null)
////				for(String key : my_variables.keySet()){
////					hasAttributes = my_store.has(key,my_variables.get(key)) && hasAttributes;
////				}
////			if(hasAttributes){
////				«FOR vr : vrs»
////					«vr.storeInput»
////				«ENDFOR»
////				return «bes.express»;
////			} else {
////				return false;
////			}
////		'''
//	}
//
//	def String checkStoreInput(VariableReference vr) {
//		switch (vr) {
//			VariableReferencePure: ''''''
//			VariableReferenceMy: '''my_variables.put("«vr.name.name»",«vr.type.storeExpress»);'''
//			VariableReferenceReceiver:
//				"receiver_store."
//			VariableReferenceSender:
//				"sender_store."
//			VariableReferenceGlobal:
//				"global_store."
//			RecordReferencePure: ''''''
//			RecordReferenceMy: '''my_variables.put("«vr.name.name»",«vr.type.storeExpress»);'''
//			RecordReferenceReceiver:
//				"receiver_store."
//			RecordReferenceSender:
//				"sender_store."
//			RecordReferenceGlobal:
//				"global_store."
//		}
//	}
//
//	def String getStoreInput(VariableReference vr) {
//		''''''
////		switch (vr) {
////			VariableReferencePure: {
////				if (vr.getContainerOfType(InputActionParameters) !=
////					null
////				) '''«vr.name.type.express» «vr.name.name» = my_store.get("«vr.name.name»",«vr.type.storeExpress»);''' else ''''''
////			}
////			VariableReferenceMy: '''«vr.name.type.express» «vr.name.name» = my_store.get("«vr.name.name»",«vr.type.storeExpress»);'''
////			VariableReferenceReceiver:
////				"receiver_store."
////			VariableReferenceSender:
////				"sender_store."
////			VariableReferenceGlobal:
////				"global_store."
////			RecordReferencePure: {
////				if (vr.getContainerOfType(InputActionParameters) !=
////					null
////				) '''«vr.name.type.express» «vr.name.name» = my_store.get("«vr.name.name»",«vr.type.storeExpress»);''' else ''''''
////			}
////			RecordReferenceMy: '''«vr.name.type.express» «vr.name.name» = my_store.get("«vr.name.name»",«vr.type.storeExpress»);'''
////			RecordReferenceReceiver:
////				"receiver_store."
////			RecordReferenceSender:
////				"sender_store."
////			RecordReferenceGlobal:
////				"global_store."
////		}
//	}
//
//	def String getInputUpdate(Action action) {
//		if (action.eAllOfType(Update).size > 0) {
//			'''
//				@Override
//				protected CarmaStoreUpdate getUpdate(final Object value) {
//					
//					return new CarmaStoreUpdate() {
//						@Override
//						public void update(RandomGenerator r, CarmaStore my_store) {
//							if (value instanceof int[]){
//								«action.inputUpdateBlock»
//							};
//						};
//					
//					};
//				};
//			'''
//		} else {
//			'''
//				@Override
//				protected CarmaStoreUpdate getUpdate(final Object value) {
//					
//					return new CarmaStoreUpdate() {
//						@Override
//						public void update(RandomGenerator r, CarmaStore my_store) {
//							return null;
//						};
//					
//					};
//				};
//			'''
//		}
//	}
//
//	def String inputUpdateBlock(Action action) {
//		''''''
////		var update = action.eAllOfType(Update).get(0)
////		var updateAssignments = update.eAllOfType(UpdateAssignment)
////		var vrs = new HashMap<String, VariableReference>()
////		for (updateAssignment : updateAssignments)
////			for (vr : updateAssignment.eAllOfType(VariableReference))
////				vrs.put(vr.name.name, vr)
////		'''
////			HashMap<String,Class> my_variables = new HashMap<String,Class>();
////			«FOR key : vrs.keySet»
////				«vrs.get(key).checkStoreInput»
////			«ENDFOR»
////			«setupInputArguments(action.eAllOfType(InputActionParameters).get(0))»
////			boolean hasAttributes = true;
////			if(my_variables != null)
////				for(String key : my_variables.keySet()){
////					hasAttributes = my_store.has(key,my_variables.get(key)) && hasAttributes;
////				}
////			if(hasAttributes){
////				«FOR key : vrs.keySet»
////					«vrs.get(key).storeInput»
////				«ENDFOR»
////				«FOR updateAssignment : updateAssignments»
////					«updateAssignment.reference.type.express» «updateAssignment.reference.name.name» = my_store.get("«updateAssignment.reference.name.name»",«updateAssignment.reference.type.storeExpress»);
////					«updateAssignment.reference.name.name» = «updateAssignment.expression.express»;
////				«ENDFOR»
////				«FOR updateAssignment : updateAssignments»
////					my_store.set("«updateAssignment.reference.name.name»",«updateAssignment.reference.name.name»);
////				«ENDFOR»
////			}
////		'''
//	}
//
////	def String setupInputArguments(InputActionParameters parameters) {
////		var ArrayList<VariableName> vns = new ArrayList<VariableName>(parameters.eAllOfType(VariableName))
////		'''
////			«FOR vn : vns»
////				int «vn.name» = ((int[]) value)[«vns.indexOf(vn)»];
////			«ENDFOR»
////		'''
////	}
//
//	def String getGuardSatisfyBlock(BooleanExpression bes) {
//		var vrs = bes.eAllOfType(VariableReference)
//		'''			
//		«FOR vr : vrs»
//		«vr.name.type.express» «vr.express» = store.get("«vr.name.name»",«vr.name.type.storeExpress»);
//		if («vr.express» == null) {
//			return false;
//		}
//		«ENDFOR»
//		return «bes.express»;
//		'''
//	}


}