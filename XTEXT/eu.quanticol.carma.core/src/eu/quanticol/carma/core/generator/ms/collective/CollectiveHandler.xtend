package eu.quanticol.carma.core.generator.ms.collective

import eu.quanticol.carma.core.utils.Util
import eu.quanticol.carma.core.typing.TypeSystem
import eu.quanticol.carma.core.generator.ms.expression.ExpressionHandler

import eu.quanticol.carma.core.carma.ProcessExpressionAction
import eu.quanticol.carma.core.carma.ProcessExpressionNext
import eu.quanticol.carma.core.carma.ProcessExpressionNil
import eu.quanticol.carma.core.carma.ProcessExpressionKill
import eu.quanticol.carma.core.carma.ProcessExpressionReference
import eu.quanticol.carma.core.carma.ComponentDefinition
import eu.quanticol.carma.core.carma.InputAction
import eu.quanticol.carma.core.carma.OutputAction
import eu.quanticol.carma.core.utils.ReferenceContext
import com.google.inject.Inject
import eu.quanticol.carma.core.generator.ms.attribute.AttributeHandler
import eu.quanticol.carma.core.carma.ProcessExpressionChoice
import eu.quanticol.carma.core.carma.ProcessExpressionGuard
import eu.quanticol.carma.core.carma.ParallelComposition
import eu.quanticol.carma.core.carma.ProcessReference
import eu.quanticol.carma.core.carma.ReferenceableElement
import eu.quanticol.carma.core.carma.Variable
import eu.quanticol.carma.core.carma.ProcessState
import eu.quanticol.carma.core.carma.ComponentBlockInstantiation
import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.core.carma.Range
import eu.quanticol.carma.core.carma.Expression
import eu.quanticol.carma.core.carma.ComponentBlockForStatement
import eu.quanticol.carma.core.carma.ComponentBlockConditionalStatement

class CollectiveHandler {

	@Inject extension TypeSystem
	@Inject extension ExpressionHandler
	@Inject extension Util
	@Inject extension AttributeHandler

	def CharSequence methofForComponentBehaviourCreation( ComponentDefinition comp ) {
		'''generate«comp.name»Behaviour( )'''
	}

	def CharSequence componentBehaviour( ComponentDefinition comp , Iterable<ProcessState> global ) {
		'''
		
		/* START COMPONENT: «comp.name»         */
		
		/* DEFINITIONS OF PROCESSES */
		public final CarmaProcessAutomaton _COMP_«comp.name» = new CarmaProcessAutomaton("«comp.name»");
		«FOR p:global»
		public final CarmaProcessAutomaton.State «p.name.stateName(comp.name)» = _COMP_«comp.name».newState("«p.name»");
		«ENDFOR»
		
		«FOR p:comp.processes.processes»
		public final CarmaProcessAutomaton.State «p.name.stateName(comp.name)» = _COMP_«comp.name».newState("«p.name»");		
		«ENDFOR»

		private void «comp.methofForComponentBehaviourCreation» {
			
			«FOR p:global»			
			«p.processExpression.componentTransitions(p.name,comp.name,newLinkedList(),0)»
			«ENDFOR»
			
			«FOR p:comp.processes.processes»
			«p.processExpression.componentTransitions(p.name,comp.name,newLinkedList(),0)»
			«ENDFOR»
			
		}
		
		public CarmaComponent createComponent«comp.name»( 
			«FOR v:comp.parameters SEPARATOR ','»«v.type.toJavaType» «v.name.variableName» «ENDFOR» 
		) {
			CarmaComponent c = new CarmaComponent();
			«IF comp.store != null»
			«FOR a:comp.store.attributes»
			c.set( "«a.name»" ,  «a.value.expressionToJava» );
			«ENDFOR»
			«ENDIF»
			«comp.initBlock.init.processInstantiation(comp.name)»
			return c;
		}	
		
		/* END COMPONENT: «comp.name» */
			
		'''
	}
	
	def dispatch instantiationCode( ComponentBlockInstantiation cbi ) {
		var ranges = cbi.getAllContentsOfType(typeof(Range)).indexed;
		cbi.recursiveIntantiationCode(ranges,0) 
	}
	
	def CharSequence recursiveIntantiationCode( ComponentBlockInstantiation cbi , Iterable<Pair<Integer,Range>> ranges , int idx ) {
		if (idx<ranges.size) {
			var r = ranges.get(idx)
			'''
			for ( int i«r.key» = «r.value.min.expressionToJava» ; 
				i«r.key» <= «r.value.max.expressionToJava» ; 
				«IF r.value.step == null» i«r.key»++ «ELSE» i«r.key» += «r.value.step.expressionToJava» «ENDIF» ) {					
				«cbi.recursiveIntantiationCode(ranges,idx+1)»
			}
			'''
		} else {
			cbi.name.name.setCurrentComponent
			'''
			system.addComponent( 
				createComponent«cbi.name.name»(					
					«cbi.arguments.map[it.argumentCode(ranges)].invocationParameters(cbi.name.parameters.map[it.type])»
				) 
			);
			'''		
		}
	}
	
	def argumentCode( Expression e , Iterable<Pair<Integer,Range>> ranges ) {
		if (e instanceof Range) {
			'''i«ranges.findFirst[ it.value == e ].key»'''
		} else {
			e.expressionToJava
		}
	}
	
	def dispatch CharSequence processInstantiation( ParallelComposition p , String component ) {
		'''
		«p.left.processInstantiation(component)»
		«p.right.processInstantiation(component)»
		'''
	}

	def dispatch CharSequence processInstantiation( ProcessReference p , String component ) {
		'''
		c.addAgent( «p.expression.processReferenceCode(component)»);
		'''
	}
	
	def CharSequence processReferenceCode( ReferenceableElement e , String component ) {
		switch e {
			Variable: e.name.variableName
			ProcessState: '''new CarmaSequentialProcess( c , _COMP_«component» , «e.name.stateName(component)» )'''
			default: "null"
		}
	}

	def dispatch CharSequence componentTransitions( ProcessExpressionChoice p , String state , String component , Iterable<String> guards , int guardCounter ) {
		'''
		«p.left.componentTransitions(state,component,guards,guardCounter)»
		«p.right.componentTransitions(state,component,guards,guardCounter)»
		'''
	}
	
	def dispatch CharSequence componentTransitions( ProcessExpressionGuard p , String state , String component , Iterable<String> guards , int guardCounter ) {		
		var guardVar = '''_FOO_predicate«guardCounter»'''
		'''
		{
			CarmaPredicate «guardVar» = new CarmaPredicate() {

				//@Override
				public boolean satisfy(CarmaStore store) {
					«FOR a:p.guard.booleanExpression.referencedAttibutes»
					«a.attributeTemporaryVariableDeclaration(ReferenceContext::NONE,"store")»
					«ENDFOR»
					«FOR a:p.guard.booleanExpression.myAttributes»
					«a.attributeTemporaryVariableDeclaration(ReferenceContext::MY,"store")»
					«ENDFOR»
					return «p.guard.booleanExpression.expressionToJava»;
				}
					
			};
			«p.expression.componentTransitions(state,component,guards+newLinkedList( guardVar ),guardCounter+1)»
		}
		'''
	}
	
	def dispatch CharSequence componentTransitions( ProcessExpressionAction p , String state , String component , Iterable<String> guards , int guardCounter ) {
		'''
		{
			«p.action.actionCode»
			
			«IF guards.empty»			
			_COMP_«component».addTransition( «state.stateName(component)» , action , «p.next.nextCode(component)» );			
			«ELSE»
			_COMP_«component».addTransition( «state.stateName(component)» , new CarmaPredicate.Conjunction( «FOR s:guards SEPARATOR ','» «s» «ENDFOR» ) , action , «p.next.nextCode(component)» );			
			«ENDIF»
		}
		'''
	}
	
	def nextCode( ProcessExpressionNext next , String component ) {
		switch next {
			ProcessExpressionNil: '''null'''
			ProcessExpressionKill: '''null'''
			ProcessExpressionReference: '''«next.expression.name.stateName(component)»'''
		}
	}
	
	def dispatch CharSequence actionCode( InputAction act ) {
		var t = act.activity.inferActivityType
		var idxVar = act.parameters.indexed
		'''
		CarmaAction action = new CarmaInput( 
			«act.activity.name.actionName» , «act.activity.isIsBroadacst»  		
		) {
			
			@Override
			protected CarmaStoreUpdate getUpdate(final Object value, double now) {
				
				«IF act.update != null»
				LinkedList<Object> message = (LinkedList<Object>) value;
				«FOR idv:idxVar»
				final «t.get(idv.key).toJavaType(true)» «idv.value.name.variableName» = («t.get(idv.key).toJavaType(false)») message.get(«idv.key»);
				«ENDFOR»
				return new CarmaStoreUpdate() {
					
					//@Override
					public void update(RandomGenerator r, CarmaStore store) {
						«FOR a:act.update.referencedAttributes»
						«a.attributeTemporaryVariableDeclaration(ReferenceContext::NONE,"store")»
						«ENDFOR»
						«FOR a:act.update.myAttributes»
						«a.attributeTemporaryVariableDeclaration(ReferenceContext::MY,"store")»
						«ENDFOR»
						«FOR u:act.update.updateAssignment»
						store.set( "«u.reference.name»", «u.expression.expressionToJava» );
						«ENDFOR»
					}
				};
				«ELSE»
				return new CarmaStoreUpdate() {					
					//@Override
					public void update(RandomGenerator r, CarmaStore store) {
					}
				};
				«ENDIF»
							
			}	
			
			@Override
			protected CarmaPredicate getPredicate(CarmaStore myStore, Object value, double now) {
				«IF act.activity.predicate != null»				
				LinkedList<Object> message = (LinkedList<Object>) value;
				«FOR idv:idxVar»
				final «t.get(idv.key).toJavaType(true)» «idv.value.name.variableName» = («t.get(idv.key).toJavaType(false)») message.get(«idv.key»);
				«ENDFOR»
				«FOR a:act.activity.predicate.guard.myAttributes»
				«a.attributeTemporaryVariableDeclaration(ReferenceContext::MY,"myStore")»
				«ENDFOR»
				return new CarmaPredicate() {

					//@Override
					public boolean satisfy(CarmaStore store) {
						«FOR a:act.activity.predicate.guard.referencedAttibutes»
						«a.attributeTemporaryVariableDeclaration(ReferenceContext::NONE,"store")»
						«ENDFOR»
						return «act.activity.predicate.guard.expressionToJava»;
					}
					
				};
				«ELSE»
				return CarmaPredicate.TRUE;
				«ENDIF»
				
			}
						
		};		
		'''		
	}

	def dispatch CharSequence actionCode( OutputAction act ) {
		var isSpontaneous = (act.activity.predicate == null)&&(!act.withData)
		'''
		CarmaAction action = new CarmaOutput(
			«act.activity.name.actionName» , «act.activity.isIsBroadacst»  		
		) {
			
			@Override
			protected Object getValue(CarmaStore store, double now) {
				LinkedList<Object> toReturn = new LinkedList<Object>();
				«FOR a:act.outputArguments.referencedAttibutes»
				«a.attributeTemporaryVariableDeclaration(ReferenceContext::NONE,"store")»
				«ENDFOR»
				«FOR a:act.outputArguments.myAttributes»
				«a.attributeTemporaryVariableDeclaration(ReferenceContext::MY,"store")»
				«ENDFOR»
				«FOR e:act.outputArguments»
				toReturn.add( «e.expressionToJava» );
				«ENDFOR»
				return toReturn;
			}
			
			@Override
			protected CarmaStoreUpdate getUpdate( double now ) {
				«IF act.update != null»
				return new CarmaStoreUpdate() {
					
					//@Override
					public void update(RandomGenerator r, CarmaStore store) {
						«FOR a:act.update.referencedAttributes»
						«a.attributeTemporaryVariableDeclaration(ReferenceContext::NONE,"store")»
						«ENDFOR»
						«FOR a:act.update.myAttributes»
						«a.attributeTemporaryVariableDeclaration(ReferenceContext::MY,"store")»
						«ENDFOR»
						«FOR u:act.update.updateAssignment»
						store.set( "«u.reference.name»", «u.expression.expressionToJava» );
						«ENDFOR»
					}
				};
				«ELSE»
				return new CarmaStoreUpdate() {					
					//@Override
					public void update(RandomGenerator r, CarmaStore store) {
					}
				};
				«ENDIF»			
			}
			
			@Override
			protected CarmaPredicate getPredicate(final CarmaStore myStore, double now) {
				«IF isSpontaneous»
				return CarmaPredicate.FALSE;
				«ELSE»
				«IF act.activity.predicate != null»				
				«FOR a:act.activity.predicate.guard.myAttributes»
				«a.attributeTemporaryVariableDeclaration(ReferenceContext::MY,"myStore")»
				«ENDFOR»
				return new CarmaPredicate() {

					//@Override
					public boolean satisfy(CarmaStore store) {
						
						«FOR a:act.activity.predicate.guard.referencedAttibutes»
						«a.attributeTemporaryVariableDeclaration(ReferenceContext::NONE,"store")»
						«ENDFOR»
						return «act.activity.predicate.guard.expressionToJava»;
						
					}
					
				};
				«ELSE»
				return CarmaPredicate.TRUE;
				«ENDIF»
				«ENDIF»
				
			}
		};		
		'''
	}
	
	def dispatch CharSequence instantiationCode( ComponentBlockForStatement block ) {
		var varName = block.variable.name.variableName
		'''
		for( int «varName» = 0; «block.expression.expressionToJava» ; «varName» = «varName» + «block.afterThought.expressionToJava» ) {
			«FOR c:block.collective»
			«c.instantiationCode»
			«ENDFOR»			
		}
		'''
	}

	def dispatch CharSequence instantiationCode( ComponentBlockConditionalStatement block ) {
		'''
		if ( «block.guard.expressionToJava» ) {
			«FOR c:block.thenBranch»
			«c.instantiationCode»
			«ENDFOR»			
		}«IF block.elseBranch != null» else {
			«FOR c:block.elseBranch»
			«c.instantiationCode»
			«ENDFOR»			
		}
		«ENDIF»
		'''
	}

}