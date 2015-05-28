package eu.quanticol.carma.core.generator

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.ActionStub
import eu.quanticol.carma.core.carma.BlockSystem
import eu.quanticol.carma.core.carma.BooleanExpressions
import eu.quanticol.carma.core.carma.Component
import eu.quanticol.carma.core.carma.ComponentArgument
import eu.quanticol.carma.core.carma.ComponentBlockDefinition
import eu.quanticol.carma.core.carma.ComponentBlockForStatement
import eu.quanticol.carma.core.carma.ComponentBlockNewDeclaration
import eu.quanticol.carma.core.carma.EnvironmentGuard
import eu.quanticol.carma.core.carma.EnvironmentUpdate
import eu.quanticol.carma.core.carma.EnvironmentUpdateAssignment
import eu.quanticol.carma.core.carma.LineSystem
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.NCA
import eu.quanticol.carma.core.carma.PrimitiveType
import eu.quanticol.carma.core.carma.Probability
import eu.quanticol.carma.core.carma.Range
import eu.quanticol.carma.core.carma.Rate
import eu.quanticol.carma.core.carma.RecordDeclaration
import eu.quanticol.carma.core.carma.RecordReferenceReceiver
import eu.quanticol.carma.core.carma.RecordReferenceSender
import eu.quanticol.carma.core.carma.Spawn
import eu.quanticol.carma.core.carma.System
import eu.quanticol.carma.core.carma.VariableDeclarationEnum
import eu.quanticol.carma.core.carma.VariableDeclarationRecord
import eu.quanticol.carma.core.carma.VariableReferenceReceiver
import eu.quanticol.carma.core.carma.VariableReferenceSender
import eu.quanticol.carma.core.typing.TypeProvider
import eu.quanticol.carma.core.utils.LabelUtil
import eu.quanticol.carma.core.utils.Util
import java.util.ArrayList

import static extension org.eclipse.xtext.EcoreUtil2.*
import java.util.HashSet
import eu.quanticol.carma.core.carma.VariableReference
import eu.quanticol.carma.core.carma.VariableReferencePure
import eu.quanticol.carma.core.carma.RecordReferencePure
import eu.quanticol.carma.core.carma.VariableReferenceGlobal
import eu.quanticol.carma.core.carma.RecordReferenceGlobal
import eu.quanticol.carma.core.carma.EnvironmentUpdateExpression
import eu.quanticol.carma.core.carma.EnvironmentUpdateExpressions

class GenerateSystemEnvironmentUpdate {
	
	@Inject extension TypeProvider
	@Inject extension LabelUtil
	@Inject extension Util
	@Inject extension GeneratorUtils
	
	def String defineEnvironmentUpdatesPredicates(System system){
		
		var updates = system.getContainerOfType(Model).eAllOfType(EnvironmentUpdate)
		var unicasts = new ArrayList<EnvironmentUpdate>()
		var broadcasts = new ArrayList<EnvironmentUpdate>()
		
		for(eu : updates)
			if(eu.stub.isBroadcast)
				broadcasts.add(eu)
			else
				unicasts.add(eu)
		'''
		/*ENVIRONMENT UPDATE PREDICATES*/
		//BROADCAST ENVIRONMENT UPDATE PREDICATES
		«FOR broadcast : broadcasts»
		«defineBroadcastEnvironmentUpdatePredicates(broadcast)»
		«ENDFOR»
		
		//UNICAST ENVIRONMENT UPDATE PREDICATES
		«FOR unicast : unicasts»
		«defineUnicastEnvironmentUpdatePredicates(unicast)»
		«ENDFOR»
		'''
		
	}
	
	def String defineBroadcastEnvironmentUpdatePredicates(EnvironmentUpdate cast){
		//anything NOT receiver or sender is a global store access
		var senders 	= (cast.guard.eAllOfType(RecordReferenceSender).size + cast.guard.eAllOfType(VariableReferenceSender).size) > 0
		var receivers	= (cast.guard.eAllOfType(RecordReferenceReceiver).size + cast.guard.eAllOfType(VariableReferenceReceiver).size) > 0
		
		//no! global_store always accessible!
		
		//either single with no args
		if(senders && !receivers){ 
			'''
			public static CarmaPredicate «cast.convertToPredicateName»(){
				return new CarmaPredicate() {

					@Override
					public boolean satisfy(CarmaStore sender) {
						«cast.guard.booleanExpression.getAllVariablesEnv»
						return «cast.guard.convertToJava»;
					}
					
				};
			}'''
		} else if (!senders && receivers){
			'''
			public static CarmaPredicate «cast.convertToPredicateName»(){
				return new CarmaPredicate() {

					@Override
					public boolean satisfy(CarmaStore receiver) {
						«cast.guard.booleanExpression.getAllVariablesEnv»
						return «cast.guard.convertToJava»;
					}
					
				};
			}'''
		//or double with sender always given first, and receiver sent for evaluation?
		} else if (senders && receivers) {
			'''
			public static CarmaPredicate «cast.convertToPredicateName»(CarmaStore sender){
				return new CarmaPredicate() {

					@Override
					public boolean satisfy(CarmaStore receiver) {
						«cast.guard.booleanExpression.getAllVariablesEnv»
						return «cast.guard.convertToJava»;
					}
					
				};
			}'''
		//or just global, no one cares about the store
		} else if (cast.guard.eAllOfType(VariableReference).size > 0){
			'''
			public static CarmaPredicate «cast.convertToPredicateName»(){
				return new CarmaPredicate() {

					@Override
					public boolean satisfy(CarmaStore store) {
						«cast.guard.booleanExpression.getAllVariablesEnv»
						return «cast.guard.convertToJava»;
					}
					
				};
			}'''
		}
		
		
	}
	
	def String defineUnicastEnvironmentUpdatePredicates(EnvironmentUpdate cast){
		//anything NOT receiver or sender is a global store access
		var senders 	= (cast.guard.eAllOfType(RecordReferenceSender).size + cast.guard.eAllOfType(VariableReferenceSender).size) > 0
		var receivers	= (cast.guard.eAllOfType(RecordReferenceReceiver).size + cast.guard.eAllOfType(VariableReferenceReceiver).size) > 0
		//no! global_store always accessible!
		
		//either single with no args
		if(senders && !receivers){ 
			'''
			public static CarmaPredicate «cast.convertToPredicateName»(){
				return new CarmaPredicate() {

					@Override
					public boolean satisfy(CarmaStore sender) {
						«cast.guard.booleanExpression.getAllVariablesEnv»
						return «cast.guard.convertToJava»;
					}
					
				};
			}'''
		} else if (!senders && receivers){
			'''
			public static CarmaPredicate «cast.convertToPredicateName»(){
				return new CarmaPredicate() {

					@Override
					public boolean satisfy(CarmaStore receiver) {
						«cast.guard.booleanExpression.getAllVariablesEnv»
						return «cast.guard.convertToJava»;
					}
					
				};
			}'''
		//or double with sender always given first, and receiver sent for evaluation?
		} else if (senders && receivers) {
			'''
			public static CarmaPredicate «cast.convertToPredicateName»(CarmaStore sender){
				return new CarmaPredicate() {

					@Override
					public boolean satisfy(CarmaStore receiver) {
						«cast.guard.booleanExpression.getAllVariablesEnv»
						return «cast.guard.convertToJava»;
					}
					
				};
			}'''
		//or just global, no one cares about the store
		} else if (cast.guard.eAllOfType(VariableReference).size > 0){
			'''
			public static CarmaPredicate «cast.convertToPredicateName»(){
				return new CarmaPredicate() {

					@Override
					public boolean satisfy(CarmaStore store) {
						«cast.guard.booleanExpression.getAllVariablesEnv»
						return «cast.guard.convertToJava»;
					}
					
				};
			}'''
		}
		
		
	}
	
	def String defineEnvironmentUpdates(System system){
		
		var updates = system.getContainerOfType(Model).eAllOfType(EnvironmentUpdate)
		var unicasts = new ArrayList<EnvironmentUpdate>()
		var broadcasts = new ArrayList<EnvironmentUpdate>()
		
		for(eu : updates)
			if(eu.stub.isBroadcast)
				broadcasts.add(eu)
			else
				unicasts.add(eu)
		'''
		/*ENVIRONMENT UPDATE*/
		@Override
		public void broadcastUpdate(RandomGenerator random, CarmaStore sender, 
		int action, Object value) {
			«FOR broadcast : broadcasts»
			«defineEUpdateActionStubs(broadcast.stub)»
			«ENDFOR»
		}
		
		@Override
		public void unicastUpdate(RandomGenerator random, CarmaStore sender, CarmaStore receiver,
		int action, Object value) {
			«FOR unicast : unicasts»
			«defineEUpdateActionStubs(unicast.stub)»
			«ENDFOR»
		}
		'''
		
	}
	
	def String defineEUpdateActionStubs(ActionStub actionStub){
		'''
		if (action == «actionStub.getContainerOfType(Model).label»Definition.«actionStub.name.name.toUpperCase» 
		&& «actionStub.predicateHandlerEnvironmentUpdate») {
		}
		'''
	}
	
	def String predicateHandlerEnvironmentUpdate(ActionStub actionStub){
		var booleanExpression = actionStub.getContainerOfType(EnvironmentUpdate).eAllOfType(EnvironmentGuard).get(0)
		'''
		«booleanToPredicate(booleanExpression.booleanExpression)»
		'''
	}
	
	def String booleanToPredicate(BooleanExpressions be){
		if(be.label.equals("True") || be.label.equals("true")){
			return '''CarmaPredicate.TRUE.satisfy(sender)'''
		}
		else if(be.label.equals("False") || be.label.equals("false")){
			return '''CarmaPredicate.FALSE.satisfy(sender)'''
		}
		else {
			var cast = be.getContainerOfType(EnvironmentUpdate) 
			var senders 	= (cast.eAllOfType(RecordReferenceSender).size + cast.eAllOfType(VariableReferenceSender).size) > 0
			var receivers	= (cast.eAllOfType(RecordReferenceReceiver).size + cast.eAllOfType(VariableReferenceReceiver).size) > 0
			if(senders && !receivers){
				'''«cast.convertToPredicateName»().satisfy(sender)'''
			} else if (!senders && receivers) {
				'''«cast.convertToPredicateName»().satisfy(receiver)'''
			} else if (senders && receivers) {
				'''«cast.convertToPredicateName»(sender).satisfy(receiver)'''
			} else {
				'''«cast.convertToPredicateName»().satisfy(sender)'''
			}
		}
	}
	
	def String defineEUpdateActionStub(ActionStub actionStub){
		if(actionStub.getContainerOfType(EnvironmentUpdate).eAllOfType(Spawn).size > 0){
			'''//spawns'''
		} else if (actionStub.getContainerOfType(EnvironmentUpdate).eAllOfType(EnvironmentUpdateAssignment).size > 0){
			'''
			«FOR e :actionStub.getContainerOfType(EnvironmentUpdate).expression»
			«e.anAssignment»
			«ENDFOR»
			'''
		} else {
			'''//«actionStub.label» invalid'''
		}
	}
	
}