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

class GenerateSystemProbability {
	
	@Inject extension TypeProvider
	@Inject extension LabelUtil
	@Inject extension Util
	@Inject extension GeneratorUtils
	
	def String defineEnvironmentProbPredicates(System system){
		
		var updates = system.getContainerOfType(Model).eAllOfType(Probability)
		var unicasts = new ArrayList<Probability>()
		var broadcasts = new ArrayList<Probability>()
		
		for(eu : updates)
			if(eu.stub.isBroadcast)
				broadcasts.add(eu)
			else
				unicasts.add(eu)
		'''
		/*ENVIRONMENT PROBABILITY PREDICATES*/
		//BROADCAST ENVIRONMENT PROBABILITY PREDICATES
		«FOR broadcast : broadcasts»
		«defineBroadcastProbabilityPredicates(broadcast)»
		«ENDFOR»
		
		//UNICAST ENVIRONMENT PROBABILITY PREDICATES
		«FOR unicast : unicasts»
		«defineUnicastProbabilityPredicates(unicast)»
		«ENDFOR»
		'''
		
	}
	
	def String defineBroadcastProbabilityPredicates(Probability cast){
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
	
	def String defineUnicastProbabilityPredicates(Probability cast){
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

	def String defineEnvironmentProb(System system){
		
		var probs = system.getContainerOfType(Model).eAllOfType(Probability)
		var unicasts = new ArrayList<Probability>()
		var broadcasts = new ArrayList<Probability>()
		
		for(eu : probs)
			if(eu.stub.isBroadcast)
				broadcasts.add(eu)
			else
				unicasts.add(eu)
		'''
		/*ENVIRONMENT PROBABILITY*/
		@Override
		public double broadcastProbability(CarmaStore sender, CarmaStore receiver,
		int action) {
			«FOR broadcast : broadcasts»
			«defineProbActionStubs(broadcast.stub)»
			«ENDFOR»
			return 0;
		}
		
		@Override
		public double unicastProbability(CarmaStore sender, CarmaStore receiver,
		int action) {
			«FOR unicast : unicasts»
			«defineProbActionStubs(unicast.stub)»
			«ENDFOR»
			return 0;
		}
		'''
	}
	
	def String defineProbActionStubs(ActionStub actionStub){
		'''
		if (action == «actionStub.getContainerOfType(Model).label»Definition.«actionStub.name.name.toUpperCase»
		&& «actionStub.predicateHandlerProbability») {
				«actionStub.defineProbActionStub»
		}
		'''
	}
	
	def String predicateHandlerProbability(ActionStub actionStub){
		var booleanExpression = actionStub.getContainerOfType(Probability).eAllOfType(EnvironmentGuard).get(0)
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
			var cast = be.getContainerOfType(Probability) 
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
	
	def String defineProbActionStub(ActionStub actionStub){
		'''return «actionStub.getContainerOfType(Probability).expression.label»;'''
	}
	

}