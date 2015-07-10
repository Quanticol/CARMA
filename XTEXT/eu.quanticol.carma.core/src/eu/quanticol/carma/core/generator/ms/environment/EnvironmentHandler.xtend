package eu.quanticol.carma.core.generator.ms.environment

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.Action
import eu.quanticol.carma.core.carma.BlockSystem
import eu.quanticol.carma.core.carma.BooleanExpression
import eu.quanticol.carma.core.carma.CarmaDouble
import eu.quanticol.carma.core.carma.EnvironmentUpdate
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.Probability
import eu.quanticol.carma.core.carma.ProbabilityBlock
import eu.quanticol.carma.core.carma.Rate
import eu.quanticol.carma.core.carma.RateBlock
import eu.quanticol.carma.core.carma.RecordReferenceGlobal
import eu.quanticol.carma.core.carma.RecordReferenceReceiver
import eu.quanticol.carma.core.carma.RecordReferenceSender
import eu.quanticol.carma.core.carma.System
import eu.quanticol.carma.core.carma.UpdateBlock
import eu.quanticol.carma.core.carma.VariableReference
import eu.quanticol.carma.core.carma.VariableReferenceGlobal
import eu.quanticol.carma.core.carma.VariableReferenceReceiver
import eu.quanticol.carma.core.carma.VariableReferenceSender
import eu.quanticol.carma.core.typing.TypeProvider
import eu.quanticol.carma.core.utils.Express
import java.util.ArrayList

import static extension org.eclipse.xtext.EcoreUtil2.*
import java.util.HashMap

class EnvironmentHandler {
	
	@Inject extension TypeProvider
	@Inject extension EnvironmentJavaniser
	@Inject extension Express
		
	def String getEnvironment(System system, Model model){
		'''
		«IF((system as BlockSystem).environment != null)»
			
			«var ProbabilityBlock probabilityBlock = (system as BlockSystem).environment.probabilityBlock»
			«var probabilityBrocast = new ArrayList<Probability>()»
			«var probabilityUnicast = new ArrayList<Probability>()»
			«var probabilityValue = probabilityBlock.value as CarmaDouble»
			«splitProbability(probabilityBrocast,probabilityUnicast,probabilityBlock)»
			«probabilityBlock.probabilityPredicates»
			«getBroadcastProbability(probabilityBrocast,probabilityValue)»
			«getUnicastProbability(probabilityUnicast,probabilityValue)»
			
			«var RateBlock rateBlock = (system as BlockSystem).environment.rateBlock»
			«var rateBrocast = new ArrayList<Rate>()»
			«var rateUnicast = new ArrayList<Rate>()»
			«var rateValue = rateBlock.value»
			
			«var UpdateBlock updateBlock = (system as BlockSystem).environment.updateBlock»
			«var updateBrocast = new ArrayList<EnvironmentUpdate>()»
			«var updateUnicast = new ArrayList<EnvironmentUpdate>()»
			
		«ENDIF»
		'''
	}
	
	def void splitProbability(ArrayList<Probability> probabilityBrocast, 
		ArrayList<Probability> probabilityUnicast, 
		ProbabilityBlock probabilityBlock){
		
		for(probability : probabilityBlock.eAllOfType(Probability)){
			if(probability.stub.name.getContainerOfType(Action).type.me.equals("broadcast")){
				probabilityBrocast.add(probability)
			} else {
				probabilityUnicast.add(probability)
			}
		}
		
	}
	
	def String probabilityPredicates(ProbabilityBlock probabilityBlock){
		'''
		«FOR probability : probabilityBlock.eAllOfType(Probability)»
		«probability.probabilityPredicates»
		«ENDFOR»
		'''
		
	}
	
	def String getProbabilityPredicates(Probability probability){
		'''
		public static CarmaPredicate get«probability.javanise»Predicate(CarmaStore sender_store){
			return new CarmaPredicate() {
				@Override
				public boolean satisfy(CarmaStore receiver_store) {
					«getSatisfyBlock(probability.guard.booleanExpression)»
				}
			};
		}
		'''
	}
	
	def String getBroadcastProbability(ArrayList<Probability> probabilities, CarmaDouble value){
		var defaulter = "0"
		if(value != null){
			defaulter = value.javanise
		}
		'''
		@Override
		public double broadcastProbability(CarmaStore sender_store, CarmaStore receiver_store,
				int action) {
			«FOR probability : probabilities»
			if(action == «probability.stub.name.getContainerOfType(Action).actionName»
			&& get«probability.javanise»Predicate(sender_store).satisfy(receiver_store){
			
			}
			«ENDFOR»
			return «defaulter»;
		}		
		'''
	}



	def String getUnicastProbability(ArrayList<Probability> probabilities, CarmaDouble value){
		var defaulter = "0"
		if(value != null){
			defaulter = value.javanise
		}		
		'''
		@Override
		public double unicastProbability(CarmaStore sender_store, CarmaStore receiver_store,
				int action) {
			«FOR probability : probabilities»
			if(action == «probability.stub.name.getContainerOfType(Action).actionName»
			&& get«probability.javanise»Predicate(sender_store).satisfy(receiver_store)){
				
			}
			«ENDFOR»
			return «defaulter»;
		}		
		'''
	}
	
	def String getSatisfyBlock(BooleanExpression bes){
		var vrs = bes.eAllOfType(VariableReference)
		var vrsh = new HashMap<String,VariableReference>()
		for(vr : vrs){
			vrsh.put(vr.name.name, vr)
		}
		'''
		HashMap<String,Class> receiver_variables = new HashMap<String,Class>();
		HashMap<String,Class> sender_variables = new HashMap<String,Class>();
		HashMap<String,Class> global_variables = new HashMap<String,Class>();
		«FOR key : vrsh.keySet»
		«vrsh.get(key).checkStorePredicate»
		«ENDFOR»
		boolean hasAttributes = true;
		if(receiver_variables != null)
			for(String key : receiver_variables.keySet()){
				hasAttributes = receiver_store.has(key,receiver_variables.get(key)) && hasAttributes;
			}
		if(sender_variables != null)
			for(String key : sender_variables.keySet()){
				hasAttributes = sender_store.has(key,sender_variables.get(key)) && hasAttributes;
			}
		if(global_variables != null)
			for(String key : global_variables.keySet()){
				hasAttributes = global_store.has(key,global_variables.get(key)) && hasAttributes;
			}
		if(hasAttributes){
			«FOR key : vrsh.keySet»
			«vrsh.get(key).getStorePredicate»
			«ENDFOR»
			return «bes.express»;
		} else {
			return false;
		}
		'''
	}
	
	def String checkStorePredicate(VariableReference vr){
		switch (vr) {
			VariableReferenceReceiver: 	'''receiver_variables.put("«vr.name.name»",«vr.type.storeExpress»);'''
			VariableReferenceSender: 	'''sender_variables.put("«vr.name.name»",«vr.type.storeExpress»);'''
			VariableReferenceGlobal: 	'''global_variables.put("«vr.name.name»",«vr.type.storeExpress»);'''
			RecordReferenceReceiver: 	'''receiver_variables.put("«vr.name.name»",«vr.type.storeExpress»);'''
			RecordReferenceSender: 		'''sender_variables.put("«vr.name.name»",«vr.type.storeExpress»);'''
			RecordReferenceGlobal: 		'''global_variables.put("«vr.name.name»",«vr.type.storeExpress»);'''
		}
	}
	
	def String getStorePredicate(VariableReference vr){
		switch (vr) {
			VariableReferenceReceiver: 	'''«vr.name.type.express» «vr.name.name» = receiver_store.get("«vr.name.name»",«vr.name.type.storeExpress»);'''
			VariableReferenceSender: 	'''«vr.name.type.express» «vr.name.name» = sender_store.get("«vr.name.name»",«vr.name.type.storeExpress»);'''
			VariableReferenceGlobal: 	'''«vr.name.type.express» «vr.name.name» = global_store.get("«vr.name.name»",«vr.name.type.storeExpress»);'''
			RecordReferenceReceiver: 	'''«vr.name.type.express» «vr.name.name» = receiver_store.get("«vr.name.name»",«vr.name.type.storeExpress»);'''
			RecordReferenceSender: 		'''«vr.name.type.express» «vr.name.name» = sender_store.get("«vr.name.name»",«vr.name.type.storeExpress»);'''
			RecordReferenceGlobal: 		'''«vr.name.type.express» «vr.name.name» = global_store.get("«vr.name.name»",«vr.name.type.storeExpress»);'''
		}
	}
	
	def String getBroadcastRate(Model model){
		'''
		@Override
		public double broadcastRate(CarmaStore sender, int action) {
			// TODO Auto-generated method stub
			return 0;
		}		
		'''
	}
	
	def String getBroadcastRatePredicates(Model model){
		
	}
	
	def String getUnicastRate(Model model){
		'''
		@Override
		public double unicastRate(CarmaStore sender, int action) {
			// TODO Auto-generated method stub
			return 0;
		}		
		'''
	}
	
	def String getUnicastRatePredicates(Model model){
		
	}
	
	def String getBroadcastUpdate(Model model){
		'''
		@Override
		public void broadcastUpdate(RandomGenerator random, CarmaStore sender,
				int action, Object value) {
			// TODO Auto-generated method stub
			
		}		
		'''
	}
	
	def String getBroadcastUpdatePredicates(Model model){
		
	}
	
	def String getUnicastUpdate(Model model){
		'''
		@Override
		public void unicastUpdate(RandomGenerator random, CarmaStore sender,
				CarmaStore receiver, int action, Object value) {
			// TODO Auto-generated method stub
			
		}		
		'''
	}
	
	def String getUnicastUpdatePredicates(Model model){
		
	}
}