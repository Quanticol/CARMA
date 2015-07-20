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
import java.util.ArrayList

import static extension org.eclipse.xtext.EcoreUtil2.*
import java.util.HashMap
import eu.quanticol.carma.core.carma.SetComp
import eu.quanticol.carma.core.carma.Spawn
import eu.quanticol.carma.core.carma.EnvironmentUpdateAssignment
import eu.quanticol.carma.core.carma.ComponentBlockSpawn
import eu.quanticol.carma.core.carma.CBND
import eu.quanticol.carma.core.carma.ComponentBlockArguments
import eu.quanticol.carma.core.generator.ms.SharedJavaniser

class EnvironmentHandler {
	
	@Inject extension TypeProvider
	@Inject extension SharedJavaniser
		
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
		«var rateValue = rateBlock.value as CarmaDouble»
		«splitRate(rateBrocast,rateUnicast,rateBlock)»
		«rateBlock.ratePredicates»
		«getBroadcastRate(rateBrocast,rateValue)»
		«getUnicastRate(rateUnicast,rateValue)»
		«var UpdateBlock updateBlock = (system as BlockSystem).environment.updateBlock»
		«var updateBrocast = new ArrayList<EnvironmentUpdate>()»
		«var updateUnicast = new ArrayList<EnvironmentUpdate>()»
		«splitUpdate(updateBrocast,updateUnicast,updateBlock)»
		«updateBlock.updatePredicates»
		«getBroadcastUpdate(updateBrocast)»
		«getUnicastUpdate(updateUnicast)»
		«ELSE»
		@Override
		public double broadcastProbability(CarmaStore sender, CarmaStore receiver, int action) {
			// TODO Auto-generated method stub
			return 0;
		}
		@Override
		public double unicastProbability(CarmaStore sender, CarmaStore receiver, int action) {
			// TODO Auto-generated method stub
			return 0;
		}
		@Override
		public double broadcastRate(CarmaStore sender, int action) {
			// TODO Auto-generated method stub
			return 0;
		}
		@Override
		public double unicastRate(CarmaStore sender, int action) {
			// TODO Auto-generated method stub
			return 0;
		}
		@Override
		public void broadcastUpdate(RandomGenerator random, CarmaStore sender, int action, Object value) {
			// TODO Auto-generated method stub
			
		}
		@Override
		public void unicastUpdate(RandomGenerator random, CarmaStore sender, CarmaStore receiver, int action,
				Object value) {
			// TODO Auto-generated method stub
			
		}
		«ENDIF»
		'''
	}
	
	///////////////////////////////PROBABILITY
	
	def void splitProbability(ArrayList<Probability> probabilityBrocast, 
		ArrayList<Probability> probabilityUnicast, 
		ProbabilityBlock probabilityBlock){
		
		for(probability : probabilityBlock.eAllOfType(Probability)){
			if(probability.stub.name.getContainerOfType(Action).type.me.equals("broad")){
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
		public CarmaPredicate get«probability.javanise»Predicate(final CarmaStore sender_store){
			return new CarmaPredicate() {
				@Override
				public boolean satisfy(CarmaStore receiver_store) {
					«getProbabilitySatisfyBlock(probability.guard.booleanExpression)»
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
			if(action == «probability.stub.name.getContainerOfType(Action).name.name.hashCode»
			&& get«probability.javanise»Predicate(sender_store).satisfy(receiver_store){
				«probability.getValue»
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
			if(action == «probability.stub.name.getContainerOfType(Action).name.name.hashCode»
			&& get«probability.javanise»Predicate(sender_store).satisfy(receiver_store)){
				«probability.getValue»
			}
			«ENDFOR»
			return «defaulter»;
		}		
		'''
	}
	
	def String getProbabilitySatisfyBlock(BooleanExpression bes){
		var vrs = bes.eAllOfType(VariableReference)
		var vrsh = new HashMap<String,VariableReference>()
		for(vr : vrs){
			vrsh.put(vr.prefix + vr.name.name, vr)
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
			«vrsh.get(key).getStore»
			«ENDFOR»
			return «bes.javanise»;
		} else {
			return false;
		}
		'''
	}
	
	def String checkStorePredicate(VariableReference vr){
		switch (vr) {
			VariableReferenceReceiver: 	'''receiver_variables.put("«vr.name.name»",«vr.type.classJavanise»);'''
			VariableReferenceSender: 	'''sender_variables.put("«vr.name.name»",«vr.type.classJavanise»);'''
			VariableReferenceGlobal: 	'''global_variables.put("«vr.name.name»",«vr.type.classJavanise»);'''
			RecordReferenceReceiver: 	'''receiver_variables.put("«vr.name.name»",«vr.type.classJavanise»);'''
			RecordReferenceSender: 		'''sender_variables.put("«vr.name.name»",«vr.type.classJavanise»);'''
			RecordReferenceGlobal: 		'''global_variables.put("«vr.name.name»",«vr.type.classJavanise»);'''
		}
	}
	
	def String getValue(Probability probability){
		var epe = probability.expression
		var vrs = epe.eAllOfType(VariableReference)
		var vrsh = new HashMap<String,VariableReference>()
		for(vr : vrs){
			vrsh.put(vr.prefix + vr.name.name, vr)
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
			«vrsh.get(key).getStore»
			«ENDFOR»
			«IF epe.eAllOfType(SetComp).size > 0»
			«epe.declarePrimitiveTypes»
			«ENDIF»
			return «epe.javanise»;
		}
		'''
	}
	
	def String getStore(VariableReference vr){
		switch (vr) {
			VariableReferenceReceiver: 	'''«vr.name.type.javanise» receiver_«vr.name.name» = receiver_store.get("«vr.name.name»",«vr.name.type.classJavanise»);'''
			VariableReferenceSender: 	'''«vr.name.type.javanise» sender_«vr.name.name» = sender_store.get("«vr.name.name»",«vr.name.type.classJavanise»);'''
			VariableReferenceGlobal: 	'''«vr.name.type.javanise» global_«vr.name.name» = global_store.get("«vr.name.name»",«vr.name.type.classJavanise»);'''
			RecordReferenceReceiver: 	'''«vr.name.type.javanise» receiver_«vr.name.name» = receiver_store.get("«vr.name.name»",«vr.name.type.classJavanise»);'''
			RecordReferenceSender: 		'''«vr.name.type.javanise» sender_«vr.name.name» = sender_store.get("«vr.name.name»",«vr.name.type.classJavanise»);'''
			RecordReferenceGlobal: 		'''«vr.name.type.javanise» global_«vr.name.name» = global_store.get("«vr.name.name»",«vr.name.type.classJavanise»);'''
		}
	}
	
	///////////////////////////////RATE
	
	
	def void splitRate(ArrayList<Rate> rateBrocast, 
		ArrayList<Rate> rateUnicast, 
		RateBlock rateBlock){
		
		for(rate : rateBlock.eAllOfType(Rate)){
			if(rate.stub.name.getContainerOfType(Action).type.me.equals("broad")){
				rateBrocast.add(rate)
			} else {
				rateUnicast.add(rate)
			}
		}
	}
	
	def String ratePredicates(RateBlock rateBlock){
		'''
		«FOR rate : rateBlock.eAllOfType(Rate)»
		«rate.ratePredicates»
		«ENDFOR»
		'''
		
	}
	
	def String getRatePredicates(Rate rate){
		'''
		public CarmaPredicate get«rate.javanise»Predicate(){
			return new CarmaPredicate() {
				@Override
				public boolean satisfy(CarmaStore sender_store) {
					«getRateSatisfyBlock(rate.guard.booleanExpression)»
				}
			};
		}
		'''
	}
	
	def String getRateSatisfyBlock(BooleanExpression bes){
		var vrs = bes.eAllOfType(VariableReference)
		var vrsh = new HashMap<String,VariableReference>()
		for(vr : vrs){
			vrsh.put(vr.prefix + vr.name.name, vr)
		}
		'''
		HashMap<String,Class> sender_variables = new HashMap<String,Class>();
		HashMap<String,Class> global_variables = new HashMap<String,Class>();
		«FOR key : vrsh.keySet»
		«vrsh.get(key).checkStorePredicate»
		«ENDFOR»
		boolean hasAttributes = true;
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
			«vrsh.get(key).getStore»
			«ENDFOR»
			return «bes.javanise»;
		} else {
			return false;
		}
		'''
	}
	
	def String getBroadcastRate(ArrayList<Rate> rates, CarmaDouble value){
		var defaulter = "0"
		if(value != null){
			defaulter = value.javanise
		}
		'''
		@Override
		public double broadcastRate(CarmaStore sender_store, int action){
			«FOR rate : rates»
			if(action == «rate.stub.name.getContainerOfType(Action).name.name.hashCode»
			&& get«rate.javanise»Predicate().satisfy(sender_store)){
				«rate.getValue»
			}
			«ENDFOR»
			return «defaulter»;
		}		
		'''
	}

	def String getUnicastRate(ArrayList<Rate> rates, CarmaDouble value){
		var defaulter = "0"
		if(value != null){
			defaulter = value.javanise
		}		
		'''
		@Override
		public double unicastRate(CarmaStore sender_store, int action){
			«FOR rate : rates»
			if(action == «rate.stub.name.getContainerOfType(Action).name.name.hashCode»
			&& get«rate.javanise»Predicate().satisfy(sender_store)){
				«rate.getValue»
			}
			«ENDFOR»
			return «defaulter»;
		}		
		'''
	}
	
	def String getValue(Rate rate){
		var epe = rate.expression
		var vrs = epe.eAllOfType(VariableReference)
		var vrsh = new HashMap<String,VariableReference>()
		for(vr : vrs){
			vrsh.put(vr.prefix + vr.name.name, vr)
		}
		'''
		HashMap<String,Class> sender_variables = new HashMap<String,Class>();
		HashMap<String,Class> global_variables = new HashMap<String,Class>();
		«FOR key : vrsh.keySet»
		«ENDFOR»
		boolean hasAttributes = true;
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
			«vrsh.get(key).getStore»
			«ENDFOR»
			«IF epe.eAllOfType(SetComp).size > 0»
			«epe.declarePrimitiveTypes»
			«ENDIF»
			return «epe.javanise»;
		}
		'''
	}
	
	def String getPrefix(VariableReference vr){
		switch (vr) {
			VariableReferenceReceiver: 	'''receiver_'''
			VariableReferenceSender: 	'''sender_'''
			VariableReferenceGlobal: 	'''global_'''
			RecordReferenceReceiver: 	'''receiver_'''
			RecordReferenceSender: 		'''sender_'''
			RecordReferenceGlobal: 		'''global_'''
		}
	}
	
	///////////////////////////////UPDATE
	
	def void splitUpdate(ArrayList<EnvironmentUpdate> updateBrocast, 
		ArrayList<EnvironmentUpdate> updateUnicast, 
		UpdateBlock updateBlock){
		
		for(update : updateBlock.eAllOfType(EnvironmentUpdate)){
			if(update.stub.name.getContainerOfType(Action).type.me.equals("broad")){
				updateBrocast.add(update)
			} else {
				updateUnicast.add(update)
			}
		}
		
	}
	
	def String updatePredicates(UpdateBlock updateBlock){
		'''
		«FOR update : updateBlock.eAllOfType(EnvironmentUpdate)»
		«update.updatePredicates»
		«ENDFOR»
		'''
		
	}
	
	def String getUpdatePredicates(EnvironmentUpdate update){
		'''
		public CarmaPredicate get«update.javanise»Predicate(final CarmaStore sender_store){
			return new CarmaPredicate() {
				@Override
				public boolean satisfy(CarmaStore receiver_store) {
					«getUpdateSatisfyBlock(update.guard.booleanExpression)»
				}
			};
		}
		'''
	}
	
	def String getUpdateSatisfyBlock(BooleanExpression bes){
		var vrs = bes.eAllOfType(VariableReference)
		var vrsh = new HashMap<String,VariableReference>()
		for(vr : vrs){
			vrsh.put(vr.prefix + vr.name.name, vr)
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
			«vrsh.get(key).getStore»
			«ENDFOR»
			return «bes.javanise»;
		} else {
			return false;
		}
		'''
	}
	
	def String getBroadcastUpdate(ArrayList<EnvironmentUpdate> updates){

		'''
		@Override
		public void broadcastUpdate(RandomGenerator random, CarmaStore sender_store,
				int action, Object value) {
			«FOR update : updates»
			if(action == «update.stub.name.getContainerOfType(Action).name.name.hashCode»
			&& get«update.javanise»Predicate(sender_store).satisfy(null)){
				HashMap<String,Class> sender_variables = new HashMap<String,Class>();
				HashMap<String,Class> global_variables = new HashMap<String,Class>();
				«IF update.eAllOfType(EnvironmentUpdateAssignment).size > 0»
				«update.getBroadValue»
				«ENDIF»
				«IF update.eAllOfType(Spawn).size > 0»
				«update.getBroadSpawns»
				«ENDIF»
			}
			«ENDFOR»
		}		
		'''
	}

	def String getUnicastUpdate(ArrayList<EnvironmentUpdate> updates){
		'''
		@Override
		public void unicastUpdate(RandomGenerator random, CarmaStore sender_store,
				CarmaStore receiver_store, int action, Object value) {
			«FOR update : updates»
			if(action == «update.stub.name.getContainerOfType(Action).name.name.hashCode»
			&& get«update.javanise»Predicate(sender_store).satisfy(receiver_store)){
				HashMap<String,Class> receiver_variables = new HashMap<String,Class>();
				HashMap<String,Class> sender_variables = new HashMap<String,Class>();
				HashMap<String,Class> global_variables = new HashMap<String,Class>();
				«IF update.eAllOfType(EnvironmentUpdateAssignment).size > 0»
				«update.getValue»
				«ENDIF»
				«IF update.eAllOfType(Spawn).size > 0»
				«update.getSpawns»
				«ENDIF»
			}
			«ENDFOR»
		}		
		'''
	}
	
	def String getBroadValue(EnvironmentUpdate update){
		var updateAssignments = update.eAllOfType(EnvironmentUpdateAssignment)
		var vrs = new HashMap<String,VariableReference>()
		for(updateAssignment : updateAssignments)
			for(vr : updateAssignment.eAllOfType(VariableReference))
				vrs.put(vr.prefix + vr.name.name, vr)
		'''
		«FOR key : vrs.keySet»
		«vrs.get(key).checkStorePredicate»
		«ENDFOR»
		boolean hasAttributes = true;
		if(sender_variables != null)
			for(String key : sender_variables.keySet()){
				hasAttributes = sender_store.has(key,sender_variables.get(key)) && hasAttributes;
			}
		if(global_variables != null)
			for(String key : global_variables.keySet()){
				hasAttributes = global_store.has(key,global_variables.get(key)) && hasAttributes;
			}
		if(hasAttributes){
			«FOR key : vrs.keySet»
			«vrs.get(key).getStore»
			«ENDFOR»
			«FOR updateAssignment : updateAssignments»
			«IF updateAssignment.expression.eAllOfType(SetComp).size > 0»
			«updateAssignment.expression.declarePrimitiveTypes»
			«ENDIF»
			«updateAssignment.assignStore»
			«ENDFOR»
			«FOR updateAssignment : updateAssignments»
			global_store.set("«updateAssignment.reference.name.name»",«updateAssignment.label»);
			«ENDFOR»
		}
		
		'''
	}
	
	def String getValue(EnvironmentUpdate update){
		var updateAssignments = update.eAllOfType(EnvironmentUpdateAssignment)
		var vrs = new HashMap<String,VariableReference>()
		for(updateAssignment : updateAssignments)
			for(vr : updateAssignment.eAllOfType(VariableReference))
				vrs.put(vr.prefix + vr.name.name, vr)
		'''
		«FOR key : vrs.keySet»
		«vrs.get(key).checkStorePredicate»
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
			«FOR key : vrs.keySet»
			«vrs.get(key).getStore»
			«ENDFOR»
			«FOR updateAssignment : updateAssignments»
			«IF updateAssignment.expression.eAllOfType(SetComp).size > 0»
			«updateAssignment.expression.declarePrimitiveTypes»
			«ENDIF»
			«updateAssignment.assignStore»
			«ENDFOR»
			«FOR updateAssignment : updateAssignments»
			global_store.set("«updateAssignment.reference.name.name»",«updateAssignment.label»);
			«ENDFOR»
		}
		
		'''
	}
	
	def String assignStore(EnvironmentUpdateAssignment eua){
		var vr = eua.reference
		var expression = eua.expression
		switch (eua.reference) {
			VariableReferenceReceiver: 	'''receiver_«vr.name.name» = «expression.javanise»;'''
			VariableReferenceSender: 	'''sender_«vr.name.name» = «expression.javanise»;'''
			VariableReferenceGlobal: 	'''global_«vr.name.name» = «expression.javanise»;'''
			RecordReferenceReceiver: 	'''receiver_«vr.name.name» = «expression.javanise»;'''
			RecordReferenceSender: 		'''sender_«vr.name.name» = «expression.javanise»;'''
			RecordReferenceGlobal: 		'''global_«vr.name.name» = «expression.javanise»;'''
		}
	}
	
	def String label(EnvironmentUpdateAssignment eua){
		var vr = eua.reference
		switch (eua.reference) {
			VariableReferenceReceiver: 	'''receiver_«vr.name.name»'''
			VariableReferenceSender: 	'''sender_«vr.name.name»'''
			VariableReferenceGlobal: 	'''global_«vr.name.name»'''
			RecordReferenceReceiver: 	'''receiver_«vr.name.name»'''
			RecordReferenceSender: 		'''sender_«vr.name.name»'''
			RecordReferenceGlobal: 		'''global_«vr.name.name»'''
		}
	}
	
	def String getSpawns(EnvironmentUpdate update){

		'''
			«FOR declaration : update.spawn.spawn.comp»
			«declaration.addComponent»
			«ENDFOR»
		'''		
	}
	
	def String addComponent(CBND componentBlockDeclaration){
		switch(componentBlockDeclaration){
			ComponentBlockSpawn:			addComponent(componentBlockDeclaration)
		}
	}
	

	def String addComponent(ComponentBlockSpawn componentBlockDeclaration){
		var products = new ArrayList<ArrayList<String>>()
		(componentBlockDeclaration.arguments as ComponentBlockArguments).product.cartesianProduct(products)
		var name = (componentBlockDeclaration as ComponentBlockSpawn).name.name
		var vrs = new HashMap<String,VariableReference>()
		for(vr : (componentBlockDeclaration.arguments as ComponentBlockArguments).eAllOfType(VariableReference))
			vrs.put(vr.prefix + vr.name.name, vr)
		'''
		«FOR key : vrs.keySet»
		«vrs.get(key).checkStorePredicate»
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
			«FOR key : vrs.keySet»
			«vrs.get(key).getStore»
			«ENDFOR»			
			«FOR args : products»
			addComponent(get«name.toFirstUpper»(«args.asArguments»));
			«ENDFOR»
		}
		'''
	}
	
	def String addBroadComponent(CBND componentBlockDeclaration){
		switch(componentBlockDeclaration){
			ComponentBlockSpawn:			addBroadComponent(componentBlockDeclaration)
		}
	}
	

	def String addBroadComponent(ComponentBlockSpawn componentBlockDeclaration){
		var products = new ArrayList<ArrayList<String>>()
		(componentBlockDeclaration.arguments as ComponentBlockArguments).product.cartesianProduct(products)
		var name = (componentBlockDeclaration as ComponentBlockSpawn).name.name
		var vrs = new HashMap<String,VariableReference>()
		for(vr : (componentBlockDeclaration.arguments as ComponentBlockArguments).eAllOfType(VariableReference))
			vrs.put(vr.prefix + vr.name.name, vr)
		'''
		«FOR key : vrs.keySet»
		«vrs.get(key).checkStorePredicate»
		«ENDFOR»
		boolean hasAttributes = true;
		if(sender_variables != null)
			for(String key : sender_variables.keySet()){
				hasAttributes = sender_store.has(key,sender_variables.get(key)) && hasAttributes;
			}
		if(global_variables != null)
			for(String key : global_variables.keySet()){
				hasAttributes = global_store.has(key,global_variables.get(key)) && hasAttributes;
			}
		if(hasAttributes){
			«FOR key : vrs.keySet»
			«vrs.get(key).getStore»
			«ENDFOR»			
			«FOR args : products»
			addComponent(get«name.toFirstUpper»(«args.asArguments»));
			«ENDFOR»
		}
		'''
	}
	
	def String getBroadSpawns(EnvironmentUpdate update){

		'''
			«FOR declaration : update.spawn.spawn.comp»
			«declaration.addBroadComponent»
			«ENDFOR»
		'''		
	}
	
	def String asArguments(ArrayList<String> args){
		var String toReturn = ""
		if(args.size > 0){
			toReturn = args.get(0)
			for(var i = 1; i < args.size; i++)
				toReturn = toReturn + ", " + args.get(i)
		}
		return toReturn
	}
	
}