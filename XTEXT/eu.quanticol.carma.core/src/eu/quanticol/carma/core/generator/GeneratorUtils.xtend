package eu.quanticol.carma.core.generator

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.ActionStub
import eu.quanticol.carma.core.carma.BlockSpawn
import eu.quanticol.carma.core.carma.BooleanExpressions
import eu.quanticol.carma.core.carma.ComponentBlockNewDeclarationSpawn
import eu.quanticol.carma.core.carma.Environment
import eu.quanticol.carma.core.carma.EnvironmentExpression
import eu.quanticol.carma.core.carma.EnvironmentExpressions
import eu.quanticol.carma.core.carma.EnvironmentGuard
import eu.quanticol.carma.core.carma.EnvironmentOperation
import eu.quanticol.carma.core.carma.EnvironmentUpdate
import eu.quanticol.carma.core.carma.EnvironmentUpdateAssignment
import eu.quanticol.carma.core.carma.EnvironmentUpdateExpressions
import eu.quanticol.carma.core.carma.LineSpawn
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.NCA
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnDeclare
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnMethod
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnReference
import eu.quanticol.carma.core.carma.ProbabilityBlock
import eu.quanticol.carma.core.carma.RateBlock
import eu.quanticol.carma.core.carma.RecordDeclaration
import eu.quanticol.carma.core.carma.RecordReferenceGlobal
import eu.quanticol.carma.core.carma.RecordReferenceMy
import eu.quanticol.carma.core.carma.RecordReferencePure
import eu.quanticol.carma.core.carma.RecordReferenceReceiver
import eu.quanticol.carma.core.carma.RecordReferenceSender
import eu.quanticol.carma.core.carma.RecordReferenceThis
import eu.quanticol.carma.core.carma.Spawn
import eu.quanticol.carma.core.carma.System
import eu.quanticol.carma.core.carma.UpdateBlock
import eu.quanticol.carma.core.carma.UpdateExpressions
import eu.quanticol.carma.core.carma.VariableDeclaration
import eu.quanticol.carma.core.carma.VariableDeclarationCarmaDouble
import eu.quanticol.carma.core.carma.VariableDeclarationCarmaIntger
import eu.quanticol.carma.core.carma.VariableDeclarationEnum
import eu.quanticol.carma.core.carma.VariableDeclarationRecord
import eu.quanticol.carma.core.carma.VariableReference
import eu.quanticol.carma.core.carma.VariableReferenceGlobal
import eu.quanticol.carma.core.carma.VariableReferenceMy
import eu.quanticol.carma.core.carma.VariableReferencePure
import eu.quanticol.carma.core.carma.VariableReferenceReceiver
import eu.quanticol.carma.core.carma.VariableReferenceSender
import eu.quanticol.carma.core.carma.VariableReferenceThis
import eu.quanticol.carma.core.typing.TypeProvider
import eu.quanticol.carma.core.utils.LabelUtil
import eu.quanticol.carma.core.utils.Util
import java.util.ArrayList
import java.util.HashSet
import java.util.List

import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.core.generator.carmaVariable.CarmaVariableManager
import eu.quanticol.carma.core.carma.VariableType
import eu.quanticol.carma.core.carma.VariableTypeEnum
import eu.quanticol.carma.core.carma.VariableTypeRecord
import eu.quanticol.carma.core.carma.VariableTypeCarmaDouble
import eu.quanticol.carma.core.carma.VariableTypeCarmaIntger
import eu.quanticol.carma.core.carma.MeasureBlock
import eu.quanticol.carma.core.generator.actions.ActionManager
import eu.quanticol.carma.core.carma.Action
import eu.quanticol.carma.core.carma.Rate
import eu.quanticol.carma.core.carma.Probability
import eu.quanticol.carma.core.carma.Component
import eu.quanticol.carma.core.generator.components.ComponentManager

class GeneratorUtils {
	
	@Inject extension TypeProvider
	@Inject extension LabelUtil
	@Inject extension Util
	@Inject extension GenerateSystems
	@Inject extension ExpressionHandler
	

	def String getStore(String carma_name, String carma_type, String java_assign, String store, String booleanFail){
		'''
		if(store.get(«carma_name»,«carma_type») != null){
			«java_assign» = «store».get(«carma_name»,«carma_type»);
		} else {
			«booleanFail» = false;
		}
		'''
	}
	
	def String declareVariable(VariableReference vr){
		switch(vr){
			VariableReferencePure		: {vr.getStore}
			VariableReferenceMy			: {vr.getStore}
			VariableReferenceThis		: {vr.getStore}
			VariableReferenceReceiver	: {vr.getReceiver}
			VariableReferenceSender		: {vr.getSender}
			RecordReferencePure			: {vr.getStore}
			RecordReferenceMy			: {vr.getStore}
			RecordReferenceThis			: {vr.getStore}
			RecordReferenceReceiver		: {vr.getReceiver}
			RecordReferenceSender		: {vr.getSender}
			VariableReferenceGlobal		: {vr.getGlobal}
			RecordReferenceGlobal		: {vr.getGlobal}
		}
	}
	
	def String getStoreVariableDeclaration(VariableDeclarationEnum vde, String ext, String store){
		'''
		«vde.convertPrimitiveType» «vde.name.label»«ext» = 0;
		if(«store».get("«vde.name.label»" , «vde.convertType».class) != null){
			«vde.name.label»«ext» = «store».get("«vde.name.label»" , «vde.convertType».class); 
		} else { 
			hasAttributes = false;
		}'''
	}
	
	def String getStoreVariableDeclaration(VariableDeclarationRecord vdr, String ext, String store){
		var rds = vdr.recordDeclarations
		'''
		«FOR rd : rds»
		«vdr.convertPrimitiveType» «vdr.name.label»_«rd.name.label»«ext» = 0;
		if(«store».get("«vdr.name.label»_«rd.name.label»" , «vdr.convertType».class) != null){
			«vdr.name.label»_«rd.name.label»«ext» = «store».get("«vdr.name.label»_«rd.name.label»" , «vdr.convertType».class );
		} else { 
			hasAttributes = false;
		}
		«ENDFOR»
		'''
	}
	
	
	def String getStore(VariableReference vr){
		'''
		«var vd = vr.getVariableDeclaration»
		«switch(vd){
			VariableDeclarationEnum:	vd.getStoreVariableDeclaration("", "store")
			VariableDeclarationRecord:	vd.getStoreVariableDeclaration("", "store")
		}»
		'''
	}
	
	def String getInputStore(VariableReference vr){
		'''
		«var vd = vr.getVariableDeclaration»
		«switch(vd){
			VariableDeclarationEnum:	vd.getStoreVariableDeclaration("_i", "inputStore")
			VariableDeclarationRecord:	vd.getStoreVariableDeclaration("_i", "inputStore")
		}»
		'''
	}
	
	def String getOutputTheirStore(VariableReference vr){
		'''
		«var vd = vr.getVariableDeclaration»
		«switch(vd){
			VariableDeclarationEnum:	vd.getStoreVariableDeclaration("_i", "inputStore")
			VariableDeclarationRecord:	vd.getStoreVariableDeclaration("_i", "inputStore")
		}»
		'''
	}
	
	def String getOutputMyStore(VariableReference vr){
		'''
		«var vd = vr.getVariableDeclaration»
		«switch(vd){
			VariableDeclarationEnum:	vd.getStoreVariableDeclaration("_o", "outputStore")
			VariableDeclarationRecord:	vd.getStoreVariableDeclaration("_o", "outputStore")
		}»
		'''
	}
	
	def String getSender(VariableReference vr){
		'''
		«var vd = vr.getVariableDeclaration»
		«switch(vd){
			VariableDeclarationEnum:	vd.getStoreVariableDeclaration("_s", "sender")
			VariableDeclarationRecord:	vd.getStoreVariableDeclaration("_s", "sender")
		}»
		'''
	}
	
	def String getReceiver(VariableReference vr){
		'''
		«var vd = vr.getVariableDeclaration»
		«switch(vd){
			VariableDeclarationEnum:	vd.getStoreVariableDeclaration("_r", "receiver")
			VariableDeclarationRecord:	vd.getStoreVariableDeclaration("_r", "receiver")
		}»
		'''
	}
	
	def String getStore(VariableDeclaration vd){
		'''
		«switch(vd){
			VariableDeclarationEnum:	vd.getStoreVariableDeclaration("", "store")
			VariableDeclarationRecord:	vd.getStoreVariableDeclaration("", "store")
		}»
		'''
	}
	
	def String getGlobal(VariableReference vr){
		'''
		«var vd = vr.getVariableDeclarationEnv»
		«switch(vd){
			VariableDeclarationEnum:	vd.getStoreVariableDeclaration("", "global_store")
			VariableDeclarationRecord:	vd.getStoreVariableDeclaration("", "global_store")
		}»
		'''
	}
	
	def String getAllVariablesEnv(BooleanExpressions bes){
		var HashSet<String> output = new HashSet<String>()
		for(vr : bes.getGlobals)
			output.add(vr.getGlobal)
		for(vr : bes.getReceivers)
			output.add(vr.getReceiver)
		for(vr : bes.getSenders){
			output.add(vr.getSender)
		}
		'''
		«FOR vr : output»
		«vr»
		«ENDFOR»
		'''
	}
	
	def String getAllVariablesInputAction(BooleanExpressions bes){
		var HashSet<String> output = new HashSet<String>()
		for(vr : bes.getStores)
			output.add(vr.getInputStore)
		'''
		«FOR vr : output»
		«vr»
		«ENDFOR»
		'''
	}
	
	def String getAllVariablesOutputAction(BooleanExpressions bes){
		var HashSet<String> output = new HashSet<String>()
		for(vr : bes.getOutputTheirStores)
			output.add(vr.getOutputTheirStore)
		for(vr : bes.getOutputMyStores)
			output.add(vr.getOutputMyStore)
		
		'''
		«FOR vr : output»
		«vr»
		«ENDFOR»
		'''
	}
	
	def String getAllVariables(EnvironmentUpdateExpressions eues){
		var HashSet<String> output = new HashSet<String>()
		for(vr : eues.getGlobals)
			output.add(vr.getGlobal)
		for(vr : eues.getReceivers)
			output.add(vr.getReceiver)
		for(vr : eues.getSenders)
			output.add(vr.getSender)
		'''
		«FOR vr : output»
		«vr»
		«ENDFOR»
		'''
	}
	
	def String getAllVariables(EnvironmentExpressions ees){
		var HashSet<String> output = new HashSet<String>()
		for(vr : ees.getGlobals){
			output.add(vr.getStore)
		}
		for(vr : ees.getReceivers){
			output.add(vr.getStore)
		}
		for(vr : ees.getSenders){
			output.add(vr.getStore)
		}
		'''
		«FOR vr : output»
		«vr»
		«ENDFOR»
		'''
	}
	
	def String getAllVariablesMeasure(BooleanExpressions bes){
		
		var attributes = new ArrayList<VariableReference>()
		for(vr : bes.eAllOfType(VariableReference)){
			if(vr.variableDeclaration != null)
				attributes.add(vr)
		}
		var HashSet<String> output = new HashSet<String>()
		for(vr : attributes){
			output.add(vr.getStore)
		}
		
		'''
		«FOR vr : output»
		«vr»
		«ENDFOR»
		'''
	}
	
	def String getAllVariablesComponentBlockNewDeclarationSpawn(ComponentBlockNewDeclarationSpawn cbnds){
		var HashSet<String> output = new HashSet<String>()
		var arguments = cbnds.eAllOfType(NCA)
		for(arg : arguments){
			switch(arg){
				NewComponentArgumentSpawnReference: output.add(arg.value.getNCAVR)
			}
		}
		'''
		«FOR vr : output»
		«vr»
		«ENDFOR»
		'''
	}
	
	def String getNCAVR(VariableReference vr){
		switch(vr){
			VariableReferencePure:		vr.global
			VariableReferenceReceiver:	vr.receiver
			VariableReferenceSender:	vr.sender
			VariableReferenceGlobal:	vr.global
			RecordReferencePure:		vr.global
			RecordReferenceReceiver:	vr.receiver
			RecordReferenceSender:		vr.sender
			RecordReferenceGlobal:		vr.global
		}
	}
	
	def String getAllVariablesComponentBlockNewDeclarationSpawnArgs(ComponentBlockNewDeclarationSpawn cbnds){
		var HashSet<String> output = new HashSet<String>()
		var arguments = cbnds.eAllOfType(NCA)
		for(arg : arguments){
			switch(arg){
				NewComponentArgumentSpawnReference: output.add(arg.value.getNCAVRArgs)
				NewComponentArgumentSpawnMethod: output.add(arg.getLabelForArgs)
				NewComponentArgumentSpawnDeclare: output.add(arg.getLabelForArgs)
			}
		}
		var returns = ""
		if(output.size > 0){
			returns = output.get(0)
			for(var i = 1; i < output.size; i++){
				returns = returns + ", " + output.get(i) 
			}
		}
		return returns
	}
	
	def ArrayList<String> getAllVariablesNCA(NCA nca){
		var HashSet<String> output = new HashSet<String>()
		switch(nca){
			NewComponentArgumentSpawnReference: output.add(nca.value.getNCAVRArgs)
			default: output.add(nca.getLabelForArgs)
		}
		var ArrayList<String> returns = new ArrayList<String>(output)
		return returns
	}
	
	def String getNCAVRArgs(VariableReference vr){
		switch(vr){
			VariableReferencePure:		vr.globalArg
			VariableReferenceReceiver:	vr.receiverArg
			VariableReferenceSender:	vr.senderArg
			VariableReferenceGlobal:	vr.globalArg
			RecordReferencePure:		vr.globalArg
			RecordReferenceReceiver:	vr.receiverArg
			RecordReferenceSender:		vr.senderArg
			RecordReferenceGlobal:		vr.globalArg
		}
	}
	
	def String getGlobalArg(VariableReference vr){
		'''
		«var vd = vr.getVariableDeclarationEnv»
		«switch(vd){
			VariableDeclarationEnum:	vd.getVariable("")
			VariableDeclarationRecord:	vd.getVariable("")
		}»'''
	}
	
	def String getSenderArg(VariableReference vr){
		'''
		«var vd = vr.getVariableDeclaration»
		«switch(vd){
			VariableDeclarationEnum:	vd.getVariable("_s")
			VariableDeclarationRecord:	vd.getVariable("_s")
		}»'''
	}
	
	def String getReceiverArg(VariableReference vr){
		'''
		«var vd = vr.getVariableDeclaration»
		«switch(vd){
			VariableDeclarationEnum:	vd.getVariable("_r")
			VariableDeclarationRecord:	vd.getVariable("_r")
		}»'''
	}
	
	def String getVariable(VariableDeclarationEnum vde, String ext){
		'''«vde.name.label»«ext»'''
	}
	
	def String getVariable(VariableDeclarationRecord vdr, String ext){
		var output = ""
		var rds = vdr.recordDeclarations
		if(rds.size > 0){
			output = vdr.name.label+"_"+rds.get(0).name.label+ext		
			for(var i = 1; i < rds.size; i++){
				output = output + ", "+vdr.name.label+"_"+rds.get(i).name.label+ext
			}
		}
		return output
	}
	
	def String defineCastPredicates(EnvironmentOperation cast){
		//anything NOT receiver or sender is a global store access
		var senders 	= (cast.guard.eAllOfType(RecordReferenceSender).size + cast.guard.eAllOfType(VariableReferenceSender).size) > 0
		var receivers	= (cast.guard.eAllOfType(RecordReferenceReceiver).size + cast.guard.eAllOfType(VariableReferenceReceiver).size) > 0
		//no! global_store always accessible!
		
		//either single with no args
		if(senders && !receivers){ 
			'''«cast.predicate("sender","")»'''
		} else if (!senders && receivers){
			'''«cast.predicate("receiver","")»'''
		//or double with sender always given first, and receiver sent for evaluation?
		} else if (senders && receivers) {
			'''«cast.predicate("receiver","CarmaStore sender")»'''
		//or just global, no one cares about the store
		} else if (cast.guard.eAllOfType(VariableReference).size > 0){
			'''«cast.predicate("store","")»'''
		}
	}
	
	def String predicate(EnvironmentOperation cast, String store, String args){
		'''
		public static CarmaPredicate «cast.convertToPredicateName»(«args»){
			return new CarmaPredicate() {
				@Override
				public boolean satisfy(CarmaStore «store») {
					«cast.satisfy»
				}
			};
		}
		'''
	}
	
	def String satisfy(EnvironmentOperation cast){
		'''
		boolean hasAttributes = true;
		«cast.guard.booleanExpression.getAllVariablesEnv»
		if(hasAttributes)
			return «cast.guard.convertToJava»;
		else
			return false;
		'''
	}
	
	def String booleanToPredicate(BooleanExpressions be){
		if(be.label.equals("true")){
			return '''CarmaPredicate.TRUE.satisfy(sender)'''
		}
		else if(be.label.equals("false")){
			return '''CarmaPredicate.FALSE.satisfy(sender)'''
		}
		else {
			var cast = be.getContainerOfType(EnvironmentOperation) 
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
	
	def String defineActionStubs(ActionStub actionStub){
		'''
		if (action == «actionStub.getContainerOfType(Model).label»Definition.«actionStub.name.name.toUpperCase»
		&& «actionStub.predicateHandler») {
				«actionStub.defineActionStub»
		}
		'''
	}
	
	def String predicateHandler(ActionStub actionStub){
		var booleanExpression = actionStub.getContainerOfType(EnvironmentOperation).eAllOfType(EnvironmentGuard).get(0)
		'''
		«booleanToPredicate(booleanExpression.booleanExpression)»
		'''
	}
	
	def String defineActionStub(ActionStub actionStub){
		if(actionStub.getContainerOfType(EnvironmentUpdate) != null){
			'''«defineEUActionStub(actionStub)»'''
		} else {
			if(actionStub.getContainerOfType(EnvironmentOperation).eAllOfType(EnvironmentExpression).size > 0){
				'''
				«actionStub.getContainerOfType(EnvironmentOperation).eAllOfType(EnvironmentExpression).get(0).anAssignment»
				'''
			} 
			
		}
	}
	
	def String defineEnvironmentUpdates(System system){
		if(system.eAllOfType(UpdateBlock).size > 0){
			system.defineEnvironmentOperation(system.eAllOfType(UpdateBlock).get(0).eAllOfType(EnvironmentOperation),
			"void",
			"Update(RandomGenerator r , CarmaStore sender, int action, Object value)",
			"Update(RandomGenerator r , CarmaStore sender, CarmaStore receiver, int action, Object value)",
			"")
		} else {
			'''
			@Override
			public void broadcastUpdate(RandomGenerator random, CarmaStore sender,
					int action, Object value) {
			}
			@Override
			public void unicastUpdate(RandomGenerator random, CarmaStore sender,
					CarmaStore receiver, int action, Object value) {
			}
			'''
		}
	}
	
	def String defineRates(System system){
		if(system.eAllOfType(RateBlock).size > 0){
			system.defineEnvironmentOperation(system.eAllOfType(RateBlock).get(0).eAllOfType(EnvironmentOperation),
			"double",
			"Rate(CarmaStore sender, int action)",
			"Rate(CarmaStore sender, int action)",
			"return 1.0;")
		} else {
			'''
			@Override
			public double unicastRate(CarmaStore sender, int action){
				return 1.0;
			}
			@Override
			public double broadcastRate(CarmaStore sender, int action){
				return 1.0;
			}'''
		}
	}
	
	def String defineProbabilities(System system){
		if(system.eAllOfType(ProbabilityBlock).size > 0){
			system.defineEnvironmentOperation(system.eAllOfType(ProbabilityBlock).get(0).eAllOfType(EnvironmentOperation),
			"double",
			"Probability(CarmaStore sender, CarmaStore receiver,int action)",
			"Probability(CarmaStore sender, CarmaStore receiver,int action)",
			"return 1.0;")
		} else {
			'''
			@Override
			public double broadcastProbability(CarmaStore sender, CarmaStore receiver,
			int action) {
				return 1;
			}
			@Override
			public double unicastProbability(CarmaStore sender, CarmaStore receiver,
			int action) {
				return 1;
			}
			'''
		}
	}
	
	def String defineEnvironmentOperation(System system, List<EnvironmentOperation> actions, 
		String returns, 
		String broadcastOperation, 
		String unicastOperation,
		String returnValue
	){
		
		var unicasts = new ArrayList<EnvironmentOperation>()
		var broadcasts = new ArrayList<EnvironmentOperation>()
		
		for(eo : actions)
			if(eo.stub.isBroadcast)
				broadcasts.add(eo)
			else
				unicasts.add(eo)
		'''
		@Override
		public «returns» broadcast«broadcastOperation»{
			«FOR broadcast : broadcasts»
			«defineActionStubs(broadcast.stub)»
			«ENDFOR»
			«returnValue»
		}
		
		@Override
		public «returns» unicast«unicastOperation»{
			«FOR unicast : unicasts»
			«defineActionStubs(unicast.stub)»
			«ENDFOR»
			«returnValue»
		}
		'''
	}
	
	def String defineEUActionStub(ActionStub actionStub){
		
		var spawns = new ArrayList<Spawn>(actionStub.getContainerOfType(EnvironmentUpdate).eAllOfType(Spawn))
		var updates = new ArrayList<EnvironmentUpdateAssignment>(actionStub.getContainerOfType(EnvironmentUpdate).eAllOfType(EnvironmentUpdateAssignment))
		
		'''
		boolean hasAttributes = true;
		«FOR spawn : spawns»
		«spawn.aSpawn»
		«ENDFOR»
		«FOR update : updates»
		«update.anAssignment»
		«ENDFOR»
		'''
	}
	
	def String aSpawn(Spawn s){
		var spawn = s.spawn
		switch(spawn){
			BlockSpawn: spawn.blockSpawn
			LineSpawn : spawn.lineSpawn
		}
	}
	
	def String anAssignment(EnvironmentUpdateAssignment eua){
		'''
		«eua.expression.getAllVariables»
		«eua.storeReference.setVariable(eua.storeReference.convertToJava,eua.expression.label)»
		'''
	}
	
	def String anAssignment(EnvironmentExpressions ee){
		'''
		«ee.getAllVariables»
		return «ee.convertToJava»;
		'''
	}
	
	def String setVariable(VariableReference vr, String variable, String expression){
		switch(vr){
			VariableReferencePure		: {vr.setStore(variable,expression)}
			VariableReferenceMy			: {vr.setStore(variable,expression)}
			VariableReferenceThis		: {vr.setStore(variable,expression)}
			VariableReferenceReceiver	: {vr.setReceiver(variable,expression)}
			VariableReferenceSender		: {vr.setSender(variable,expression)}
			RecordReferencePure			: {vr.setStore(variable,expression)}
			RecordReferenceMy			: {vr.setStore(variable,expression)}
			RecordReferenceThis			: {vr.setStore(variable,expression)}
			RecordReferenceReceiver		: {vr.setReceiver(variable,expression)}
			RecordReferenceSender		: {vr.setSender(variable,expression)}
			VariableReferenceGlobal		: {vr.setGlobal(variable,expression)}
			RecordReferenceGlobal		: {vr.setGlobal(variable,expression)}
		}
	}
	
	def String setStore(VariableDeclaration vd, UpdateExpressions expression){
		var output = ""
		switch(vd){
			VariableDeclarationEnum: {
						output = 
						'''
						if(hasAttributes){
							store.set("«vd.name.label»",«expression»);
						}
						'''
					}
					VariableDeclarationRecord: {
						var rds = vd.eAllOfType(RecordDeclaration)
						for(rd : rds){
							output = output + '''
							if(hasAttributes){
								store.set("«vd.name.label»_«rd.name.label»",«expression»);
							}
							'''
						}
					}
				}
		return output
	}
	
	def String setStore(VariableReference vr, String variable, String expression){
		if(vr.getContainerOfType(Environment) != null){
			'''
			if(hasAttributes){
				global_store.set("«variable»",«expression»);
			}'''
		} else {
			'''
			if(hasAttributes){
				store.set("«variable»",«expression»);
			}'''
		}
		
	}

	def String setReceiver(VariableReference vr, String variable, String expression){
		'''
		if(hasAttributes){
			receiver.set("«variable»",«expression»);
		}
		'''
	}
	
	def String setSender(VariableReference vr, String variable, String expression){
		'''
		if(hasAttributes){
			sender.set("«variable»",«expression»);
		}
		'''
	}
	
	def String setGlobal(VariableReference vr, String variable, String expression){
		'''
		if(hasAttributes){
			global_store.set("«variable»",«expression»);
		}
		'''
	}
	
	def void populateCarmaVariableManager(CarmaVariableManager cvm, Model model){
		for(vd : model.eAllOfType(VariableDeclaration)){
			if(vd.getContainerOfType(MeasureBlock) == null){
				switch(vd){
					VariableDeclarationEnum:		cvm.loadDeclaration(vd.name.label,true,false,vd.hashCode,vd.assign)
					VariableDeclarationRecord:{
							var rds = vd.recordDeclarations
							for(rd : rds)
								cvm.loadDeclaration(vd.name.label+"_"+rd.name.label,true,true,rd.hashCode,rd.assign)
					}
					VariableDeclarationCarmaDouble:	cvm.loadDeclaration(vd.name.label,false,false,vd.hashCode,vd.assign)
					VariableDeclarationCarmaIntger: cvm.loadDeclaration(vd.name.label,true,false,vd.hashCode,vd.assign)
				}
			}
		}
		for(vt : model.eAllOfType(VariableType)){
			switch(vt){
				VariableTypeEnum:			cvm.loadType(vt.name.label,true,false,vt.hashCode,null)
				VariableTypeRecord:			cvm.loadType(vt.name.label,true,true,vt.hashCode,null)
				VariableTypeCarmaDouble:	cvm.loadType(vt.name.label,false,false,vt.hashCode,null)
				VariableTypeCarmaIntger:	cvm.loadType(vt.name.label,true,false,vt.hashCode,null)
			}
		}
		for(vr : model.eAllOfType(VariableReference)){
			switch(vr){
				VariableReferencePure:		cvm.loadReference(vr.name.label,true,false,vr.hashCode,null,"")
				VariableReferenceMy:		cvm.loadReference(vr.name.label,true,false,vr.hashCode,null,"my_")
				VariableReferenceThis:		cvm.loadReference(vr.name.label,true,false,vr.hashCode,null,"this_")
				VariableReferenceReceiver:	cvm.loadReference(vr.name.label,true,false,vr.hashCode,null,"receiver_")
				VariableReferenceSender:	cvm.loadReference(vr.name.label,true,false,vr.hashCode,null,"sender_")
				VariableReferenceGlobal:	cvm.loadReference(vr.name.label,true,false,vr.hashCode,null,"global_")
				RecordReferencePure:		cvm.loadReference(vr.name.label+"_"+vr.record.label,true,true,vr.hashCode,null,"")
				RecordReferenceMy:			cvm.loadReference(vr.name.label+"_"+vr.record.label,true,true,vr.hashCode,null,"my_")
				RecordReferenceThis:		cvm.loadReference(vr.name.label+"_"+vr.record.label,true,true,vr.hashCode,null,"this_")
				RecordReferenceReceiver:	cvm.loadReference(vr.name.label+"_"+vr.record.label,true,true,vr.hashCode,null,"receiver_")
				RecordReferenceSender:		cvm.loadReference(vr.name.label+"_"+vr.record.label,true,true,vr.hashCode,null,"sender_")
				RecordReferenceGlobal:		cvm.loadReference(vr.name.label+"_"+vr.record.label,true,true,vr.hashCode,null,"global_")
			}
		}
	}
	
	def void populateActionManager(ActionManager am, Model model){
		
		var actions = model.eAllOfType(Action)
		var actionStubs = model.eAllOfType(ActionStub)
		
		for(action : actions)
			am.loadAction(action.labelIO)
			
		for(actionStub : actionStubs){
			var String type = ""
			var String guard = ""
			var String expression = ""
			var eo = actionStub.getContainerOfType(EnvironmentOperation)
			switch(eo){
				Rate:				{
					type = "rate" 
					guard = eo.guard.booleanExpression.disarmExpression.toUpperCase
					expression = eo.expression.evaluateExpression
				}
				EnvironmentUpdate:  {
					type = "upda"
					guard = eo.guard.booleanExpression.disarmExpression.toUpperCase
					expression = eo.expression.evaluateExpression
				}
				Probability:		{
					type = "prob"
					guard = eo.guard.booleanExpression.disarmExpression.toUpperCase
					expression = eo.expression .evaluateExpression
				}
			}
			am.loadStub(actionStub.name.label,type,guard,expression)
		}
	}
	
	def void populateComponentManager(ComponentManager cm, Model model,ActionManager am, CarmaVariableManager vm){
		
		var components = model.eAllOfType(Component)
		
		for(component : components)
			cm.loadComponent(component.label,component.getTree,am,vm);
		
	}
	
}