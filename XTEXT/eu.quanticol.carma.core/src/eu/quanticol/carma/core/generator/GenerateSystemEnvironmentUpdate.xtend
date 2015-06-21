package eu.quanticol.carma.core.generator

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.EnvironmentUpdate
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.System
import eu.quanticol.carma.core.generator.carmavariable.CarmaVariableManager
import eu.quanticol.carma.core.generator.predicates.Predicates
import eu.quanticol.carma.core.utils.LabelUtil

import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.core.utils.Util
import eu.quanticol.carma.core.generator.actions.ActionManager
import eu.quanticol.carma.core.carma.Probability
import eu.quanticol.carma.core.carma.EnvironmentOperation
import java.util.ArrayList
import eu.quanticol.carma.core.carma.EnvironmentUpdateAssignment
import eu.quanticol.carma.core.generator.actions.NullUpdate
import eu.quanticol.carma.core.carma.VariableReference
import eu.quanticol.carma.core.carma.EnvironmentUpdateExpressions
import java.util.HashMap
import eu.quanticol.carma.core.carma.VariableReferenceReceiver
import eu.quanticol.carma.core.carma.RecordReferenceReceiver
import eu.quanticol.carma.core.carma.VariableReferenceSender
import eu.quanticol.carma.core.carma.RecordReferenceSender
import eu.quanticol.carma.core.carma.VariableReferencePure
import eu.quanticol.carma.core.carma.RecordReferencePure
import eu.quanticol.carma.core.carma.VariableReferenceGlobal
import eu.quanticol.carma.core.carma.RecordReferenceGlobal
import eu.quanticol.carma.core.carma.Spawn
import eu.quanticol.carma.core.carma.BlockSpawn
import eu.quanticol.carma.core.carma.LineSpawn
import eu.quanticol.carma.core.carma.ComponentBlockNewDeclarationSpawn
import eu.quanticol.carma.core.carma.Range
import eu.quanticol.carma.core.carma.NCA
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnPrimitive
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnMethod
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnDeclare
import eu.quanticol.carma.core.carma.RecordDeclaration
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnReference
import eu.quanticol.carma.core.typing.TypeProvider
import eu.quanticol.carma.core.generator.components.ComponentManager

class GenerateSystemEnvironmentUpdate {
	
	@Inject extension LabelUtil
	@Inject extension Predicates
	@Inject extension ExpressionHandler
	@Inject extension Util
	@Inject extension GeneratorUtils
	@Inject extension TypeProvider
		
	def String defineEnvironmentUpdatePredicates(System system, CarmaVariableManager cvm){
		
		var casts = system.getContainerOfType(Model).eAllOfType(EnvironmentUpdate)
		'''
		«FOR cast : casts»
		«cast.convertToPredicateName.getEvolutionRulePredicate("rstore","sstore",cast.guard.booleanExpression,cvm)»
		«ENDFOR»
		'''
		
	}
	
	def String defineEnvironmentUpdateRules(System system, CarmaVariableManager cvm, ComponentManager cm){
		'''
		«defineBroadcastUpdate(system,cvm,cm)»
		«defineUnicastUpdate(system,cvm,cm)»
		'''
	}
	
	def String defineBroadcastUpdate(System system, CarmaVariableManager cvm, ComponentManager cm){
		var updates = system.getContainerOfType(Model).eAllOfType(Probability)
		var casts = new ArrayList<EnvironmentOperation>()
		
		for(eu : updates)
			if(eu.stub.isBroadcast)
				casts.add(eu)
		
		'''
		@Override
		public void broadcastUpdate( RandomGenerator random , CarmaStore sstore , int action , Object value ){
			«casts.defineConditions(system,cvm,cm)»
		}
		'''
	}
	
	def String defineUnicastUpdate(System system, CarmaVariableManager cvm, ComponentManager cm){
		var updates = system.getContainerOfType(Model).eAllOfType(EnvironmentUpdate)
		var casts = new ArrayList<EnvironmentOperation>()
		
		for(eu : updates)
			if(!eu.stub.isBroadcast)
				casts.add(eu)
		
		'''
		@Override
		public void unicastUpdate( RandomGenerator random , CarmaStore sstore , CarmaStore rstore, int action , Object value ){
			«casts.defineConditions(system,cvm,cm)»
		}
		'''
	}
	
	def String defineConditions(ArrayList<EnvironmentOperation> casts, System system, CarmaVariableManager cvm, ComponentManager cm){
		var prefix = cvm.definitionsPrefix
		'''
		«FOR cast : casts»
		if(
		action == «prefix»«cast.stub.label.toUpperCase.replace("*","")»
		&&
		«cast.convertToPredicateName»(rstore).satisfy(sstore)
		){
			«outputUpdateBlock((cast as EnvironmentUpdate), cvm, cm)»
		}
		«ENDFOR»
		'''
	}
	
	def String outputUpdateBlock(EnvironmentUpdate updateBlock, CarmaVariableManager manager, ComponentManager cm){
		switch(updateBlock){
			NullUpdate: ''''''
			default: '''
		boolean hasAttributes = true;
		«FOR ua : updateBlock.eAllOfType(EnvironmentUpdateAssignment)»
		«defineEnvironmentUpdateBlock(ua, manager)»
		«ENDFOR»
		«FOR spawn : updateBlock.eAllOfType(Spawn)»
		«defineSpawn(spawn, manager, cm)»
		«ENDFOR»
		'''
		}
	}
	
	def String defineEnvironmentUpdateBlock(EnvironmentUpdateAssignment updateAssignment, CarmaVariableManager manager){
		var assign = updateAssignment.storeReference
		var expression = updateAssignment.expression
		var HashMap<String,VariableReference> vrs_r = getAllOutputR(expression,manager)
		var HashMap<String,VariableReference> vrs_s = getAllOutputS(expression,manager)
		var HashMap<String,VariableReference> vrs_g = getAllOutputG(expression,manager)
		'''
		«FOR item : manager.declareAllCheck(vrs_r,"r")»
		«item»
		«ENDFOR»
		«FOR item : manager.declareAllCheck(vrs_s,"s")»
		«item»
		«ENDFOR»
		«FOR item : manager.declareAll(vrs_g,"global_")»
		«item»
		«ENDFOR»
		«manager.setupStores(vrs_r,"r")»
		«manager.setupStores(vrs_s,"s")»
		«manager.setupStores(vrs_g,"global_")»
		«expression.express(manager,assign,"global_")»
		'''
	}
	
	def HashMap<String,VariableReference> getAllOutputR(EnvironmentUpdateExpressions expressions, CarmaVariableManager manager){
		var HashMap<String,VariableReference> vrs 	= new HashMap<String,VariableReference>()
		
		for(vr : expressions.eAllOfType(VariableReferenceReceiver)){
			vrs.put(manager.cleanName(vr.asFullJava),vr);
		}
		for(vr : expressions.eAllOfType(RecordReferenceReceiver)){
			vrs.put(manager.cleanName(vr.asFullJava),vr);
		}	
		return vrs
	}
	
	def HashMap<String,VariableReference> getAllOutputS(EnvironmentUpdateExpressions expressions, CarmaVariableManager manager){
		var HashMap<String,VariableReference> vrs 	= new HashMap<String,VariableReference>()
		
		for(vr : expressions.eAllOfType(VariableReferenceSender)){
			vrs.put(manager.cleanName(vr.asFullJava),vr);
		}
		for(vr : expressions.eAllOfType(RecordReferenceSender)){
			vrs.put(manager.cleanName(vr.asFullJava),vr);
		}	
		return vrs
	}
	
	def HashMap<String,VariableReference> getAllOutputG(EnvironmentUpdateExpressions expressions, CarmaVariableManager manager){
		var HashMap<String,VariableReference> vrs 	= new HashMap<String,VariableReference>()
		
		for(vr : expressions.eAllOfType(VariableReferencePure)){
			vrs.put(manager.cleanName(vr.asFullJava),vr);
		}
		for(vr : expressions.eAllOfType(RecordReferencePure)){
			vrs.put(manager.cleanName(vr.asFullJava),vr);
		}
		for(vr : expressions.eAllOfType(VariableReferenceGlobal)){
			vrs.put(manager.cleanName(vr.asFullJava),vr);
		}
		for(vr : expressions.eAllOfType(RecordReferenceGlobal)){
			vrs.put(manager.cleanName(vr.asFullJava),vr);
		}
		return vrs
	}
	

	
	def String setupStores(CarmaVariableManager manager, HashMap<String,VariableReference> vrs, String modifier){
		'''
		«FOR key : vrs.keySet»
		«var vr = vrs.get(key)»
		«manager.getCarmaName(key,vr).getStore("Integer.class",manager.getJavaAssign(key,vr,modifier),modifier+"store","hasAttributes")»
		«ENDFOR»
		'''
	}
	
	def String express(EnvironmentUpdateExpressions expressions, CarmaVariableManager manager, VariableReference vr, String modifier){
		var key = manager.cleanName(vr.asFullJava)
		'''
		if(hasAttributes){
			«manager.getCarmaName(key,vr).setStore("Integer.class",manager.getJavaAssign(key,vr,modifier),modifier+"store",expressions.asJavaEvolutionRule)»
		}
		'''
	}
	
	def String defineSpawn(Spawn sp, CarmaVariableManager cvm, ComponentManager cm){
		
		if(sp.eAllOfType(BlockSpawn).size > 0){
			sp.eAllOfType(BlockSpawn).get(0).blockSpawn(cvm,cm)
		} else if (sp.eAllOfType(LineSpawn).size > 0){
			sp.eAllOfType(LineSpawn).get(0).lineSpawn()
		}
	}
	
	def String blockSpawn(BlockSpawn bs, CarmaVariableManager cvm, ComponentManager manager){
		'''
		«FOR spawn : bs.eAllOfType(ComponentBlockNewDeclarationSpawn)»
		«defineSingleBlockSpawn(spawn,cvm,manager)»
		«ENDFOR»
		'''
	}
	
	def String defineSingleBlockSpawn(ComponentBlockNewDeclarationSpawn spawn, CarmaVariableManager cvm, ComponentManager manager){
		var HashMap<String,VariableReference> vrs_r = getAllOutputR(spawn,cvm)
		var HashMap<String,VariableReference> vrs_s = getAllOutputS(spawn,cvm)
		var HashMap<String,VariableReference> vrs_g = getAllOutputG(spawn,cvm)
		'''
		«FOR item : cvm.declareAll(vrs_r,"r")»
		«item»
		«ENDFOR»
		«FOR item : cvm.declareAll(vrs_s,"s")»
		«item»
		«ENDFOR»
		«FOR item : cvm.declareAll(vrs_g,"global_")»
		«item»
		«ENDFOR»
		«cvm.setupStores(vrs_r,"r")»
		«cvm.setupStores(vrs_s,"s")»
		«cvm.setupStores(vrs_g,"global_")»
		«var sp = manager.spawns.get(spawn.hashCode)»
		if(hasAttributes){
			«FOR l : sp.produce»
			«singleDeclaration(sp.name,l.convertArrayList)»
			«ENDFOR»
		}
		'''
	}
	
	def String lineSpawn(LineSpawn bs){
		//TODO
	}
	
	def String singleDeclaration(String name, String args){
		'''
		addComponent(get«name»(«args»));
		'''
	}
	
	def String convertArrayList(ArrayList<String> list){
		var output = ""
		
		if(list.size > 0){
			output = list.get(0)
			for(var i = 1; i < list.size; i++){
				output = output + "," + list.get(i)
			}
		}
		
		return output;
	}
	
	def HashMap<String,VariableReference> getAllOutputR(ComponentBlockNewDeclarationSpawn expressions, CarmaVariableManager manager){
		var HashMap<String,VariableReference> vrs 	= new HashMap<String,VariableReference>()
		
		for(vr : expressions.eAllOfType(VariableReferenceReceiver)){
			vrs.put(manager.cleanName(vr.asFullJava),vr);
		}
		for(vr : expressions.eAllOfType(RecordReferenceReceiver)){
			vrs.put(manager.cleanName(vr.asFullJava),vr);
		}	
		return vrs
	}
	
	def HashMap<String,VariableReference> getAllOutputS(ComponentBlockNewDeclarationSpawn expressions, CarmaVariableManager manager){
		var HashMap<String,VariableReference> vrs 	= new HashMap<String,VariableReference>()
		
		for(vr : expressions.eAllOfType(VariableReferenceSender)){
			vrs.put(manager.cleanName(vr.asFullJava),vr);
		}
		for(vr : expressions.eAllOfType(RecordReferenceSender)){
			vrs.put(manager.cleanName(vr.asFullJava),vr);
		}	
		return vrs
	}
	
	def HashMap<String,VariableReference> getAllOutputG(ComponentBlockNewDeclarationSpawn expressions, CarmaVariableManager manager){
		var HashMap<String,VariableReference> vrs 	= new HashMap<String,VariableReference>()
		
		for(vr : expressions.eAllOfType(VariableReferencePure)){
			vrs.put(manager.cleanName(vr.asFullJava),vr);
		}
		for(vr : expressions.eAllOfType(RecordReferencePure)){
			vrs.put(manager.cleanName(vr.asFullJava),vr);
		}
		for(vr : expressions.eAllOfType(VariableReferenceGlobal)){
			vrs.put(manager.cleanName(vr.asFullJava),vr);
		}
		for(vr : expressions.eAllOfType(RecordReferenceGlobal)){
			vrs.put(manager.cleanName(vr.asFullJava),vr);
		}
		return vrs
	}
	
}