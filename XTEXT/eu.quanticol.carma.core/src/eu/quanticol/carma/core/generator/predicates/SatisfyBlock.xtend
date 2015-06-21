package eu.quanticol.carma.core.generator.predicates

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.BooleanExpressions
import eu.quanticol.carma.core.carma.InputActionArguments
import eu.quanticol.carma.core.carma.RecordReferenceMy
import eu.quanticol.carma.core.carma.RecordReferencePure
import eu.quanticol.carma.core.carma.RecordReferenceThis
import eu.quanticol.carma.core.carma.VariableName
import eu.quanticol.carma.core.carma.VariableReference
import eu.quanticol.carma.core.carma.VariableReferenceMy
import eu.quanticol.carma.core.carma.VariableReferencePure
import eu.quanticol.carma.core.carma.VariableReferenceThis
import eu.quanticol.carma.core.generator.ExpressionHandler
import eu.quanticol.carma.core.generator.GeneratorUtils
import eu.quanticol.carma.core.generator.actions.NullBooleanExpression
import eu.quanticol.carma.core.generator.carmavariable.CarmaVariableManager
import eu.quanticol.carma.core.utils.LabelUtil
import java.util.ArrayList
import java.util.HashMap

import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.core.carma.VariableReferenceReceiver
import eu.quanticol.carma.core.carma.RecordReferenceReceiver
import eu.quanticol.carma.core.carma.VariableReferenceSender
import eu.quanticol.carma.core.carma.RecordReferenceSender
import eu.quanticol.carma.core.carma.VariableReferenceGlobal
import eu.quanticol.carma.core.carma.RecordReferenceGlobal

class SatisfyBlock {
	
	@Inject extension LabelUtil
	@Inject extension GeneratorUtils
	@Inject extension ExpressionHandler
	
	def String getMeasureSatisfyBlock(BooleanExpressions bes, CarmaVariableManager manager){
		var HashMap<String,VariableReference> vrs = getAllInput(bes,manager)
		'''
		boolean hasAttributes = true;
		«FOR item : manager.declareAll(vrs,"")»
		«item»
		«ENDFOR»
		«manager.setupStores(vrs,"")»
		«bes.express»
		'''
	}
	
	def String getGuardSatisfyBlock(BooleanExpressions bes, CarmaVariableManager manager){
		var vrs = getAll(bes,manager)
		'''
		boolean hasAttributes = true;
		«FOR item : manager.declareAll(vrs,"")»
		«item»
		«ENDFOR»
		«manager.setupStores(vrs,"")»
		«bes.express»
		'''
	}
	
	def HashMap<String,VariableReference> getAll(BooleanExpressions expressions, CarmaVariableManager manager){
		var HashMap<String,VariableReference> vrs 	= new HashMap<String,VariableReference>()
		for(vr : expressions.eAllOfType(VariableReference)){
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
	
	def String express(BooleanExpressions expressions){
		'''
		if(hasAttributes){
			return «expressions.evaluateExpression»;
		} else {
			return false;
		}
		'''
	}
	
	def String getInputSatisfyBlock(BooleanExpressions bes, InputActionArguments value, CarmaVariableManager manager){
		switch(bes){
			NullBooleanExpression: '''return true;'''
			default: {
				var HashMap<String,VariableReference> vrs = getAllInput(bes,manager)
				'''
				boolean hasAttributes = true;
				«FOR item : manager.declareAll(vrs,"i")»
				«item»
				«ENDFOR»
				«setupInputArguments(value)»
				«manager.setupStores(vrs,"i")»
				«bes.expressInputAction»
				'''
			}
		}
		
	}
	
	def HashMap<String,VariableReference> getAllInput(BooleanExpressions expressions, CarmaVariableManager manager){
		var HashMap<String,VariableReference> vrs 	= new HashMap<String,VariableReference>()
		
		for(vr : expressions.eAllOfType(VariableReference)){
			if(manager.contains(manager.cleanName(vr.asJava)))
				vrs.put(manager.cleanName(vr.asFullJava),vr);
		}
		return vrs
	}
	
	def String setupInputArguments(InputActionArguments value){
		var ArrayList<VariableName> vns = new ArrayList<VariableName>(value.eAllOfType(VariableName))
		'''
		«FOR vn : vns»
		int «vn.label»_i = ((int[]) value)[«vns.indexOf(vn)»];
		«ENDFOR»
		'''	
	}
	
	def String expressInputAction(BooleanExpressions expressions){
		'''
		if(hasAttributes){
			return «expressions.asJavaInputAction»;
		} else {
			return false;
		}
		'''
	}
	
	def String getOutputSatisfyBlock(BooleanExpressions bes, CarmaVariableManager manager){
		switch(bes){
			NullBooleanExpression: '''return true;'''
			default: {
				var HashMap<String,VariableReference> vrs_i = getAllOutputI(bes,manager)
				var HashMap<String,VariableReference> vrs_o = getAllOutputO(bes,manager)
				'''
				boolean hasAttributes = true;
				«FOR item : manager.declareAllCheck(vrs_i,"i")»
				«item»
				«ENDFOR»
				«FOR item : manager.declareAllCheck(vrs_o,"o")»
				«item»
				«ENDFOR»
				«manager.setupStores(vrs_i,"i")»
				«manager.setupStores(vrs_o,"o")»
				«bes.expressOutputAction»
				'''
			}
		}
	}
	
	def HashMap<String,VariableReference> getAllOutputI(BooleanExpressions expressions, CarmaVariableManager manager){
		var HashMap<String,VariableReference> vrs 	= new HashMap<String,VariableReference>()
		
		for(vr : expressions.eAllOfType(VariableReferencePure)){
			vrs.put(manager.cleanName(vr.asFullJava),vr);
		}
		for(vr : expressions.eAllOfType(RecordReferencePure)){
			vrs.put(manager.cleanName(vr.asFullJava),vr);
		}	
		return vrs
	}
	
	def HashMap<String,VariableReference> getAllOutputO(BooleanExpressions expressions, CarmaVariableManager manager){
		var HashMap<String,VariableReference> vrs 	= new HashMap<String,VariableReference>()

		for(vr : expressions.eAllOfType(VariableReferenceMy)){
			vrs.put(manager.cleanName(vr.asFullJava),vr);
		}
		for(vr : expressions.eAllOfType(VariableReferenceThis)){
			vrs.put(manager.cleanName(vr.asFullJava),vr);
		}
		for(vr : expressions.eAllOfType(RecordReferenceMy)){
			vrs.put(manager.cleanName(vr.asFullJava),vr);
		}		
		for(vr : expressions.eAllOfType(RecordReferenceThis)){
			vrs.put(manager.cleanName(vr.asFullJava),vr);
		}
		return vrs	
	}
	
	def String expressOutputAction(BooleanExpressions expressions){
		'''
		if(hasAttributes){
			return «expressions.asJavaOutputAction»;
		} else {
			return false;
		}
		'''
	}
	
	def String getCastSatisfyBlock(BooleanExpressions bes, CarmaVariableManager manager){
		switch(bes){
			NullBooleanExpression: '''return false;'''
			default: {
				var HashMap<String,VariableReference> vrs_r = getAllOutputR(bes,manager)
				var HashMap<String,VariableReference> vrs_s = getAllOutputS(bes,manager)
				var HashMap<String,VariableReference> vrs_g = getAllOutputG(bes,manager)
				'''
				boolean hasAttributes = true;
				«FOR item : manager.declareAll(vrs_r,"r")»
				«item»
				«ENDFOR»
				«FOR item : manager.declareAll(vrs_s,"s")»
				«item»
				«ENDFOR»
				«FOR item : manager.declareAll(vrs_g,"global_")»
				«item»
				«ENDFOR»
				«manager.setupStores(vrs_r,"r")»
				«manager.setupStores(vrs_s,"s")»
				«manager.setupStores(vrs_g,"global_")»
				«bes.expressEvolutionRule»
				'''
			}
		}
	}
	
	def String expressEvolutionRule(BooleanExpressions expressions){
		'''
		if(hasAttributes){
			return «expressions.asJavaEvolutionRule»;
		} else {
			return false;
		}
		'''
	}
	
	def HashMap<String,VariableReference> getAllOutputR(BooleanExpressions expressions, CarmaVariableManager manager){
		var HashMap<String,VariableReference> vrs 	= new HashMap<String,VariableReference>()
		
		for(vr : expressions.eAllOfType(VariableReferenceReceiver)){
			vrs.put(manager.cleanName(vr.asFullJava),vr);
		}
		for(vr : expressions.eAllOfType(RecordReferenceReceiver)){
			vrs.put(manager.cleanName(vr.asFullJava),vr);
		}	
		return vrs
	}
	
	def HashMap<String,VariableReference> getAllOutputS(BooleanExpressions expressions, CarmaVariableManager manager){
		var HashMap<String,VariableReference> vrs 	= new HashMap<String,VariableReference>()
		
		for(vr : expressions.eAllOfType(VariableReferenceSender)){
			vrs.put(manager.cleanName(vr.asFullJava),vr);
		}
		for(vr : expressions.eAllOfType(RecordReferenceSender)){
			vrs.put(manager.cleanName(vr.asFullJava),vr);
		}	
		return vrs
	}
	
	def HashMap<String,VariableReference> getAllOutputG(BooleanExpressions expressions, CarmaVariableManager manager){
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