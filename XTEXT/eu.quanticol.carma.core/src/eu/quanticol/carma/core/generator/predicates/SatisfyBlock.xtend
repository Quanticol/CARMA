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
import eu.quanticol.carma.core.generator.carmaVariable.CarmaVariableManager
import eu.quanticol.carma.core.utils.LabelUtil
import java.util.ArrayList
import java.util.HashMap

import static extension org.eclipse.xtext.EcoreUtil2.*

class SatisfyBlock {
	
	@Inject extension LabelUtil
	@Inject extension GeneratorUtils
	@Inject extension ExpressionHandler
	
	def String getInputSatisfyBlock(BooleanExpressions bes, InputActionArguments value, CarmaVariableManager manager){
		switch(bes){
			NullBooleanExpression: '''return true;'''
			default: '''
		boolean hasAttributes = true;
		«bes.declareAll(manager)»
		«setupInputArguments(value)»
		«bes.setupInputActionStores(manager)»
		«bes.express»
		'''
		}
		
	}
	
	def String setupInputArguments(InputActionArguments value){
		var ArrayList<VariableName> vns = new ArrayList<VariableName>(value.eAllOfType(VariableName))
		'''
		«FOR vn : vns»
		«vn.label» = ((int[]) value)[«vns.indexOf(vn)»];
		«ENDFOR»
		'''	
	}
	
	def String getOutputSatisfyBlock(BooleanExpressions bes, CarmaVariableManager manager){
		switch(bes){
			NullBooleanExpression: '''return true;'''
			default: '''
		boolean hasAttributes = true;
		«bes.declareAll(manager)»
		«bes.setupOutputActionStores(manager)»
		«bes.express»
		'''
		}
		
	}
	
	//always its own Store, no "outside" values
	def String getGuardSatisfyBlock(BooleanExpressions bes, CarmaVariableManager manager){
		'''
		boolean hasAttributes = true;
		«bes.declareAll(manager)»
		«bes.setupStores(manager)»
		«bes.express»
		'''
	}
	
	def String declareAll(BooleanExpressions expressions, CarmaVariableManager manager){
		var vrs = expressions.eAllOfType(VariableReference)
		'''
		«FOR vr : vrs»
		«var name = manager.cleanName(vr.label)»
		«manager.getJavaDeclaration(name,vr,"")» = 0;
		«ENDFOR»
		'''
	}
	
	def String setupStores(BooleanExpressions expressions, CarmaVariableManager manager){
		var vrs = expressions.eAllOfType(VariableReference)
		'''
		«FOR vr : vrs»
		«var name = manager.cleanName(vr.label)»
		«manager.getCarmaName(name,vr).getStore("Integer.class",manager.getJavaAssign(name,vr,""),"store","hasAttributes")»
		«ENDFOR»
		'''
	}
	
	def String setupInputActionStores(BooleanExpressions expressions, CarmaVariableManager manager){
		var HashMap<String,VariableReference> vrs 	= new HashMap<String,VariableReference>()
		
		for(vr : expressions.eAllOfType(VariableReferencePure)){
			vrs.put(manager.cleanName(vr.name.label),vr);
		}
		for(vr : expressions.eAllOfType(VariableReferenceMy)){
			vrs.put(manager.cleanName(vr.name.label),vr);
		}
		for(vr : expressions.eAllOfType(VariableReferenceThis)){
			vrs.put(manager.cleanName(vr.name.label),vr);
		}
		for(vr : expressions.eAllOfType(RecordReferencePure)){
			vrs.put(manager.cleanName(vr.name.label+"_"+vr.record.label),vr);
		}	
		for(vr : expressions.eAllOfType(RecordReferenceMy)){
			vrs.put(manager.cleanName(vr.name.label+"_"+vr.record.label),vr);
		}		
		for(vr : expressions.eAllOfType(RecordReferenceThis)){
			vrs.put(manager.cleanName(vr.name.label+"_"+vr.record.label),vr);
		}				
		'''
		«FOR key : vrs.keySet»
		«IF manager.contains(key)»
		«manager.getCarmaName(key,vrs.get(key)).getStore("Integer.class",manager.getJavaAssign(key,vrs.get(key),""),"inputStore","hasAttributes")»
		«ENDIF»
		«ENDFOR»
		'''
	}
	
	def String setupOutputActionStores(BooleanExpressions expressions, CarmaVariableManager manager){
		var HashMap<String,VariableReference> vrs_ia 	= new HashMap<String,VariableReference>()
		var HashMap<String,VariableReference> vrs_oa 	= new HashMap<String,VariableReference>()
		
		for(vr : expressions.eAllOfType(VariableReferencePure)){
			vrs_ia.put(manager.cleanName(vr.label),vr);
		}
		for(vr : expressions.eAllOfType(VariableReferenceMy)){
			vrs_oa.put(manager.cleanName(vr.label),vr);
		}
		for(vr : expressions.eAllOfType(VariableReferenceThis)){
			vrs_oa.put(manager.cleanName(vr.label),vr);
		}
		for(vr : expressions.eAllOfType(RecordReferencePure)){
			vrs_ia.put(manager.cleanName(vr.label),vr);
		}	
		for(vr : expressions.eAllOfType(RecordReferenceMy)){
			vrs_oa.put(manager.cleanName(vr.label),vr);
		}		
		for(vr : expressions.eAllOfType(RecordReferenceThis)){
			vrs_oa.put(manager.cleanName(vr.label),vr);
		}	
		'''
		«FOR key : vrs_ia.keySet»
		«manager.getCarmaName(key,vrs_ia.get(key)).getStore("Integer.class",manager.getJavaAssign(key,vrs_ia.get(key),""),"inputStore","hasAttributes")»
		«ENDFOR»
		«FOR key : vrs_oa.keySet»
		«manager.getCarmaName(key,vrs_oa.get(key)).getStore("Integer.class",manager.getJavaAssign(key,vrs_oa.get(key),""),"outputStore","hasAttributes")»
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

	
}