package eu.quanticol.carma.core.generator.predicates

import eu.quanticol.carma.core.carma.BooleanExpressions
import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.core.carma.VariableReference
import com.google.inject.Inject
import eu.quanticol.carma.core.utils.LabelUtil
import eu.quanticol.carma.core.generator.carmaVariable.CarmaVariableManager
import eu.quanticol.carma.core.generator.GeneratorUtils
import eu.quanticol.carma.core.generator.ExpressionHandler

class SatisfyBlock {
	
	@Inject extension LabelUtil
	@Inject extension GeneratorUtils
	@Inject extension ExpressionHandler
	
	def String getSatisfyBlock(BooleanExpressions bes){
		
		
	}
	
	def String getSatisfyBlock(BooleanExpressions bes, Object value){
		
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
		«manager.getJavaDeclaration(name,vr)» = 0;
		«ENDFOR»
		'''
	}
	
	def String setupStores(BooleanExpressions expressions, CarmaVariableManager manager){
		var vrs = expressions.eAllOfType(VariableReference)
		'''
		«FOR vr : vrs»
		«var name = manager.cleanName(vr.label)»
		«manager.getCarmaName(name,vr).getStore("Integer.class",manager.getJavaAssign(name,vr),"store","hasAttributes")»
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