package eu.quanticol.carma.core.generator.components

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.BooleanExpressions
import eu.quanticol.carma.core.carma.ForVariableDeclaration
import eu.quanticol.carma.core.carma.MethodExpressions
import eu.quanticol.carma.core.carma.VariableReference
import eu.quanticol.carma.core.generator.ExpressionHandler
import java.util.ArrayList
import eu.quanticol.carma.core.utils.LabelUtil

class ComponentNew {
	
	
	@Inject extension ExpressionHandler
	@Inject extension LabelUtil
	
	def String setupComponents(ComponentManager cm){
		'''
		«FOR key : cm.newDecs.keySet»
		«var cn = cm.newDecs.get(key)»
		«IF cn.isFor»
		«cn.fvd.forBlock(cn.bes,cn.vr,cn.mes,cn.getname,cn.arguments)»
		«ENDIF»
		«FOR list : cn.produce»
		«singleDeclaration(cn.getname,convertArrayList(list))»
		«ENDFOR»
		«ENDFOR»
		'''
	}
	
	
	def String forBlock(ForVariableDeclaration fvd, 
		BooleanExpressions bes, 
		VariableReference vr, 
		MethodExpressions mes,
		String componentName,
		ArrayList<String> args
	){
		'''
		for(«fvd.declareAsJava»;«bes.evaluateExpression»;«vr.asJava» = «mes.evaluateExpression»){
			«componentName.singleDeclaration(args.convertArrayList)»
		};
		'''
	}
	
	def String declareAsJava(ForVariableDeclaration fvd){
		return "int " + fvd.name.label + " = " + fvd.assign.label
	}
	
	def String singleDeclaration(String componentName, String args){
		'''
		addComponent(get«componentName»(«args»));
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
	
}