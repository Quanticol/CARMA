package eu.quanticol.carma.core.generator.ms.function

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.Functions
import eu.quanticol.carma.core.carma.FunctionDefinition
import eu.quanticol.carma.core.carma.FunctionReturn
import eu.quanticol.carma.core.carma.FunctionStatement
import eu.quanticol.carma.core.carma.Parameter
import java.util.ArrayList

import static extension org.eclipse.xtext.EcoreUtil2.*

class FunctionHandler {
	
	@Inject extension FunctionJavaniser
	
	def String printFunctions(Functions functions){
		'''
		«FOR function : functions.functions»
		«function.getFunction»
		«ENDFOR»
		'''
	}
	
	def String getFunction(FunctionDefinition functionDefinition){
		
		var String type = functionDefinition.type.javanise
		var String name = functionDefinition.name.name.toFirstLower
		var ArrayList<Parameter> parameters = new ArrayList<Parameter>(functionDefinition.eAllOfType(Parameter))
		var ArrayList<FunctionStatement> functionStatements = new ArrayList<FunctionStatement>(functionDefinition.functionBody.statements)
		var FunctionReturn functionReturn = functionDefinition.eAllOfType(FunctionReturn).get(0)
		'''
		public static «type» «name» ( «parameters.getParameters» ) {
			«FOR functionStatement : functionStatements»
				«IF functionDefinition.isAncestor(functionStatement)»
				«functionStatement.javanise»
				«ENDIF»
			«ENDFOR»
			«functionReturn.javanise»
		}
		'''
	}
	

}