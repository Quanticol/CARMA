package eu.quanticol.carma.core.generator.ms.function

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.FunctionDefinition

import eu.quanticol.carma.core.utils.Util
import org.eclipse.xtend.expression.TypeSystem
import eu.quanticol.carma.core.generator.ms.expression.ExpressionHandler

class FunctionHandler {
	
	@Inject extension Util
	@Inject extension ExpressionHandler
		
	def String printFunctions(Iterable<FunctionDefinition> functions){
		'''
		«FOR function : functions»
		«function.getFunction»
		«ENDFOR»
		'''
	}
	
	def String getFunction(FunctionDefinition functionDefinition){
		
		'''
		public static «functionDefinition.type.toJavaType» «functionDefinition.name.functionName» ( 
			«FOR p:functionDefinition.parameters SEPARATOR ','»«p.toJavaDeclaration»«ENDFOR»
		) {
			return «functionDefinition.body.expressionToJava»;
		}
		'''
	}
	

}