package eu.quanticol.carma.core.generator.ms

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.FunctionAssignment
import eu.quanticol.carma.core.carma.FunctionDeclaration
import eu.quanticol.carma.core.carma.FunctionDefinition
import eu.quanticol.carma.core.carma.FunctionForStatement
import eu.quanticol.carma.core.carma.FunctionIfStatement
import eu.quanticol.carma.core.carma.FunctionReturn
import eu.quanticol.carma.core.carma.FunctionStatement
import eu.quanticol.carma.core.carma.Parameter
import java.util.ArrayList

class FunctionHandler {
	
	@Inject extension Javaniser
	
	def String getUniform(){
		'''
		public static int uniform(ArrayList<Integer> input){
			RandomGenerator random = new DefaultRandomGenerator();
			return input.get(random.nextInt(input.size()));
		}
		'''
	}
	
	def String getFunction(FunctionDefinition fd){
		
		var String type
		var String name
		var ArrayList<Parameter> parameters
		var ArrayList<FunctionStatement> functionStatements
		var FunctionReturn functionReturn
		'''
		public static «type» «name» ( «parameters.getParameters» ) {
			«FOR functionStatement : functionStatements»
				«functionStatement.getFunctionStatement»
			«ENDFOR»
			«functionReturn.getFunctionReturn»
		}
		'''
	}
	
	def String getFunctionStatement(FunctionStatement functionStatement){
		switch(functionStatement){
			FunctionDeclaration	:
			FunctionAssignment	:
			FunctionIfStatement :
			FunctionForStatement:
		}
	}
	
	def String getFunctionReturn(FunctionReturn functionReturn){
		''' «functionReturn.expression.javanise»; '''
	}
}