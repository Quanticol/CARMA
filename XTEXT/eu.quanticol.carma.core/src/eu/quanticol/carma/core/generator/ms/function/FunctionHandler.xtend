package eu.quanticol.carma.core.generator.ms.function

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.AttribParameter
import eu.quanticol.carma.core.carma.AttribType
import eu.quanticol.carma.core.carma.AttribVariableDeclaration
import eu.quanticol.carma.core.carma.DoubleParameter
import eu.quanticol.carma.core.carma.DoubleType
import eu.quanticol.carma.core.carma.DoubleVariableDeclaration
import eu.quanticol.carma.core.carma.FunctionAssignment
import eu.quanticol.carma.core.carma.FunctionDeclaration
import eu.quanticol.carma.core.carma.FunctionDefinition
import eu.quanticol.carma.core.carma.FunctionForStatement
import eu.quanticol.carma.core.carma.FunctionIfStatement
import eu.quanticol.carma.core.carma.FunctionReturn
import eu.quanticol.carma.core.carma.FunctionStatement
import eu.quanticol.carma.core.carma.Functions
import eu.quanticol.carma.core.carma.IntgerParameter
import eu.quanticol.carma.core.carma.IntgerType
import eu.quanticol.carma.core.carma.IntgerVariableDeclaration
import eu.quanticol.carma.core.carma.Parameter
import eu.quanticol.carma.core.carma.ProcessParameter
import eu.quanticol.carma.core.carma.RecordDeclaration
import eu.quanticol.carma.core.carma.RecordParameter
import eu.quanticol.carma.core.carma.RecordType
import eu.quanticol.carma.core.carma.Type
import eu.quanticol.carma.core.generator.ms.SharedJavaniser
import java.util.ArrayList

import static extension org.eclipse.emf.ecore.util.EcoreUtil.*
import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.core.typing.TypeProvider

class FunctionHandler {
	
	@Inject extension SharedJavaniser
	@Inject extension TypeProvider
	
	def String printFunctions(Functions functions){
		'''
		«FOR function : functions.functions»
		«function.getFunction»
		«ENDFOR»
		'''
	}
	
	def String getFunction(FunctionDefinition functionDefinition){
		
		var String type = functionDefinition.type.type.type.javanise
		var String name = functionDefinition.name.name.toFirstLower
		var ArrayList<Parameter> parameters = new ArrayList<Parameter>(functionDefinition.eAllOfType(Parameter))
		var ArrayList<FunctionStatement> functionStatements = new ArrayList<FunctionStatement>(functionDefinition.functionBody.statements)
		var FunctionReturn functionReturn = functionDefinition.eAllOfType(FunctionReturn).get(0)
		'''
		public «type» «name» ( «parameters.getParameters» ) {
			«FOR functionStatement : functionStatements»
				«IF functionDefinition.isAncestor(functionStatement)»
				«functionStatement.fjavanise»;
				«ENDIF»
			«ENDFOR»
			«functionReturn.fjavanise»
		}
		'''
	}
	
	//Why here? - because this we want to handle the naming attrib_, my_ etc depending on the context...
	def dispatch String fjavanise(FunctionStatement functionStatement){	
		switch(functionStatement){
			FunctionDeclaration	: functionStatement.fjavanise 
			FunctionAssignment	: functionStatement.fjavanise 
			FunctionIfStatement : functionStatement.fjavanise 
			FunctionForStatement: functionStatement.fjavanise 
		}
	}
	
	def dispatch String fjavanise(FunctionDeclaration functionDeclaration){
		switch(functionDeclaration){
			AttribVariableDeclaration	: functionDeclaration.fjavanise
			IntgerVariableDeclaration	: functionDeclaration.fjavanise
			DoubleVariableDeclaration	: functionDeclaration.fjavanise
			RecordDeclaration			: functionDeclaration.fjavanise
		}
	}
	
	//ALWAYS attrib_
	def dispatch String fjavanise(AttribVariableDeclaration attribDeclaration){
		'''«(attribDeclaration.type as Type).type.javanise» attrib_«attribDeclaration.name.name» = «attribDeclaration.assign.javanise»'''
	}
	
	def dispatch String fjavanise(IntgerVariableDeclaration intgerDeclaration){
		'''«(intgerDeclaration.type as Type).type.javanise» attrib_«intgerDeclaration.name.name» = «intgerDeclaration.assign.javanise»'''
	}
	
	def dispatch String fjavanise(DoubleVariableDeclaration doubleDeclaration){
		'''«(doubleDeclaration.type as Type).type.javanise» attrib_«doubleDeclaration.name.name» = «doubleDeclaration.assign.javanise»'''
	}
	
	def dispatch String fjavanise(RecordDeclaration recordDeclaration){
		'''«(recordDeclaration.type as Type).type.javanise» attrib_«recordDeclaration.name.name» = «recordDeclaration.assign.javanise»'''
	}
	
	def dispatch String fjavanise(FunctionAssignment functionAssignment){
		'''«functionAssignment.reference.javanise» = «functionAssignment.expression.javanise»'''
	}
	
	def dispatch String fjavanise(FunctionIfStatement functionIfStatement){
		var toReturn = 
		'''
		if («functionIfStatement.expression.javanise») {
			«FOR functionStatement : functionIfStatement.thenBlock.statements»
			«functionStatement.fjavanise»;
			«ENDFOR»
		}'''
		if(functionIfStatement.elseBlock != null){
			toReturn = toReturn + 
		'''
		else {
			«FOR functionStatement : functionIfStatement.elseBlock.statements»
			«functionStatement.fjavanise»;
			«ENDFOR»
		}
		'''
		}
		return toReturn
	}
	
	def dispatch String fjavanise(FunctionForStatement functionForStatement){
		'''
		for( «functionForStatement.variable.javanise » ; «functionForStatement.expression.javanise » ; «functionForStatement.afterThought.functionAssignment.javanise» ){
			«FOR functionStatement : functionForStatement.functionForBlock.statements»
			«functionStatement.fjavanise»;
			«ENDFOR»		
		}'''
	}
	
	def dispatch String fjavanise(FunctionReturn functionReturn){
		'''return «functionReturn.expression.javanise»; '''
	}
	
	def  String getParameters(ArrayList<Parameter> parameters){
		var String toReturn = ""
		if(parameters.size > 0){
			toReturn = parameters.get(0).getParameter
			for(var i = 1; i < parameters.size; i++){
				toReturn = toReturn + ", " + parameters.get(i).getParameter
			}
		}
		return toReturn
	}
	
	def  String getParameter(Parameter parameter){
		switch(parameter){
			AttribParameter: '''«(parameter.type as AttribType).type.javanise» attrib_«parameter.name.name»'''
			RecordParameter: '''«(parameter.type as RecordType).type.javanise» attrib_«parameter.name.name»'''
			DoubleParameter: '''«(parameter.type as DoubleType).type.javanise» attrib_«parameter.name.name»'''
			IntgerParameter: '''«(parameter.type as IntgerType).type.javanise» attrib_«parameter.name.name»'''
			ProcessParameter: '''ArrayList<String> behaviour'''
		}
	}
	

}