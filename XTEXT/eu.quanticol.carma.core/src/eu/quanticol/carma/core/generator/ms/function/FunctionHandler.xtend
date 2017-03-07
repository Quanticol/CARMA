package eu.quanticol.carma.core.generator.ms.function

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.FunctionDefinition

import eu.quanticol.carma.core.utils.Util
import eu.quanticol.carma.core.generator.ms.expression.ExpressionHandler
import eu.quanticol.carma.core.carma.IfThenElseCommand
import eu.quanticol.carma.core.carma.ReturnCommand
import eu.quanticol.carma.core.carma.VariableDeclarationCommand
import eu.quanticol.carma.core.carma.ForCommand
import eu.quanticol.carma.core.carma.BlockCommand
import eu.quanticol.carma.core.carma.AssignmentCommand
import eu.quanticol.carma.core.carma.AssignmentTarget
import eu.quanticol.carma.core.carma.FieldAssignment
import eu.quanticol.carma.core.carma.TargetAssignmentVariable
import eu.quanticol.carma.core.carma.TargetAssignmentField
import eu.quanticol.carma.core.carma.ForEach
import eu.quanticol.carma.core.typing.TypeSystem
import eu.quanticol.carma.core.typing.CarmaType
import eu.quanticol.carma.core.carma.TargetAssignmentList
import eu.quanticol.carma.core.carma.Expression

class FunctionHandler {
	
	@Inject extension Util
	@Inject extension ExpressionHandler
	@Inject extension TypeSystem
		
	def String printFunctions(Iterable<FunctionDefinition> functions){
		'''
		«FOR function : functions»
		«function.getFunction»
		«ENDFOR»
		'''
	}
	
	def String getFunction(FunctionDefinition functionDefinition){
		
		'''
		public «functionDefinition.type.toJavaType» «functionDefinition.name.functionName» ( 
			«FOR p:functionDefinition.parameters SEPARATOR ','»«p.toJavaDeclaration»«ENDFOR»
		) {
			«functionDefinition.body.functionBodyToJava»
		}
		'''
	}
	
	def dispatch CharSequence functionBodyToJava( IfThenElseCommand  c ) {
		'''
		if («c.condition.expressionToJava») «c.thenBlock.functionBodyToJava»
		«IF c.elseBlock!=null»else «c.elseBlock.functionBodyToJava»«ENDIF»
		'''
	}

	def dispatch CharSequence functionBodyToJava( ReturnCommand  c ) {
		'''return «c.expression.expressionToJava»;'''
	}

	def dispatch CharSequence functionBodyToJava( VariableDeclarationCommand  c ) {
		'''«c.variable.type.toJavaType» «c.variable.name.variableName» «IF c.value!=null»=«c.value.expressionToJava»«ENDIF»;'''
	}

	def dispatch CharSequence functionBodyToJava( AssignmentCommand c ) {
//		'''«c.target.assignmentTargetToJava» = «c.value.expressionToJava»;'''
		c.target.assignCommand(c.value)
	}
	
	def CharSequence assignCommand( AssignmentTarget t , Expression e ) {
		switch t {
			TargetAssignmentVariable: '''«t.variable.name.variableName» = «e.expressionToJava»;'''
			TargetAssignmentField: '''«t.target.assignmentTargetToJava».«t.field.name.fieldName» = «e.expressionToJava»;'''
			TargetAssignmentList: '''«t.target.assignmentTargetToJava».set( «t.index.expressionToJava» , «e.expressionToJava»);'''
		}
	}
	
	def CharSequence assignmentTargetToJava( AssignmentTarget t ) {
		switch t {
			TargetAssignmentVariable: t.variable.name.variableName
			TargetAssignmentField: '''«t.target.assignmentTargetToJava».«t.field.name.fieldName»'''
			TargetAssignmentList: '''«t.target.assignmentTargetToJava».get( «t.index.expressionToJava» )'''
		}
	}

	def dispatch CharSequence functionBodyToJava( ForCommand  c ) {
		'''
		for( int «c.variable.name.variableName» = «IF c.start==null»0«ELSE»«c.start.expressionToJava»«ENDIF» ; «c.variable.name.variableName» < «c.end.expressionToJava» ; «c.variable.name.variableName» += «IF c.step==null»1«ELSE»«c.step.expressionToJava»«ENDIF» ) 
			«c.body.functionBodyToJava»
		'''
	}

	def dispatch CharSequence functionBodyToJava( ForEach  c ) {
		var eType = c.iteration.typeOf
		if (eType.isNone) {
			'''
			//ERROR!!!
			'''
		} else {
			'''
			for( «eType.toJavaType(false)» «c.iteration.name.variableName»:  «c.iteration.value.expressionToJava» ) 
				«c.body.functionBodyToJava»
			'''
		}
	}

	def dispatch CharSequence functionBodyToJava( BlockCommand  b ) {
		'''
		{
			«FOR c:b.commands»
			//
			«c.functionBodyToJava»
			//
			«ENDFOR»	
		}
		'''
	}
			
}