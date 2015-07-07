package eu.quanticol.carma.core.typing

import eu.quanticol.carma.core.carma.Addition
import eu.quanticol.carma.core.carma.And
import eu.quanticol.carma.core.carma.AtomicMeasure
import eu.quanticol.carma.core.carma.AtomicMethodReference
import eu.quanticol.carma.core.carma.AtomicNow
import eu.quanticol.carma.core.carma.AtomicOutcome
import eu.quanticol.carma.core.carma.AtomicPrimitive
import eu.quanticol.carma.core.carma.AtomicRecord
import eu.quanticol.carma.core.carma.AtomicVariable
import eu.quanticol.carma.core.carma.AttribType
import eu.quanticol.carma.core.carma.BooleanExpression
import eu.quanticol.carma.core.carma.Calls
import eu.quanticol.carma.core.carma.CarmaBoolean
import eu.quanticol.carma.core.carma.CarmaDouble
import eu.quanticol.carma.core.carma.CarmaInteger
import eu.quanticol.carma.core.carma.CeilingFunction
import eu.quanticol.carma.core.carma.Comparison
import eu.quanticol.carma.core.carma.Declaration
import eu.quanticol.carma.core.carma.Division
import eu.quanticol.carma.core.carma.DoubleType
import eu.quanticol.carma.core.carma.Equality
import eu.quanticol.carma.core.carma.Expressions
import eu.quanticol.carma.core.carma.FeildDeclaration
import eu.quanticol.carma.core.carma.FeildName
import eu.quanticol.carma.core.carma.FloorFunction
import eu.quanticol.carma.core.carma.FunctionCall
import eu.quanticol.carma.core.carma.FunctionDefinition
import eu.quanticol.carma.core.carma.FunctionExpression
import eu.quanticol.carma.core.carma.FunctionName
import eu.quanticol.carma.core.carma.FunctionReferenceMan
import eu.quanticol.carma.core.carma.FunctionReferencePre
import eu.quanticol.carma.core.carma.InputActionParameter
import eu.quanticol.carma.core.carma.IntgerType
import eu.quanticol.carma.core.carma.MaxFunction
import eu.quanticol.carma.core.carma.MinFunction
import eu.quanticol.carma.core.carma.Modulo
import eu.quanticol.carma.core.carma.Multiplication
import eu.quanticol.carma.core.carma.Not
import eu.quanticol.carma.core.carma.Now
import eu.quanticol.carma.core.carma.Or
import eu.quanticol.carma.core.carma.OutcomeProbability
import eu.quanticol.carma.core.carma.PDFunction
import eu.quanticol.carma.core.carma.Parameter
import eu.quanticol.carma.core.carma.PreFunctionCall
import eu.quanticol.carma.core.carma.PrimitiveTypes
import eu.quanticol.carma.core.carma.Range
import eu.quanticol.carma.core.carma.RecordReferenceGlobal
import eu.quanticol.carma.core.carma.RecordReferenceMy
import eu.quanticol.carma.core.carma.RecordReferencePure
import eu.quanticol.carma.core.carma.RecordReferenceReceiver
import eu.quanticol.carma.core.carma.RecordReferenceSender
import eu.quanticol.carma.core.carma.RecordType
import eu.quanticol.carma.core.carma.Subtraction
import eu.quanticol.carma.core.carma.Type
import eu.quanticol.carma.core.carma.UniformFunction
import eu.quanticol.carma.core.carma.UpdateExpression
import eu.quanticol.carma.core.carma.VariableName
import eu.quanticol.carma.core.carma.VariableReference
import eu.quanticol.carma.core.carma.VariableReferenceGlobal
import eu.quanticol.carma.core.carma.VariableReferenceMy
import eu.quanticol.carma.core.carma.VariableReferencePure
import eu.quanticol.carma.core.carma.VariableReferenceReceiver
import eu.quanticol.carma.core.carma.VariableReferenceSender
import java.util.ArrayList
import java.util.HashMap

import static extension org.eclipse.xtext.EcoreUtil2.*

class BaseType {
	
	public var String parent 	= "null"
	public var String me 		= "null"
	public var String operation = "null"
	public var String set 		= "null"
	
	def setMe(String t){
		me = t
	}
	
	def setOp(String o){
		operation = o
	}
	
	def boolean isLogical(){
		operation.equals("logical")
	}
	
	def boolean isState(){
		parent.equals("state")
	}
	
	def boolean isArith(){
		operation.equals("arith")
	}
	
	override toString() { parent + ":" + me + ":" + operation + ":" + set }
	
}

class TypeProvider {
	
	public static val attribType  			=	new BaseType() => [ parent="primitive"	me="int" 			operation="arith" 		set="natural"]
	public static val doubleType  			=	new BaseType() => [ parent="primitive"	me="double" 		operation="arith" 		set="real"	 ]
	public static val intgerType  			=	new BaseType() => [ parent="primitive"	me="int" 			operation="arith" 		set="integer"]
	public static val booleanType			=	new BaseType() => [ parent="primitive"	me="boolean" 		operation="logical" 	set="boolean"]
	public static val orType 				=	new BaseType() => [ parent="expression"	me="or" 			operation="logical"	]
	public static val andType 				=	new BaseType() => [ parent="expression"	me="and" 			operation="logical"	]
	public static val equalityType 			=	new BaseType() => [ parent="expression"	me="equality" 		operation="logical"	]
	public static val comparisonType		= 	new BaseType() => [ parent="expression"	me="comparison"		operation="logical"	]
	public static val subtractionType		= 	new BaseType() => [ parent="expression"	me="subtraction"	operation="arith"	]
	public static val additionType			= 	new BaseType() => [ parent="expression"	me="addition"		operation="arith"	]
	public static val multiplicationType	= 	new BaseType() => [ parent="expression"	me="multiplication"	operation="arith"	]
	public static val moduloType			= 	new BaseType() => [ parent="expression"	me="modulo"			operation="arith"	]
	public static val divisionType			= 	new BaseType() => [ parent="expression"	me="division"		operation="arith"	]
	public static val notType				=	new BaseType() => [ parent="expression"	me="not" 			operation="logical"	]
	public static val outcomeType			=	new BaseType() => [ parent="primitive" 	me="outcome" 		operation="argument" 	set="real"	 ]
	public static val rangeType				=	new BaseType() => [ parent="primitive" 	me="outcome" 		operation="argument" 	set="integer"]
	public static val stateType				=	new BaseType() => [ parent="state" 		me="state" 			operation="state" 		set="state"	 ]
	//TODO
	//get actual pre defined type
	public static val predefinedType		=	new BaseType() => [	parent="function"	me="pre"			operation="arith"]

	def BaseType getType(String type){
		var baseType = new BaseType() => [ parent="record" operation="arith" set="integer"]
		baseType.me = type
		return baseType
	}
	
	def boolean sameType(BaseType one, BaseType two){
		one.me.equals(two.me)
	}
	
	def boolean comparable(BaseType one, BaseType two){
		one.operation.equals(two.operation)
	}
	
	def BaseType getType(VariableName variableName){
		var declaration = variableName.getContainerOfType(Declaration)
		var parameter = variableName.getContainerOfType(Parameter)
		var ArrayList<Type> types = new ArrayList<Type>()
		var BaseType toReturn = new BaseType()
		
		if(declaration != null){
			types.addAll(declaration.eAllOfType(Type))
		}
		if(parameter != null){
			types.addAll(parameter.eAllOfType(Type))
		}
		
		//it could be an input action parameter, which doesn't have a "type" attribute
		if(types.size == 0){
			parameter = variableName.getContainerOfType(InputActionParameter)
			if(parameter != null)
				toReturn = attribType
		}
		
		//it is probably a record declaration, in which case there are two "RecordType" one on the left
		//one of the right of the assignment operator
		if(types.size > 1){
			var test = new HashMap<String,BaseType>()
			for(type : types){
				test.put(type.type.toString,type.type)
			}
			if(test.size == 1)
				for(key : test.keySet)
					toReturn = test.get(key)
		} 
		
		//typically there is only one type
		if(types.size == 1) {
			toReturn = types.get(0).type
		}
		
		return toReturn
	}
	
	def BaseType getType(FeildName recordName){
		var declaration = recordName.getContainerOfType(FeildDeclaration)
		var ArrayList<Type> types = new ArrayList<Type>()
		var BaseType toReturn = new BaseType()
		
		if(declaration != null){
			types.addAll(declaration.eAllOfType(Type))
		}

		if(types.size == 1) {
			toReturn = types.get(0).type
		}
		
		return toReturn
	}
	
	def BaseType getType(Type type){
		switch(type){
			DoubleType: doubleType
			IntgerType: intgerType
			AttribType: attribType
			RecordType: (type as RecordType).name.name.type
		}
	}
	
	def BaseType getType(VariableReference variableReference){
		switch(variableReference){
			VariableReferencePure		: variableReference.name.type
			VariableReferenceMy			: variableReference.name.type
			VariableReferenceReceiver	: variableReference.name.type
			VariableReferenceSender		: variableReference.name.type
			VariableReferenceGlobal		: variableReference.name.type
			RecordReferencePure			: variableReference.feild.type
			RecordReferenceMy			: variableReference.feild.type
			RecordReferenceReceiver		: variableReference.feild.type
			RecordReferenceSender		: variableReference.feild.type
			RecordReferenceGlobal		: variableReference.feild.type
		}
	}
	
	def BaseType getType(BooleanExpression expression){
		expression.expression.type
	}
	
	def BaseType getType(UpdateExpression expression){
		expression.expression.type
	}
	
	def BaseType getType(FunctionExpression expression){
		expression.expression.type
	}
	
	def BaseType getType(Expressions expressions){
		switch(expressions){
			Or						:	(expressions as Or).getType
			And						:	(expressions as And).getType
			Equality				:	(expressions as Equality).getType
			Comparison				:	(expressions as Comparison).getType
			Subtraction				:	(expressions as Subtraction).getType
			Addition				:	(expressions as Addition).getType
			Multiplication			:	(expressions as Multiplication).getType
			Modulo					:	(expressions as Modulo).getType
			Division				:	(expressions as Division).getType
			Not						:	(expressions as Not).getType
			AtomicPrimitive			:	expressions.value.getType
			AtomicVariable			:	expressions.value.getType
			AtomicMethodReference	:	(expressions as AtomicMethodReference).getType
			AtomicNow				:	(expressions as AtomicNow).getType
			AtomicMeasure			:	(expressions as AtomicMeasure).getType
			AtomicRecord			:	(expressions as AtomicRecord).getType
			AtomicOutcome			:	(expressions as AtomicOutcome).getType
		}
		
	}
	
	def BaseType getType(Or expression){
		var left 	= expression.left.getType
		var right 	= expression.right.getType
		if(left.isLogical && right.isLogical)
			return orType
		else
			return new BaseType()
	}
	
	def BaseType getType(And expression){
		var left 	= expression.left.getType
		var right 	= expression.right.getType
		if(left.isLogical && right.isLogical)
			return andType
		else
			return new BaseType()
	}
	
	def BaseType getType(Equality expression){
		var left 	= expression.left.getType
		var right 	= expression.right.getType
		if(left.comparable(right))
			return equalityType
		else
			return new BaseType()
	}
	
	def BaseType getType(Comparison expression){
		var left 	= expression.left.getType
		var right 	= expression.right.getType
		if(left.isArith && right.isArith){
			return comparisonType
		}
		else
			return new BaseType()
	}
	
	def BaseType getType(Subtraction expression){
		var left 	= expression.left.getType
		var right 	= expression.right.getType
		if(left.isArith && right.isArith)
			return subtractionType
		else
			return new BaseType()
	}
	
	def BaseType getType(Addition expression){
		var left 	= expression.left.getType
		var right 	= expression.right.getType
		if(left.isArith && right.isArith)
			return additionType
		else
			return new BaseType()
	}
	
	def BaseType getType(Multiplication expression){
		var left 	= expression.left.getType
		var right 	= expression.right.getType
		if(left.isArith && right.isArith)
			return multiplicationType
		else
			return new BaseType()
	}
	
	def BaseType getType(Modulo expression){
		var left 	= expression.left.getType
		var right 	= expression.right.getType
		if(left.isArith && right.isArith)
			return moduloType
		else
			return new BaseType()
	}
	
	def BaseType getType(Division expression){
		var left 	= expression.left.getType
		var right 	= expression.right.getType
		if(left.isArith && right.isArith)
			return divisionType
		else
			return new BaseType()
	}
	
	def BaseType getType(Not expression){
		var left = expression.expression.type
		if(left.isLogical)
			return notType
		else
			return new BaseType()
	}
	
	def BaseType getType(PrimitiveTypes expression){
		switch(expression){
			Now					: doubleType
			OutcomeProbability	: outcomeType
			Range				: rangeType
			CarmaBoolean		: booleanType
			CarmaInteger		: intgerType
			CarmaDouble			: doubleType
		}
	}
	
	def BaseType getType(AtomicMethodReference expression){
		expression.value.type
	}
	
	def BaseType getType(Calls expression){
		switch(expression){
			FunctionReferenceMan: expression.ref.type
			FunctionReferencePre: expression.ref.type
		}
	}
	
	def BaseType getType(FunctionCall expression){
		expression.name.type
	}
	
	def BaseType getType(FunctionName methodName){
		var attribute = methodName.getContainerOfType(FunctionDefinition).type
		var ArrayList<Type> types = new ArrayList<Type>()
		var BaseType toReturn = new BaseType()
		
		if(attribute != null){
			types.addAll(attribute.eAllOfType(Type))
		}
		
		if(types.size == 1) {
			toReturn = types.get(0).type
		}
		
		return toReturn
	}
	
	/**
	 * This will need to search for the type of the predefined function
	 */
	def BaseType getType(PreFunctionCall expression){
		switch(expression){
			PDFunction:			attribType
			UniformFunction:	attribType
			CeilingFunction:	attribType
			FloorFunction:		attribType
			MaxFunction:		attribType
			MinFunction:		attribType
		}
	}
	
	def BaseType getType(AtomicNow expression){
		doubleType
	}
	
	def BaseType getType(AtomicMeasure expression){
		doubleType
	}
	
	def BaseType getType(AtomicRecord expression){
		expression.value.type.type
	}
	
	def BaseType getType(AtomicOutcome expression){
		expression.value.type
	}

}