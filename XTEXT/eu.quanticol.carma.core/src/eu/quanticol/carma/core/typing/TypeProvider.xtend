package eu.quanticol.carma.core.typing

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.Spawn
import eu.quanticol.carma.core.carma.BooleanAddition
import eu.quanticol.carma.core.carma.BooleanAnd
import eu.quanticol.carma.core.carma.BooleanAtomicMethodReference
import eu.quanticol.carma.core.carma.BooleanAtomicNow
import eu.quanticol.carma.core.carma.BooleanAtomicPrimitive
import eu.quanticol.carma.core.carma.BooleanAtomicRecords
import eu.quanticol.carma.core.carma.BooleanAtomicVariable
import eu.quanticol.carma.core.carma.BooleanComparison
import eu.quanticol.carma.core.carma.BooleanDivision
import eu.quanticol.carma.core.carma.BooleanEquality
import eu.quanticol.carma.core.carma.BooleanExpression
import eu.quanticol.carma.core.carma.BooleanModulo
import eu.quanticol.carma.core.carma.BooleanMultiplication
import eu.quanticol.carma.core.carma.BooleanNot
import eu.quanticol.carma.core.carma.BooleanOr
import eu.quanticol.carma.core.carma.BooleanSubtraction
import eu.quanticol.carma.core.carma.CarmaBoolean
import eu.quanticol.carma.core.carma.CarmaDouble
import eu.quanticol.carma.core.carma.CarmaInteger
import eu.quanticol.carma.core.carma.DoubleAssignment
import eu.quanticol.carma.core.carma.DoubleTypeLabel
import eu.quanticol.carma.core.carma.EnumAssignment
import eu.quanticol.carma.core.carma.EnumTypeLabel
import eu.quanticol.carma.core.carma.EnvironmentAddition
import eu.quanticol.carma.core.carma.EnvironmentAtomicMeasure
import eu.quanticol.carma.core.carma.EnvironmentAtomicMethodReference
import eu.quanticol.carma.core.carma.EnvironmentAtomicNow
import eu.quanticol.carma.core.carma.EnvironmentAtomicPrimitive
import eu.quanticol.carma.core.carma.EnvironmentAtomicRecords
import eu.quanticol.carma.core.carma.EnvironmentAtomicVariable
import eu.quanticol.carma.core.carma.EnvironmentDivision
import eu.quanticol.carma.core.carma.EnvironmentExpression
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressions
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressionAll
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressionComponentAState
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressionComponentAllStates
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressionParallel
import eu.quanticol.carma.core.carma.EnvironmentModulo
import eu.quanticol.carma.core.carma.EnvironmentMultiplication
import eu.quanticol.carma.core.carma.EnvironmentSubtraction
import eu.quanticol.carma.core.carma.EnvironmentUpdateAddition
import eu.quanticol.carma.core.carma.EnvironmentUpdateAtomicMeasure
import eu.quanticol.carma.core.carma.EnvironmentUpdateAtomicMethodReference
import eu.quanticol.carma.core.carma.EnvironmentUpdateAtomicNow
import eu.quanticol.carma.core.carma.EnvironmentUpdateAtomicPrimitive
import eu.quanticol.carma.core.carma.EnvironmentUpdateAtomicRecords
import eu.quanticol.carma.core.carma.EnvironmentUpdateAtomicVariable
import eu.quanticol.carma.core.carma.EnvironmentUpdateExpression
import eu.quanticol.carma.core.carma.EnvironmentUpdateMultiplication
import eu.quanticol.carma.core.carma.EnvironmentUpdateSubtraction
import eu.quanticol.carma.core.carma.IntegerAssignment
import eu.quanticol.carma.core.carma.IntegerTypeLabel
import eu.quanticol.carma.core.carma.MacroExpressions
import eu.quanticol.carma.core.carma.MacroExpressionParallel
import eu.quanticol.carma.core.carma.MacroExpressionReference
import eu.quanticol.carma.core.carma.MethodAddition
import eu.quanticol.carma.core.carma.MethodAtomicMethodReference
import eu.quanticol.carma.core.carma.MethodAtomicPrimitive
import eu.quanticol.carma.core.carma.MethodAtomicRecords
import eu.quanticol.carma.core.carma.MethodAtomicVariable
import eu.quanticol.carma.core.carma.MethodDeclaration
import eu.quanticol.carma.core.carma.MethodDefinition
import eu.quanticol.carma.core.carma.MethodDivision
import eu.quanticol.carma.core.carma.MethodExpression
import eu.quanticol.carma.core.carma.MethodModulo
import eu.quanticol.carma.core.carma.MethodMultiplication
import eu.quanticol.carma.core.carma.MethodReferenceMethodDeclaration
import eu.quanticol.carma.core.carma.MethodReferencePredefinedMethodDeclaration
import eu.quanticol.carma.core.carma.MethodSubtraction
import eu.quanticol.carma.core.carma.PredefinedMethodDeclaration
import eu.quanticol.carma.core.carma.PredefinedMethodDeclarationArgument
import eu.quanticol.carma.core.carma.PredefinedMethodDeclarationArgumentADR
import eu.quanticol.carma.core.carma.PredefinedMethodDeclarationArgumentNDR
import eu.quanticol.carma.core.carma.PredefinedMethodDeclarationArgumentPT
import eu.quanticol.carma.core.carma.PredefinedMethodDeclarationArgumentRecords
import eu.quanticol.carma.core.carma.PredefinedMethodDeclarationArgumentReference
import eu.quanticol.carma.core.carma.PredefinedMethodDeclarationArgumentVariable
import eu.quanticol.carma.core.carma.PredefinedMethodDeclarationArguments
import eu.quanticol.carma.core.carma.PredefinedMethodNaturalDistributionRule
import eu.quanticol.carma.core.carma.PrimitiveType
import eu.quanticol.carma.core.carma.ProcessExpression
import eu.quanticol.carma.core.carma.ProcessExpressionAction
import eu.quanticol.carma.core.carma.ProcessExpressionChoice
import eu.quanticol.carma.core.carma.ProcessExpressionGuard
import eu.quanticol.carma.core.carma.ProcessExpressionLeaf
import eu.quanticol.carma.core.carma.ProcessExpressionReference
import eu.quanticol.carma.core.carma.Range
import eu.quanticol.carma.core.carma.RecordDeclarations
import eu.quanticol.carma.core.carma.RecordTypeLabel
import eu.quanticol.carma.core.carma.Records
import eu.quanticol.carma.core.carma.TypeLabel
import eu.quanticol.carma.core.carma.UpdateAddition
import eu.quanticol.carma.core.carma.UpdateAtomicMethodReference
import eu.quanticol.carma.core.carma.UpdateAtomicPrimitive
import eu.quanticol.carma.core.carma.UpdateAtomicRecords
import eu.quanticol.carma.core.carma.UpdateAtomicVariable
import eu.quanticol.carma.core.carma.UpdateExpression
import eu.quanticol.carma.core.carma.UpdateMultiplication
import eu.quanticol.carma.core.carma.UpdateSubtraction
import eu.quanticol.carma.core.carma.VariableDeclaration
import eu.quanticol.carma.core.carma.VariableDeclarationCarmaDouble
import eu.quanticol.carma.core.carma.VariableDeclarationCarmaIntger
import eu.quanticol.carma.core.carma.VariableDeclarationEnum
import eu.quanticol.carma.core.carma.VariableDeclarationRecord
import eu.quanticol.carma.core.utils.Util
import java.util.ArrayList
import java.util.HashSet

import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.core.carma.VariableName
import eu.quanticol.carma.core.carma.InputActionArguments
import eu.quanticol.carma.core.carma.VariableType
import eu.quanticol.carma.core.carma.ComponentBlockForStatement
import eu.quanticol.carma.core.carma.ComponentLineForStatement
import eu.quanticol.carma.core.carma.OutputActionArgument
import eu.quanticol.carma.core.carma.OutputActionArgumentVR
import eu.quanticol.carma.core.carma.OutputActionArgumentV
import eu.quanticol.carma.core.carma.ActionName
import eu.quanticol.carma.core.carma.Action
import eu.quanticol.carma.core.carma.SpontaneousAction
import eu.quanticol.carma.core.carma.MultiCast
import eu.quanticol.carma.core.carma.OutputAction
import eu.quanticol.carma.core.carma.ComponentArgument
import eu.quanticol.carma.core.carma.ComponentBlockDefinitionArgumentVariable
import eu.quanticol.carma.core.carma.ComponentBlockDefinitionArgumentMacro
import eu.quanticol.carma.core.carma.NCA
import eu.quanticol.carma.core.carma.NewComponentArgumentPrimitive
import eu.quanticol.carma.core.carma.NewComponentArgumentMacro
import eu.quanticol.carma.core.carma.NewComponentArgumentMethod
import eu.quanticol.carma.core.carma.NewComponentArgumentDeclare
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnPrimitive
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnDeclare
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnMacro
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnMethod
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnReference
import eu.quanticol.carma.core.carma.NewComponentArgumentReference
import eu.quanticol.carma.core.carma.UpdateExpressions
import eu.quanticol.carma.core.carma.MethodExpressions
import eu.quanticol.carma.core.carma.BooleanExpressions
import eu.quanticol.carma.core.carma.EnvironmentExpressions
import eu.quanticol.carma.core.carma.EnvironmentUpdateExpressions
import eu.quanticol.carma.core.carma.VariableReference
import eu.quanticol.carma.core.carma.InputAction
import eu.quanticol.carma.core.utils.LabelUtil

class BaseType {
	
	public var String type = ""
	public var String set = ""
	public var String operations = ""
	
	override toString() { type }
	
}

class TypeProvider {
	
	@Inject extension Util
	@Inject extension LabelUtil
	
	public static val booleanType 		= new BaseType() => [ type = "boolean" 		; set="logic"			; operations="logic"]
	public static val recordType		= new BaseType() => [ type = "record" 		; set="component"		; operations="arith"]
	public static val recordsType		= new BaseType() => [ type = "records" 		; set="component"		; operations="null"]
	public static val enumType			= new BaseType() => [ type = "enum" 		; set="component"		; operations="arith"]
	public static val compType			= new BaseType() => [ type = "component" 	; set="component"		; operations="arith"]
	public static val macroType			= new BaseType() => [ type = "macro" 		; set="component"		; operations="null"]
	public static val integerType		= new BaseType() => [ type = "integer" 		; set="arith"			; operations="arith"]
	public static val doubleType		= new BaseType() => [ type = "double" 		; set="arith"			; operations="arith"]
	public static val nullType			= new BaseType() => [ type = "null" 		; set="null"			; operations="null"]
	public static val choiceType		= new BaseType() => [ type = "choice" 		; set="process"			; operations="null"]
	public static val leafType			= new BaseType() => [ type = "leaf" 		; set="process"			; operations="null"]
	public static val guardType			= new BaseType() => [ type = "guard" 		; set="process"			; operations="null"]
	public static val actionType 		= new BaseType() => [ type = "action" 		; set="process"			; operations="null"]
	public static val referenceType 	= new BaseType() => [ type = "reference" 	; set="process"			; operations="null"]
	public static val measureType		= new BaseType() => [ type = "measure" 		; set="measure"			; operations="null"]
	public static val spontType			= new BaseType() => [ type = "spont"		; set="outputAction"	; operations="null"]
	public static val uniOutType		= new BaseType() => [ type = "uni"			; set="outputAction"	; operations="null"]
	public static val uniInType			= new BaseType() => [ type = "uni"			; set="inputAction"		; operations="null"]
	public static val broadOutType		= new BaseType() => [ type = "broad"		; set="outputAction"	; operations="null"]
	public static val broadInType		= new BaseType() => [ type = "broad"		; set="inputAction"		; operations="null"]
	
	
	def getType(NCA ca){
		switch(ca){
			NewComponentArgumentPrimitive 		: compType	
			NewComponentArgumentMacro 			: macroType
			NewComponentArgumentMethod			: compType
			NewComponentArgumentDeclare			: compType
			NewComponentArgumentReference		: compType
			NewComponentArgumentSpawnPrimitive 	: compType 
			NewComponentArgumentSpawnDeclare	: compType
			NewComponentArgumentSpawnMacro		: macroType
			NewComponentArgumentSpawnMethod		: compType
			NewComponentArgumentSpawnReference	: compType
		}
	}
	
	def getType(ComponentArgument ca){
		switch(ca){
			ComponentBlockDefinitionArgumentVariable:	compType
			ComponentBlockDefinitionArgumentMacro:		macroType
		}
	}
	
	def getType(OutputActionArgument oaa){
		switch(oaa){
			OutputActionArgumentVR: oaa.ref.getType
			OutputActionArgumentV:	oaa.value.getType
		}
	}
	
	def getType(VariableDeclaration vd){
		switch(vd){
			VariableDeclarationEnum: 		(vd.assign as EnumAssignment).getType
			VariableDeclarationRecord:		(vd.assign as RecordDeclarations).getType
			VariableDeclarationCarmaDouble:	(vd.assign as DoubleAssignment).getType
			VariableDeclarationCarmaIntger:	(vd.assign as IntegerAssignment).getType
		}
	}
	
	def getType(EnumAssignment ea){
		enumType
	}
	
	def getType(RecordDeclarations rd){
		recordType
	}
	
	def getType(DoubleAssignment da){
		doubleType
	}
	
	def getType(IntegerAssignment ia){
		integerType
	}
	
	def getType(PrimitiveType p){
		switch(p){
				CarmaDouble:	doubleType
				CarmaInteger:	integerType
				CarmaBoolean:	booleanType
				Range:			integerType
		}
	}
	
	/**
	 * @see InputActionArguments
	 * @see MethodAtomic
	 * @see PredefinedMethodDeclarationArgument
	 * @see VariableDeclaration
	 * @see VariableType
	 */
	def BaseType getType(VariableName vn){
		var output = nullType
		if(vn.getContainerOfType(InputActionArguments) != null){
			var ArrayList<String> options = new ArrayList<String>(vn.getTypes(vn.getContainerOfType(InputAction)))
			if(options.size == 1){
				if(options.get(0).equals("enum")){
					output = enumType
				} else if (options.get(0).equals("record")){
					output = recordType
				} else if (options.get(0).equals("double")){
					output = doubleType
				} else if (options.get(0).equals("integer")){
					output = integerType	
				}
			}
		}
		if(vn.getContainerOfType(MethodAtomicVariable) != null){
			var ArrayList<String> options = new ArrayList<String>(vn.getTypes(vn.getContainerOfType(MethodAtomicVariable)))
			if(options.size == 1){
				if(options.get(0).equals("enum")){
					output = enumType
				} else if (options.get(0).equals("record")){
					output = recordType
				} else if (options.get(0).equals("double")){
					output = doubleType
				} else if (options.get(0).equals("integer")){
					output = integerType	
				}
			}
		}
		if(vn.getContainerOfType(PredefinedMethodDeclarationArgument) != null){
			var ArrayList<String> options = new ArrayList<String>(vn.getTypes(vn.getContainerOfType(PredefinedMethodDeclarationArgument)))
			if(options.size == 1){
				if(options.get(0).equals("enum")){
					output = enumType
				} else if (options.get(0).equals("record")){
					output = recordType
				} else if (options.get(0).equals("double")){
					output = doubleType
				} else if (options.get(0).equals("integer")){
					output = integerType	
				}
			}
		}
		if(vn.getContainerOfType(VariableDeclaration) != null){
			output = vn.getContainerOfType(VariableDeclaration).getBaseType
		}
		if(vn.getContainerOfType(VariableType) != null){
			output = vn.getContainerOfType(VariableType).getBaseType
		}
		if(vn.getContainerOfType(ComponentBlockForStatement) != null){
			var ArrayList<String> options = new ArrayList<String>(vn.getTypes(vn.getContainerOfType(ComponentBlockForStatement)))
			if(options.size == 1){
				if(options.get(0).equals("enum")){
					output = enumType
				} else if (options.get(0).equals("record")){
					output = recordType
				} else if (options.get(0).equals("double")){
					output = doubleType
				} else if (options.get(0).equals("integer")){
					output = integerType	
				}
			}
		}
		if(vn.getContainerOfType(ComponentLineForStatement) != null){
			var ArrayList<String> options = new ArrayList<String>(vn.getTypes(vn.getContainerOfType(ComponentLineForStatement)))
			if(options.size == 1){
				if(options.get(0).equals("enum")){
					output = enumType
				} else if (options.get(0).equals("record")){
					output = recordType
				} else if (options.get(0).equals("double")){
					output = doubleType
				} else if (options.get(0).equals("integer")){
					output = integerType	
				}
			}
		}
		return output
	}
	
	def BaseType getBaseType(VariableDeclaration vd){
			if(vd.type.equals("enum")){
				enumType
			} else if (vd.type.equals("record")){
				recordType
			} else if (vd.type.equals("double")){
				doubleType
			} else if (vd.type.equals("integer")){
				integerType	
			} else {
				nullType
			}
	}
	
	def BaseType getBaseType(VariableType vt){
			if(vt.type.equals("enum")){
				enumType
			} else if (vt.type.equals("record")){
				recordType
			} else if (vt.type.equals("double")){
				doubleType
			} else if (vt.type.equals("integer")){
				integerType	
			} else {
				nullType
			}
	}
	
	def BaseType getType(UpdateExpressions e){
		switch(e){
			UpdateSubtraction:				(e as UpdateSubtraction).getType
			UpdateAddition:					(e as UpdateAddition).getType
			UpdateMultiplication:			(e as UpdateMultiplication).getType
			UpdateAtomicPrimitive:			(e.value as PrimitiveType).getType
			UpdateAtomicRecords:			(e.value as Records).getType
			UpdateAtomicVariable:			(e.value as VariableReference).getType
			UpdateAtomicMethodReference:	(e.value as MethodExpression).getType
			UpdateExpression:				e.expression.type
		}
	}
	
	def getType(Records rs){
		recordsType
	}
	
//	def getType(VariableOrRecordReference vorr){
//		switch(vorr){
//			OrVariableReference:			(vorr.ref as VRReference).getType
//			OrRecordReference:				(vorr.ref as VRReference).getType
//		}
//	}
	
	def getType(VariableReference vr){
		var outputType = nullType
		
		var ArrayList<String> output = new ArrayList<String>(vr.getTypes)
		if(output.size == 1){
			if(output.get(0).equals("enum")){
				outputType = enumType
			} else if (output.get(0).equals("record")){
				outputType = recordType
			} else if (output.get(0).equals("double")){
				outputType = doubleType
			} else if (output.get(0).equals("integer")){
				outputType = integerType	
			}
		}
		return outputType
		
	}
	
	def BaseType getType(MethodExpressions e){
		switch(e){
			MethodSubtraction:							(e as MethodSubtraction).getType
			MethodAddition:								(e as MethodAddition).getType
			MethodMultiplication:						(e as MethodMultiplication).getType
			MethodModulo:								(e as MethodModulo).getType
			MethodDivision:								(e as MethodDivision).getType
			MethodAtomicPrimitive:						(e.value as PrimitiveType).getType
			MethodAtomicRecords:						(e.value as Records).getType
			MethodAtomicVariable:						integerType
			MethodAtomicMethodReference:				(e.value as MethodExpression).getType 
			MethodReferenceMethodDeclaration: 			(e.ref as MethodDeclaration).getType
			MethodReferencePredefinedMethodDeclaration: (e.ref as PredefinedMethodDeclaration).getType
			MethodExpression:							e.expression.type
		}
	}
	
	def BaseType getType(MethodDeclaration md){
		return md.name.getContainerOfType(MethodDefinition).type.getTypeLabelType
	}
	
	def BaseType getTypeLabelType(TypeLabel tl){
		switch(tl){
			DoubleTypeLabel:	doubleType
			IntegerTypeLabel:	integerType
			RecordTypeLabel:	recordType
			EnumTypeLabel:		enumType
		}
		
	}
	
	def BaseType getType(PredefinedMethodDeclaration pmd){
		pmd.functionArguments.getType
	}
	
	def BaseType getType(PredefinedMethodDeclarationArguments pmdas){
		var HashSet<BaseType> types = new HashSet<BaseType>()
		for(pmda : pmdas.inputArguments){
			types.add(pmda.getType)
		}
		if(types.size > 1)
			return nullType
		else
			return types.get(0)
	}
	
	def BaseType getType(PredefinedMethodDeclarationArgument pmda){
		switch(pmda){
			PredefinedMethodDeclarationArgumentNDR:			integerType
			PredefinedMethodDeclarationArgumentADR:			(pmda.value as PredefinedMethodNaturalDistributionRule).getType
			PredefinedMethodDeclarationArgumentPT:			(pmda.value as PrimitiveType).getType
			PredefinedMethodDeclarationArgumentRecords:		recordType
			PredefinedMethodDeclarationArgumentVariable:	integerType
			PredefinedMethodDeclarationArgumentReference:   (pmda.value as MethodDeclaration).getType
		}
	}
	
	def BaseType getType(PredefinedMethodNaturalDistributionRule pmndr){
		pmndr.outcome.getType
	}
	
	
	def BaseType getType(UpdateSubtraction e){
		var left = e.left.getType
		var right = e.right.getType
		if(left.operations.equals("arith") && right.operations.equals("arith"))
			return integerType
		else
			return nullType
	}
	
	def BaseType getType(UpdateAddition e){
		var left = e.left.getType
		var right = e.right.getType
		if(left.operations.equals("arith") && right.operations.equals("arith"))
			return integerType
		else
			return nullType
	}
	
	def BaseType getType(UpdateMultiplication e){
		var left = e.left.getType
		var right = e.right.getType
		if(left.operations.equals("arith") && right.operations.equals("arith"))
			return integerType
		else
			return nullType
	}
	
	def BaseType getType(BooleanExpressions e){
		switch(e){
			BooleanOr:						(e as BooleanOr).getType
			BooleanAnd:						(e as BooleanAnd).getType
			BooleanEquality:				(e as BooleanEquality).getType
			BooleanComparison:				(e as BooleanComparison).getType
			BooleanSubtraction:				(e as BooleanSubtraction).getType
			BooleanAddition:				(e as BooleanAddition).getType
			BooleanMultiplication:			(e as BooleanMultiplication).getType
			BooleanModulo:					(e as BooleanModulo).getType
			BooleanDivision:				(e as BooleanDivision).getType
			BooleanNot:						(e as BooleanNot).getType
			BooleanAtomicPrimitive:			(e.value as PrimitiveType).getType 			
			BooleanAtomicRecords:			(e.value as Records).getType			
			BooleanAtomicVariable:			(e.value as VariableReference).getType 
			BooleanAtomicMethodReference:	(e.value as MethodExpressions).getType			
			BooleanAtomicNow:				doubleType
			BooleanExpression:				e.expression.type
		}
	}
	
	def BaseType getType(BooleanOr e){
		var left = e.left.getType
		var right = e.right.getType
		if(left == booleanType && right == booleanType)
			return booleanType
		else
			return nullType
	}
	
	def BaseType getType(BooleanAnd e){
		var left = e.left.getType
		var right = e.right.getType
		if(left == booleanType && right == booleanType)
			return booleanType
		else
			return nullType
	}
	
	def BaseType getType(BooleanEquality e){
		var left = e.left.getType
		var right = e.right.getType
		if((left.operations.equals(right.operations)))
			return booleanType
		else
			return nullType
	}
		
	def BaseType getType(BooleanComparison e){
		var left = e.left.getType
		var right = e.right.getType
		if(left.operations.equals("arith") && right.operations.equals("arith"))
			return booleanType
		else
			return nullType
	}
	
	def BaseType getType(BooleanSubtraction e){
		var left = e.left.getType
		var right = e.right.getType
		if(left.operations.equals("arith") && right.operations.equals("arith"))
			return doubleType
		else
			return nullType
	}
	
	def BaseType getType(BooleanAddition e){
		var left = e.left.getType
		var right = e.right.getType
		if(left.operations.equals("arith") && right.operations.equals("arith"))
			return doubleType
		else
			return nullType
	}
	
	def BaseType getType(BooleanMultiplication e){
		var left = e.left.getType
		var right = e.right.getType
		if(left.operations.equals("arith") && right.operations.equals("arith"))
			return doubleType
		else
			return nullType
	}	
	
	def BaseType getType(BooleanModulo e){
		var left = e.left.getType
		var right = e.right.getType
		if(left.operations.equals("arith") && right.operations.equals("arith"))
			return doubleType
		else
			return nullType
	}
	
	def BaseType getType(BooleanDivision e){
		var left = e.left.getType
		var right = e.right.getType
		if(left.operations.equals("arith") && right.operations.equals("arith"))
			return doubleType
		else
			return nullType
	}
	
	def BaseType getType(BooleanNot e){
		var left = e.expression.getType
		if(left == booleanType)
			return booleanType
		else
			return nullType
	}

	def BaseType getType(ProcessExpression e) {
		switch(e){
			ProcessExpressionChoice:	(e as ProcessExpressionChoice).getType
			ProcessExpressionLeaf: 		leafType 
			ProcessExpressionGuard: 	guardType 
			ProcessExpressionAction:	actionType
			ProcessExpressionReference:	referenceType
		}
	}
	
	def BaseType getType(ProcessExpressionChoice e) {
		choiceType
	}
	
	def BaseType getType(EnvironmentExpressions e){
		switch(e){
			EnvironmentSubtraction:				(e as EnvironmentSubtraction).getType
			EnvironmentAddition:				(e as EnvironmentAddition).getType
			EnvironmentMultiplication:			(e as EnvironmentMultiplication).getType
			EnvironmentModulo:					(e as EnvironmentModulo).getType
			EnvironmentDivision:				(e as EnvironmentDivision).getType
			EnvironmentAtomicPrimitive:			(e.value as PrimitiveType).getType 			
			EnvironmentAtomicRecords:			(e.value as Records).getType			
			EnvironmentAtomicVariable:			(e.value as VariableReference).getType 
			EnvironmentAtomicMethodReference:	(e.value as MethodExpression).getType			
			EnvironmentAtomicNow:				doubleType	
			EnvironmentAtomicMeasure:			(e.value as EnvironmentAtomicMeasure).getType
			EnvironmentExpression:				e.expression.getType
		}
	}
	
	def getType(EnvironmentSubtraction e){
		var left = e.left.getType
		var right = e.right.getType
		if(left.operations.equals("arith") && right.operations.equals("arith"))
			return doubleType
		else
			return nullType
	}
	
	def getType(EnvironmentAddition e){
		var left = e.left.getType
		var right = e.right.getType
		if(left.operations.equals("arith") && right.operations.equals("arith"))
			return doubleType
		else
			return nullType
	}
	
	def getType(EnvironmentMultiplication e){
		var left = e.left.getType
		var right = e.right.getType
		if(left.operations.equals("arith") && right.operations.equals("arith"))
			return doubleType
		else
			return nullType
	}
	
	def getType(EnvironmentModulo e){
		var left = e.left.getType
		var right = e.right.getType
		if(left.operations.equals("arith") && right.operations.equals("arith"))
			return doubleType
		else
			return nullType
	}
	
	def getType(EnvironmentDivision e){
		var left = e.left.getType
		var right = e.right.getType
		if(left.operations.equals("arith") && right.operations.equals("arith"))
			return doubleType
		else
			return nullType
	}
	
	def getType(EnvironmentAtomicMeasure e){
		doubleType
	}
	
	def BaseType getType(EnvironmentUpdateExpressions e){
		switch(e){
			EnvironmentUpdateSubtraction:			(e as EnvironmentUpdateSubtraction).getType
			EnvironmentUpdateAddition:				(e as EnvironmentUpdateAddition).getType
			EnvironmentUpdateMultiplication:		(e as EnvironmentUpdateMultiplication).getType
			EnvironmentUpdateAtomicPrimitive:		(e.value as PrimitiveType).getType 			
			EnvironmentUpdateAtomicRecords:			(e.value as Records).getType			
			EnvironmentUpdateAtomicVariable:		(e.value as VariableReference).getType 
			EnvironmentUpdateAtomicMethodReference:	(e.value as MethodExpression).getType			
			EnvironmentUpdateAtomicNow:				doubleType	
			EnvironmentUpdateAtomicMeasure:			(e.value as EnvironmentUpdateAtomicMeasure).getType
			EnvironmentUpdateExpression:			e.expression.getType
		}
	}
	
	def getType(Spawn b){
		compType
	}
	
	def BaseType getType(MacroExpressions e){
		switch(e){
			MacroExpressionParallel:	(e as MacroExpressionParallel).getType
			MacroExpressionReference:	(e as MacroExpressionReference).getType
		}
	}
	
	def BaseType getType(MacroExpressionParallel e){
		var left = e.left.getType
		var right = e.right.getType
		if(left.set.equals("reference") && right.set.equals("reference"))
			return referenceType
		else
			return nullType
	}
	
	def BaseType getType(MacroExpressionReference e){
		return referenceType
	}
	
		def BaseType getType(EnvironmentMacroExpressions e){
		switch(e){
			EnvironmentMacroExpressionParallel:				(e as EnvironmentMacroExpressionParallel).getType
			EnvironmentMacroExpressionAll:					(e as EnvironmentMacroExpressionAll).getType
			EnvironmentMacroExpressionComponentAllStates:	(e as EnvironmentMacroExpressionComponentAllStates).getType
			EnvironmentMacroExpressionComponentAState:		(e as EnvironmentMacroExpressionComponentAState).getType
		}
	}
	
	def BaseType getType(EnvironmentMacroExpressionParallel e){
		var left = e.left.getType
		var right = e.right.getType
		if(left.set.equals("measure") && right.set.equals("measure"))
			return measureType
		else
			return nullType
	}
	
	def BaseType getType(EnvironmentMacroExpressionAll e){
		return measureType
	}
	
	def BaseType getType(EnvironmentMacroExpressionComponentAllStates e){
		return measureType
	}
	
	def BaseType getType(EnvironmentMacroExpressionComponentAState e){
		return measureType
	}
	
	def BaseType getType(ActionName actionName){
		if(actionName.getContainerOfType(Action) != null){
			return actionName.getContainerOfType(Action).getType
		} else {
			return nullType
		}
		
	}
	
	def BaseType getType(Action action){
		if(action.eAllOfType(SpontaneousAction).size > 0){
			return spontType
		}else if(action.eAllOfType(MultiCast).size > 0 && !(action.eAllOfType(SpontaneousAction).size > 0)){
			if(action.eAllOfType(OutputAction).size > 0)
				return broadOutType
			else
				return broadInType
		} else {
			if(action.eAllOfType(OutputAction).size > 0)
				return uniOutType
			else
				return uniInType
		}
	}
	
	
}