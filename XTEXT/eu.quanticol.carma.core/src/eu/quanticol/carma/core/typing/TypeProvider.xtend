package eu.quanticol.carma.core.typing

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.Action
import eu.quanticol.carma.core.carma.ActionName
import eu.quanticol.carma.core.carma.AttribAssignment
import eu.quanticol.carma.core.carma.AttribParameter
import eu.quanticol.carma.core.carma.AttribTypeLabel
import eu.quanticol.carma.core.carma.BooleanAddition
import eu.quanticol.carma.core.carma.BooleanAnd
import eu.quanticol.carma.core.carma.BooleanAtomicMethodReference
import eu.quanticol.carma.core.carma.BooleanAtomicNow
import eu.quanticol.carma.core.carma.BooleanAtomicPrimitive
import eu.quanticol.carma.core.carma.BooleanAtomicVariable
import eu.quanticol.carma.core.carma.BooleanComparison
import eu.quanticol.carma.core.carma.BooleanDivision
import eu.quanticol.carma.core.carma.BooleanEquality
import eu.quanticol.carma.core.carma.BooleanExpression
import eu.quanticol.carma.core.carma.BooleanExpressions
import eu.quanticol.carma.core.carma.BooleanModulo
import eu.quanticol.carma.core.carma.BooleanMultiplication
import eu.quanticol.carma.core.carma.BooleanNot
import eu.quanticol.carma.core.carma.BooleanOr
import eu.quanticol.carma.core.carma.BooleanSubtraction
import eu.quanticol.carma.core.carma.CarmaBoolean
import eu.quanticol.carma.core.carma.CarmaDouble
import eu.quanticol.carma.core.carma.CarmaInteger
import eu.quanticol.carma.core.carma.CarmaProb
import eu.quanticol.carma.core.carma.CompArguments
import eu.quanticol.carma.core.carma.CompParameters
import eu.quanticol.carma.core.carma.DoubleAssignment
import eu.quanticol.carma.core.carma.DoubleParameter
import eu.quanticol.carma.core.carma.DoubleTypeLabel
import eu.quanticol.carma.core.carma.EnvironmentAddition
import eu.quanticol.carma.core.carma.EnvironmentAtomicMeasure
import eu.quanticol.carma.core.carma.EnvironmentAtomicMethodReference
import eu.quanticol.carma.core.carma.EnvironmentAtomicNow
import eu.quanticol.carma.core.carma.EnvironmentAtomicPrimitive
import eu.quanticol.carma.core.carma.EnvironmentAtomicVariable
import eu.quanticol.carma.core.carma.EnvironmentDivision
import eu.quanticol.carma.core.carma.EnvironmentExpression
import eu.quanticol.carma.core.carma.EnvironmentExpressions
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressionAll
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressionComponentAState
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressionComponentAllStates
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressionParallel
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressions
import eu.quanticol.carma.core.carma.EnvironmentModulo
import eu.quanticol.carma.core.carma.EnvironmentMultiplication
import eu.quanticol.carma.core.carma.EnvironmentOperation
import eu.quanticol.carma.core.carma.EnvironmentSubtraction
import eu.quanticol.carma.core.carma.EnvironmentUpdateAddition
import eu.quanticol.carma.core.carma.EnvironmentUpdateAtomicMeasure
import eu.quanticol.carma.core.carma.EnvironmentUpdateAtomicMethodReference
import eu.quanticol.carma.core.carma.EnvironmentUpdateAtomicNow
import eu.quanticol.carma.core.carma.EnvironmentUpdateAtomicPrimitive
import eu.quanticol.carma.core.carma.EnvironmentUpdateAtomicVariable
import eu.quanticol.carma.core.carma.EnvironmentUpdateExpression
import eu.quanticol.carma.core.carma.EnvironmentUpdateExpressions
import eu.quanticol.carma.core.carma.EnvironmentUpdateMultiplication
import eu.quanticol.carma.core.carma.EnvironmentUpdateSubtraction
import eu.quanticol.carma.core.carma.IntegerTypeLabel
import eu.quanticol.carma.core.carma.IntgerAssignment
import eu.quanticol.carma.core.carma.IntgerParameter
import eu.quanticol.carma.core.carma.MacroExpressionParallel
import eu.quanticol.carma.core.carma.MacroExpressionReference
import eu.quanticol.carma.core.carma.MacroExpressions
import eu.quanticol.carma.core.carma.MethodAddition
import eu.quanticol.carma.core.carma.MethodAtomicMethodReference
import eu.quanticol.carma.core.carma.MethodAtomicPrimitive
import eu.quanticol.carma.core.carma.MethodAtomicVariable
import eu.quanticol.carma.core.carma.MethodDeclaration
import eu.quanticol.carma.core.carma.MethodDefinition
import eu.quanticol.carma.core.carma.MethodDivision
import eu.quanticol.carma.core.carma.MethodExpression
import eu.quanticol.carma.core.carma.MethodExpressions
import eu.quanticol.carma.core.carma.MethodModulo
import eu.quanticol.carma.core.carma.MethodMultiplication
import eu.quanticol.carma.core.carma.MethodSubtraction
import eu.quanticol.carma.core.carma.MultiCast
import eu.quanticol.carma.core.carma.OutputAction
import eu.quanticol.carma.core.carma.OutputActionArgument
import eu.quanticol.carma.core.carma.OutputActionArgumentCI
import eu.quanticol.carma.core.carma.OutputActionArgumentVR
import eu.quanticol.carma.core.carma.Parameters
import eu.quanticol.carma.core.carma.PrimitiveType
import eu.quanticol.carma.core.carma.Process
import eu.quanticol.carma.core.carma.ProcessExpression
import eu.quanticol.carma.core.carma.ProcessExpressionAction
import eu.quanticol.carma.core.carma.ProcessExpressionChoice
import eu.quanticol.carma.core.carma.ProcessExpressionGuard
import eu.quanticol.carma.core.carma.ProcessExpressionLeaf
import eu.quanticol.carma.core.carma.ProcessExpressionReference
import eu.quanticol.carma.core.carma.Range
import eu.quanticol.carma.core.carma.RecordDeclaration
import eu.quanticol.carma.core.carma.RecordParameter
import eu.quanticol.carma.core.carma.RecordParameters
import eu.quanticol.carma.core.carma.RecordReferenceGlobal
import eu.quanticol.carma.core.carma.RecordReferenceMy
import eu.quanticol.carma.core.carma.RecordReferencePure
import eu.quanticol.carma.core.carma.RecordReferenceReceiver
import eu.quanticol.carma.core.carma.RecordReferenceSender
import eu.quanticol.carma.core.carma.RecordReferenceThis
import eu.quanticol.carma.core.carma.RecordTypeLabel
import eu.quanticol.carma.core.carma.Records
import eu.quanticol.carma.core.carma.SetComp
import eu.quanticol.carma.core.carma.Spawn
import eu.quanticol.carma.core.carma.SpontaneousAction
import eu.quanticol.carma.core.carma.StoreBlock
import eu.quanticol.carma.core.carma.StoreLine
import eu.quanticol.carma.core.carma.Types
import eu.quanticol.carma.core.carma.UpdateAddition
import eu.quanticol.carma.core.carma.UpdateAtomicMethodReference
import eu.quanticol.carma.core.carma.UpdateAtomicPrimitive
import eu.quanticol.carma.core.carma.UpdateAtomicVariable
import eu.quanticol.carma.core.carma.UpdateExpression
import eu.quanticol.carma.core.carma.UpdateExpressions
import eu.quanticol.carma.core.carma.UpdateMultiplication
import eu.quanticol.carma.core.carma.UpdateSubtraction
import eu.quanticol.carma.core.carma.VariableReference
import eu.quanticol.carma.core.carma.VariableReferenceGlobal
import eu.quanticol.carma.core.carma.VariableReferenceMy
import eu.quanticol.carma.core.carma.VariableReferencePure
import eu.quanticol.carma.core.carma.VariableReferenceReceiver
import eu.quanticol.carma.core.carma.VariableReferenceSender
import eu.quanticol.carma.core.carma.VariableReferenceThis
import eu.quanticol.carma.core.utils.Util

import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.core.carma.AttribVariableDeclaration
import eu.quanticol.carma.core.carma.IntgerVariableDeclaration
import eu.quanticol.carma.core.carma.DoubleVariableDeclaration
import eu.quanticol.carma.core.carma.StoreDeclaration
import eu.quanticol.carma.core.utils.LabelUtil
import eu.quanticol.carma.core.carma.MeasureVariableDeclaration

class BaseType {
	
	public var String type = "null"
	public var String set = "null"
	public var String operations = "null"
	
	override toString() { type }
	
}

class TypeProvider {
	
	@Inject extension Util
	@Inject extension LabelUtil
	
	public static val booleanType 		= new BaseType() => [ type = "boolean" 		; set="logic"			; operations="logic"]
	public static val attribType		= new BaseType() => [ type = "attrib" 		; set="component"		; operations="arith"]
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
	
	def getType(PrimitiveType p){
		switch(p){
				CarmaDouble:	doubleType
				CarmaInteger:	integerType
				CarmaBoolean:	booleanType
				Range:			integerType
				CarmaProb:		doubleType
		}
	}
	
	/**
	 * What is the variable references' type? - All variables inside a record are of type attrib. 
	 * Otherwise we have to find the declaration or parameter, and check if this is of 
	 * type attrib/intger/double/record
	 * 
	 * Then if I have a VariableReference, I need a function that returns the parameter or declaration? AKA root.
	 */
	def getType(VariableReference vr){
		var outputType = nullType
		var declarations = vr.declarations
		
		//TODO
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
			MethodAtomicVariable:						integerType
			MethodAtomicMethodReference:				(e.value as MethodExpression).getType 
			MethodExpression:							e.expression.type
		}
	}
	
	def BaseType getType(Parameters p){
		switch(p){
			AttribParameter: attribType
			RecordParameter: p.getBaseType
			DoubleParameter: doubleType
			IntgerParameter: integerType
		}
	}
	
	def BaseType getBaseType(AttribParameter ap){
		attribType
	}
	
	def BaseType getBaseType(MeasureVariableDeclaration mvd){
		attribType
	}
	
	def BaseType getBaseType(RecordParameter rp){
		new BaseType() => [ type = rp.type.disarm  ; set="component"		; operations="arith"]
	}
	
	def BaseType getType(MethodDeclaration d){
		switch(d){
			AttribVariableDeclaration: attribType
			IntgerVariableDeclaration: integerType
			DoubleVariableDeclaration: doubleType
			RecordDeclaration: d.getBaseType
		}
	}
	
	def BaseType getBaseType(AttribVariableDeclaration avd){
		attribType
	}
	
	def BaseType getBaseType(RecordDeclaration rd){
		new BaseType() => [ type = rd.type.disarm  ; set="component"		; operations="arith"]
	}
	
	def BaseType getType(StoreDeclaration d){
		switch(d){
			AttribVariableDeclaration: attribType
			RecordDeclaration: d.getBaseType
		}
	}
	
	def BaseType getType(CompParameters p){
		switch(p){
			AttribParameter: attribType
			RecordParameter: p.getBaseType
			DoubleParameter: doubleType
			IntgerParameter: integerType
		}
	}
	
	def BaseType getType(RecordParameters p){
		switch(p){
			AttribParameter: attribType
			RecordParameter: p.getBaseType
			DoubleParameter: doubleType
			IntgerParameter: integerType
		}
	}
	
	def BaseType getTypeLabelType(Types tl){
		switch(tl){
			DoubleTypeLabel: 		doubleType
			IntegerTypeLabel: 		integerType
			AttribTypeLabel:		attribType
			RecordTypeLabel:		tl.getTypeLabelType
		}
	}
	
	def BaseType getTypeLabelType(RecordTypeLabel rtl){
		new BaseType() => [ type = rtl.ref.disarm  ; set="component"		; operations="arith"]
	}
	
	
//////////////////////////here be dragons 
	
	def getType(CompArguments ca){
		switch(ca.value){
			VariableReference 	: compType	
			MacroExpressions	: macroType
			CarmaInteger		: compType
		}
	}
	
	def getType(OutputActionArgument oaa){
		switch(oaa){
			OutputActionArgumentVR: oaa.ref.getType
			OutputActionArgumentCI:	oaa.value.getType
		}
	}
	
	def getType(AttribAssignment ea){
		attribType
	}
	
	def getType(DoubleAssignment da){
		doubleType
	}
	
	def getType(IntgerAssignment ia){
		integerType
	}
	
	def BaseType getType(UpdateExpressions e){
		switch(e){
			UpdateSubtraction:				(e as UpdateSubtraction).getType
			UpdateAddition:					(e as UpdateAddition).getType
			UpdateMultiplication:			(e as UpdateMultiplication).getType
			UpdateAtomicPrimitive:			(e.value as PrimitiveType).getType
			UpdateAtomicVariable:			(e.value as VariableReference).getType
			UpdateAtomicMethodReference:	(e.value as MethodExpression).getType
			UpdateExpression:				e.expression.type
		}
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