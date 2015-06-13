package eu.quanticol.carma.core.generator;

import com.google.inject.Inject
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
import eu.quanticol.carma.core.carma.CeilingFunction
import eu.quanticol.carma.core.carma.EnvironmentAddition
import eu.quanticol.carma.core.carma.EnvironmentAtomicMeasure
import eu.quanticol.carma.core.carma.EnvironmentAtomicMethodReference
import eu.quanticol.carma.core.carma.EnvironmentAtomicNow
import eu.quanticol.carma.core.carma.EnvironmentAtomicPrimitive
import eu.quanticol.carma.core.carma.EnvironmentAtomicVariable
import eu.quanticol.carma.core.carma.EnvironmentDivision
import eu.quanticol.carma.core.carma.EnvironmentExpression
import eu.quanticol.carma.core.carma.EnvironmentExpressions
import eu.quanticol.carma.core.carma.EnvironmentMeasure
import eu.quanticol.carma.core.carma.EnvironmentModulo
import eu.quanticol.carma.core.carma.EnvironmentMultiplication
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
import eu.quanticol.carma.core.carma.FloorFunction
import eu.quanticol.carma.core.carma.MaxFunction
import eu.quanticol.carma.core.carma.MethodAddition
import eu.quanticol.carma.core.carma.MethodAtomicMethodReference
import eu.quanticol.carma.core.carma.MethodAtomicPrimitive
import eu.quanticol.carma.core.carma.MethodAtomicVariable
import eu.quanticol.carma.core.carma.MethodDeclaration
import eu.quanticol.carma.core.carma.MethodDivision
import eu.quanticol.carma.core.carma.MethodExpression
import eu.quanticol.carma.core.carma.MethodExpressions
import eu.quanticol.carma.core.carma.MethodModulo
import eu.quanticol.carma.core.carma.MethodMultiplication
import eu.quanticol.carma.core.carma.MethodReferenceMethodDeclaration
import eu.quanticol.carma.core.carma.MethodReferencePredefinedMethodDeclaration
import eu.quanticol.carma.core.carma.MethodSubtraction
import eu.quanticol.carma.core.carma.MinFunction
import eu.quanticol.carma.core.carma.PDFunction
import eu.quanticol.carma.core.carma.PredefinedMethodDeclaration
import eu.quanticol.carma.core.carma.PrimitiveType
import eu.quanticol.carma.core.carma.Range
import eu.quanticol.carma.core.carma.RecordReferenceGlobal
import eu.quanticol.carma.core.carma.RecordReferenceMy
import eu.quanticol.carma.core.carma.RecordReferencePure
import eu.quanticol.carma.core.carma.RecordReferenceReceiver
import eu.quanticol.carma.core.carma.RecordReferenceSender
import eu.quanticol.carma.core.carma.RecordReferenceThis
import eu.quanticol.carma.core.carma.UniformFunction
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
import eu.quanticol.carma.core.utils.LabelUtil

public class ExpressionHandler {
	
	@Inject extension LabelUtil
	
	def String evaluateExpression(Object obj){
		var String toReturn = "evaluateExpression";
		switch(obj){
			EnvironmentExpressions: 		return obj.asJava
			EnvironmentUpdateExpressions:	return obj.asJava
			BooleanExpressions:				return obj.asJava
			UpdateExpressions:				return obj.asJava
			MethodExpressions:				return obj.asJava
			default:						return toReturn
		}
	}
	
	def String asJava(UpdateExpressions e){
		switch(e){
			UpdateSubtraction:							{e.left.asJava + " - " + e.right.asJava }
			UpdateAddition:								{e.left.asJava + " + " + e.right.asJava }
			UpdateMultiplication:						{e.left.asJava + " * " + e.right.asJava }
			UpdateAtomicPrimitive:						(e.value as PrimitiveType).asJava
			UpdateAtomicVariable:						(e.value as VariableReference).asJava 
			UpdateAtomicMethodReference:				(e.value as MethodExpressions).asJava 
			UpdateExpression:							e.expression.asJava
		}
	}
	
	def String asJava(EnvironmentExpressions e){
		switch(e){
			EnvironmentSubtraction:							{e.left.asJava + " - " + e.right.asJava}
			EnvironmentAddition:							{e.left.asJava + " + " + e.right.asJava}
			EnvironmentMultiplication:						{e.left.asJava + " * " + e.right.asJava}
			EnvironmentModulo:								{e.left.asJava + " % " + e.right.asJava}
			EnvironmentDivision:							{e.left.asJava + " / " + e.right.asJava}
			EnvironmentAtomicPrimitive:						(e.value as PrimitiveType).asJava
			EnvironmentAtomicVariable:						(e.value as VariableReference).asJava
			EnvironmentAtomicMethodReference:				(e.value as MethodExpressions).asJava 
			EnvironmentAtomicNow:							"now()"
			EnvironmentAtomicMeasure:						e.value.asJava
			EnvironmentExpression:							e.expression.asJava
			EnvironmentMeasure:								"Measure"+e.hashCode+"_"+e.componentReference.getLabel.toFirstUpper
		}
	}
	
	def String asJava(EnvironmentUpdateExpressions e){
		switch(e){
			EnvironmentUpdateSubtraction:							{e.left.asJava + " - " + e.right.asJava }
			EnvironmentUpdateAddition:								{e.left.asJava + " + " + e.right.asJava }
			EnvironmentUpdateMultiplication:						{e.left.asJava + " * " + e.right.asJava }
			EnvironmentUpdateAtomicPrimitive:						(e.value as PrimitiveType).asJava
			EnvironmentUpdateAtomicVariable:						(e.value as VariableReference).asJava
			EnvironmentUpdateAtomicMethodReference:					(e.value as MethodExpressions).asJava 
			EnvironmentUpdateExpression:							e.expression.asJava
			EnvironmentUpdateAtomicNow:								"now()"
			EnvironmentUpdateAtomicMeasure:							"getMeasure"+e.hashCode+"()"
		}
	}
	
	def String asJava(PrimitiveType e){
		switch(e){
			CarmaDouble:	{
				var String output = ""
				output = e.left.toString + "." + e.right.toString
				if(e.exponent != null)
					output = output + e.exponent.label
				return output
			}
			CarmaInteger:	""+e.value
			CarmaBoolean:	e.value
			Range:			"//" + e.min + "..." + e.max + " LabelUtil.getLabel"
		}
	}
	
	def String asJava(BooleanExpressions e){
		switch(e){
			BooleanOr:						{e.left.asJava + " || " + e.right.asJava }
			BooleanAnd:						{e.left.asJava + " && " + e.right.asJava }
			BooleanEquality:				{e.left.asJava + " " + e.op + " " + e.right.asJava }
			BooleanComparison:				{e.left.asJava + " " + e.op + " " + e.right.asJava }
			BooleanSubtraction:				{e.left.asJava + " - " + e.right.asJava }
			BooleanAddition:				{e.left.asJava + " + " + e.right.asJava }
			BooleanMultiplication:			{e.left.asJava + " * " + e.right.asJava }
			BooleanModulo:					{e.left.asJava + " % " + e.right.asJava }
			BooleanDivision:				{e.left.asJava + " / " + e.right.asJava }
			BooleanNot:						{"!"+e.expression.asJava}
			BooleanAtomicPrimitive:			(e.value as PrimitiveType).asJava 	
			BooleanAtomicVariable:			(e.value as VariableReference).asJava 
			BooleanAtomicMethodReference:	(e.value as MethodExpressions).asJava			
			BooleanAtomicNow:				"now()"	
			BooleanExpression:				e.expression.asJava
		}
	}
	
	def String asJava(VariableReference vr){
		switch(vr){
			VariableReferencePure: 			vr.name.label
			VariableReferenceMy: 			vr.name.label
			VariableReferenceThis: 			vr.name.label
			VariableReferenceReceiver:		vr.name.label
			VariableReferenceSender:		vr.name.label
			VariableReferenceGlobal:		vr.name.label
			RecordReferencePure:			vr.name.label + "_" + vr.record.label
			RecordReferenceMy:				vr.name.label + "_" + vr.record.label
			RecordReferenceThis:			vr.name.label + "_" + vr.record.label
			RecordReferenceReceiver:		vr.name.label + "_" + vr.record.label
			RecordReferenceSender:			vr.name.label + "_" + vr.record.label
			RecordReferenceGlobal:			vr.name.label + "_" + vr.record.label
		}
	}
	
	def String asJava(MethodExpressions e){
		switch(e){
			MethodSubtraction:							{e.left.asJava + " - " + e.right.asJava }
			MethodAddition:								{e.left.asJava + " + " + e.right.asJava }
			MethodMultiplication:						{e.left.asJava + " * " + e.right.asJava }
			MethodModulo:								{e.left.asJava + " % " + e.right.asJava }
			MethodDivision:								{e.left.asJava + " / " + e.right.asJava }
			MethodAtomicPrimitive:						(e.value as PrimitiveType).asJava
			MethodAtomicVariable:						(e.value as VariableReference).asJava 
			MethodAtomicMethodReference:				(e.value as MethodExpressions).asJava 
			MethodReferenceMethodDeclaration: 			(e.ref as MethodDeclaration).asJava
			MethodReferencePredefinedMethodDeclaration: (e.ref as PredefinedMethodDeclaration).asJava
			MethodExpression:							e.expression.asJava
		}
	}
	
	def String asJava(MethodDeclaration e){
		e.name.label
	}
	
	def String asJava(PredefinedMethodDeclaration e){
		switch(e){	
			PDFunction:			"PDF()"
			UniformFunction: 	"Uniform()"
			CeilingFunction: 	"Ceiling()"
			FloorFunction: 		"Floor()"
			MaxFunction:		"Max()"
			MinFunction:		"Min()"
		}
	}
	
	def String disarmExpression(Object obj){
		var String toReturn = "disarmExpression";
		switch(obj){
			EnvironmentExpressions: 		return obj.javaSafe
			EnvironmentUpdateExpressions:	return obj.javaSafe
			BooleanExpressions:				return obj.javaSafe
			UpdateExpressions:				return obj.javaSafe
			MethodExpressions:				return obj.javaSafe
			default:						return toReturn
		}
	}
	
	def String javaSafe(MethodExpressions e){
		switch(e){
			MethodSubtraction:							{e.left.javaSafe + "_SUB_" + e.right.javaSafe }
			MethodAddition:								{e.left.javaSafe + "_PLU_" + e.right.javaSafe }
			MethodMultiplication:						{e.left.javaSafe + "_MUL_" + e.right.javaSafe }
			MethodModulo:								{e.left.javaSafe + "_MOD_" + e.right.javaSafe }
			MethodDivision:								{e.left.javaSafe + "_DIV_" + e.right.javaSafe }
			MethodAtomicPrimitive:						(e.value as PrimitiveType).javaSafe
			MethodAtomicVariable:						(e.value as VariableReference).asJava 
			MethodAtomicMethodReference:				(e.value as MethodExpressions).asJava 
			MethodReferenceMethodDeclaration: 			(e.ref as MethodDeclaration).asJava
			MethodReferencePredefinedMethodDeclaration: (e.ref as PredefinedMethodDeclaration).asJava
			MethodExpression:							e.expression.javaSafe
		}
	}
	
	def String javaSafe(UpdateExpressions e){
		switch(e){
			UpdateSubtraction:							{e.left.javaSafe + "_SUB_" + e.right.javaSafe }
			UpdateAddition:								{e.left.javaSafe + "_PLU_" + e.right.javaSafe }
			UpdateMultiplication:						{e.left.javaSafe + "_MUL_" + e.right.javaSafe }
			UpdateAtomicPrimitive:						(e.value as PrimitiveType).javaSafe
			UpdateAtomicVariable:						(e.value as VariableReference).asJava 
			UpdateAtomicMethodReference:				(e.value as MethodExpressions).asJava 
			UpdateExpression:							e.expression.javaSafe
		}
	}
	
	def String javaSafe(EnvironmentExpressions e){
		switch(e){
			EnvironmentSubtraction:							{e.left.javaSafe + "_SUB_" + e.right.javaSafe }
			EnvironmentAddition:							{e.left.javaSafe + "_PLU_" + e.right.javaSafe }
			EnvironmentMultiplication:						{e.left.javaSafe + "_MUL_" + e.right.javaSafe }
			EnvironmentModulo:								{e.left.javaSafe + "_MOD_" + e.right.javaSafe }
			EnvironmentDivision:							{e.left.javaSafe + "_DIV_" + e.right.javaSafe }
			EnvironmentAtomicPrimitive:						(e.value as PrimitiveType).javaSafe			
			EnvironmentAtomicVariable:						(e.value as VariableReference).asJava 
			EnvironmentAtomicMethodReference:				(e.value as MethodExpressions).asJava
			EnvironmentAtomicNow:							"_NOW"	
			EnvironmentAtomicMeasure:						e.value.javaSafe
			EnvironmentExpression:							e.expression.javaSafe
			EnvironmentMeasure:								"Measure"+e.hashCode+"_"+e.componentReference.getLabel.toFirstUpper
		}
	}
	
	def String javaSafe(EnvironmentUpdateExpressions e){
		switch(e){
			EnvironmentUpdateSubtraction:							{e.left.javaSafe + "_SUB_" + e.right.javaSafe }
			EnvironmentUpdateAddition:								{e.left.javaSafe + "_PLU_" + e.right.javaSafe }
			EnvironmentUpdateMultiplication:						{e.left.javaSafe + "_MUL_" + e.right.javaSafe }
			EnvironmentUpdateAtomicPrimitive:						(e.value as PrimitiveType).javaSafe		
			EnvironmentUpdateAtomicVariable:						(e.value as VariableReference).asJava
			EnvironmentUpdateAtomicMethodReference:					(e.value as MethodExpressions).asJava 
			EnvironmentUpdateExpression:							e.expression.javaSafe
			EnvironmentUpdateAtomicNow:								"_NOW"
			EnvironmentUpdateAtomicMeasure:							"Measure"+e.hashCode
		}
	}
	
	def String javaSafe(BooleanExpressions e){
		switch(e){
			BooleanOr:						{e.left.javaSafe + "_OR_" + e.right.javaSafe }
			BooleanAnd:						{e.left.javaSafe + "_AND_" + e.right.javaSafe }
			BooleanEquality:				{e.left.javaSafe + "_EQUA_"+ e.right.javaSafe }
			BooleanComparison:				{e.left.javaSafe + "_COMP_"+ e.right.javaSafe }
			BooleanSubtraction:				{e.left.javaSafe + "_SUB_" + e.right.javaSafe }
			BooleanAddition:				{e.left.javaSafe + "_PLU_" + e.right.javaSafe }
			BooleanMultiplication:			{e.left.javaSafe + "_MUL_" + e.right.javaSafe }
			BooleanModulo:					{e.left.javaSafe + "_MOD_" + e.right.javaSafe }
			BooleanDivision:				{e.left.javaSafe + "_DIV_" + e.right.javaSafe }
			BooleanNot:						{"_NOT_"+e.expression.javaSafe}
			BooleanAtomicPrimitive:			(e.value as PrimitiveType).javaSafe			
			BooleanAtomicVariable:			(e.value as VariableReference).asJava
			BooleanAtomicMethodReference:	(e.value as MethodExpressions).asJava	
			BooleanAtomicNow:				"_NOW"	
			BooleanExpression:				e.expression.javaSafe
		}
	}
	
	def String javaSafe(PrimitiveType e){
		switch(e){
			CarmaDouble:	{
				var String output = ""
				output = e.left.toString + "_POINT_" + e.right.toString
				if(e.exponent != null)
					output = output + e.exponent.convertToJavaName
				return output
			}
			CarmaInteger:	""+e.value
			CarmaBoolean:	e.value
			Range:			e.min + "_PPP_" + e.max
		}
	}


}
