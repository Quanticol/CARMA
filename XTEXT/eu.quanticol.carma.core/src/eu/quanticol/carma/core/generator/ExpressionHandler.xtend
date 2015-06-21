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
import eu.quanticol.carma.core.carma.Component
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
import eu.quanticol.carma.core.carma.ForVariableDeclaration
import eu.quanticol.carma.core.carma.MacroExpressionReference
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
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.NCA
import eu.quanticol.carma.core.carma.NewComponentArgumentDeclare
import eu.quanticol.carma.core.carma.NewComponentArgumentMacro
import eu.quanticol.carma.core.carma.NewComponentArgumentMethod
import eu.quanticol.carma.core.carma.NewComponentArgumentPrimitive
import eu.quanticol.carma.core.carma.NewComponentArgumentReference
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
import eu.quanticol.carma.core.utils.Util
import java.util.ArrayList
import java.util.HashMap
import java.util.HashSet
import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.core.carma.Records
import eu.quanticol.carma.core.carma.RecordDeclaration

public class ExpressionHandler {
	
	@Inject extension LabelUtil
	@Inject extension Util
	
	def String evaluateExpression(Object obj){
		var String toReturn = "evaluateExpression";
		switch(obj){
			EnvironmentExpressions: 		return obj.asJava
			EnvironmentUpdateExpressions:	return obj.asJava
			BooleanExpressions:				return obj.asJava
			UpdateExpressions:				return obj.asJava
			MethodExpressions:				return obj.asJava
			EnvironmentMacroExpressions:	return obj.asJava
			default:						return toReturn
		}
	}
	
	def String asJava(EnvironmentMacroExpressions ems){
		var componentState = new HashMap<String,ArrayList<String>>()
		ems.getComponentState(componentState)
		var output = ""
		if(componentState.keySet.size > 0){
			output = output + '''
			( 
			(((CarmaSequentialProcess) p).automaton() ==  «ems.getContainerOfType(Model).label»Definition.«componentState.keySet.get(0)») && (
			'''
			for(state : componentState.get(componentState.keySet.get(0))){
				output = output + '''
				(((CarmaSequentialProcess) p).automaton().getState("«state»") != null ) ||
				'''
			}
			output = output + '''
			(((CarmaSequentialProcess) p).getState() !=  null))
			)
			'''
			for(var int i = 1; i < componentState.keySet.size; i++){
				output = output + '''
				||( 
				(((CarmaSequentialProcess) p).automaton() ==  «ems.getContainerOfType(Model).label»Definition.«componentState.keySet.get(i)») && (
				'''
				for(state : componentState.get(componentState.keySet.get(i))){
					output = output + '''
					(((CarmaSequentialProcess) p).automaton().getState("«state»") != null ) ||
					'''
				}
				
				output = output + '''
				(((CarmaSequentialProcess) p).getState() !=  null))
				)
				'''
			}
		}
		return output + ";"
	}
	
	def void getComponentState(EnvironmentMacroExpressions ems, HashMap<String,ArrayList<String>> componentState){
		
		switch(ems){
			EnvironmentMacroExpressionParallel: 			{ems.left.getComponentState(componentState)  ems.right.getComponentState(componentState)}  
			EnvironmentMacroExpressionAll:					ems.allComponents(componentState)
			EnvironmentMacroExpressionComponentAllStates:	ems.comp.getContainerOfType(Component).aComponentAllStates(componentState)
			EnvironmentMacroExpressionComponentAState:		ems.comp.getContainerOfType(Component).aComponentAState(componentState,new ArrayList<MacroExpressionReference>(ems.state.eAllOfType(MacroExpressionReference)))
		}
	}
	
	def void allComponents(EnvironmentMacroExpressions ems, HashMap<String,ArrayList<String>> componentState){
		var components = ems.getContainerOfType(Model).eAllOfType(Component)
		for(component : components){
			componentState.put('''«component.label»Process''',component.allStates)
		}
	}
	
	def void aComponentAllStates(Component component, HashMap<String,ArrayList<String>> componentState){
		componentState.put('''«component.label»Process''',component.allStates)
	}
	
	def void aComponentAState(Component component, HashMap<String,ArrayList<String>> componentState, ArrayList<MacroExpressionReference> states){
		var allStates = allStates(component)
		var ArrayList<String> output = new ArrayList<String>()
		for(state : states){
			for(s : allStates){
				if(s.equals("state_"+state.label)){
					output.add(s)
				}			
			}
		}
		componentState.put('''«component.label»Process''',output)
	}
	
	def ArrayList<String> allStates(Component component){
		var output = new ArrayList<String>()
		var HashSet<String> states = new HashSet<String>()
		var tree = component.getTree
		tree.getStates(states)
		for(state : states)
			output.add(state)
		return output
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
			EnvironmentMeasure:								"getMeasure"+Math.abs(e.hashCode % 1969)+"().measure(this)"
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
			EnvironmentUpdateAtomicMeasure:							"getMeasure"+Math.abs(e.hashCode % 1969)+"().measure(this)"
		}
	}
	
	def String asJavaEvolutionRule(EnvironmentUpdateExpressions e){
		switch(e){
			EnvironmentUpdateSubtraction:							{e.left.asJavaEvolutionRule + " - " + e.right.asJavaEvolutionRule }
			EnvironmentUpdateAddition:								{e.left.asJavaEvolutionRule + " + " + e.right.asJavaEvolutionRule }
			EnvironmentUpdateMultiplication:						{e.left.asJavaEvolutionRule + " * " + e.right.asJavaEvolutionRule }
			EnvironmentUpdateAtomicPrimitive:						(e.value as PrimitiveType).asJava
			EnvironmentUpdateAtomicVariable:						(e.value as VariableReference).asJavaEvolutionRule
			EnvironmentUpdateAtomicMethodReference:					(e.value as MethodExpressions).asJava 
			EnvironmentUpdateExpression:							e.expression.asJavaEvolutionRule
			EnvironmentUpdateAtomicNow:								"now()"
			EnvironmentUpdateAtomicMeasure:							"getMeasure"+Math.abs(e.hashCode % 1969)+"().measure(this)"
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
	
	def String asFullJava(VariableReference vr){
		switch(vr){
			VariableReferencePure: 			vr.name.label
			VariableReferenceMy: 			"my_"+vr.name.label
			VariableReferenceThis: 			"this_"+vr.name.label
			VariableReferenceReceiver:		"receiver_"+vr.name.label
			VariableReferenceSender:		"sender_"+vr.name.label
			VariableReferenceGlobal:		"global_"+vr.name.label
			RecordReferencePure:			vr.name.label + "_" + vr.record.label
			RecordReferenceMy:				"my_"+vr.name.label + "_" + vr.record.label
			RecordReferenceThis:			"this_"+vr.name.label + "_" + vr.record.label
			RecordReferenceReceiver:		"receiver_"+vr.name.label + "_" + vr.record.label
			RecordReferenceSender:			"sender_"+vr.name.label + "_" + vr.record.label
			RecordReferenceGlobal:			"global_"+vr.name.label + "_" + vr.record.label
		}
	}
	
	def String asJavaEvolutionRule(BooleanExpressions e){
		switch(e){
			BooleanOr:						{e.left.asJavaEvolutionRule + " || " + e.right.asJavaEvolutionRule }
			BooleanAnd:						{e.left.asJavaEvolutionRule + " && " + e.right.asJavaEvolutionRule }
			BooleanEquality:				{e.left.asJavaEvolutionRule + " " + e.op + " " + e.right.asJavaEvolutionRule }
			BooleanComparison:				{e.left.asJavaEvolutionRule + " " + e.op + " " + e.right.asJavaEvolutionRule }
			BooleanSubtraction:				{e.left.asJavaEvolutionRule + " - " + e.right.asJavaEvolutionRule }
			BooleanAddition:				{e.left.asJavaEvolutionRule + " + " + e.right.asJavaEvolutionRule }
			BooleanMultiplication:			{e.left.asJavaEvolutionRule + " * " + e.right.asJavaEvolutionRule }
			BooleanModulo:					{e.left.asJavaEvolutionRule + " % " + e.right.asJavaEvolutionRule }
			BooleanDivision:				{e.left.asJavaEvolutionRule + " / " + e.right.asJavaEvolutionRule }
			BooleanNot:						{"!"+e.expression.asJavaEvolutionRule}
			BooleanAtomicPrimitive:			(e.value as PrimitiveType).asJava 	
			BooleanAtomicVariable:			(e.value as VariableReference).asJavaEvolutionRule 
			BooleanAtomicMethodReference:	(e.value as MethodExpressions).asJava			
			BooleanAtomicNow:				"now()"	
			BooleanExpression:				e.expression.asJavaEvolutionRule
		}
	}
	
	def String asJavaEvolutionRule(VariableReference vr){
		switch(vr){
			VariableReferencePure: 			vr.name.label+"_global_"
			VariableReferenceMy: 			vr.name.label
			VariableReferenceThis: 			vr.name.label
			VariableReferenceReceiver:		vr.name.label+"_r"
			VariableReferenceSender:		vr.name.label+"_s"
			VariableReferenceGlobal:		vr.name.label+"_global_"
			RecordReferencePure:			vr.name.label + "_" + vr.record.label+"_global_"
			RecordReferenceMy:				vr.name.label + "_" + vr.record.label
			RecordReferenceThis:			vr.name.label + "_" + vr.record.label
			RecordReferenceReceiver:		vr.name.label + "_" + vr.record.label+"_r"
			RecordReferenceSender:			vr.name.label + "_" + vr.record.label+"_s"
			RecordReferenceGlobal:			vr.name.label + "_" + vr.record.label+"_global_"
		}
	}
	
	def String asJavaOutputAction(BooleanExpressions e){
		switch(e){
			BooleanOr:						{e.left.asJavaOutputAction + " || " + e.right.asJavaOutputAction }
			BooleanAnd:						{e.left.asJavaOutputAction + " && " + e.right.asJavaOutputAction }
			BooleanEquality:				{e.left.asJavaOutputAction + " " + e.op + " " + e.right.asJavaOutputAction }
			BooleanComparison:				{e.left.asJavaOutputAction + " " + e.op + " " + e.right.asJavaOutputAction }
			BooleanSubtraction:				{e.left.asJavaOutputAction + " - " + e.right.asJavaOutputAction }
			BooleanAddition:				{e.left.asJavaOutputAction + " + " + e.right.asJavaOutputAction }
			BooleanMultiplication:			{e.left.asJavaOutputAction + " * " + e.right.asJavaOutputAction }
			BooleanModulo:					{e.left.asJavaOutputAction + " % " + e.right.asJavaOutputAction }
			BooleanDivision:				{e.left.asJavaOutputAction + " / " + e.right.asJavaOutputAction }
			BooleanNot:						{"!"+e.expression.asJavaOutputAction}
			BooleanAtomicPrimitive:			(e.value as PrimitiveType).asJava 	
			BooleanAtomicVariable:			(e.value as VariableReference).asJavaOutputAction 
			BooleanAtomicMethodReference:	(e.value as MethodExpressions).asJava			
			BooleanAtomicNow:				"now()"	
			BooleanExpression:				e.expression.asJavaOutputAction
		}
	}
	
	def String asJavaOutputAction(VariableReference vr){
		switch(vr){
			VariableReferencePure: 			vr.name.label+"_i"
			VariableReferenceMy: 			vr.name.label+"_o"
			VariableReferenceThis: 			vr.name.label+"_o"
			VariableReferenceReceiver:		vr.name.label
			VariableReferenceSender:		vr.name.label
			VariableReferenceGlobal:		vr.name.label
			RecordReferencePure:			vr.name.label + "_" + vr.record.label+"_i"
			RecordReferenceMy:				vr.name.label + "_" + vr.record.label+"_o"
			RecordReferenceThis:			vr.name.label + "_" + vr.record.label+"_o"
			RecordReferenceReceiver:		vr.name.label + "_" + vr.record.label
			RecordReferenceSender:			vr.name.label + "_" + vr.record.label
			RecordReferenceGlobal:			vr.name.label + "_" + vr.record.label
		}
	}
	
	def String asJavaInputAction(BooleanExpressions e){
		switch(e){
			BooleanOr:						{e.left.asJavaInputAction + " || " + e.right.asJavaInputAction }
			BooleanAnd:						{e.left.asJavaInputAction + " && " + e.right.asJavaInputAction }
			BooleanEquality:				{e.left.asJavaInputAction + " " + e.op + " " + e.right.asJavaInputAction }
			BooleanComparison:				{e.left.asJavaInputAction + " " + e.op + " " + e.right.asJavaInputAction }
			BooleanSubtraction:				{e.left.asJavaInputAction + " - " + e.right.asJavaInputAction }
			BooleanAddition:				{e.left.asJavaInputAction + " + " + e.right.asJavaInputAction }
			BooleanMultiplication:			{e.left.asJavaInputAction + " * " + e.right.asJavaInputAction }
			BooleanModulo:					{e.left.asJavaInputAction + " % " + e.right.asJavaInputAction }
			BooleanDivision:				{e.left.asJavaInputAction + " / " + e.right.asJavaInputAction }
			BooleanNot:						{"!"+e.expression.asJavaInputAction}
			BooleanAtomicPrimitive:			(e.value as PrimitiveType).asJava 	
			BooleanAtomicVariable:			(e.value as VariableReference).asJavaInputAction 
			BooleanAtomicMethodReference:	(e.value as MethodExpressions).asJava			
			BooleanAtomicNow:				"now()"	
			BooleanExpression:				e.expression.asJavaInputAction
		}
	}
	
	def String asJavaInputAction(VariableReference vr){
		switch(vr){
			VariableReferencePure: 			vr.name.label+"_i"
			VariableReferenceMy: 			vr.name.label+"_i"
			VariableReferenceThis: 			vr.name.label+"_i"
			VariableReferenceReceiver:		vr.name.label
			VariableReferenceSender:		vr.name.label
			VariableReferenceGlobal:		vr.name.label
			RecordReferencePure:			vr.name.label + "_" + vr.record.label+"_i"
			RecordReferenceMy:				vr.name.label + "_" + vr.record.label+"_i"
			RecordReferenceThis:			vr.name.label + "_" + vr.record.label+"_i"
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
			EnvironmentMeasure:								"Measure"+Math.abs(e.hashCode % 1969)
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
			EnvironmentUpdateAtomicMeasure:							"Measure"+Math.abs(e.hashCode % 1969)
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
			BooleanAtomicVariable:			(e.value as VariableReference).javaSafe
			BooleanAtomicMethodReference:	(e.value as MethodExpressions).javaSafe	
			BooleanAtomicNow:				"_NOW"	
			BooleanExpression:				e.expression.javaSafe
		}
	}
	
	def String javaSafe(VariableReference vr){
		switch(vr){
			VariableReferencePure: 			vr.name.label
			VariableReferenceMy: 			"MY_"+vr.name.label
			VariableReferenceThis: 			"THIS_"+vr.name.label
			VariableReferenceReceiver:		"RECEIVER_"+vr.name.label
			VariableReferenceSender:		"SENDER_"+vr.name.label
			VariableReferenceGlobal:		"GLOBAL_"+vr.name.label
			RecordReferencePure:			vr.name.label + "_" + vr.record.label
			RecordReferenceMy:				"MY_"+vr.name.label + "_" + vr.record.label
			RecordReferenceThis:			"THIS_"+vr.name.label + "_" + vr.record.label
			RecordReferenceReceiver:		"RECEIVER_"+vr.name.label + "_" + vr.record.label
			RecordReferenceSender:			"SENDER_"+vr.name.label + "_" + vr.record.label
			RecordReferenceGlobal:			"GLOBAL_"+vr.name.label + "_" + vr.record.label
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
