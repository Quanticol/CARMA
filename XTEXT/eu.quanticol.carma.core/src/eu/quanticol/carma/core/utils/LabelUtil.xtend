package eu.quanticol.carma.core.utils

import eu.quanticol.carma.core.carma.Action
import eu.quanticol.carma.core.carma.ActionName
import eu.quanticol.carma.core.carma.ActionStub
import eu.quanticol.carma.core.carma.BlockSystem
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
import eu.quanticol.carma.core.carma.CarmaExponent
import eu.quanticol.carma.core.carma.CarmaInteger
import eu.quanticol.carma.core.carma.CeilingFunction
import eu.quanticol.carma.core.carma.ComponentBlockDefinition
import eu.quanticol.carma.core.carma.ComponentName
import eu.quanticol.carma.core.carma.ComponentStyle
import eu.quanticol.carma.core.carma.DoubleAssignment
import eu.quanticol.carma.core.carma.DoubleAssignmentCarmaDouble
import eu.quanticol.carma.core.carma.DoubleAssignmentMethodReference
import eu.quanticol.carma.core.carma.DoubleAssignmentVariableName
import eu.quanticol.carma.core.carma.DoubleTypeLabel
import eu.quanticol.carma.core.carma.EnumAssignment
import eu.quanticol.carma.core.carma.EnumAssignmentCarmaInteger
import eu.quanticol.carma.core.carma.EnumAssignmentMethodReference
import eu.quanticol.carma.core.carma.EnumAssignmentRange
import eu.quanticol.carma.core.carma.EnumAssignmentVariableName
import eu.quanticol.carma.core.carma.EnumTypeLabel
import eu.quanticol.carma.core.carma.FloorFunction
import eu.quanticol.carma.core.carma.InputAction
import eu.quanticol.carma.core.carma.IntegerAssignment
import eu.quanticol.carma.core.carma.IntegerAssignmentCarmaInteger
import eu.quanticol.carma.core.carma.IntegerAssignmentMethodReference
import eu.quanticol.carma.core.carma.IntegerAssignmentVariableName
import eu.quanticol.carma.core.carma.IntegerTypeLabel
import eu.quanticol.carma.core.carma.LineSystem
import eu.quanticol.carma.core.carma.MacroName
import eu.quanticol.carma.core.carma.MaxFunction
import eu.quanticol.carma.core.carma.MeasureName
import eu.quanticol.carma.core.carma.MeasureBlock
import eu.quanticol.carma.core.carma.MethodAddition
import eu.quanticol.carma.core.carma.MethodAtomicMethodReference
import eu.quanticol.carma.core.carma.MethodAtomicPrimitive
import eu.quanticol.carma.core.carma.MethodAtomicRecords
import eu.quanticol.carma.core.carma.MethodAtomicVariable
import eu.quanticol.carma.core.carma.MethodDeclaration
import eu.quanticol.carma.core.carma.MethodDefinition
import eu.quanticol.carma.core.carma.MethodDefinitionArgument
import eu.quanticol.carma.core.carma.MethodDefinitionArguments
import eu.quanticol.carma.core.carma.MethodDivision
import eu.quanticol.carma.core.carma.MethodExpression
import eu.quanticol.carma.core.carma.MethodModulo
import eu.quanticol.carma.core.carma.MethodMultiplication
import eu.quanticol.carma.core.carma.MethodName
import eu.quanticol.carma.core.carma.MethodReferenceMethodDeclaration
import eu.quanticol.carma.core.carma.MethodReferencePredefinedMethodDeclaration
import eu.quanticol.carma.core.carma.MethodSubtraction
import eu.quanticol.carma.core.carma.Methods
import eu.quanticol.carma.core.carma.MinFunction
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.MultiCast
import eu.quanticol.carma.core.carma.Name
import eu.quanticol.carma.core.carma.OutputAction
import eu.quanticol.carma.core.carma.PDFunction
import eu.quanticol.carma.core.carma.PredefinedMethodDeclaration
import eu.quanticol.carma.core.carma.PrimitiveType
import eu.quanticol.carma.core.carma.ProcessName
import eu.quanticol.carma.core.carma.Range
import eu.quanticol.carma.core.carma.RecordDeclaration
import eu.quanticol.carma.core.carma.RecordDeclarations
import eu.quanticol.carma.core.carma.RecordName
import eu.quanticol.carma.core.carma.RecordReferenceMy
import eu.quanticol.carma.core.carma.RecordReferencePure
import eu.quanticol.carma.core.carma.RecordReferenceReceiver
import eu.quanticol.carma.core.carma.RecordReferenceSender
import eu.quanticol.carma.core.carma.RecordReferenceThis
import eu.quanticol.carma.core.carma.RecordTypeLabel
import eu.quanticol.carma.core.carma.Records
import eu.quanticol.carma.core.carma.System
import eu.quanticol.carma.core.carma.SystemName
import eu.quanticol.carma.core.carma.TypeLabel
import eu.quanticol.carma.core.carma.UniformFunction
import eu.quanticol.carma.core.carma.VariableDeclaration
import eu.quanticol.carma.core.carma.VariableDeclarationCarmaDouble
import eu.quanticol.carma.core.carma.VariableDeclarationCarmaIntger
import eu.quanticol.carma.core.carma.VariableDeclarationEnum
import eu.quanticol.carma.core.carma.VariableDeclarationRecord
import eu.quanticol.carma.core.carma.VariableName
import eu.quanticol.carma.core.carma.VariableReferenceMy
import eu.quanticol.carma.core.carma.VariableReferencePure
import eu.quanticol.carma.core.carma.VariableReferenceReceiver
import eu.quanticol.carma.core.carma.VariableReferenceSender
import eu.quanticol.carma.core.carma.VariableReferenceThis
import eu.quanticol.carma.core.carma.VariableType
import eu.quanticol.carma.core.carma.VariableTypeCarmaDouble
import eu.quanticol.carma.core.carma.VariableTypeCarmaIntger
import eu.quanticol.carma.core.carma.VariableTypeEnum
import eu.quanticol.carma.core.carma.VariableTypeRecord

import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.core.carma.ProcessExpressionLeaf
import eu.quanticol.carma.core.carma.ProcessExpressionGuard
import eu.quanticol.carma.core.carma.Guard
import eu.quanticol.carma.core.carma.Process
import eu.quanticol.carma.core.carma.ProcessExpression
import eu.quanticol.carma.core.carma.ProcessExpressionChoice
import eu.quanticol.carma.core.carma.ProcessExpressionAction
import eu.quanticol.carma.core.carma.ProcessExpressionReference
import java.util.ArrayList
import eu.quanticol.carma.core.carma.Component
import eu.quanticol.carma.core.carma.ComponentBlockStyle
import eu.quanticol.carma.core.carma.ComponentLineDefinition
import eu.quanticol.carma.core.carma.EnvironmentExpression
import eu.quanticol.carma.core.carma.EnvironmentSubtraction
import eu.quanticol.carma.core.carma.EnvironmentAddition
import eu.quanticol.carma.core.carma.EnvironmentMultiplication
import eu.quanticol.carma.core.carma.EnvironmentModulo
import eu.quanticol.carma.core.carma.EnvironmentDivision
import eu.quanticol.carma.core.carma.EnvironmentAtomicPrimitive
import eu.quanticol.carma.core.carma.EnvironmentAtomicRecords
import eu.quanticol.carma.core.carma.EnvironmentAtomicVariable
import eu.quanticol.carma.core.carma.EnvironmentAtomicMethodReference
import eu.quanticol.carma.core.carma.EnvironmentAtomicNow
import eu.quanticol.carma.core.carma.EnvironmentAtomicMeasure
import com.google.inject.Inject
import eu.quanticol.carma.core.carma.UpdateExpression
import eu.quanticol.carma.core.carma.UpdateSubtraction
import eu.quanticol.carma.core.carma.UpdateAddition
import eu.quanticol.carma.core.carma.UpdateMultiplication
import eu.quanticol.carma.core.carma.UpdateAtomicPrimitive
import eu.quanticol.carma.core.carma.UpdateAtomicRecords
import eu.quanticol.carma.core.carma.UpdateAtomicVariable
import eu.quanticol.carma.core.carma.UpdateAtomicMethodReference
import eu.quanticol.carma.core.carma.ComponentBlockNewDeclaration
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
import eu.quanticol.carma.core.carma.MacroExpressions
import eu.quanticol.carma.core.carma.NewComponentArgumentReference
import eu.quanticol.carma.core.carma.MacroExpressionParallel
import eu.quanticol.carma.core.carma.MacroExpressionReference
import eu.quanticol.carma.core.carma.ComponentBlockForStatement
import eu.quanticol.carma.core.carma.ComponentAfterThought
import eu.quanticol.carma.core.carma.BooleanExpressions
import eu.quanticol.carma.core.carma.MethodExpressions
import eu.quanticol.carma.core.carma.UpdateExpressions
import eu.quanticol.carma.core.carma.EnvironmentExpressions
import eu.quanticol.carma.core.carma.ComponentBlockDefinitionArgumentVariable
import eu.quanticol.carma.core.carma.ComponentArgument
import eu.quanticol.carma.core.carma.ComponentBlockDefinitionArgumentMacro
import eu.quanticol.carma.core.carma.MacroType
import eu.quanticol.carma.core.carma.VariableReference
import eu.quanticol.carma.core.carma.OutputActionArgument
import eu.quanticol.carma.core.carma.OutputActionArgumentVR
import eu.quanticol.carma.core.carma.OutputActionArgumentV
import java.util.HashMap
import eu.quanticol.carma.core.carma.Rate
import eu.quanticol.carma.core.carma.EnvironmentGuard
import eu.quanticol.carma.core.carma.ComponentBlockStyleCollective
import eu.quanticol.carma.core.carma.ComponentBlockDefinitionArguments
import eu.quanticol.carma.core.carma.EnvironmentUpdate
import eu.quanticol.carma.core.carma.ComponentBlockNewDeclarationSpawn
import eu.quanticol.carma.core.carma.CBND
import java.util.List
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressions
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressionParallel
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressionAll
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressionComponentAllStates
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressionComponentAState

class LabelUtil {
	
	@Inject extension Util
	
	def String flatten(Records records){
		var output = ""
		for(r : records.eAllOfType(RecordDeclaration))
			output = output + r.name.label
		return output
	}
	
	def String flatten(NCA nca){
		switch(nca){
			NewComponentArgumentPrimitive 		: (nca.value as PrimitiveType).getLabel
			NewComponentArgumentMacro 			: (nca.value as MacroExpressions).getLabel
			NewComponentArgumentMethod			: (nca.value as MethodExpression).getLabel
			NewComponentArgumentDeclare			: (nca.value as Records).flatten
			NewComponentArgumentReference		: (nca.value as VariableReference).getLabel
			NewComponentArgumentSpawnPrimitive 	: (nca.value as PrimitiveType).getLabel
			NewComponentArgumentSpawnDeclare	: (nca.value as MacroExpressions).getLabel
			NewComponentArgumentSpawnMacro		: (nca.value as MethodExpression).getLabel
			NewComponentArgumentSpawnMethod		: (nca.value as Records).flatten
			NewComponentArgumentSpawnReference	: (nca.value as VariableReference).getLabel
		}
	}
	
	def String getLabel(EnvironmentMacroExpressions eme){
		switch(eme){
			EnvironmentMacroExpressionParallel:				eme.left +"_"+ eme.right
			EnvironmentMacroExpressionAll:					eme+"_All"
			EnvironmentMacroExpressionComponentAllStates:	eme.comp.getLabel+"_All"
			EnvironmentMacroExpressionComponentAState:		eme.comp.getLabel+"_"+eme.state.getLabel
		}
	}
	
	def String getLabel(Rate rate){
		return "[" + rate.guard.label + "]" + rate.stub.label
	}
	
	def String getLabel(EnvironmentGuard eg){
		eg.booleanExpression.getLabel
	}
	
	def String getNameValue(ComponentAfterThought cat){
		cat.name.label + " = " + cat.expression.label
	}
	
	def String convertToJava(VariableDeclaration vd){
		switch(vd){
			VariableDeclarationEnum: 		"int " 		+ vd.name.label + " = " + (vd.assign as EnumAssignment).label
			VariableDeclarationRecord:		"int " 		+ vd.name.label + " = " + (vd.assign as RecordDeclarations).label
			VariableDeclarationCarmaDouble:	"double " 	+ vd.name.label + " = " + (vd.assign as DoubleAssignment).label
			VariableDeclarationCarmaIntger:	"int " 		+ vd.name.label + " = " + (vd.assign as IntegerAssignment).label
		}
	}
	
	def String getLabel(ComponentArgument ca){
		switch(ca){
			ComponentBlockDefinitionArgumentVariable:	(ca.value as VariableType).getLabel
			ComponentBlockDefinitionArgumentMacro:		(ca.value as MacroType).getLabel
		}
	}
	
	def String getLabel(OutputActionArgument oa){
		switch(oa){
			OutputActionArgumentVR:	(oa.ref as VariableReference).getLabel
			OutputActionArgumentV:	(oa.value as PrimitiveType).getLabel
		}
	}
	
	def String convertToJava(ComponentArgument ca){
		switch(ca){
			ComponentBlockDefinitionArgumentVariable:	(ca.value as VariableType).convertToJava
			ComponentBlockDefinitionArgumentMacro:		(ca.value as MacroType).getLabel
		}
	}
	
	def String getLabel(MacroType mt){
		mt.assignee.getLabel
	}
	
	def String getLabel(NCA ca){
		switch(ca){
			NewComponentArgumentPrimitive 		: (ca.value as PrimitiveType).getLabel
			NewComponentArgumentMacro 			: (ca.value as MacroExpressions).getLabel
			NewComponentArgumentMethod			: (ca.value as MethodExpression).getLabel
			NewComponentArgumentDeclare			: (ca.value as Records).getLabel
			NewComponentArgumentReference		: (ca.value as VariableReference).getLabel
			NewComponentArgumentSpawnPrimitive 	: (ca.value as PrimitiveType).getLabel
			NewComponentArgumentSpawnDeclare	: (ca.value as MacroExpressions).getLabel
			NewComponentArgumentSpawnMacro		: (ca.value as MethodExpression).getLabel
			NewComponentArgumentSpawnMethod		: (ca.value as Records).getLabel
			NewComponentArgumentSpawnReference	: (ca.value as VariableReference).getLabel
		}
	}
	
	def String getLabel(MacroExpressions e){
		switch(e){
			MacroExpressionParallel		: {e.left.getLabel + "_" + e.right.getLabel}
			MacroExpressionReference 	: e.name.getLabel
		}
	}
	
	
	def String getLabel(ComponentBlockNewDeclaration dc){
		dc.name.label
	}
	
	def String getLabel(UpdateExpressions e){
		switch(e){
			UpdateSubtraction:							{e.left.label + " - " + e.right.label }
			UpdateAddition:								{e.left.label + " + " + e.right.label }
			UpdateMultiplication:						{e.left.label + " * " + e.right.label }
			UpdateAtomicPrimitive:						(e.value as PrimitiveType).label
			UpdateAtomicRecords:						(e.value as Records).label
			UpdateAtomicVariable:						e.value.label
			UpdateAtomicMethodReference:				(e.value as MethodExpression).label 
			UpdateExpression:							e.expression.label
		}
	}
	
	def String getLabel(EnvironmentExpressions e){
		switch(e){
			EnvironmentSubtraction:							{e.left.label + " - " + e.right }
			EnvironmentAddition:							{e.left.label + " + " + e.right }
			EnvironmentMultiplication:						{e.left.label + " * " + e.right }
			EnvironmentModulo:								{e.left.label + " % " + e.right }
			EnvironmentDivision:							{e.left.label + " / " + e.right }
			EnvironmentAtomicPrimitive:						(e.value as PrimitiveType).label
			EnvironmentAtomicRecords:						(e.value as Records).label
			EnvironmentAtomicVariable:						e.value.label
			EnvironmentAtomicMethodReference:				(e.value as MethodExpressions).label 
			EnvironmentAtomicNow:							"now.LabelUtil"
			EnvironmentAtomicMeasure:						"measure.LabelUtil"
			EnvironmentExpression:							e.expression.label
		}
	}
	
	def String getLabel(Model model){
		model.eResource.URI.lastSegment.split("\\.").get(0)
	}
	
	def String getLabel(System system){
		switch(system){
			BlockSystem:	system.name.label
			LineSystem:		system.name.label
		}
	}
	
	def String getLabel(Methods m){
		"Functions"
	}
	
	def String getLabel(MethodDefinition md){
		"fun" + " " + md.type.label + " " md.name.label + " (" + md.functionArguments.label + " )"
	}
	
	def String getLabel(TypeLabel typeLabel){
		switch(typeLabel){
			DoubleTypeLabel: 	"double" 	
			IntegerTypeLabel: 	"integer"	
			RecordTypeLabel:	"record"	
			EnumTypeLabel:		"enum"
		}
	}
	
	def String getLabel(Name name){
		switch(name){
			VariableName: 	name.name
			RecordName: 	name.name
			ActionName: 	name.name
			ProcessName: 	name.name
			ComponentName: 	name.name
			MacroName: 		name.name.name
			MethodName: 	name.name
			MeasureName: 	name.name
			SystemName: 	name.name
		}
		
	}
	
	def HashMap<String,String>  getNameValueLabel(VariableName name){
		var vd = name.getVariableDeclaration
		var output = new HashMap<String,String>()
		if(name.isEnum){
			output.put(name.label,vd.label)
		}
		if(name.isRecord){
			for(rd : vd.eAllOfType(RecordDeclaration))
				output.put(name.label+"_"+rd.name.label,rd.assign.label)
		}
		return output
	}
	
	def String getLabel(ActionStub actionStub){
		
		var output = actionStub.name.name
		
		if(actionStub.cast != null){
			output = output + "*"
		}
		
//		if(actionStub.io != null){
//			if(actionStub.io.in != null)
//				output = output + "()"
//			else
//				output = output + "<>"
//		}
		
		
		return output
		
	}
	
	def String getLabelName(ActionStub actionStub){
		
		var output = actionStub.name.name
		return output
		
	}
	
	def String getLabelInOut(ActionStub actionStub){
		if(actionStub.label.contains("()"))
			"INTPUT"
		else
			"OUTPUT"
	}
	
	def String getLabelFull(ActionName name){
		
		var action = name.getContainerOfType(Action)
		return action.getLabel
	}
	
	def String getLabel(MethodDefinitionArguments mdas){
		var String output = ""
		for(mda : mdas.inputArguments)
			output = output + " " + mda.label
		
		return output
	}
	
	def String getLabel(MethodDefinitionArgument mda){
		mda.argument.label
	}
	
	def String getLabel(VariableType vt){
		switch(vt){
			VariableTypeEnum:			"enum" 		+ ":"  	+	vt.name.label 
			VariableTypeRecord:			"record"	+ ":"  	+	vt.name.label
			VariableTypeCarmaDouble:	"double" 	+ ":"  	+	vt.name.label
			VariableTypeCarmaIntger:	"integer" 	+ ":"  	+	vt.name.label
		}
	}
	
	def String convertToJava(VariableType vt){
		switch(vt){
			VariableTypeEnum:			"int " 		+	vt.name.label 
			VariableTypeRecord:			{vt.spread}
			VariableTypeCarmaDouble:	"double "	+	vt.name.label
			VariableTypeCarmaIntger:	"int " 		+	vt.name.label
		}
	}
	
	def String spread(VariableTypeRecord vtr){
		
		var ArrayList<RecordDeclaration> rds = new ArrayList<RecordDeclaration>()
		//get position in the ComponentBlockDefinitionArguments
		var position = vtr.getPosition
		//get ComponentBlockDeclaration
		var cbnds = vtr.getCBNDs
			
		for(c : cbnds){
			if(c.getContainerOfType(ComponentBlockStyleCollective) != null){
				rds.addAll((c as ComponentBlockNewDeclaration).componentInputArguments.inputArguments.get(position).eAllOfType(RecordDeclaration))
			} else if (c.getContainerOfType(EnvironmentUpdate) != null) {
				rds.addAll((c as ComponentBlockNewDeclarationSpawn).componentInputArguments.inputArguments.get(position).eAllOfType(RecordDeclaration))
			}
		}
		
		var String output = ""
		
		if(rds.size > 0){
			output = " int " + rds.get(0).name.label
			for(var i = 1; i < rds.size; i++){
				output = output + ", int " + rds.get(i).name.label
			}
		} else {
			output = "//UNTIL SENDER/RECEIVER/GLOBAL RESOLVED NO ARGUMENTS FOR SPAWN @ LabelUtil.spread()"
		}
		
		
		return output
	}
	
	def String getLabel(ComponentStyle cs){
		"Model"
	}
	
	def String getLabel(ComponentBlockDefinition cbd){
		"Component" + " " + cbd.name.label
	}
	
	def String getLabel(MeasureBlock m){
		"Measures"
	}
	
	def String getLabel(BlockSystem bs){
		"System"
	}
	
	def String getLabel(BooleanExpressions e){
		switch(e){
			BooleanOr:						{e.left.label + " || " + e.right.label }
			BooleanAnd:						{e.left.label + " && " + e.right.label }
			BooleanEquality:				{e.left.label + " " + e.op + " " + e.right.label }
			BooleanComparison:				{e.left.label + " " + e.op + " " + e.right.label }
			BooleanSubtraction:				{e.left.label + " - " + e.right.label }
			BooleanAddition:				{e.left.label + " + " + e.right.label }
			BooleanMultiplication:			{e.left.label + " * " + e.right.label }
			BooleanModulo:					{e.left.label + " % " + e.right.label }
			BooleanDivision:				{e.left.label + " / " + e.right.label }
			BooleanNot:						{"!"+e.expression.label}
			BooleanAtomicPrimitive:			(e.value as PrimitiveType).label			
			BooleanAtomicRecords:			(e.value as Records).label			
			BooleanAtomicVariable:			(e.value as VariableReference).label 
			BooleanAtomicMethodReference:	(e.value as MethodExpression).label			
			BooleanAtomicNow:				"now"	
			BooleanExpression:				e.expression.label
		}
	}
	
	def String getLabelJava(BooleanExpressions e){
		switch(e){
			BooleanOr:						{e.left.labelJava + " || " + e.right.labelJava }
			BooleanAnd:						{e.left.labelJava + " && " + e.right.labelJava }
			BooleanEquality:				{e.left.labelJava + " " + e.op + " " + e.right.labelJava }
			BooleanComparison:				{e.left.labelJava + " " + e.op + " " + e.right.labelJava }
			BooleanSubtraction:				{e.left.labelJava + " - " + e.right.labelJava }
			BooleanAddition:				{e.left.labelJava + " + " + e.right.labelJava }
			BooleanMultiplication:			{e.left.labelJava + " * " + e.right.labelJava }
			BooleanModulo:					{e.left.labelJava + " % " + e.right.labelJava }
			BooleanDivision:				{e.left.labelJava + " / " + e.right.labelJava }
			BooleanNot:						{"!"+e.expression.labelJava}
			BooleanAtomicPrimitive:			(e.value as PrimitiveType).label			
			BooleanAtomicRecords:			(e.value as Records).label			
			BooleanAtomicVariable:			(e.value as VariableReference).labelJava
			BooleanAtomicMethodReference:	(e.value as MethodExpression).label			
			BooleanAtomicNow:				"now"	
			BooleanExpression:				e.expression.labelJava
		}
	}
	
	def getLabel(VariableDeclaration vd){
		switch(vd){
			VariableDeclarationEnum: 		(vd.assign as EnumAssignment).label
			VariableDeclarationRecord:		(vd.assign as RecordDeclarations).label
			VariableDeclarationCarmaDouble:	(vd.assign as DoubleAssignment).label
			VariableDeclarationCarmaIntger:	(vd.assign as IntegerAssignment).label
		}
	}
	
	def getLabel(EnumAssignment ea){
		switch(ea){
			EnumAssignmentCarmaInteger: 	(ea.naturalValue as CarmaInteger).label
			EnumAssignmentMethodReference: 	(ea.method as MethodExpression).label
			EnumAssignmentRange:			(ea.range as Range).label
			EnumAssignmentVariableName: 	(ea.ref as VariableReference).label
		}
	}
	
	def getLabel(RecordDeclarations rds){
		if(rds.ref != null)
			rds.ref.label
		else
			(rds as Records).label 
	}
	
	def getLabel(DoubleAssignment da){
		switch(da){
		DoubleAssignmentCarmaDouble: 		da.doubleValue.label	
		DoubleAssignmentMethodReference: 	da.method.label	
		DoubleAssignmentVariableName: 		da.reference.label
		}
	}
	
	def getLabel(IntegerAssignment ia){
		switch(ia){
		IntegerAssignmentCarmaInteger: 		ia.integerValue.label
		IntegerAssignmentMethodReference: 	ia.method.label	
		IntegerAssignmentVariableName: 		ia.reference.label
		}
	}
	
	def String getLabel(PrimitiveType e){
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
			Range:			e.min + "..." + e.max
		}
	}
	
	def String getLabel(CarmaExponent ce){
		var String output = "^"
		if(ce.negative != null)
			output = output + ce.negative
		output = output + ce.exponent
	}
	
	def String getLabel(Records e){
		var String output = "{"
		
		for(rd : e.recordDeclarations)
			output = output + " " + rd.label + " "
		
		output = output + "}"
		return output
	}
	
	def String getLabel(RecordDeclaration e){
		e.name.label + " := " + e.assign.label
	}
	
//	def String getLabel(VariableOrRecordReference e){
//		switch(e){
//			OrVariableReference:	(e.ref as VRReference).label 
//			OrRecordReference:		(e.ref as VRReference).label 
//		}
//	}
	
	def String getLabel(VariableReference vr){
		switch(vr){
			VariableReferencePure: 			vr.name.label
			VariableReferenceMy: 			{"my." 			+ vr.name.label}
			VariableReferenceThis: 			{"this." 		+ vr.name.label}
			VariableReferenceReceiver:		{"receiver." 	+ vr.name.label}
			VariableReferenceSender:		{"sender."		+ vr.name.label}
			RecordReferencePure:			{vr.name.label + "." + vr.record.label	}
			RecordReferenceMy:				{"my" 				+ "." + vr.name.label + "." + vr.record.label	}
			RecordReferenceThis:			{"this" 			+ "." + vr.name.label + "." + vr.record.label	}
			RecordReferenceReceiver:		{"receiver" 		+ "." + vr.name.label + "." + vr.record.label	}
			RecordReferenceSender:			{"sender" 			+ "." + vr.name.label + "." + vr.record.label	}
		}
	}
	
	def String getLabelJava(VariableReference vr){
		switch(vr){
			VariableReferencePure: 			vr.name.label
			VariableReferenceMy: 			vr.name.label
			VariableReferenceThis: 			vr.name.label
			VariableReferenceReceiver:		vr.name.label
			VariableReferenceSender:		vr.name.label
			RecordReferencePure:			vr.name.label + "_" + vr.record.label
			RecordReferenceMy:				vr.name.label + "_" + vr.record.label
			RecordReferenceThis:			vr.name.label + "_" + vr.record.label
			RecordReferenceReceiver:		vr.name.label + "_" + vr.record.label
			RecordReferenceSender:			vr.name.label + "_" + vr.record.label
		}
	}
	
	def String getLabel(MethodExpressions e){
		switch(e){
			MethodSubtraction:							{e.left.label + " - " + e.right.label }
			MethodAddition:								{e.left.label + " + " + e.right.label }
			MethodMultiplication:						{e.left.label + " * " + e.right.label }
			MethodModulo:								{e.left.label + " % " + e.right.label }
			MethodDivision:								{e.left.label + " / " + e.right.label }
			MethodAtomicPrimitive:						(e.value as PrimitiveType).label
			MethodAtomicRecords:						(e.value as Records).label
			MethodAtomicVariable:						e.value.label
			MethodAtomicMethodReference:				(e.value as MethodExpression).label 
			MethodReferenceMethodDeclaration: 			(e.ref as MethodDeclaration).label
			MethodReferencePredefinedMethodDeclaration: (e.ref as PredefinedMethodDeclaration).label
			MethodExpression:							e.expression.label
		}
	}
	
	def String getLabel(MethodDeclaration e){
		e.name.label
	}
	
	def String getLabel(PredefinedMethodDeclaration e){
		switch(e){	
			PDFunction:			"PDF()"
			UniformFunction: 	"Uniform()"
			CeilingFunction: 	"Ceiling()"
			FloorFunction: 		"Floor()"
			MaxFunction:		"Max()"
			MinFunction:		"Min()"
		}
	}
	
	def String getLabel(ProcessExpression pe){
		switch(pe){
			ProcessExpressionChoice: 	pe.left.getLabel + " + " + pe.right.getLabel
			ProcessExpressionLeaf:		pe.expression
			ProcessExpressionGuard:		pe.expression.getLabel + " " + pe.reference.getLabel
			ProcessExpressionAction: 	pe.expression.getLabel + " " + pe.reference.getLabel
			ProcessExpressionReference: pe.expression.getLabel
		}
	}
	
	/**
	 * This labelling function is used to create state names
	 */
	def String getLabelAsState(ProcessExpression pe){
		switch(pe){
			ProcessExpressionChoice: 	pe.left.getLabelAsState + "_c_" + pe.right.getLabelAsState
			ProcessExpressionLeaf:		pe.expression
			ProcessExpressionGuard:		pe.expression.getLabel + "_" + pe.reference.getLabelAsState
			ProcessExpressionAction: 	pe.expression.name.getLabel + "_" + pe.reference.getLabelAsState
			ProcessExpressionReference: pe.expression.name
		}
	}
	
	def String getLabelAsState(Process p){
		p.name.label
	}
	
	def String getLabel(Guard g){
		"[" + g.booleanExpression.getLabel + "]"
	}
	
	def String getLabel(Action a){
		
		var multicast 		= a.eAllOfType(MultiCast).size > 0
		var inputAction  	= a.eAllOfType(InputAction).size > 0
		var outputAction 	= a.eAllOfType(OutputAction).size > 0
		var output 			= a.name.getLabel
		
		if(multicast){
			output = output + "*"
		} 
		
//		if(inputAction){
//			output = output + "()"
//		}
//		
//		if(outputAction){
//			output = output + "<>"
//		}
		
		return output
		
	}
	
	def String getLabel(Component c){
		if(c.getContainerOfType(ComponentBlockStyle) != null){
			(c as ComponentBlockDefinition).name.getLabel
		} else {
			(c as ComponentLineDefinition).name.getLabel
		}
	}

}