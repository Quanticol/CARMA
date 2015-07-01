package eu.quanticol.carma.core.utils

class LabelUtil {
	
	
//	def String disarm(VariableReference vr){
//		switch(vr){
//			VariableReferencePure: 			vr.name.disarm
//			VariableReferenceMy: 			{"my_" 			+ vr.name.disarm}
//			VariableReferenceThis: 			{"this_" 		+ vr.name.disarm}
//			VariableReferenceReceiver:		{"receiver_" 	+ vr.name.disarm}
//			VariableReferenceSender:		{"sender_"		+ vr.name.disarm}
//			RecordReferencePure:			{vr.name.disarm 		+ "_" + vr.record.disarm	}
//			RecordReferenceMy:				{"my" 				+ "_" + vr.name.disarm + "_" + vr.record.disarm	}
//			RecordReferenceThis:			{"this" 			+ "_" + vr.name.disarm + "_" + vr.record.disarm	}
//			RecordReferenceReceiver:		{"receiver" 		+ "_" + vr.name.disarm + "_" + vr.record.disarm	}
//			RecordReferenceSender:			{"sender" 			+ "_" + vr.name.disarm + "_" + vr.record.disarm	}
//			VariableReferenceGlobal:		{"global."			+ vr.name.disarm}
//			RecordReferenceGlobal:			{"global" 			+ "_" + vr.name.disarm + "_" + vr.record.disarm	}
//		}
//	}
//	
//	def String disarm(Name name){
//		name.name
//	}
//	
//	def String express(AttribAssignment aa){
//		switch(aa){
//			AttribAssignmentCarmaInteger: aa.naturalValue.express
//			AttribAssignmentVariableName: aa.ref.disarm
//		}
//	}
//	
//	def String express(PrimitiveType e){
//		switch(e){
//			CarmaDouble:	{
//				var String output = ""
//				output = e.left.toString + "." + e.right.toString
//				if(e.exponent != null)
//					output = output + e.exponent.label
//				return output
//			}
//			CarmaInteger:	""+e.value
//			CarmaBoolean:	e.value
//			Range:			"//" + e.min + "..." + e.max + " LabelUtil.getLabel"
//		}
//	}
//	
//	//here be dragons
//	
//	def String flatten(Records records){
//		var output = ""
//		for(r : records.eAllOfType(RecordDeclaration))
//			output = output + r.name.label
//		return output
//	}
//	
//	def String flatten(NCA nca){
//		switch(nca){
//			NewComponentArgumentPrimitive 		: (nca.value as PrimitiveType).getLabel
//			NewComponentArgumentDeclare			: (nca.value as Records).flatten
//			NewComponentArgumentMacro 			: (nca.value as MacroExpressions).getLabel
//			NewComponentArgumentMethod			: (nca.value as MethodExpressions).getLabel
//			NewComponentArgumentReference		: (nca.value as VariableReference).getLabel
//			
//			NewComponentArgumentSpawnPrimitive 	: (nca.value as PrimitiveType).getLabel
//			NewComponentArgumentSpawnDeclare	: (nca.value as Records).flatten
//			NewComponentArgumentSpawnMacro		: (nca.value as MacroExpressions).getLabel
//			NewComponentArgumentSpawnMethod		: (nca.value as MethodExpressions).getLabel
//			NewComponentArgumentSpawnReference	: (nca.value as VariableReference).getLabel
//		}
//	}
//	
//	def String getLabel(EnvironmentMacroExpressions eme){
//		switch(eme){
//			EnvironmentMacroExpressionParallel:				eme.left +"_"+ eme.right
//			EnvironmentMacroExpressionAll:					"_All"
//			EnvironmentMacroExpressionComponentAllStates:	eme.comp.getLabel+"_All"
//			EnvironmentMacroExpressionComponentAState:		eme.comp.getLabel+"_"+eme.state.getLabel
//		}
//	}
//	
//	def String getLabel(Probability eu){
//		return "[" + eu.guard.label + "]" + eu.stub.label
//	}
//	
//	def String getLabel(Rate rate){
//		return "[" + rate.guard.label + "]" + rate.stub.label
//	}
//	
//	def String getLabel(EnvironmentUpdate eu){
//		return "[" + eu.guard.label + "]" + eu.stub.label
//	}
//	
//	def String convertToPredicateName(EnvironmentOperation eo){
//		switch(eo){
//			Probability:		eo.convertToPredicateName
//			Rate:				eo.convertToPredicateName
//			EnvironmentUpdate:  eo.convertToPredicateName
//		}
//	}
//	
//	def String convertToPredicateName(Rate cast){
//		if(cast.stub.isBroadcast)
//		'''get'''+cast.convertToJavaName+'''_BroadcastPredicateRate'''
//		else
//		'''get'''+cast.convertToJavaName+'''_UnicastPredicateRate'''
//	}
//	
//	def String convertToPredicateName(Probability cast){
//		if(cast.stub.isBroadcast)
//		'''get'''+cast.convertToJavaName+'''_BroadcastPredicateProb'''
//		else
//		'''get'''+cast.convertToJavaName+'''_UnicastPredicateProb'''
//	}
//	
//	def String convertToPredicateName(EnvironmentUpdate cast){
//		if(cast.stub.isBroadcast)
//		'''get'''+cast.convertToJavaName+'''_BroadcastPredicateUpdate'''
//		else
//		'''get'''+cast.convertToJavaName+'''_UnicastPredicateUpdate'''
//	}
//	
//	def String convertToJavaName(Probability eu){
//		return "_" + eu.guard.disarmExpression + "_" + eu.stub.disarm
//	}
//	
//	def String convertToJavaName(Rate eu){
//		return "_" + eu.guard.disarmExpression + "_" + eu.stub.disarm
//	}
//	
//	def String convertToJavaName(EnvironmentUpdate eu){
//		return "_" + eu.guard.disarmExpression + "_" + eu.stub.disarm
//	}
//	
//	def String getLabel(EnvironmentGuard eg){
//		eg.booleanExpression.disarmExpression
//	}
//	
//	def String disarmExpression(EnvironmentGuard eg){
//		eg.booleanExpression.disarmExpression
//	}
//	
//	def String getNameValue(ComponentAfterThought cat){
//		cat.name.label + " = " + cat.expression.label
//	}
//	def String convertToJava(ForVariableDeclaration fvd){
//		return "int " + fvd.name.label + " = " + fvd.assign
//	}
//	
//	def String convertToJava(VariableDeclaration vd){
//		switch(vd){
//			VariableDeclarationEnum: 		(vd.assign as EnumAssignment).convertToJava("int " + vd.name.label)
//			VariableDeclarationRecord:		(vd.assign as RecordDeclarations).convertToJava("int " + vd.name.label)
//			VariableDeclarationCarmaDouble:	(vd.assign as DoubleAssignment).convertToJava("double " + vd.name.label)
//			VariableDeclarationCarmaIntger:	(vd.assign as IntegerAssignment).convertToJava("int " + vd.name.label)
//		}
//	}
//	
//	def String convertToJavaType(VariableDeclaration vd){
//		switch(vd){
//			VariableDeclarationEnum: 		(vd.assign as EnumAssignment).convertToJava("int " + vd.name.label)
//			VariableDeclarationRecord:		(vd.assign as RecordDeclarations).convertToJava("int " + vd.name.label)
//			VariableDeclarationCarmaDouble:	(vd.assign as DoubleAssignment).convertToJava("double " + vd.name.label)
//			VariableDeclarationCarmaIntger:	(vd.assign as IntegerAssignment).convertToJava("int " + vd.name.label)
//		}
//	}
//	
//	def String getLabel(ComponentArgument ca){
//		switch(ca){
//			ComponentBlockDefinitionArgumentVariable:	(ca.value as VariableType).getLabel
//			ComponentBlockDefinitionArgumentMacro:		(ca.value as MacroType).getLabel
//		}
//	}
//	
//	def String getLabel(OutputActionArgument oa){
//		switch(oa){
//			OutputActionArgumentVR:	(oa.ref as VariableReference).getLabel
//			OutputActionArgumentV:	(oa.value as PrimitiveType).getLabel
//		}
//	}
//	
//	def String getLabel(MacroType mt){
//		mt.assignee.getLabel
//	}
//	
//	def String getLabel(NCA ca){
//		switch(ca){
//			NewComponentArgumentPrimitive 		: (ca.value as PrimitiveType).getLabel
//			NewComponentArgumentMacro 			: (ca.value as MacroExpressions).getLabel
//			NewComponentArgumentMethod			: (ca.value as MethodExpressions).getLabel
//			NewComponentArgumentDeclare			: (ca.value as Records).getLabel
//			NewComponentArgumentReference		: (ca.value as VariableReference).getLabel
//			NewComponentArgumentSpawnPrimitive 	: (ca.value as PrimitiveType).getLabel
//			NewComponentArgumentSpawnDeclare	: (ca.value as MacroExpressions).getLabel
//			NewComponentArgumentSpawnMacro		: (ca.value as MethodExpressions).getLabel
//			NewComponentArgumentSpawnMethod		: (ca.value as Records).getLabel
//			NewComponentArgumentSpawnReference	: (ca.value as VariableReference).getLabel
//		}
//	}
//	
//	def String getLabelForArgs(NCA ca){
//		switch(ca){
//			NewComponentArgumentPrimitive 		: (ca.value as PrimitiveType).getLabel
//			NewComponentArgumentMacro 			: (ca.value as MacroExpressions).getLabel
//			NewComponentArgumentMethod			: (ca.value as MethodExpressions).getLabel
//			NewComponentArgumentDeclare			: (ca.value as Records).getLabelForArgs
//			NewComponentArgumentReference		: (ca.value as VariableReference).getLabel
//			
//			NewComponentArgumentSpawnPrimitive 	: (ca.value as PrimitiveType).getLabel
//			NewComponentArgumentSpawnMacro		: (ca.value as MacroExpressions).getLabel
//			NewComponentArgumentSpawnMethod		: (ca.value as MethodExpressions).getLabel
//			NewComponentArgumentSpawnDeclare	: (ca.value as Records).getLabelForArgs
//			NewComponentArgumentSpawnReference	: (ca.value as VariableReference).getLabel
//		}
//	}
//	
//	def String getLabel(MacroExpressions e){
//		switch(e){
//			MacroExpressionParallel		: {e.left.getLabel + "_" + e.right.getLabel}
//			MacroExpressionReference 	: e.name.getLabel
//		}
//	}
//	
//	
//	def String getLabel(ComponentBlockNewDeclaration dc){
//		dc.name.label
//	}
//	
//	def String getLabel(ComponentBlockNewDeclarationSpawn dc){
//		dc.name.label
//	}
//	
//	def String getLabel(UpdateExpressions e){
//		switch(e){
//			UpdateSubtraction:							{e.left.label + " - " + e.right.label }
//			UpdateAddition:								{e.left.label + " + " + e.right.label }
//			UpdateMultiplication:						{e.left.label + " * " + e.right.label }
//			UpdateAtomicPrimitive:						(e.value as PrimitiveType).label
//			UpdateAtomicVariable:						e.value.label
//			UpdateAtomicMethodReference:				(e.value as MethodExpressions).label 
//			UpdateExpression:							e.expression.label
//		}
//	}
//	
//	def String convertToJava(UpdateExpressions e){
//		switch(e){
//			UpdateSubtraction:							{e.left.convertToJava + " - " + e.right.convertToJava }
//			UpdateAddition:								{e.left.convertToJava + " + " + e.right.convertToJava }
//			UpdateMultiplication:						{e.left.convertToJava + " * " + e.right.convertToJava }
//			UpdateAtomicPrimitive:						(e.value as PrimitiveType).label
//			UpdateAtomicVariable:						e.value.convertToJava
//			UpdateAtomicMethodReference:				(e.value as MethodExpressions).label
//			UpdateExpression:							e.expression.convertToJava
//		}
//	}
//	
//	def String getLabel(EnvironmentExpressions e){
//		switch(e){
//			EnvironmentSubtraction:							{e.left.label + " - " + e.right.label}
//			EnvironmentAddition:							{e.left.label + " + " + e.right.label}
//			EnvironmentMultiplication:						{e.left.label + " * " + e.right.label}
//			EnvironmentModulo:								{e.left.label + " % " + e.right.label}
//			EnvironmentDivision:							{e.left.label + " / " + e.right.label}
//			EnvironmentAtomicPrimitive:						(e.value as PrimitiveType).label
//			EnvironmentAtomicVariable:						e.value.label
//			EnvironmentAtomicMethodReference:				(e.value as MethodExpressions).label 
//			EnvironmentAtomicNow:							"now()"
//			EnvironmentAtomicMeasure:						e.value.label
//			EnvironmentExpression:							e.expression.label
//			SetComp:										"Measure"+e.hashCode+"_"+e.componentReference.getLabel.toFirstUpper
//		}
//	}
//	
//	def String convertToJava(EnvironmentExpressions e){
//		switch(e){
//			EnvironmentSubtraction:							{e.left.convertToJava + " - " + e.right.convertToJava }
//			EnvironmentAddition:							{e.left.convertToJava + " + " + e.right.convertToJava }
//			EnvironmentMultiplication:						{e.left.convertToJava + " * " + e.right.convertToJava }
//			EnvironmentModulo:								{e.left.convertToJava + " % " + e.right.convertToJava }
//			EnvironmentDivision:							{e.left.convertToJava + " / " + e.right.convertToJava }
//			EnvironmentAtomicPrimitive:						(e.value as PrimitiveType).label
//			EnvironmentAtomicVariable:						e.value.label
//			EnvironmentAtomicMethodReference:				(e.value as MethodExpressions).label 
//			EnvironmentAtomicNow:							"now()"
//			EnvironmentAtomicMeasure:						e.value.convertToJava
//			EnvironmentExpression:							e.expression.convertToJava
//			SetComp:										"1.0; //getMeasure"+e.hashCode+"_"+e.componentReference.getLabel.toFirstUpper+"().measure(t)"
//		}
//	}
//	
//	def String getLabel(EnvironmentUpdateExpressions e){
//		switch(e){
//			EnvironmentUpdateSubtraction:							{e.left.label + " - " + e.right.label }
//			EnvironmentUpdateAddition:								{e.left.label + " + " + e.right.label }
//			EnvironmentUpdateMultiplication:						{e.left.label + " * " + e.right.label }
//			EnvironmentUpdateAtomicPrimitive:						(e.value as PrimitiveType).label
//			EnvironmentUpdateAtomicVariable:						e.value.convertToJava
//			EnvironmentUpdateAtomicMethodReference:					(e.value as MethodExpressions).label 
//			EnvironmentUpdateExpression:							e.expression.label
//			EnvironmentUpdateAtomicNow:								"now()"
//			EnvironmentUpdateAtomicMeasure:							"getMeasure"+e.hashCode+"()"
//		}
//	}
//	
//	def String getLabel(Model model){
//		model.eResource.URI.lastSegment.split("\\.").get(0)
//	}
//	
//	def String getLabel(System system){
//		switch(system){
//			BlockSystem:	system.name.label
//			LineSystem:		system.name.label
//		}
//	}
//	
//	def String getLabel(Methods m){
//		"Functions"
//	}
//	
//	def String getLabel(MethodDefinition md){
//		"fun" + " " + md.type.label + " " md.name.label + " (" + md.functionArguments.label + " )"
//	}
//	
//	def String getLabel(Types typeLabel){
//		switch(typeLabel){
//			DoubleTypeLabel: 	"double" 	
//			IntegerTypeLabel: 	"integer"	
//			RecordTypeLabel:	"record"	
//			AttribTypeLabel:	"attrib"
//		}
//	}
//	
//	def HashMap<String,String>  getNameValueLabel(VariableName name){
//		var vd = name.getVariableDeclaration
//		var output = new HashMap<String,String>()
//		if(name.isEnum){
//			output.put(name.label,vd.label)
//		}
//		if(name.isRecord){
//			for(rd : vd.eAllOfType(RecordDeclaration))
//				output.put(name.label+"_"+rd.name.label,rd.assign.label)
//		}
//		return output
//	}
//	
//	def String getLabel(ActionStub actionStub){
//		
//		var output = actionStub.name.name
//		
//		if(actionStub.cast != null){
//			output = output + "*"
//		}
//		
//		return output
//		
//	}
//	
//	
//	def String disarm(ActionStub actionStub){
//		
//		var output = actionStub.name.name
//		
//		if(actionStub.cast != null){
//			output = output + "_BROADCAST_"
//		}
//			
//		return output
//		
//	}
//	
//	def String convertToJavaName(EnvironmentOperation eo){
//		eo.guard.disarmExpression + "_" + eo.stub.getLabelName
//	}
//	
//	def String convertToJavaNameDefinitions(ActionStub actionStub){
//		actionStub.getContainerOfType(EnvironmentOperation).convertToJavaName
//	}
//	
//	def String getLabelName(ActionStub actionStub){
//		
//		var output = actionStub.name.name
//		return output
//		
//	}
//	
//	def String getLabelInOut(ActionStub actionStub){
//		if(actionStub.label.contains("()"))
//			"INTPUT"
//		else
//			"OUTPUT"
//	}
//	
//	def String getLabelFull(ActionName name){
//		
//		var action = name.getContainerOfType(Action)
//		return action.getLabel
//	}
//	
//	def String getLabel(MethodDefinitionArguments mdas){
//		var String output = ""
//		for(mda : mdas.inputArguments)
//			output = output + " " + mda.label
//		
//		return output
//	}
//	
//	def String getLabel(MethodDefinitionArgument mda){
//		mda.argument.label
//	}
//	
//	def String getLabel(VariableType vt){
//		switch(vt){
//			VariableTypeEnum:			"enum" 		+ ":"  	+	vt.name.label 
//			VariableTypeRecord:			"record"	+ ":"  	+	vt.name.label
//			VariableTypeCarmaDouble:	"double" 	+ ":"  	+	vt.name.label
//			VariableTypeCarmaIntger:	"integer" 	+ ":"  	+	vt.name.label
//		}
//	}
//	
//	def String getLabel(ComponentStyle cs){
//		"Model"
//	}
//	
//	def String getLabel(ComponentBlockDefinition cbd){
//		"Component" + " " + cbd.name.label
//	}
//	
//	def String getLabel(MeasureBlock m){
//		"Measures"
//	}
//	
//	def String getLabel(BlockSystem bs){
//		"System"
//	}
//	
//	def getLabel(VariableDeclaration vd){
//		switch(vd){
//			VariableDeclarationEnum: 		(vd.assign as EnumAssignment).label
//			VariableDeclarationRecord:		(vd.assign as RecordDeclarations).label
//			VariableDeclarationCarmaDouble:	(vd.assign as DoubleAssignment).label
//			VariableDeclarationCarmaIntger:	(vd.assign as IntegerAssignment).label
//		}
//	}
//	
//	def getLabel(EnumAssignment ea){
//		switch(ea){
//			EnumAssignmentCarmaInteger: 	(ea.naturalValue as CarmaInteger).label
//			EnumAssignmentMethodReference: 	(ea.method as MethodExpressions).label
//			EnumAssignmentRange:			(ea.range as Range).label
//			EnumAssignmentVariableName: 	(ea.ref as VariableReference).label
//		}
//	}
//	
//	def String convertToJava(EnumAssignment ea, String assignment){
//		switch(ea){
//			EnumAssignmentCarmaInteger: 	(ea.naturalValue as CarmaInteger).convertToJava(assignment)
//			EnumAssignmentMethodReference: 	(ea.method as MethodExpressions).convertToJava(assignment)
//			EnumAssignmentRange:			(ea.range as Range).convertToJava(assignment)
//			EnumAssignmentVariableName: 	(ea.ref as VariableReference).convertToJava(assignment)
//		}
//	}
//	
//	def convertToJavaName(EnumAssignment ea){
//		switch(ea){
//			EnumAssignmentCarmaInteger: 	(ea.naturalValue as CarmaInteger).convertToJavaName
//			EnumAssignmentMethodReference: 	(ea.method as MethodExpressions).convertToJavaName
//			EnumAssignmentRange:			(ea.range as Range).convertToJavaName
//			EnumAssignmentVariableName: 	(ea.ref as VariableReference).convertToJavaName
//		}
//	}
//	
//	def getLabel(RecordDeclarations rds){
//		if(rds.ref != null)
//			rds.ref.label
//		else
//			(rds as Records).label 
//	}
//	
//	def convertToJava(RecordDeclarations rds, String assignment){
//		if(rds.ref != null)
//			assignment + " = " + rds.ref.label
//		else
//			(rds as Records).convertToJava(assignment) 
//	}
//	
//	def getLabel(DoubleAssignment da){
//		switch(da){
//		DoubleAssignmentCarmaDouble: 		da.doubleValue.label	
//		DoubleAssignmentMethodReference: 	da.method.label	
//		DoubleAssignmentVariableName: 		da.reference.label
//		}
//	}
//	
//	def convertToJava(DoubleAssignment da, String assignment){
//		switch(da){
//		DoubleAssignmentCarmaDouble: 		assignment + " = " + da.doubleValue.label	
//		DoubleAssignmentMethodReference: 	assignment + " = " + da.method.label	
//		DoubleAssignmentVariableName: 		assignment + " = " + da.reference.label
//		}
//	}
//	
//	def getLabel(IntegerAssignment ia){
//		switch(ia){
//		IntegerAssignmentCarmaInteger: 		ia.integerValue.label
//		IntegerAssignmentMethodReference: 	ia.method.label	
//		IntegerAssignmentVariableName: 		ia.reference.label
//		}
//	}
//	
//	def convertToJava(IntegerAssignment ia, String assignment){
//		switch(ia){
//		IntegerAssignmentCarmaInteger: 		assignment + " = " + ia.integerValue.label
//		IntegerAssignmentMethodReference: 	assignment + " = " + ia.method.label	
//		IntegerAssignmentVariableName: 		assignment + " = " + ia.reference.label
//		}
//	}
//
//	
//	def String convertToJava(PrimitiveType e, String assignment){
//		switch(e){
//			CarmaDouble:	{
//				var String output = assignment+"= " 
//				output = e.left.toString + "." + e.right.toString
//				if(e.exponent != null)
//					output = output + e.exponent.label
//				return output
//			}
//			CarmaInteger:	assignment+" = "+e.value
//			CarmaBoolean:	assignment+" = "+e.value
//			Range:			assignment+"Min = "+ e.min + ", "+assignment+"Max = " + e.max
//		}
//	}
//	
//	def String convertToJavaName(PrimitiveType e){
//		switch(e){
//			CarmaDouble:	{
//				var String output = ""
//				output = e.left.toString + "_POINT_" + e.right.toString
//				if(e.exponent != null)
//					output = output + e.exponent.convertToJavaName
//				return output
//			}
//			CarmaInteger:	""+e.value
//			CarmaBoolean:	e.value
//			Range:			e.min + "_PPP_" + e.max
//		}
//	}
//	
//	def String getLabel(CarmaExponent ce){
//		var String output = "^"
//		if(ce.negative != null)
//			output = output + ce.negative
//		output = output + ce.exponent
//	}
//	
//	def String convertToJavaName(CarmaExponent ce){
//		var String output = "_HAT_"
//		if(ce.negative != null)
//			output = output + "_NEG_"
//		output = output + "_EXP_"
//	}
//	
//	def String getLabel(Records e){
//		var String output = "{"
//		
//		for(rd : e.recordDeclarations)
//			output = output + " " + rd.label + " "
//		
//		output = output + "}"
//		return output
//	}
//	
//	def String convertToJava(Records e, String assignment){
//		var String output = ""
//		
//		if(e.recordDeclarations.size > 0){
//			output = output + e.recordDeclarations.get(0).convertToJava(assignment)
//			for(var i = 1; i < e.recordDeclarations.size; i++)
//				output = output + " , " + e.recordDeclarations.get(i).convertToJava(assignment)
//		}
//		
//		output = output + "}"
//		return output
//	}
//	
//	def String getLabelForArgs(Records e){
//		var String output = ""
//		
//		if(e.recordDeclarations.size >0){
//			output = output + e.recordDeclarations.get(0).labelForArgs
//			for(var i = 1; i < e.recordDeclarations.size; i++){
//				output = output + "," + e.recordDeclarations.get(i).labelForArgs
//			}
//				
//		}
//		
//			
//		
//		return output
//		
//	}
//	
//	def String convertToJavaName(Records e){
//		var String output = "_RR_"
//		
//		for(rd : e.recordDeclarations)
//			output = output + " " + rd.convertToJavaName + " "
//		
//		output = output + "_RR_"
//		return output
//	}
//	
//	def String convertToJavaName(RecordDeclaration e){
//		e.name.label + "_ASS_" + e.assign.convertToJavaName
//	}
//	
//	def String getLabel(RecordDeclaration e){
//		e.name.label + " := " + e.assign.label
//	}
//	
//	def String convertToJava(RecordDeclaration e, String assign){
//		assign + "_" + e.name.label + " = " + e.assign.label
//	}
//	
//	def String getLabelForArgs(RecordDeclaration e){
//		e.assign.label
//	}
//	
//	def String convertToJava(VariableReference vr, String assignment){
//		switch(vr){
//			VariableReferencePure: 			{assignment + " = " + vr.name.label}
//			VariableReferenceMy: 			{assignment + " = " + vr.name.label}
//			VariableReferenceThis: 			{assignment + " = " + vr.name.label}
//			VariableReferenceReceiver:		{assignment + " = " + vr.name.label+"_r"}
//			VariableReferenceSender:		{assignment + " = " + vr.name.label+"_s"}
//			RecordReferencePure:			{assignment + " = " + vr.name.label+"_"+vr.record.label}
//			RecordReferenceMy:				{assignment + " = " + vr.name.label+"_"+vr.record.label}
//			RecordReferenceThis:			{assignment + " = " + vr.name.label+"_"+vr.record.label}
//			RecordReferenceReceiver:		{assignment + " = " + vr.name.label+"_"+vr.record.label+"_r"}
//			RecordReferenceSender:			{assignment + " = " + vr.name.label+"_"+vr.record.label+"_s"}
//			VariableReferenceGlobal:		{assignment + " = " + vr.name.label}
//			RecordReferenceGlobal:			{assignment + " = " + vr.name.label+"_"+vr.record.label}
//		}
//	}
//	
//	def String convertToJava(VariableReference vr){
//		switch(vr){
//			VariableReferencePure: 			{vr.name.label}
//			VariableReferenceMy: 			{vr.name.label}
//			VariableReferenceThis: 			{vr.name.label}
//			VariableReferenceReceiver:		{vr.name.label+"_r"}
//			VariableReferenceSender:		{vr.name.label+"_s"}
//			RecordReferencePure:			{vr.name.label+"_"+vr.record.label}
//			RecordReferenceMy:				{vr.name.label+"_"+vr.record.label}
//			RecordReferenceThis:			{vr.name.label+"_"+vr.record.label}
//			RecordReferenceReceiver:		{vr.name.label+"_"+vr.record.label+"_r"}
//			RecordReferenceSender:			{vr.name.label+"_"+vr.record.label+"_s"}
//			VariableReferenceGlobal:		{vr.name.label}
//			RecordReferenceGlobal:			{vr.name.label+"_"+vr.record.label}
//		}
//	}
//	
//	def String convertToJavaInputAction(VariableReference vr){
//		switch(vr){
//			VariableReferencePure: 			{vr.name.label+"_i"}
//			VariableReferenceMy: 			{vr.name.label+"_i"}
//			VariableReferenceThis: 			{vr.name.label+"_i"}
//			VariableReferenceReceiver:		{vr.name.label}
//			VariableReferenceSender:		{vr.name.label}
//			RecordReferencePure:			{vr.name.label+"_"+vr.record.label+"_i"}
//			RecordReferenceMy:				{vr.name.label+"_"+vr.record.label+"_i"}
//			RecordReferenceThis:			{vr.name.label+"_"+vr.record.label+"_i"}
//			RecordReferenceReceiver:		{vr.name.label+"_"+vr.record.label}
//			RecordReferenceSender:			{vr.name.label+"_"+vr.record.label}
//			VariableReferenceGlobal:		{vr.name.label}
//			RecordReferenceGlobal:			{vr.name.label+"_"+vr.record.label}
//		}
//	}
//	
//	def String convertToJavaOutputAction(VariableReference vr){
//		switch(vr){
//			VariableReferencePure: 			{vr.name.label+"_i"}
//			VariableReferenceMy: 			{vr.name.label+"_o"}
//			VariableReferenceThis: 			{vr.name.label+"_o"}
//			VariableReferenceReceiver:		{vr.name.label}
//			VariableReferenceSender:		{vr.name.label}
//			RecordReferencePure:			{vr.name.label+"_"+vr.record.label+"_i"}
//			RecordReferenceMy:				{vr.name.label+"_"+vr.record.label+"_o"}
//			RecordReferenceThis:			{vr.name.label+"_"+vr.record.label+"_o"}
//			RecordReferenceReceiver:		{vr.name.label+"_"+vr.record.label}
//			RecordReferenceSender:			{vr.name.label+"_"+vr.record.label}
//			VariableReferenceGlobal:		{vr.name.label}
//			RecordReferenceGlobal:			{vr.name.label+"_"+vr.record.label}
//		}
//	}
//	
//	def String convertToJavaName(VariableReference vr){
//		switch(vr){
//			VariableReferencePure: 			vr.name.label
//			VariableReferenceMy: 			{"my_" 			+ vr.name.label}
//			VariableReferenceThis: 			{"this_" 		+ vr.name.label}
//			VariableReferenceReceiver:		{"receiver_" 	+ vr.name.label}
//			VariableReferenceSender:		{"sender_"		+ vr.name.label}
//			RecordReferencePure:			{vr.name.label 		+ "." + vr.record.label	}
//			RecordReferenceMy:				{"my" 				+ "_" + vr.name.label 	+ "_" + vr.record.label	}
//			RecordReferenceThis:			{"this" 			+ "_." + vr.name.label 	+ "_" + vr.record.label	}
//			RecordReferenceReceiver:		{"receiver" 		+ "_" + vr.name.label 	+ "_" + vr.record.label	}
//			RecordReferenceSender:			{"sender" 			+ "_" + vr.name.label 	+ "_" + vr.record.label	}
//			VariableReferenceGlobal:		{"global_"			+ vr.name.label}
//			RecordReferenceGlobal:			{"global" 			+ "_" + vr.name.label 	+ "_" + vr.record.label	}
//		}
//	}
//	
//	def String getLabelJava(VariableReference vr){
//		switch(vr){
//			VariableReferencePure: 			vr.name.label
//			VariableReferenceMy: 			vr.name.label
//			VariableReferenceThis: 			vr.name.label
//			VariableReferenceReceiver:		vr.name.label
//			VariableReferenceSender:		vr.name.label
//			RecordReferencePure:			vr.name.label + "_" + vr.record.label
//			RecordReferenceMy:				vr.name.label + "_" + vr.record.label
//			RecordReferenceThis:			vr.name.label + "_" + vr.record.label
//			RecordReferenceReceiver:		vr.name.label + "_" + vr.record.label
//			RecordReferenceSender:			vr.name.label + "_" + vr.record.label
//		}
//	}
//	
//	def String getLabel(MethodExpressions e){
//		switch(e){
//			MethodSubtraction:							{e.left.label + " - " + e.right.label }
//			MethodAddition:								{e.left.label + " + " + e.right.label }
//			MethodMultiplication:						{e.left.label + " * " + e.right.label }
//			MethodModulo:								{e.left.label + " % " + e.right.label }
//			MethodDivision:								{e.left.label + " / " + e.right.label }
//			MethodAtomicPrimitive:						(e.value as PrimitiveType).label
//			MethodAtomicVariable:						e.value.label
//			MethodAtomicMethodReference:				(e.value as MethodExpressions).label 
//			MethodReferenceMethodDeclaration: 			(e.ref as MethodDeclaration).label
//			MethodReferencePredefinedMethodDeclaration: (e.ref as PredefinedMethodDeclaration).label
//			MethodExpression:							e.expression.label
//		}
//	}
//	
//	def String convertToJava(MethodExpressions e, String assignment){
//		switch(e){
//			MethodSubtraction:							{e.left.convertToJava(assignment) + " - " + e.right.convertToJava(assignment) }
//			MethodAddition:								{e.left.convertToJava(assignment) + " + " + e.right.convertToJava(assignment) }
//			MethodMultiplication:						{e.left.convertToJava(assignment) + " * " + e.right.convertToJava(assignment) }
//			MethodModulo:								{e.left.convertToJava(assignment) + " % " + e.right.convertToJava(assignment) }
//			MethodDivision:								{e.left.convertToJava(assignment) + " / " + e.right.convertToJava(assignment) }
//			MethodAtomicPrimitive:						(e.value as PrimitiveType).convertToJava(assignment)
//			MethodAtomicVariable:						assignment + " = "+ e.value
//			MethodAtomicMethodReference:				(e.value as MethodExpressions).convertToJava(assignment)
//			MethodReferenceMethodDeclaration: 			(e.ref as MethodDeclaration).convertToJava(assignment)
//			MethodReferencePredefinedMethodDeclaration: (e.ref as PredefinedMethodDeclaration).convertToJava(assignment)
//			MethodExpression:							e.expression.convertToJava(assignment)
//		}
//	}
//	
//	def String convertToJavaName(MethodExpressions e){
//		switch(e){
//			MethodSubtraction:							{e.left.convertToJavaName + "_SUB_" + e.right.convertToJavaName }
//			MethodAddition:								{e.left.convertToJavaName + "_ADD_" + e.right.convertToJavaName }
//			MethodMultiplication:						{e.left.convertToJavaName + "_MUL_" + e.right.convertToJavaName }
//			MethodModulo:								{e.left.convertToJavaName + "_MOD_" + e.right.convertToJavaName }
//			MethodDivision:								{e.left.convertToJavaName + "_DIV_" + e.right.convertToJavaName }
//			MethodAtomicPrimitive:						(e.value as PrimitiveType).convertToJavaName
//			MethodAtomicVariable:						e.value.label
//			MethodAtomicMethodReference:				(e.value as MethodExpressions).convertToJavaName 
//			MethodReferenceMethodDeclaration: 			(e.ref as MethodDeclaration).label
//			MethodReferencePredefinedMethodDeclaration: (e.ref as PredefinedMethodDeclaration).label
//			MethodExpression:							e.expression.label
//		}
//	}
//	
//	def String getLabel(MethodDeclaration e){
//		e.name.label
//	}
//	
//	def String convertToJava(MethodDeclaration e, String assignment){
//		assignment + " = " + e.name.label
//	}
//	
//	def String getLabel(PredefinedMethodDeclaration e){
//		switch(e){	
//			PDFunction:			"PDF()"
//			UniformFunction: 	"Uniform()"
//			CeilingFunction: 	"Ceiling()"
//			FloorFunction: 		"Floor()"
//			MaxFunction:		"Max()"
//			MinFunction:		"Min()"
//		}
//	}
//	
//	def String convertToJava(PredefinedMethodDeclaration e, String assignment){
//		switch(e){	
//			PDFunction:			assignment + "= PDF()"
//			UniformFunction: 	assignment + "= Uniform()"
//			CeilingFunction: 	assignment + "= Ceiling()"
//			FloorFunction: 		assignment + "= Floor()"
//			MaxFunction:		assignment + "= Max()"
//			MinFunction:		assignment + "= Min()"
//		}
//	}
//	
//	def String getLabel(ProcessExpression pe){
//		switch(pe){
//			ProcessExpressionChoice: 	pe.left.getLabel + " + " + pe.right.getLabel
//			ProcessExpressionLeaf:		pe.expression
//			ProcessExpressionGuard:		pe.expression.getLabel + " " + pe.reference.getLabel
//			ProcessExpressionAction: 	pe.expression.getLabel + " " + pe.reference.getLabel
//			ProcessExpressionReference: pe.expression.getLabel
//		}
//	}
//	
//	/**
//	 * This labelling function is used to create state names
//	 */
//	def String getLabelAsState(ProcessExpression pe){
//		switch(pe){
//			ProcessExpressionChoice: 	pe.left.getLabelAsState + "_c_" + pe.right.getLabelAsState
//			ProcessExpressionLeaf:		pe.expression
//			ProcessExpressionGuard:		pe.expression.getLabel + "_" + pe.reference.getLabelAsState
//			ProcessExpressionAction: 	pe.expression.name.getLabel + "_" + pe.reference.getLabelAsState
//			ProcessExpressionReference: pe.expression.name
//		}
//	}
//	
//	def String getLabelAsState(Process p){
//		p.name.label
//	}
//	
//	def String getLabel(Guard g){
//		"[" + g.booleanExpression.disarmExpression + "]"
//	}
//	
//	def String getLabel(Action a){
//		
//		var multicast 		= a.eAllOfType(MultiCast).size > 0
//		var output 			= a.name.getLabel
//		
//		if(multicast){
//			output = output + "*"
//		} 
//
//		
//		return output
//		
//	}
//	
//	def String getLabelIO(Action a){
//		
//		var multicast 		= a.eAllOfType(MultiCast).size > 0
//		var inputAction  	= a.eAllOfType(InputAction).size > 0
//		var outputAction 	= a.eAllOfType(OutputAction).size > 0
//		var output 			= a.name.getLabel
//		
//		if(multicast){
//			output = output + "*"
//		} 
//		
//		if(inputAction){
//			output = output + "()"
//		}
//		
//		if(outputAction){
//			output = output + "<>"
//		}
//		
//		return output
//		
//	}
//	
//	def String getLabel(Component c){
//		if(c.getContainerOfType(ComponentBlockStyle) != null){
//			(c as ComponentBlockDefinition).name.getLabel
//		} else {
//			(c as ComponentLineDefinition).name.getLabel
//		}
//	}

}