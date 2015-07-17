package eu.quanticol.carma.core.generator.ms.collective

class CollectiveJavaniser {
	
//	def String javanise(Expressions e) {
//		switch (e) {
//			Or: 						{e.javanise}
//			And:						{e.javanise}
//			Equality: 					{e.javanise}
//			Comparison: 				{e.javanise}
//			Subtraction: 				{e.javanise}
//			Addition: 					{e.javanise}
//			Multiplication: 			{e.javanise}
//			Modulo: 					{e.javanise}
//			Division: 					{e.javanise}
//			Not: 						{e.javanise}
//			AtomicPrimitive: 			{e.javanise}
//			AtomicVariable: 			{e.javanise}
//			AtomicCalls: 				{e.javanise}
//			AtomicNow: 					{"//eu.quanticol.carma.core.generator.ms.collective.javanise.Now"}
//			AtomicMeasure: 				{"//eu.quanticol.carma.core.generator.ms.collective.javanise.AtomicOutcome"}
//			AtomicRecord: 				{e.javanise}
//			AtomicOutcome: 				{"//eu.quanticol.carma.core.generator.ms.collective.javanise.AtomicOutcome"}
//			AtomicProcessComposition:	{e.javanise}
//		}
//	}
//	
//	def String javanise(BaseType bt){
//		if(bt.me.equals("int")){
//			'''Integer.class'''
//		} else {
//			'''«bt.me».class'''
//		}
//	}
//	
//	def String javanise(OutputActionArgument oaa){
//		switch (oaa.value) {
//			VariableReferenceMy: 		(oaa.value as VariableReference).javanise
//			RecordReferenceMy: 			(oaa.value as VariableReference).javanise
//			CarmaInteger:				(oaa.value as CarmaInteger).javanise
//		}
//	}
//
//	def String javanise(Or e) {
//		'''(«e.left.javanise» || «e.right.javanise»)'''
//	}
//
//	def String javanise(And e) {
//		'''(«e.left.javanise» && «e.right.javanise»)'''
//	}
//
//	def String javanise(Equality e) {
//		'''(«e.left.javanise» «e.op» «e.right.javanise»)'''
//	}
//
//	def String javanise(Comparison e) {
//		'''(«e.left.javanise» «e.op» «e.right.javanise»)'''
//	}	
//
//	def String javanise(Subtraction e) {
//		'''(«e.left.javanise» - «e.right.javanise»)'''
//	}
//
//	def String javanise(Addition e) {
//		'''(«e.left.javanise» + «e.right.javanise»)'''
//	}
//
//	def String javanise(Multiplication e) {
//		'''(«e.left.javanise» * «e.right.javanise»)'''
//	}
//
//	def String javanise(Modulo e) {
//		'''(«e.left.javanise» % «e.right.javanise»)'''
//	}
//
//	def String javanise(Division e) {
//		'''(«e.left.javanise» / «e.right.javanise»)'''
//	}
//	
//	def String javanise(Not e) {
//		'''!(«e.expression.javanise»)'''
//	}
//	
//	def String javanise(AtomicPrimitive e) {
//		e.value.javanise
//	}
//	
//	def String javanise(PrimitiveTypes pts) {
//		switch (pts) {
//			CarmaDouble: pts.javanise
//			CarmaInteger: pts.javanise
//			CarmaBoolean: pts.javanise
//			Range: pts.javanise
//		}
//	}
//	
//	def String javanise(AtomicVariable expression) {
//		expression.value.javanise
//	}
//	
//	def String javanise(VariableReference vr) {
//		switch (vr) {
//			VariableReferencePure: 		vr.name.name
//			VariableReferenceMy: 		vr.name.name
//			VariableReferenceReceiver: 	vr.name.name
//			VariableReferenceSender: 	vr.name.name
//			VariableReferenceGlobal: 	vr.name.name
//			RecordReferencePure: 		vr.name.name + "." + vr.feild.name
//			RecordReferenceMy: 			vr.name.name + "." + vr.feild.name
//			RecordReferenceReceiver: 	vr.name.name + "." + vr.feild.name
//			RecordReferenceSender: 		vr.name.name + "." + vr.feild.name
//			RecordReferenceGlobal: 		vr.name.name + "." + vr.feild.name
//		}
//	}
//	
//	def String javanise(AtomicCalls expression) {
//		expression.value.javanise
//	}
//
//	def String javanise(Calls calls) {
//		switch (calls) {
//			FunctionReferenceMan: calls.ref.javanise
//			FunctionReferencePre: calls.ref.javanise
//		}
//	}
//
//	def String javanise(FunctionCall functionCall) {
//		'''
//			«(functionCall.name as Name).name»(«(functionCall.arguments as FunctionCallArguments).javanise»);
//		'''
//	}
//	
//	def String javanise(FunctionCallArguments arguments){
//			var ArrayList<FunctionArgument> args = new ArrayList<FunctionArgument>(arguments.eAllOfType(FunctionArgument))
//			var toReturn = ""
//			if(args.size > 0){
//				toReturn = toReturn + args.get(0).javanise
//				for(var i = 1; i < args.size; i++){
//					toReturn = toReturn + ", " + args.get(i).javanise
//				}
//			}
//			return toReturn
//	}
//	
//	def String javanise(FunctionArgument argument){
//		argument.value.javanise
//	}
//	
//	def String javanise(PreFunctionCall preFunctionCall) {
//		switch (preFunctionCall) {
//			PDFunction:			{'''pdf(«(preFunctionCall.arguments as PredFunctionCallArguments).javanise»)'''}
//			UniformFunction:	{'''uniform(«(preFunctionCall.arguments as PredFunctionCallArguments).javanise»)'''}
//			CeilingFunction:	{'''ceil(«(preFunctionCall.arguments as PredFunctionCallArguments).javanise»)'''}
//			FloorFunction:		{'''floor(«(preFunctionCall.arguments as PredFunctionCallArguments).javanise»)'''}
//			MaxFunction:		{'''max(«(preFunctionCall.arguments as PredFunctionCallArguments).javanise»)'''}
//			MinFunction:		{'''min(«(preFunctionCall.arguments as PredFunctionCallArguments).javanise»)'''}		
//		}
//	}
//	
//	def String javanise(PredFunctionCallArguments arguments){
//			var ArrayList<PreArgument> args = new ArrayList<PreArgument>(arguments.eAllOfType(PreArgument))
//			var toReturn = '''new ArrayList<Object>(Arrays.asList('''
//			if(args.size > 0){
//				toReturn = toReturn + '''«args.get(0).javanise»'''
//				for(var i = 1; i < args.size; i++){
//					toReturn = toReturn + ''', «args.get(i).javanise»'''
//				}
//			}
//			return toReturn + "))"
//	}
//	
//	def String javanise(PreArgument argument){
//		argument.value.javanise
//	}
//	
//	def String javanise(AtomicRecord expression){
//		var instance = (expression.value as InstantiateRecord)
//		'''new «(instance.type as Type).javanise» ( «(instance.arguments as RecordArguments).javanise» )'''
//	}
//	
//	def String javanise(Types types){
//		types.type.javanise
//	}
//	
//	def String javanise(Type type){
//		switch(type){
//			DoubleType: "double"
//			IntgerType: "int"
//			AttribType: "int"
//			RecordType: type.name.name
//		}
//	}
//	
//	def String javanise(RecordArguments arguments){
//			var ArrayList<RecordArgument> args = new ArrayList<RecordArgument>(arguments.eAllOfType(RecordArgument))
//			var toReturn = ""
//			if(args.size > 0){
//				toReturn = toReturn + args.get(0).javanise
//				for(var i = 1; i < args.size; i++){
//					toReturn = toReturn + ", " + args.get(i).javanise
//				}
//			}
//			return toReturn
//	}
//	
//	def String javanise(RecordArgument argument){
//		argument.value.javanise
//	}
//	
//	def String javanise(ComponentForVariableDeclaration componentForVariableDeclaration){
//		'''«(componentForVariableDeclaration.type as Type).javanise» «componentForVariableDeclaration.name.name» = «componentForVariableDeclaration.assign.javanise»'''
//	}
//	
//	def String javanise(BooleanExpression expression) {
//		expression.expression.javanise
//	}
//	
//	def String javanise(ComponentAssignment componentAssignment){
//		'''«componentAssignment.reference.javanise» = «componentAssignment.expression.javanise»'''
//	}
//	
//	def String javanise(ComponentExpression componentExpression){
//		componentExpression.expression.javanise
//	}
//	
//	def String getParameters(ArrayList<Parameter> parameters){
//		var String toReturn = ""
//		if(parameters.size > 0){
//			toReturn = parameters.get(0).getParameter
//			for(var i = 1; i < parameters.size; i++){
//				toReturn = toReturn + ", " + parameters.get(i).getParameter
//			}
//		}
//		return toReturn
//	}
//	
//	def String getParameter(Parameter parameter){
//		switch(parameter){
//			AttribParameter: '''«(parameter.type as AttribType).javanise» «parameter.name.name»'''
//			RecordParameter: '''«(parameter.type as RecordType).javanise» «parameter.name.name»'''
//			ProcessParameter: '''ArrayList<String> behaviour'''
//		}
//	}
//	
//	def String javanise(ArrayList<String> behaviours){
//		var String toReturn = ""
//		if(behaviours.size > 0){
//			toReturn = '''"«behaviours.get(0)»"'''
//			for(var i = 1; i < behaviours.size; i++)
//				toReturn = '''«toReturn», "«behaviours.get(i)»"'''
//		}
//		return toReturn
//	}

}