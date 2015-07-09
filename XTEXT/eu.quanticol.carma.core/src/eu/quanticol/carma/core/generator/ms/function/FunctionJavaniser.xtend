package eu.quanticol.carma.core.generator.ms.function

import eu.quanticol.carma.core.carma.Addition
import eu.quanticol.carma.core.carma.And
import eu.quanticol.carma.core.carma.AtomicMeasure
import eu.quanticol.carma.core.carma.AtomicNow
import eu.quanticol.carma.core.carma.AtomicOutcome
import eu.quanticol.carma.core.carma.AtomicPrimitive
import eu.quanticol.carma.core.carma.AtomicRecord
import eu.quanticol.carma.core.carma.AtomicVariable
import eu.quanticol.carma.core.carma.AttribParameter
import eu.quanticol.carma.core.carma.AttribType
import eu.quanticol.carma.core.carma.AttribVariableDeclaration
import eu.quanticol.carma.core.carma.BooleanExpression
import eu.quanticol.carma.core.carma.Calls
import eu.quanticol.carma.core.carma.CarmaBoolean
import eu.quanticol.carma.core.carma.CarmaDouble
import eu.quanticol.carma.core.carma.CarmaExponent
import eu.quanticol.carma.core.carma.CarmaInteger
import eu.quanticol.carma.core.carma.CeilingFunction
import eu.quanticol.carma.core.carma.Comparison
import eu.quanticol.carma.core.carma.Division
import eu.quanticol.carma.core.carma.DoubleParameter
import eu.quanticol.carma.core.carma.DoubleType
import eu.quanticol.carma.core.carma.DoubleVariableDeclaration
import eu.quanticol.carma.core.carma.Equality
import eu.quanticol.carma.core.carma.Expressions
import eu.quanticol.carma.core.carma.FloorFunction
import eu.quanticol.carma.core.carma.FunctionArgument
import eu.quanticol.carma.core.carma.FunctionAssignment
import eu.quanticol.carma.core.carma.FunctionCall
import eu.quanticol.carma.core.carma.FunctionCallArguments
import eu.quanticol.carma.core.carma.FunctionDeclaration
import eu.quanticol.carma.core.carma.FunctionExpression
import eu.quanticol.carma.core.carma.FunctionForStatement
import eu.quanticol.carma.core.carma.FunctionIfStatement
import eu.quanticol.carma.core.carma.FunctionReferenceMan
import eu.quanticol.carma.core.carma.FunctionReferencePre
import eu.quanticol.carma.core.carma.FunctionStatement
import eu.quanticol.carma.core.carma.InstantiateRecord
import eu.quanticol.carma.core.carma.IntgerParameter
import eu.quanticol.carma.core.carma.IntgerType
import eu.quanticol.carma.core.carma.IntgerVariableDeclaration
import eu.quanticol.carma.core.carma.MaxFunction
import eu.quanticol.carma.core.carma.MinFunction
import eu.quanticol.carma.core.carma.Modulo
import eu.quanticol.carma.core.carma.Multiplication
import eu.quanticol.carma.core.carma.Name
import eu.quanticol.carma.core.carma.Not
import eu.quanticol.carma.core.carma.Or
import eu.quanticol.carma.core.carma.PDFunction
import eu.quanticol.carma.core.carma.Parameter
import eu.quanticol.carma.core.carma.PreArgument
import eu.quanticol.carma.core.carma.PreFunctionCall
import eu.quanticol.carma.core.carma.PredFunctionCallArguments
import eu.quanticol.carma.core.carma.PrimitiveTypes
import eu.quanticol.carma.core.carma.Range
import eu.quanticol.carma.core.carma.RecordArgument
import eu.quanticol.carma.core.carma.RecordArguments
import eu.quanticol.carma.core.carma.RecordDeclaration
import eu.quanticol.carma.core.carma.RecordParameter
import eu.quanticol.carma.core.carma.RecordReferenceGlobal
import eu.quanticol.carma.core.carma.RecordReferenceMy
import eu.quanticol.carma.core.carma.RecordReferencePure
import eu.quanticol.carma.core.carma.RecordReferenceReceiver
import eu.quanticol.carma.core.carma.RecordReferenceSender
import eu.quanticol.carma.core.carma.RecordType
import eu.quanticol.carma.core.carma.SetComp
import eu.quanticol.carma.core.carma.Subtraction
import eu.quanticol.carma.core.carma.UniformFunction
import eu.quanticol.carma.core.carma.VariableReference
import eu.quanticol.carma.core.carma.VariableReferenceGlobal
import eu.quanticol.carma.core.carma.VariableReferenceMy
import eu.quanticol.carma.core.carma.VariableReferencePure
import eu.quanticol.carma.core.carma.VariableReferenceReceiver
import eu.quanticol.carma.core.carma.VariableReferenceSender
import java.util.ArrayList

import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.core.carma.Type
import eu.quanticol.carma.core.carma.Types
import eu.quanticol.carma.core.carma.FunctionReturn
import eu.quanticol.carma.core.carma.AtomicCalls
import eu.quanticol.carma.core.carma.AtomicProcessComposition

class FunctionJavaniser {
	
	def String javanise(Types types){
		types.type.javanise
	}
	
	def String javanise(Type type){
		switch(type){
			DoubleType: "double"
			IntgerType: "int"
			AttribType: "int"
			RecordType: type.name.name
		}
	}
	
	def String getParameters(ArrayList<Parameter> parameters){
		var String toReturn = ""
		if(parameters.size > 0){
			toReturn = parameters.get(0).getParameter
			for(var i = 1; i < parameters.size; i++){
				toReturn = toReturn + ", " + parameters.get(i).getParameter
			}
		}
		return toReturn
	}
	
	def String getParameter(Parameter parameter){
		switch(parameter){
			AttribParameter: '''«(parameter.type as AttribType).javanise» «parameter.name.name»'''
			RecordParameter: '''«(parameter.type as RecordType).javanise» «parameter.name.name»'''
			DoubleParameter: '''«(parameter.type as DoubleType).javanise» «parameter.name.name»'''
			IntgerParameter: '''«(parameter.type as IntgerType).javanise» «parameter.name.name»'''
		}
	}

	def String javanise(FunctionExpression functionExpression) {
		functionExpression.expression.javanise
	}
	
	def String javanise(BooleanExpression functionExpression) {
		functionExpression.expression.javanise
	}

	def String javanise(Expressions e) {
		switch (e) {
			Or: 						{e.javanise}
			And:						{e.javanise}
			Equality: 					{e.javanise}
			Comparison: 				{e.javanise}
			Subtraction: 				{e.javanise}
			Addition: 					{e.javanise}
			Multiplication: 			{e.javanise}
			Modulo: 					{e.javanise}
			Division: 					{e.javanise}
			Not: 						{e.javanise}
			AtomicPrimitive: 			{e.javanise}
			AtomicVariable: 			{e.javanise}
			AtomicCalls: 				{e.javanise}
			AtomicNow: 					{e.javanise}
			AtomicMeasure: 				{e.javanise}
			AtomicRecord: 				{e.javanise}
			AtomicOutcome: 				{"//eu.quanticol.carma.core.generator.ms.function.javanise.AtomicOutcome"}
			AtomicProcessComposition:	{"//eu.quanticol.carma.core.generator.ms.function.javanise.AtomicProcessComposition"}
		}

	}

	def String javanise(Or e) {
		'''(«e.left.javanise» || «e.right.javanise»)'''
	}

	def String javanise(And e) {
		'''(«e.left.javanise» && «e.right.javanise»)'''
	}

	def String javanise(Equality e) {
		'''(«e.left.javanise» «e.op» «e.right.javanise»)'''
	}

	def String javanise(Comparison e) {
		'''(«e.left.javanise» «e.op» «e.right.javanise»)'''
	}

	def String javanise(Subtraction e) {
		'''(«e.left.javanise» - «e.right.javanise»)'''
	}

	def String javanise(Addition e) {
		'''(«e.left.javanise» - «e.right.javanise»)'''
	}

	def String javanise(Multiplication e) {
		'''(«e.left.javanise» * «e.right.javanise»)'''
	}

	def String javanise(Modulo e) {
		'''(«e.left.javanise» % «e.right.javanise»)'''
	}

	def String javanise(Division e) {
		'''(«e.left.javanise» / «e.right.javanise»)'''
	}

	def String javanise(Not e) {
		'''!(«e.expression.javanise»)'''
	}

	def String javanise(AtomicPrimitive e) {
		e.value.javanise
	}

	def String javanise(PrimitiveTypes pts) {
		switch (pts) {
			CarmaDouble: pts.javanise
			CarmaInteger: pts.javanise
			CarmaBoolean: pts.javanise
			Range: "//eu.quanticol.carma.core.generator.ms.function.javanise.Range"
		}
	}

	def String javanise(CarmaDouble pt) {
		var String toReturn = ""
		if (pt.negative != null)
			toReturn = toReturn + "-"
		toReturn = toReturn + pt.left + "." + pt.right
		if (pt.exponent != null)
			toReturn = toReturn + pt.exponent.javanise
		return toReturn
	}

	def String javanise(CarmaExponent exp) {
		var String negative = ""
		if (exp.negative != null)
			negative = "-"
		''' * «negative» Math.pow(«exp.base»,«exp.exponent»)'''
	}

	def String javanise(CarmaInteger pt) {
		if (pt.negative != null)
			return "-" + pt.value
		else
			return "" + pt.value
	}

	def String javanise(CarmaBoolean pt) {
		'''«pt.value»'''
	}

	def String javanise(AtomicVariable expression) {
		expression.value.javanise
	}

	def String javanise(VariableReference vr) {
		switch (vr) {
			VariableReferencePure: 		vr.name.name
			VariableReferenceMy: 		vr.name.name
			VariableReferenceReceiver: 	vr.name.name
			VariableReferenceSender: 	vr.name.name
			VariableReferenceGlobal: 	vr.name.name
			RecordReferencePure: 		vr.name.name + "." + vr.feild.name
			RecordReferenceMy: 			vr.name.name + "." + vr.feild.name
			RecordReferenceReceiver: 	vr.name.name + "." + vr.feild.name
			RecordReferenceSender: 		vr.name.name + "." + vr.feild.name
			RecordReferenceGlobal: 		vr.name.name + "." + vr.feild.name
		}
	}

	def String javanise(AtomicCalls expression) {
		expression.value.javanise
	}

	def String javanise(Calls calls) {
		switch (calls) {
			FunctionReferenceMan: calls.ref.javanise
			FunctionReferencePre: calls.ref.javanise
		}
	}

	def String javanise(FunctionCall functionCall) {
		'''
			«(functionCall.name as Name).name.toLowerCase»(«(functionCall.arguments as FunctionCallArguments).javanise»)
		'''
	}
	
	def String javanise(FunctionCallArguments arguments){
			var ArrayList<FunctionArgument> args = new ArrayList<FunctionArgument>(arguments.eAllOfType(FunctionArgument))
			var toReturn = ""
			if(args.size > 0){
				toReturn = toReturn + args.get(0).javanise
				for(var i = 1; i < args.size; i++){
					toReturn = toReturn + ", " + args.get(i).javanise
				}
			}
			return toReturn
	}
	
	def String javanise(FunctionArgument argument){
		argument.value.javanise
	}
	
	def String javanise(PreFunctionCall preFunctionCall) {
		switch (preFunctionCall) {
			PDFunction:			{'''pdf(«(preFunctionCall.arguments as PredFunctionCallArguments).javanise»)'''}
			UniformFunction:	{'''uniform(«(preFunctionCall.arguments as PredFunctionCallArguments).javanise»)'''}
			CeilingFunction:	{'''ceil(«(preFunctionCall.arguments as PredFunctionCallArguments).javanise»)'''}
			FloorFunction:		{'''floor(«(preFunctionCall.arguments as PredFunctionCallArguments).javanise»)'''}
			MaxFunction:		{'''max(«(preFunctionCall.arguments as PredFunctionCallArguments).javanise»)'''}
			MinFunction:		{'''min(«(preFunctionCall.arguments as PredFunctionCallArguments).javanise»)'''}		
		}
	}
	
	def String javanise(PredFunctionCallArguments arguments){
			var ArrayList<PreArgument> args = new ArrayList<PreArgument>(arguments.eAllOfType(PreArgument))
			var toReturn = '''new ArrayList<Object>(Arrays.asList('''
			if(args.size > 0){
				toReturn = toReturn + '''«args.get(0).javanise»'''
				for(var i = 1; i < args.size; i++){
					toReturn = toReturn + ''', «args.get(i).javanise»'''
				}
			}
			return toReturn + "))"
	}
	
	def String javanise(PreArgument argument){
		argument.value.javanise
	}
	
	def String javanise(AtomicNow expression){
		'''now()'''
	}
	
	def String javanise(AtomicMeasure expression){
		'''getMeasure«expression.value.javanise»().measure(this)'''
	}
	
	def String javanise(SetComp setComp){
		((setComp.hashCode*setComp.hashCode)*1000 + "").substring(0,3)
	}
	
	def String javanise(AtomicRecord expression){
		var instance = (expression.value as InstantiateRecord)
		'''new «(instance.type as Type).javanise» ( «(instance.arguments as RecordArguments).javanise» )'''
	}
	
	def String javanise(RecordArguments arguments){
			var ArrayList<RecordArgument> args = new ArrayList<RecordArgument>(arguments.eAllOfType(RecordArgument))
			var toReturn = ""
			if(args.size > 0){
				toReturn = toReturn + args.get(0).javanise
				for(var i = 1; i < args.size; i++){
					toReturn = toReturn + ", " + args.get(i).javanise
				}
			}
			return toReturn
	}
	
	def String javanise(RecordArgument argument){
		argument.value.javanise
	}
	
	def String javanise(FunctionDeclaration functionDeclaration){
		switch(functionDeclaration){
			AttribVariableDeclaration	: functionDeclaration.javanise
			IntgerVariableDeclaration	: functionDeclaration.javanise
			DoubleVariableDeclaration	: functionDeclaration.javanise
			RecordDeclaration			: functionDeclaration.javanise
		}
	}
	
	def String javanise(AttribVariableDeclaration attribDeclaration){
		'''«(attribDeclaration.type as Type).javanise» «attribDeclaration.name.name» = «attribDeclaration.assign.javanise»'''
	}
	
	def String javanise(IntgerVariableDeclaration intgerDeclaration){
		'''«(intgerDeclaration.type as Type).javanise» «intgerDeclaration.name.name» = «intgerDeclaration.assign.javanise»'''
	}
	
	def String javanise(DoubleVariableDeclaration doubleDeclaration){
		'''«(doubleDeclaration.type as Type).javanise» «doubleDeclaration.name.name» = «doubleDeclaration.assign.javanise»'''
	}
	
	def String javanise(RecordDeclaration recordDeclaration){
		'''«(recordDeclaration.type as Type).javanise» «recordDeclaration.name.name» = «recordDeclaration.assign.javanise»'''
	}
	
	def String javanise(FunctionStatement functionStatement){		
		switch(functionStatement){
			FunctionDeclaration	: functionStatement.javanise + ";"
			FunctionAssignment	: functionStatement.javanise + ";"
			FunctionIfStatement : functionStatement.javanise + ";"
			FunctionForStatement: functionStatement.javanise + ";"
		}
	}
	
	def String javanise(FunctionAssignment functionAssignment){
		'''«functionAssignment.reference.javanise» = «functionAssignment.expression.javanise»'''
	}
	
	def String javanise(FunctionIfStatement functionIfStatement){
		var toReturn = 
'''if («functionIfStatement.expression.javanise») {
	«FOR functionStatement : functionIfStatement.thenBlock.statements»
	«functionStatement.javanise»
	«ENDFOR»
}'''
		if(functionIfStatement.elseBlock != null){
			toReturn = toReturn + 
'''else {
	«FOR functionStatement : functionIfStatement.elseBlock.statements»
	«functionStatement.javanise»
	«ENDFOR»
}
'''
		}
		return toReturn
	}
	
	def String javanise(FunctionForStatement functionForStatement){
'''for( «functionForStatement.variable.javanise » ; «functionForStatement.expression.javanise » ; «functionForStatement.afterThought.functionAssignment.javanise» ){
	«FOR functionStatement : functionForStatement.functionForBlock.statements»
	«functionStatement.javanise»
	«ENDFOR»		
}'''
	}
	
	def String javanise(FunctionReturn functionReturn){
		'''return «functionReturn.expression.javanise»; '''
	}

	
//	def String javanise(Arguments arguments) {
//		switch(arguments){
//			PredFunctionCallArguments	:	{arguments.javanise}
//			ComponentBlockArguments		:	{arguments.javanise}
//			FunctionCallArguments		:	{arguments.javanise}
//			OutputActionArguments		:	{arguments.javanise}
//			RecordArguments				:	{arguments.javanise}
//		}
//	}
//	def String javanise(ComponentBlockArguments arguments){
//			var ArrayList<CompArgument> args = new ArrayList<CompArgument>(arguments.eAllOfType(CompArgument))
//			var toReturn = ""
//			if(args.size > 0){
//				toReturn = toReturn + args.get(0).javanise
//				for(var i = 1; i < args.size; i++){
//					toReturn = toReturn + ", " + args.get(i).javanise
//				}
//			}
//			return toReturn
//	}
//	def String javanise(CompArgument argument){
//		switch(argument.value){
//			VariableReference	: 	{(argument.value as VariableReference).javanise}
//			CarmaInteger		: 	{(argument.value as CarmaInteger).javanise}	
//			Range				: 	{(argument.value as Range).javanise}	
//			InstantiateRecord	:	{(argument.value as InstantiateRecord).arguments.javanise}
//		}
//	}
//	def String javanise(OutputActionArguments arguments){
//			var ArrayList<OutputActionArgument> args = new ArrayList<OutputActionArgument>(arguments.eAllOfType(OutputActionArgument))
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
//	def String javanise(OutputActionArgument argument){
//		switch(argument.value){
//			VariableReference	: 	{(argument.value as VariableReference).javanise}
//			CarmaInteger		: 	{(argument.value as CarmaInteger).javanise}	
//		}
//	}
}