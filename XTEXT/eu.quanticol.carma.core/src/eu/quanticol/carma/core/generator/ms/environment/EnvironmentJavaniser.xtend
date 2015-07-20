package eu.quanticol.carma.core.generator.ms.environment

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.Action
import eu.quanticol.carma.core.carma.Addition
import eu.quanticol.carma.core.carma.And
import eu.quanticol.carma.core.carma.AtomicCalls
import eu.quanticol.carma.core.carma.AtomicMeasure
import eu.quanticol.carma.core.carma.AtomicNow
import eu.quanticol.carma.core.carma.AtomicOutcome
import eu.quanticol.carma.core.carma.AtomicPrimitive
import eu.quanticol.carma.core.carma.AtomicProcessComposition
import eu.quanticol.carma.core.carma.AtomicRecord
import eu.quanticol.carma.core.carma.AtomicVariable
import eu.quanticol.carma.core.carma.AttribType
import eu.quanticol.carma.core.carma.BooleanExpression
import eu.quanticol.carma.core.carma.Calls
import eu.quanticol.carma.core.carma.CarmaBoolean
import eu.quanticol.carma.core.carma.CarmaDouble
import eu.quanticol.carma.core.carma.CarmaExponent
import eu.quanticol.carma.core.carma.CarmaInteger
import eu.quanticol.carma.core.carma.CeilingFunction
import eu.quanticol.carma.core.carma.CompArgument
import eu.quanticol.carma.core.carma.Comparison
import eu.quanticol.carma.core.carma.ComponentBlockArguments
import eu.quanticol.carma.core.carma.Division
import eu.quanticol.carma.core.carma.DoubleType
import eu.quanticol.carma.core.carma.EnvironmentProbExpression
import eu.quanticol.carma.core.carma.EnvironmentRateExpression
import eu.quanticol.carma.core.carma.EnvironmentUpdate
import eu.quanticol.carma.core.carma.EnvironmentUpdateExpression
import eu.quanticol.carma.core.carma.Equality
import eu.quanticol.carma.core.carma.Expressions
import eu.quanticol.carma.core.carma.FloorFunction
import eu.quanticol.carma.core.carma.FunctionArgument
import eu.quanticol.carma.core.carma.FunctionCall
import eu.quanticol.carma.core.carma.FunctionCallArguments
import eu.quanticol.carma.core.carma.FunctionExpression
import eu.quanticol.carma.core.carma.FunctionReferenceMan
import eu.quanticol.carma.core.carma.FunctionReferencePre
import eu.quanticol.carma.core.carma.InstantiateRecord
import eu.quanticol.carma.core.carma.IntgerType
import eu.quanticol.carma.core.carma.MaxFunction
import eu.quanticol.carma.core.carma.MinFunction
import eu.quanticol.carma.core.carma.Modulo
import eu.quanticol.carma.core.carma.Multiplication
import eu.quanticol.carma.core.carma.Name
import eu.quanticol.carma.core.carma.Not
import eu.quanticol.carma.core.carma.Or
import eu.quanticol.carma.core.carma.PDFunction
import eu.quanticol.carma.core.carma.ParallelComposition
import eu.quanticol.carma.core.carma.PreArgument
import eu.quanticol.carma.core.carma.PreFunctionCall
import eu.quanticol.carma.core.carma.PredFunctionCallArguments
import eu.quanticol.carma.core.carma.PrimitiveTypes
import eu.quanticol.carma.core.carma.Probability
import eu.quanticol.carma.core.carma.ProcessComposition
import eu.quanticol.carma.core.carma.ProcessReference
import eu.quanticol.carma.core.carma.Range
import eu.quanticol.carma.core.carma.Rate
import eu.quanticol.carma.core.carma.RecordArgument
import eu.quanticol.carma.core.carma.RecordArguments
import eu.quanticol.carma.core.carma.RecordReferenceGlobal
import eu.quanticol.carma.core.carma.RecordReferenceMy
import eu.quanticol.carma.core.carma.RecordReferencePure
import eu.quanticol.carma.core.carma.RecordReferenceReceiver
import eu.quanticol.carma.core.carma.RecordReferenceSender
import eu.quanticol.carma.core.carma.RecordType
import eu.quanticol.carma.core.carma.Subtraction
import eu.quanticol.carma.core.carma.Type
import eu.quanticol.carma.core.carma.Types
import eu.quanticol.carma.core.carma.UniformFunction
import eu.quanticol.carma.core.carma.UpdateExpression
import eu.quanticol.carma.core.carma.VariableReference
import eu.quanticol.carma.core.carma.VariableReferenceGlobal
import eu.quanticol.carma.core.carma.VariableReferenceMy
import eu.quanticol.carma.core.carma.VariableReferencePure
import eu.quanticol.carma.core.carma.VariableReferenceReceiver
import eu.quanticol.carma.core.carma.VariableReferenceSender
import eu.quanticol.carma.core.generator.ms.SharedJavaniser
import eu.quanticol.carma.core.typing.BaseType
import java.util.ArrayList

import static extension org.eclipse.xtext.EcoreUtil2.*

class EnvironmentJavaniser {
	
	@Inject extension SharedJavaniser
		
	def String javanise(EnvironmentUpdate update){
		(Math.abs(update.hashCode*update.hashCode)+"").substring(0,4)
	}
		
	def String javanise(Probability probability){
		(Math.abs(probability.hashCode*probability.hashCode)+"").substring(0,4)
	}
	
	def String javanise(Rate rate){
		(Math.abs(rate.hashCode*rate.hashCode)+"").substring(0,4)
	}
		
	def int actionName(Action action){
		var toReturn = 10 * 13
		
//		for(var i = 0 ; i < action.name.name.length; i++){
//			toReturn = toReturn + action.name.name.charAt(i) * 13
//		}
//		
		return toReturn
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

	def String storeExpress(BaseType bt){
		if(bt.me.equals("int")){
			'''Integer.class'''
		} else {
			'''«bt.me».class'''
		}
	}
	
	def String express(BaseType bt){
		if(bt.me.equals("int")){
			'''int'''
		} else {
			'''«bt.me»'''
		}
	}

	
	def String express(Types types){
		types.type.express
	}
	
	def String express(Type type){
		switch(type){
			DoubleType: "double"
			IntgerType: "int"
			AttribType: "int"
			RecordType: type.name.name
		}
	}
	
	def String declarePrimitiveTypes(EnvironmentRateExpression functionExpression){
		functionExpression.expression.declarePrimitiveTypes
	}
	
	def String declarePrimitiveTypes(EnvironmentProbExpression functionExpression){
		functionExpression.expression.declarePrimitiveTypes
	}
	
	def String declarePrimitiveTypes(EnvironmentUpdateExpression functionExpression){
		functionExpression.expression.declarePrimitiveTypes
	}
		
	def String express(EnvironmentProbExpression functionExpression) {
		functionExpression.expression.express
	}

	def String express(FunctionExpression functionExpression) {
		functionExpression.expression.express
	}
	
	def String express(BooleanExpression functionExpression) {
		functionExpression.expression.express
	}
	
	def String express(UpdateExpression functionExpression) {
		functionExpression.expression.express
	}
	
	def String express(EnvironmentRateExpression functionExpression) {
		functionExpression.expression.express
	}

	def String express(EnvironmentUpdateExpression functionExpression) {
		functionExpression.expression.express
	}

	def String express(Expressions e) {
		switch (e) {
			Or: 						{e.express}
			And:						{e.express}
			Equality: 					{e.express}
			Comparison: 				{e.express}
			Subtraction: 				{e.express}
			Addition: 					{e.express}
			Multiplication: 			{e.express}
			Modulo: 					{e.express}
			Division: 					{e.express}
			Not: 						{e.express}
			AtomicPrimitive: 			{e.express}
			AtomicVariable: 			{e.express}
			AtomicCalls: 				{e.express}
			AtomicNow: 					{e.express}
			AtomicMeasure: 				{e.express}
			AtomicRecord: 				{e.express}
			AtomicOutcome: 				{"//eu.quanticol.carma.core.generator.ms.function.express.AtomicOutcome"}
			AtomicProcessComposition:	{"//eu.quanticol.carma.core.generator.ms.function.express.AtomicProcessComposition"}
		}

	}

	def String express(Or e) {
		'''(«e.left.express» || «e.right.express»)'''
	}

	def String express(And e) {
		'''(«e.left.express» && «e.right.express»)'''
	}

	def String express(Equality e) {
		'''(«e.left.express» «e.op» «e.right.express»)'''
	}

	def String express(Comparison e) {
		'''(«e.left.express» «e.op» «e.right.express»)'''
	}

	def String express(Subtraction e) {
		'''(«e.left.express» - «e.right.express»)'''
	}

	def String express(Addition e) {
		'''(«e.left.express» + «e.right.express»)'''
	}

	def String express(Multiplication e) {
		'''(«e.left.express» * «e.right.express»)'''
	}

	def String express(Modulo e) {
		'''(«e.left.express» % «e.right.express»)'''
	}

	def String express(Division e) {
		'''(«e.left.express» / «e.right.express»)'''
	}

	def String express(Not e) {
		'''!(«e.expression.express»)'''
	}

	def String express(AtomicPrimitive e) {
		e.value.express
	}

	def String express(PrimitiveTypes pts) {
		switch (pts) {
			CarmaDouble: pts.express
			CarmaInteger: pts.express
			CarmaBoolean: pts.express
			Range: "//eu.quanticol.carma.core.generator.ms.function.express.Range"
		}
	}

	def String express(CarmaDouble pt) {
		var String toReturn = ""
		if (pt.negative != null)
			toReturn = toReturn + "-"
		toReturn = toReturn + pt.left + "." + pt.right
		if (pt.exponent != null)
			toReturn = toReturn + pt.exponent.express
		return toReturn
	}

	def String express(CarmaExponent exp) {
		var String negative = ""
		if (exp.negative != null)
			negative = "-"
		''' * «negative» Math.pow(«exp.base»,«exp.exponent»)'''
	}

	def String express(CarmaInteger pt) {
		if (pt.negative != null)
			return "-" + pt.value
		else
			return "" + pt.value
	}

	def String express(CarmaBoolean pt) {
		'''«pt.value»'''
	}

	def String express(AtomicVariable expression) {
		expression.value.express
	}

	def String express(VariableReference vr) {
		switch (vr) {
			VariableReferenceReceiver: 	"receiver_"+vr.name.name
			VariableReferenceSender: 	"sender_"+vr.name.name
			VariableReferenceGlobal: 	"global_"+vr.name.name
			RecordReferenceReceiver: 	"receiver_"+vr.name.name + "." + vr.feild.name
			RecordReferenceSender: 		"sender_"+vr.name.name + "." + vr.feild.name
			RecordReferenceGlobal: 		"global_"+vr.name.name + "." + vr.feild.name
		}
	}

	def String express(AtomicCalls expression) {
		expression.value.express
	}

	def String express(Calls calls) {
		switch (calls) {
			FunctionReferenceMan: calls.ref.express
			FunctionReferencePre: calls.ref.express
		}
	}

	def String express(FunctionCall functionCall) {
		'''«(functionCall.name as Name).name.toFirstLower»(«(functionCall.arguments as FunctionCallArguments).express»)'''
	}
	
	def String express(FunctionCallArguments arguments){
			var ArrayList<FunctionArgument> args = new ArrayList<FunctionArgument>(arguments.eAllOfType(FunctionArgument))
			var toReturn = ""
			if(args.size > 0){
				toReturn = toReturn + args.get(0).express
				for(var i = 1; i < args.size; i++){
					toReturn = toReturn + ", " + args.get(i).express
				}
			}
			return toReturn
	}
	
	def String express(FunctionArgument argument){
		argument.value.express
	}
	
	def String express(PreFunctionCall preFunctionCall) {
		switch (preFunctionCall) {
			PDFunction:			{'''pdf(«(preFunctionCall.arguments as PredFunctionCallArguments).express»)'''}
			UniformFunction:	{'''uniform(«(preFunctionCall.arguments as PredFunctionCallArguments).express»)'''}
			CeilingFunction:	{'''ceil(«(preFunctionCall.arguments as PredFunctionCallArguments).express»)'''}
			FloorFunction:		{'''floor(«(preFunctionCall.arguments as PredFunctionCallArguments).express»)'''}
			MaxFunction:		{'''max(«(preFunctionCall.arguments as PredFunctionCallArguments).express»)'''}
			MinFunction:		{'''min(«(preFunctionCall.arguments as PredFunctionCallArguments).express»)'''}		
		}
	}
	
	def String express(PredFunctionCallArguments arguments){
			var ArrayList<PreArgument> args = new ArrayList<PreArgument>(arguments.eAllOfType(PreArgument))
			var toReturn = '''new ArrayList<Object>(Arrays.asList('''
			if(args.size > 0){
				toReturn = toReturn + '''«args.get(0).express»'''
				for(var i = 1; i < args.size; i++){
					toReturn = toReturn + ''', «args.get(i).express»'''
				}
			}
			return toReturn + "))"
	}
	
	def String express(PreArgument argument){
		argument.value.express
	}
	
	def String express(AtomicNow expression){
		'''now()'''
	}
	
	def String express(AtomicMeasure expression){
		'''«expression.value.expressMeasure(true)»'''
	}
	
	def String express(AtomicRecord expression){
		var instance = (expression.value as InstantiateRecord)
		'''new «(instance.type as Type).express» ( «(instance.arguments as RecordArguments).express» )'''
	}
	
	def String express(RecordArguments arguments){
			var ArrayList<RecordArgument> args = new ArrayList<RecordArgument>(arguments.eAllOfType(RecordArgument))
			var toReturn = ""
			if(args.size > 0){
				toReturn = toReturn + args.get(0).express
				for(var i = 1; i < args.size; i++){
					toReturn = toReturn + ", " + args.get(i).express
				}
			}
			return toReturn
	}
	
	def String express(RecordArgument argument){
		argument.value.express
	}

	def ArrayList<ArrayList<String>> product(ComponentBlockArguments arguments){
			var ArrayList<CompArgument> args = new ArrayList<CompArgument>(arguments.eAllOfType(CompArgument))
			var toReturn = new ArrayList<ArrayList<String>>();
			if(args.size > 0){
				toReturn.add(args.get(0).array)
				for(var i = 1; i < args.size; i++){
					toReturn.add(args.get(i).array)
				}
			}
			return toReturn
	}

	def ArrayList<String> array(CompArgument argument){
		var ArrayList<String> toReturn = new ArrayList<String>();
		argument.value.array(toReturn)
		return toReturn
	}
	
	def void array(Expressions e, ArrayList<String> array) {
		switch (e) {
			Or: 						{array.add("//eu.quanticol.carma.core.generator.ms.collective.array.Or")}
			And:						{array.add("//eu.quanticol.carma.core.generator.ms.collective.array.And")}
			Equality: 					{array.add("//eu.quanticol.carma.core.generator.ms.collective.array.Equality")}
			Comparison: 				{array.add("//eu.quanticol.carma.core.generator.ms.collective.array.Comparison")}
			Subtraction: 				{array.add(e.javanise)}
			Addition: 					{array.add(e.javanise)}
			Multiplication: 			{array.add(e.javanise)}
			Modulo: 					{array.add(e.javanise)}
			Division: 					{array.add(e.javanise)}
			Not: 						{array.add("//eu.quanticol.carma.core.generator.ms.collective.array.Not")}
			AtomicPrimitive: 			{e.array(array)}
			AtomicVariable: 			{array.add(e.javanise)}
			AtomicCalls: 				{array.add(e.javanise)}
			AtomicNow: 					{array.add("//eu.quanticol.carma.core.generator.ms.collective.array.Now")}
			AtomicMeasure: 				{array.add("//eu.quanticol.carma.core.generator.ms.collective.array.Measure")}
			AtomicRecord: 				{array.add(e.javanise)}
			AtomicOutcome: 				{array.add("//eu.quanticol.carma.core.generator.ms.collective.array.AtomicOutcome")}
			AtomicProcessComposition: 	{array.add(e.javanise)}
		}

	}
	
	def void array(AtomicPrimitive e, ArrayList<String> array) {
		e.value.array(array)
	}
	
	def void array(PrimitiveTypes pts, ArrayList<String> array) {
		switch (pts) {
			CarmaDouble: {array.add("//eu.quanticol.carma.core.generator.ms.function.javanise.CarmaDouble")}
			CarmaInteger: {array.add(pts.javanise)}
			CarmaBoolean: {array.add("//eu.quanticol.carma.core.generator.ms.function.javanise.CarmaBoolean")}
			Range: pts.array(array)
		}
	}
	
	def void array(Range pt, ArrayList<String> array) {
		for(var i = pt.min; i <= pt.max; i++){
			array.add(""+i)
		}
	}
	def String javanise(Subtraction e) {
		'''(«e.left.javanise» - «e.right.javanise»)'''
	}

	def String javanise(Addition e) {
		'''(«e.left.javanise» + «e.right.javanise»)'''
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
			«(functionCall.name as Name).name»(«(functionCall.arguments as FunctionCallArguments).javanise»);
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
	
	def String javanise(AtomicVariable expression) {
		expression.value.javanise
	}
	
	def String javanise(VariableReference vr) {
		switch (vr) {
			VariableReferenceReceiver: 	'''receiver_«vr.name.name»'''
			VariableReferenceSender: 	'''sender_«vr.name.name»'''
			VariableReferenceGlobal: 	'''global_«vr.name.name»'''
			RecordReferenceReceiver: 	'''receiver_«vr.name.name».«vr.feild.name»'''
			RecordReferenceSender: 		'''sender_«vr.name.name».«vr.feild.name»'''
			RecordReferenceGlobal: 		'''global_«vr.name.name».«vr.feild.name»'''
		}
	}	
	
	def String javanise(AtomicRecord expression){
		var instance = (expression.value as InstantiateRecord)
		'''new «(instance.type as Type).javanise» ( «(instance.arguments as RecordArguments).javanise» )'''
	}
		
	def String javanise(AtomicProcessComposition expression){
		'''new ArrayList<String>(Arrays.asList( «expression.value.javanise» ))'''
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
			AtomicNow: 					{"//eu.quanticol.carma.core.generator.ms.collective.javanise.Now"}
			AtomicMeasure: 				{"//eu.quanticol.carma.core.generator.ms.collective.javanise.AtomicOutcome"}
			AtomicRecord: 				{e.javanise}
			AtomicOutcome: 				{"//eu.quanticol.carma.core.generator.ms.collective.javanise.AtomicOutcome"}
			AtomicProcessComposition:	{e.javanise}
		}
	}	
	
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
	
	def String javanise(ProcessComposition processComposition){
		switch(processComposition){
			ParallelComposition	: 	'''«processComposition.left.javanise», «processComposition.right.javanise»'''
			ProcessReference	:	'''"«processComposition.expression.name»"'''
		}
	}		
	
	def void cartesianProduct(ArrayList<ArrayList<String>> in, ArrayList<ArrayList<String>> out){
		if(in.size() > 1){
			var ArrayList<String> head = in.remove(0);
			var ArrayList<ArrayList<String>> exit = new ArrayList<ArrayList<String>>();
			cartesianProduct(in,out);
			for(var int i = 0; i < out.size(); i++){
				for(String item : head){
					var ArrayList<String> inter = new ArrayList<String>();
					inter.add(item);
					inter.addAll(out.get(i));
					exit.add(inter);
				}
			}
			out.clear();
			out.addAll(exit);
		} else {
			var ArrayList<String> head = in.remove(0);
			for(String item : head){
				var ArrayList<String> tail = new ArrayList<String>();
				tail.add(item);
				out.add(tail);
			}
		}
	}
}