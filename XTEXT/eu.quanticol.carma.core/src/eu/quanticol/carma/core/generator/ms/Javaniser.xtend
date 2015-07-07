package eu.quanticol.carma.core.generator.ms

import eu.quanticol.carma.core.carma.Addition
import eu.quanticol.carma.core.carma.And
import eu.quanticol.carma.core.carma.Arguments
import eu.quanticol.carma.core.carma.AtomicMeasure
import eu.quanticol.carma.core.carma.AtomicMethodReference
import eu.quanticol.carma.core.carma.AtomicNow
import eu.quanticol.carma.core.carma.AtomicOutcome
import eu.quanticol.carma.core.carma.AtomicPrimitive
import eu.quanticol.carma.core.carma.AtomicRecord
import eu.quanticol.carma.core.carma.AtomicVariable
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
import eu.quanticol.carma.core.carma.Equality
import eu.quanticol.carma.core.carma.Expressions
import eu.quanticol.carma.core.carma.FloorFunction
import eu.quanticol.carma.core.carma.FunctionCall
import eu.quanticol.carma.core.carma.FunctionCallArguments
import eu.quanticol.carma.core.carma.FunctionExpression
import eu.quanticol.carma.core.carma.FunctionReferenceMan
import eu.quanticol.carma.core.carma.FunctionReferencePre
import eu.quanticol.carma.core.carma.InstantiateRecord
import eu.quanticol.carma.core.carma.MaxFunction
import eu.quanticol.carma.core.carma.MinFunction
import eu.quanticol.carma.core.carma.Modulo
import eu.quanticol.carma.core.carma.Multiplication
import eu.quanticol.carma.core.carma.Name
import eu.quanticol.carma.core.carma.Not
import eu.quanticol.carma.core.carma.Or
import eu.quanticol.carma.core.carma.OutputActionArguments
import eu.quanticol.carma.core.carma.PDFunction
import eu.quanticol.carma.core.carma.PreArgument
import eu.quanticol.carma.core.carma.PreFunctionCall
import eu.quanticol.carma.core.carma.PredFunctionCallArguments
import eu.quanticol.carma.core.carma.PrimitiveTypes
import eu.quanticol.carma.core.carma.Range
import eu.quanticol.carma.core.carma.RecordArguments
import eu.quanticol.carma.core.carma.RecordReferenceGlobal
import eu.quanticol.carma.core.carma.RecordReferenceMy
import eu.quanticol.carma.core.carma.RecordReferencePure
import eu.quanticol.carma.core.carma.RecordReferenceReceiver
import eu.quanticol.carma.core.carma.RecordReferenceSender
import eu.quanticol.carma.core.carma.State
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
import eu.quanticol.carma.core.carma.RecordArgument
import eu.quanticol.carma.core.carma.FunctionArgument
import eu.quanticol.carma.core.carma.OutputActionArgument
import eu.quanticol.carma.core.carma.SetComp

class Javaniser {

	def String javanise(Name name, String prefix) {
		return prefix + name.name
	}

	def String javanise(FunctionExpression functionExpression, String prefix) {
		functionExpression.expression.javanise(prefix)
	}

	def String javanise(Expressions e, String prefix) {
		switch (e) {
			Or: 					{e.javanise(prefix)}
			And:					{e.javanise(prefix)}
			Equality: 				{e.javanise(prefix)}
			Comparison: 			{e.javanise(prefix)}
			Subtraction: 			{e.javanise(prefix)}
			Addition: 				{e.javanise(prefix)}
			Multiplication: 		{e.javanise(prefix)}
			Modulo: 				{e.javanise(prefix)}
			Division: 				{e.javanise(prefix)}
			Not: 					{e.javanise(prefix)}
			AtomicPrimitive: 		{e.javanise(prefix)}
			AtomicVariable: 		{e.javanise(prefix)}
			AtomicMethodReference: 	{e.javanise(prefix)}
			AtomicNow: 				{e.javanise(prefix)}
			AtomicMeasure: 			{e.javanise(prefix)}
			AtomicRecord: 			{e.javanise(prefix)}
			AtomicOutcome: 			{e.javanise(prefix)}
		}

	}

	def String javanise(Or e, String prefix) {
		'''(«e.left.javanise(prefix)» || «e.right.javanise(prefix)»)'''
	}

	def String javanise(And e, String prefix) {
		'''(«e.left.javanise(prefix)» && «e.right.javanise(prefix)»)'''
	}

	def String javanise(Equality e, String prefix) {
		'''(«e.left.javanise(prefix)» «e.op» «e.right.javanise(prefix)»)'''
	}

	def String javanise(Comparison e, String prefix) {
		'''(«e.left.javanise(prefix)» «e.op» «e.right.javanise(prefix)»)'''
	}

	def String javanise(Subtraction e, String prefix) {
		'''(«e.left.javanise(prefix)» - «e.right.javanise(prefix)»)'''
	}

	def String javanise(Addition e, String prefix) {
		'''(«e.left.javanise(prefix)» - «e.right.javanise(prefix)»)'''
	}

	def String javanise(Multiplication e, String prefix) {
		'''(«e.left.javanise(prefix)» * «e.right.javanise(prefix)»)'''
	}

	def String javanise(Modulo e, String prefix) {
		'''(«e.left.javanise(prefix)» % «e.right.javanise(prefix)»)'''
	}

	def String javanise(Division e, String prefix) {
		'''(«e.left.javanise(prefix)» / «e.right.javanise(prefix)»)'''
	}

	def String javanise(Not e, String prefix) {
		'''!(«e.expression.javanise(prefix)»)'''
	}

	def String javanise(AtomicPrimitive e, String prefix) {
		e.value.javanise(prefix)
	}

	def String javanise(PrimitiveTypes pts, String prefix) {
		switch (pts) {
			CarmaDouble: pts.javanise(prefix)
			CarmaInteger: pts.javanise(prefix)
			CarmaBoolean: pts.javanise(prefix)
			Range: pts.javanise(prefix)
		}
	}

	def String javanise(CarmaDouble pt, String prefix) {
		var String toReturn = ""
		if (pt.negative != null)
			toReturn = toReturn + "-"
		toReturn = toReturn + pt.left + "." + pt.right
		if (pt.exponent != null)
			toReturn = toReturn + pt.exponent.javanise(prefix)
		return toReturn
	}

	def String javanise(CarmaExponent exp, String prefix) {
		var String negative = ""
		if (exp.negative != null)
			negative = "-"
		''' * «negative» Math.pow(«exp.base»,«exp.exponent»)'''
	}

	def String javanise(CarmaInteger pt, String prefix) {
		if (pt.negative != null)
			return "-" + pt.value
		else
			return "" + pt.value
	}

	def String javanise(CarmaBoolean pt, String prefix) {
		'''«pt.value»'''
	}

	def String javanise(Range pt, String prefix) {
		"range_"+pt.hashCode
	}

	def String javanise(AtomicVariable expression, String prefix) {
		expression.value.javanise(prefix)
	}

	def String javanise(VariableReference vr, String prefix) {
		switch (vr) {
			VariableReferencePure: 		prefix + vr.name.name
			VariableReferenceMy: 		prefix + vr.name.name
			VariableReferenceReceiver: 	prefix + vr.name.name
			VariableReferenceSender: 	prefix + vr.name.name
			VariableReferenceGlobal: 	prefix + vr.name.name
			RecordReferencePure: 		prefix + vr.name.name + "_" + vr.feild.name
			RecordReferenceMy: 			prefix + vr.name.name + "_" + vr.feild.name
			RecordReferenceReceiver: 	prefix + vr.name.name + "_" + vr.feild.name
			RecordReferenceSender: 		prefix + vr.name.name + "_" + vr.feild.name
			RecordReferenceGlobal: 		prefix + vr.name.name + "_" + vr.feild.name
		}
	}

	def String javanise(AtomicMethodReference expression, String prefix) {
		expression.value.javanise(prefix)
	}

	def String javanise(Calls calls, String prefix) {
		switch (calls) {
			FunctionReferenceMan: calls.ref.javanise(prefix)
			FunctionReferencePre: calls.ref.javanise(prefix)
		}
	}

	def String javanise(FunctionCall functionCall, String prefix) {
		'''
			«prefix»«(functionCall.name as Name).javanise(prefix)»(«functionCall.arguments.javanise(prefix)»);
		'''

	}

	def String javanise(Arguments arguments, String prefix) {
		switch(arguments){
			PredFunctionCallArguments	:	{arguments.javanise(prefix)}
			ComponentBlockArguments		:	{arguments.javanise(prefix)}
			FunctionCallArguments		:	{arguments.javanise(prefix)}
			OutputActionArguments		:	{arguments.javanise(prefix)}
			RecordArguments				:	{arguments.javanise(prefix)}
		}
	}
	
	def String javanise(PredFunctionCallArguments arguments, String prefix){
			var ArrayList<PreArgument> args = new ArrayList<PreArgument>(arguments.eAllOfType(PreArgument))
			var toReturn = ""
			if(args.size > 0){
				toReturn = toReturn + args.get(0).javanise(prefix)
				for(var i = 1; i < args.size; i++){
					toReturn = toReturn + ", " + args.get(i).javanise(prefix)
				}
			}
			return toReturn
	}
	
	def String javanise(PreArgument argument, String prefix){
		switch(argument.value){
			VariableReference	: 	{(argument.value as VariableReference).javanise(prefix)}
			CarmaInteger		: 	{(argument.value as CarmaInteger).javanise(prefix)}	
			CarmaDouble			: 	{(argument.value as CarmaDouble).javanise(prefix)}
		}
	}
	
	def String javanise(ComponentBlockArguments arguments, String prefix){
			var ArrayList<CompArgument> args = new ArrayList<CompArgument>(arguments.eAllOfType(CompArgument))
			var toReturn = ""
			if(args.size > 0){
				toReturn = toReturn + args.get(0).javanise(prefix)
				for(var i = 1; i < args.size; i++){
					toReturn = toReturn + ", " + args.get(i).javanise(prefix)
				}
			}
			return toReturn
	}
	
	def String javanise(CompArgument argument, String prefix){
		switch(argument.value){
			VariableReference	: 	{(argument.value as VariableReference).javanise(prefix)}
			CarmaInteger		: 	{(argument.value as CarmaInteger).javanise(prefix)}	
			Range				: 	{(argument.value as Range).javanise(prefix)}	
			InstantiateRecord	:	{(argument.value as InstantiateRecord).arguments.javanise(prefix)}
			State				:	'''null'''
		}
	}
	
	def String javanise(RecordArguments arguments, String prefix){
			var ArrayList<RecordArgument> args = new ArrayList<RecordArgument>(arguments.eAllOfType(RecordArgument))
			var toReturn = ""
			if(args.size > 0){
				toReturn = toReturn + args.get(0).javanise(prefix)
				for(var i = 1; i < args.size; i++){
					toReturn = toReturn + ", " + args.get(i).javanise(prefix)
				}
			}
			return toReturn
	}
	
	def String javanise(RecordArgument argument, String prefix){
		switch(argument.value){
			VariableReference	: 	{(argument.value as VariableReference).javanise(prefix)}
			CarmaInteger		: 	{(argument.value as CarmaInteger).javanise(prefix)}	
			Range				: 	{(argument.value as Range).javanise(prefix)}	
		}
	}
	
	def String javanise(FunctionCallArguments arguments, String prefix){
			var ArrayList<FunctionArgument> args = new ArrayList<FunctionArgument>(arguments.eAllOfType(FunctionArgument))
			var toReturn = ""
			if(args.size > 0){
				toReturn = toReturn + args.get(0).javanise(prefix)
				for(var i = 1; i < args.size; i++){
					toReturn = toReturn + ", " + args.get(i).javanise(prefix)
				}
			}
			return toReturn
	}
	
	def String javanise(FunctionArgument argument, String prefix){
		switch(argument.value){
			VariableReference	: 	{(argument.value as VariableReference).javanise(prefix)}
			CarmaInteger		: 	{(argument.value as CarmaInteger).javanise(prefix)}	
			CarmaDouble			: 	{(argument.value as CarmaDouble).javanise(prefix)}
		}
	}
	
	def String javanise(OutputActionArguments arguments, String prefix){
			var ArrayList<OutputActionArgument> args = new ArrayList<OutputActionArgument>(arguments.eAllOfType(OutputActionArgument))
			var toReturn = ""
			if(args.size > 0){
				toReturn = toReturn + args.get(0).javanise(prefix)
				for(var i = 1; i < args.size; i++){
					toReturn = toReturn + ", " + args.get(i).javanise(prefix)
				}
			}
			return toReturn
	}
	
	def String javanise(OutputActionArgument argument, String prefix){
		switch(argument.value){
			VariableReference	: 	{(argument.value as VariableReference).javanise(prefix)}
			CarmaInteger		: 	{(argument.value as CarmaInteger).javanise(prefix)}	
		}
	}

	def String javanise(PreFunctionCall preFunctionCall, String prefix) {
		switch (preFunctionCall) {
			PDFunction:			{'''«prefix»pdf(«preFunctionCall.arguments.javanise(prefix)»);'''}
			UniformFunction:	{'''«prefix»uniform(«preFunctionCall.arguments.javanise(prefix)»);'''}
			CeilingFunction:	{'''«prefix»ceil(«preFunctionCall.arguments.javanise(prefix)»);'''}
			FloorFunction:		{'''«prefix»floor(«preFunctionCall.arguments.javanise(prefix)»);'''}
			MaxFunction:		{'''«prefix»max(«preFunctionCall.arguments.javanise(prefix)»);'''}
			MinFunction:		{'''«prefix»min(«preFunctionCall.arguments.javanise(prefix)»);'''}		
		}
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
}