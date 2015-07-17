package eu.quanticol.carma.core.generator.ms

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.AbsFunction
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
import eu.quanticol.carma.core.carma.ComponentAssignment
import eu.quanticol.carma.core.carma.ComponentBlockArguments
import eu.quanticol.carma.core.carma.ComponentExpression
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
import eu.quanticol.carma.core.carma.MeasureVariableDeclaration
import eu.quanticol.carma.core.carma.MeasureVariableDeclarations
import eu.quanticol.carma.core.carma.MinFunction
import eu.quanticol.carma.core.carma.Modulo
import eu.quanticol.carma.core.carma.Multiplication
import eu.quanticol.carma.core.carma.Name
import eu.quanticol.carma.core.carma.Not
import eu.quanticol.carma.core.carma.Or
import eu.quanticol.carma.core.carma.OutputActionArgument
import eu.quanticol.carma.core.carma.PDFunction
import eu.quanticol.carma.core.carma.ParallelComposition
import eu.quanticol.carma.core.carma.PowFunction
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
import eu.quanticol.carma.core.carma.SetComp
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
import eu.quanticol.carma.core.typing.BaseType
import eu.quanticol.carma.core.typing.TypeProvider
import java.util.ArrayList

import static extension org.eclipse.xtext.EcoreUtil2.*

class SharedJavaniser {
	
	@Inject extension TypeProvider
	

	
	//MISC
	def int actionName(Action action){
		var toReturn = 10 * 13
		
		for(var i = 0 ; i < action.name.name.length; i++){
			toReturn = toReturn + action.name.name.charAt(i) * 13
		}
		
		return toReturn
	}

	def ArrayList<String> list(BooleanExpression bes){
		bes.listArguments
	}
	
	//RANGE HANDLING
	def ArrayList<ArrayList<String>> product(MeasureVariableDeclarations measureVariableDeclarations){
			var ArrayList<MeasureVariableDeclaration> args = new ArrayList<MeasureVariableDeclaration>(measureVariableDeclarations.eAllOfType(MeasureVariableDeclaration))
			var toReturn = new ArrayList<ArrayList<String>>();
			if(args.size > 0){
				toReturn.add(args.get(0).array)
				for(var i = 1; i < args.size; i++){
					toReturn.add(args.get(i).array)
				}
			}
			return toReturn
	}

	def ArrayList<String> array(MeasureVariableDeclaration measureVariableDeclaration){
		var ArrayList<String> toReturn = new ArrayList<String>();
		measureVariableDeclaration.assign.array(toReturn)
		return toReturn
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
		} else if (in.size == 1) {
			var ArrayList<String> head = in.remove(0);
			for(String item : head){
				var ArrayList<String> tail = new ArrayList<String>();
				tail.add(item);
				out.add(tail);
			}
		} 
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
			Or: 						{array.add(e.javanise)}
			And:						{array.add(e.javanise)}
			Equality: 					{array.add(e.javanise)}
			Comparison: 				{array.add(e.javanise)}
			Subtraction: 				{array.add(e.javanise)}
			Addition: 					{array.add(e.javanise)}
			Multiplication: 			{array.add(e.javanise)}
			Modulo: 					{array.add(e.javanise)}
			Division: 					{array.add(e.javanise)}
			Not: 						{array.add(e.javanise)}
			AtomicPrimitive: 			{e.array(array)}
			AtomicVariable: 			{array.add(e.javanise)}
			AtomicCalls: 				{array.add(e.javanise)}
			AtomicNow: 					{array.add(e.javanise)}
			AtomicMeasure: 				{array.add(e.javanise)}
			AtomicRecord: 				{array.add(e.javanise)}
			AtomicOutcome: 				{array.add(e.javanise)}
			AtomicProcessComposition: 	{array.add(e.javanise)}
		}

	}
	
	def void array(AtomicPrimitive e, ArrayList<String> array) {
		e.value.array(array)
	}
	
	def void array(PrimitiveTypes pts, ArrayList<String> array) {
		switch (pts) {
			CarmaDouble: {array.add(pts.javanise)}
			CarmaInteger: {array.add(pts.javanise)}
			CarmaBoolean: {array.add(pts.javanise)}
			Range: pts.array(array)
		}
	}
	
	def void array(Range pt, ArrayList<String> array) {
		for(var i = pt.min; i <= pt.max; i++){
			array.add(""+i)
		}
	}
	
	//DECLARATION
	
	def String declare(BooleanExpression bes){
		bes.declareArguments
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
	
	def String declarePrimitiveTypes(Expressions expression){
		var pts = expression.eAllOfType(PrimitiveTypes)
		
		'''
		«FOR pt : pts»
		«pt.type.javanise» «pt.disarm» = «pt.javanise»;
		«ENDFOR»
		'''
	}

	//MEASURE
	def String expressMeasure(SetComp setComp, boolean outside){
		'''getMeasure«(Math.abs(setComp.hashCode*setComp.hashCode)+"").substring(0,3)»("",«setComp.predicate.disarmArgumentsShared(outside)»).measure(this)'''
	}

	
	def String disarmArgumentsShared(BooleanExpression be, boolean outside){
		var vrs = new ArrayList<VariableReference>(be.eAllOfType(VariableReference))
		vrs = vrs.clean
		var String toReturn = ""
		if(vrs.size > 0){
			toReturn = '''«vrs.get(0).disarm(outside)»'''
			for(var i = 1; i < vrs.size; i++){
				toReturn = '''«toReturn»,«vrs.get(i).disarm(outside)»'''
			}
		}
		var primitives = be.eAllOfType(PrimitiveTypes)
		if(primitives.size > 0){
			if(toReturn.length > 0)
				toReturn = '''«toReturn», «primitives.get(0).disarm»'''
			else 
				toReturn = '''«primitives.get(0).disarm»'''
			for(var i = 1; i < primitives.size; i++){
				toReturn = '''«toReturn», «primitives.get(i).disarm»'''
			}
		}
		return toReturn
	}
	
	def String disarmArgumentsSharedString(BooleanExpression be, boolean outside){
		var vrs = new ArrayList<VariableReference>(be.eAllOfType(VariableReference))
		vrs = vrs.clean
		var String toReturn = ""
		if(vrs.size > 0){
			toReturn = '''«vrs.get(0).disarm(outside)»'''
			for(var i = 1; i < vrs.size; i++){
				toReturn = '''«toReturn»+" ; "+«vrs.get(i).disarm(outside)»'''
			}
		}
		var primitives = be.eAllOfType(PrimitiveTypes)
		if(primitives.size > 0){
			if(toReturn.length > 0)
				toReturn = '''«toReturn»+" ; "+«primitives.get(0).disarm»'''
			else 
				toReturn = '''«primitives.get(0).disarm»'''
			for(var i = 1; i < primitives.size; i++){
				toReturn = '''«toReturn»+" ; "+«primitives.get(i).disarm»'''
			}
		}
		return toReturn
	}
	
	def  String declareArguments(BooleanExpression be){
		var vrs = new ArrayList<VariableReference>(be.eAllOfType(VariableReference))
		vrs = vrs.clean
		var String toReturn = ""
		if(vrs.size > 0){
			toReturn = '''Double «vrs.get(0).disarm(true)»;'''
			for(var i = 1; i < vrs.size; i++){
				toReturn = 
				'''
				«toReturn»
				Double «vrs.get(i).disarm(true)»;'''
							}
						}
						var primitives = be.eAllOfType(PrimitiveTypes)
						if(primitives.size > 0){
							if(toReturn.length > 0)
								toReturn = 
				'''
				«toReturn»
				«primitives.get(0).declare»;'''
							else 
								toReturn = 
				'''«primitives.get(0).declare»;'''
			for(var i = 1; i < primitives.size; i++){
				toReturn = '''«toReturn», «primitives.get(i).declare»'''
			}
		}
		return toReturn
	}
	
	def  ArrayList<String> listArguments(BooleanExpression be){
		var vrs = new ArrayList<VariableReference>(be.eAllOfType(VariableReference))
		vrs = vrs.clean
		var ArrayList<String> toReturn = new ArrayList<String>()
		if(vrs.size > 0){
			toReturn.add(vrs.get(0).disarm(true))
			for(var i = 1; i < vrs.size; i++){
				toReturn.add(vrs.get(i).disarm(true))
			}
		}
		var primitives = be.eAllOfType(PrimitiveTypes)
		if(primitives.size > 0){
			toReturn.add(primitives.get(0).declare)
			for(var i = 1; i < primitives.size; i++){
				toReturn.add(primitives.get(i).declare)
			}
		}
		return toReturn
	}
	
	def  String disarm(VariableReference vr, boolean outside) {
		switch (vr) {
			VariableReferencePure: 		"attrib_"	+vr.name.name
			VariableReferenceMy: 		"my_"		+vr.name.name
			VariableReferenceReceiver: 	"receiver_"	+vr.name.name
			VariableReferenceSender: 	"sender_"	+vr.name.name
			VariableReferenceGlobal: 	"global_"	+vr.name.name
			RecordReferencePure: 		"attrib_"	+vr.name.name//if(outside){"attrib_"+vr.name.name+"."+vr.feild.name}else{"attrib_"+vr.name.name + "_" + vr.feild.name}
			RecordReferenceMy: 			"my_"		+vr.name.name//if(outside){"my_"+vr.name.name+"."+vr.feild.name}else{"my_"+vr.name.name + "_" + vr.feild.name}
			RecordReferenceReceiver: 	"receiver_"	+vr.name.name//if(outside){"receiver_"+vr.name.name+"."+vr.feild.name}else{"receiver_"+vr.name.name + "_" + vr.feild.name}
			RecordReferenceSender: 		"sender_"	+vr.name.name//if(outside){"sender_"+vr.name.name+"."+vr.feild.name}else{"sender_"+vr.name.name + "_" + vr.feild.name}
			RecordReferenceGlobal: 		"global_"	+vr.name.name//if(outside){"global_"+vr.name.name+"."+vr.feild.name}else{"global_"+vr.name.name + "_" + vr.feild.name}
		}
	}
	
	def  String disarm(PrimitiveTypes pts) {
		switch (pts) {
			CarmaDouble: 	"double_"+(Math.abs(pts.hashCode*pts.hashCode)+"").substring(0,4)
			CarmaInteger: 	"integer_"+(Math.abs(pts.hashCode*pts.hashCode)+"").substring(0,4)
			CarmaBoolean: 	"boolean_"+(Math.abs(pts.hashCode*pts.hashCode)+"").substring(0,4)
			Range: 			"integer_"+(Math.abs(pts.hashCode*pts.hashCode)+"").substring(0,4)
		}
	}
	
	def  String declare(PrimitiveTypes pts) {
		switch (pts) {
			CarmaDouble: 	"Double double_"+(Math.abs(pts.hashCode*pts.hashCode)+"").substring(0,4)+"="+pts.javanise
			CarmaInteger: 	"Double integer_"+(Math.abs(pts.hashCode*pts.hashCode)+"").substring(0,4)+"="+pts.javanise
			CarmaBoolean: 	"boolean boolean_"+(Math.abs(pts.hashCode*pts.hashCode)+"").substring(0,4)+"="+pts.javanise
			Range: 			"Double integer_"+(Math.abs(pts.hashCode*pts.hashCode)+"").substring(0,4)+"="+pts.javanise
		}
	}
	
	/**
	 * Get rid of all the "my." because these will be accessed INSIDE the measure function
	 */
	def  ArrayList<VariableReference> clean(ArrayList<VariableReference> dirty){
		var ArrayList<VariableReference> toReturn = new ArrayList<VariableReference>()
		var list = newArrayList
		for(vr : dirty)
			switch (vr) {
				VariableReferencePure: 		{if(!list.contains("attrib_"+vr.name.name)){toReturn.add(vr) 	list.add("attrib_"+vr.name.name)}}
				VariableReferenceReceiver: 	{if(!list.contains("receiver_"+vr.name.name)){toReturn.add(vr) 	list.add("receiver_"+vr.name.name)}}
				VariableReferenceSender: 	{if(!list.contains("sender_"+vr.name.name)){toReturn.add(vr) 	list.add("sender_"+vr.name.name)}}
				VariableReferenceGlobal: 	{if(!list.contains("global_"+vr.name.name)){toReturn.add(vr) 	list.add("global_"+vr.name.name)}}
				RecordReferencePure: 		{if(!list.contains("attrib_"+vr.name.name)){toReturn.add(vr) 	list.add("attrib_"+vr.name.name)}}
				RecordReferenceReceiver: 	{if(!list.contains("receiver_"+vr.name.name)){toReturn.add(vr) 	list.add("receiver_"+vr.name.name)}}
				RecordReferenceSender: 		{if(!list.contains("sender_"+vr.name.name)){toReturn.add(vr) 	list.add("sender_"+vr.name.name)}}
				RecordReferenceGlobal: 		{if(!list.contains("global_"+vr.name.name)){toReturn.add(vr) 	list.add("global_"+vr.name.name)}}
			}
		return toReturn
	}

	//PARAMETER HANDLING
	def ArrayList<VariableReference> reverseClean(ArrayList<VariableReference> dirty){
		var ArrayList<VariableReference> toReturn = new ArrayList<VariableReference>()
		for(vr : dirty)
			switch (vr) {
				VariableReferenceMy: 		toReturn.add(vr)
				RecordReferenceMy: 			toReturn.add(vr)
			}
		return toReturn
	}
	
	def String disarmString(BooleanExpression bes){
		bes.disarmArgumentsSharedString(false)
	}
	
	def String disarmOut(BooleanExpression bes){
		bes.disarmArgumentsShared(false)
	}
	
	def String disarmParameters(BooleanExpression be){
		var vrs = new ArrayList<VariableReference>(be.eAllOfType(VariableReference))
		vrs = vrs.clean
		var String toReturn = ""
		if(vrs.size > 0){
			toReturn = '''final «vrs.get(0).type.javanise» «vrs.get(0).disarm(false)»'''
			for(var i = 1; i < vrs.size; i++){
				toReturn = '''«toReturn», final «vrs.get(i).type.javanise» «vrs.get(i).disarm(false)»'''
			}
		}
		var primitives = be.eAllOfType(PrimitiveTypes)
		if(primitives.size > 0){
			if(toReturn.length > 0)
				toReturn = '''«toReturn», final «primitives.get(0).type.javanise» «primitives.get(0).disarm»'''
			else 
				toReturn = '''final «primitives.get(0).type.javanise» «primitives.get(0).disarm»'''
			for(var i = 1; i < primitives.size; i++){
				toReturn = '''«toReturn», final «primitives.get(i).type.javanise» «primitives.get(i).disarm»'''
			}
		}
		return toReturn
	}
	
	//SHARED JAVANISE
	def dispatch String javanise(ArrayList<String> behaviours){
		var String toReturn = ""
		if(behaviours.size > 0){
			toReturn = '''"«behaviours.get(0)»"'''
			for(var i = 1; i < behaviours.size; i++)
				toReturn = '''«toReturn», "«behaviours.get(i)»"'''
		}
		return toReturn
	}
	def dispatch String javanise(OutputActionArgument oaa){
		switch (oaa.value) {
			VariableReferenceMy: 		(oaa.value as VariableReference).javanise
			RecordReferenceMy: 			(oaa.value as VariableReference).javanise
			CarmaInteger:				(oaa.value as CarmaInteger).javanise
		}
	}
	def dispatch String javanise(EnvironmentUpdate update){
		(Math.abs(update.hashCode*update.hashCode)+"").substring(0,4)
	}
		
	def dispatch String javanise(Probability probability){
		(Math.abs(probability.hashCode*probability.hashCode)+"").substring(0,4)
	}
	
	def dispatch String javanise(Rate rate){
		(Math.abs(rate.hashCode*rate.hashCode)+"").substring(0,4)
	}
	def dispatch String javanise(BaseType bt){
		if(bt.me.equals("int")){
			'''Double'''
		}else if(bt.me.equals("double")){
			'''Double'''
		}else if(bt.me.equals("boolean")){
			'''boolean'''
		} else {
			'''«bt.me»'''
		}
	}
	def dispatch String javanise(SetComp setComp){
		(Math.abs(setComp.hashCode*setComp.hashCode)+"").substring(0,3)
	}	
	def dispatch String javanise(Types types){
		types.type.javanise
	}
	
	def dispatch String javanise(Type type){
		switch(type){
			DoubleType: "Double"
			IntgerType: "Double"
			AttribType: "Double"
			RecordType: type.name.name
		}
	}
	
	def dispatch String javanise(Expressions e) {
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
			AtomicOutcome: 				{e.javanise}
			AtomicProcessComposition:	{e.javanise}
		}
	}
	
	def dispatch String javanise(Or e) {
		'''(«e.left.javanise» || «e.right.javanise»)'''
	}

	def dispatch String javanise(And e) {
		'''(«e.left.javanise» && «e.right.javanise»)'''
	}

	def dispatch String javanise(Equality e) {
		'''(«e.left.javanise» «e.op» «e.right.javanise»)'''
	}

	def dispatch String javanise(Comparison e) {
		'''(«e.left.javanise» «e.op» «e.right.javanise»)'''
	}

	def dispatch String javanise(Subtraction e) {
		'''(«e.left.javanise» - «e.right.javanise»)'''
	}

	def dispatch String javanise(Addition e) {
		'''(«e.left.javanise» + «e.right.javanise»)'''
	}

	def dispatch String javanise(Multiplication e) {
		'''(«e.left.javanise» * «e.right.javanise»)'''
	}

	def dispatch String javanise(Modulo e) {
		'''(«e.left.javanise» % «e.right.javanise»)'''
	}

	def dispatch String javanise(Division e) {
		'''(«e.left.javanise» / «e.right.javanise»)'''
	}

	def dispatch String javanise(Not e) {
		'''!(«e.expression.javanise»)'''
	}
	
	def dispatch String javanise(AtomicPrimitive e) {
		e.value.javanise
	}

	def dispatch String javanise(AtomicVariable expression) {
		expression.value.javanise
	}

	def dispatch String javanise(AtomicCalls expression) {
		expression.value.javanise
	}

	def dispatch String javanise(PrimitiveTypes pts) {
		switch (pts) {
			CarmaDouble: pts.javanise
			CarmaInteger: pts.javanise
			CarmaBoolean: pts.javanise
			Range: pts.javanise
		}
	}
	
	def dispatch String javanise(CarmaDouble pt) {
		var String toReturn = ""
		if (pt.negative != null)
			toReturn = toReturn + "-"
		toReturn = toReturn + pt.left + "." + pt.right
		if (pt.exponent != null)
			toReturn = toReturn + pt.exponent.javanise
		return toReturn
	}

	def dispatch String javanise(CarmaExponent exp) {
		var String negative = ""
		if (exp.negative != null)
			negative = "-"
		''' * «negative» Math.pow(«exp.base»,«exp.exponent»)'''
	}	
	
	def dispatch String javanise(CarmaInteger pt) {
		if (pt.negative != null)
			return "-" + pt.value
		else
			return "" + pt.value
	}

	def dispatch String javanise(CarmaBoolean pt) {
		'''«pt.value»'''
	}
	
	def dispatch String javanise(Range pt) {
		var size = pt.max - pt.min
		if(size < 0){
			''''''
		} else if (size == 0) {
			'''
			{«pt.max»}
			'''
		} else {
			''' 
			{ «(pt.min..pt.max)» }
			'''
		}
	}
	
	def dispatch String javanise(VariableReference vr) {
		switch (vr) {
			VariableReferencePure: 		"attrib_"+vr.name.name
			VariableReferenceMy: 		"my_"+vr.name.name
			VariableReferenceReceiver: 	"receiver_"+vr.name.name
			VariableReferenceSender: 	"sender_"+vr.name.name
			VariableReferenceGlobal: 	"global_"+vr.name.name
			RecordReferencePure: 		"attrib_"+vr.name.name + "." + vr.feild.name
			RecordReferenceMy: 			"my_"+vr.name.name + "." + vr.feild.name
			RecordReferenceReceiver: 	"receiver_"+vr.name.name + "." + vr.feild.name
			RecordReferenceSender: 		"sender_"+vr.name.name + "." + vr.feild.name
			RecordReferenceGlobal: 		"global_"+vr.name.name + "." + vr.feild.name
		}
	}
	
	def dispatch String javanise(Calls calls) {
		switch (calls) {
			FunctionReferenceMan: calls.ref.javanise
			FunctionReferencePre: calls.ref.javanise
		}
	}

	def dispatch String javanise(FunctionCall functionCall) {
		'''
			«(functionCall.name as Name).name.toLowerCase»(«(functionCall.arguments as FunctionCallArguments).javanise»)
		'''
	}
	
	def dispatch String javanise(FunctionCallArguments arguments){
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
	
	def dispatch String javanise(FunctionArgument argument){
		argument.value.javanise
	}
	
	def dispatch String javanise(PreFunctionCall preFunctionCall) {
		switch (preFunctionCall) {
			PDFunction:			{'''pdf(«(preFunctionCall.arguments as PredFunctionCallArguments).javanise»)'''}
			UniformFunction:	{'''uniform(«(preFunctionCall.arguments as PredFunctionCallArguments).javanise»)'''}
			CeilingFunction:	{'''ceil(«(preFunctionCall.arguments as PredFunctionCallArguments).javanise»)'''}
			FloorFunction:		{'''floor(«(preFunctionCall.arguments as PredFunctionCallArguments).javanise»)'''}
			MaxFunction:		{'''max(«(preFunctionCall.arguments as PredFunctionCallArguments).javanise»)'''}
			MinFunction:		{'''min(«(preFunctionCall.arguments as PredFunctionCallArguments).javanise»)'''}
			PowFunction:		{'''pow(«(preFunctionCall.arguments as PredFunctionCallArguments).javanise»)'''}
			AbsFunction:		{'''abs(«(preFunctionCall.arguments as PredFunctionCallArguments).javanise»)'''}
		}
	}
	
	def dispatch String javanise(PredFunctionCallArguments arguments){
			var ArrayList<PreArgument> args = new ArrayList<PreArgument>(arguments.eAllOfType(PreArgument))
			var toReturn = '''new ArrayList<Double>(Arrays.asList('''
			if(args.size > 0){
				toReturn = toReturn + '''«args.get(0).javanise»'''
				for(var i = 1; i < args.size; i++){
					toReturn = toReturn + ''', «args.get(i).javanise»'''
				}
			}
			return toReturn + "))"
	}
	
	def dispatch String javanise(PreArgument argument){
		argument.value.javanise
	}
	
	def dispatch String javanise(AtomicNow expression){
		'''now()'''
	}
	
	def dispatch String javanise(AtomicMeasure expression){
		'''«expression.value.expressMeasure(true)»'''
	}
	
	def dispatch String javanise(AtomicRecord expression){
		var instance = (expression.value as InstantiateRecord)
		'''new «(instance.type as Type).javanise» ( «(instance.arguments as RecordArguments).javanise» )'''
	}
	
	def dispatch String javanise(RecordArguments arguments){
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
	
	def dispatch String javanise(RecordArgument argument){
		argument.value.javanise
	}
	
	def dispatch String javanise(ProcessComposition processComposition){
		switch(processComposition){
			ParallelComposition	: 	'''«processComposition.left.javanise», «processComposition.right.javanise»'''
			ProcessReference	:	'''"«processComposition.expression.name»"'''
		}
	}
	
	def dispatch String javanise(AtomicProcessComposition expression){
		'''new ArrayList<String>(Arrays.asList( «expression.value.javanise» ))'''
	}
	
	def dispatch String javanise(ComponentAssignment componentAssignment){
		'''«componentAssignment.reference.javanise» = «componentAssignment.expression.express»'''
	}
	
	def void array(ProcessComposition processComposition, ArrayList<String> array){
		switch(processComposition){
			ParallelComposition	: 	{processComposition.left.array(array) processComposition.right.array(array)}
			ProcessReference	:	array.add(processComposition.expression.name)
		}
	}
	
	//EXPRESS
	def dispatch String express(ComponentExpression functionExpression) {
		functionExpression.expression.express
	}
	def dispatch String express(EnvironmentProbExpression functionExpression) {
		functionExpression.expression.express
	}

	def dispatch String express(EnvironmentRateExpression functionExpression) {
		functionExpression.expression.express
	}

	def dispatch String express(EnvironmentUpdateExpression functionExpression) {
		functionExpression.expression.express
	}
	def dispatch String express(FunctionExpression functionExpression) {
		functionExpression.expression.express
	}
	
	def dispatch String express(BooleanExpression functionExpression) {
		functionExpression.expression.express
	}
	
	def dispatch String express(UpdateExpression functionExpression) {
		functionExpression.expression.express
	}
	def dispatch String express(Types types){
		types.type.express
	}
	
	def dispatch String express(Type type){
		switch(type){
			DoubleType: "Double"
			IntgerType: "Double"
			AttribType: "Double"
			RecordType: type.name.name
		}
	}



	def dispatch String express(Expressions e) {
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
			AtomicOutcome: 				{e.express}
			AtomicProcessComposition:	{e.express}
		}

	}

	def dispatch String express(Or e) {
		'''(«e.left.express» || «e.right.express»)'''
	}

	def dispatch String express(And e) {
		'''(«e.left.express» && «e.right.express»)'''
	}

	def dispatch String express(Equality e) {
		'''(«e.left.express» «e.op» «e.right.express»)'''
	}

	def dispatch String express(Comparison e) {
		'''(«e.left.express» «e.op» «e.right.express»)'''
	}

	def dispatch String express(Subtraction e) {
		'''(«e.left.express» - «e.right.express»)'''
	}

	def dispatch String express(Addition e) {
		'''(«e.left.express» + «e.right.express»)'''
	}

	def dispatch String express(Multiplication e) {
		'''(«e.left.express» * «e.right.express»)'''
	}

	def dispatch String express(Modulo e) {
		'''(«e.left.express» % «e.right.express»)'''
	}

	def dispatch String express(Division e) {
		'''(«e.left.express» / «e.right.express»)'''
	}

	def dispatch String express(Not e) {
		'''!(«e.expression.express»)'''
	}

	def dispatch String express(AtomicPrimitive e) {
		e.value.express
	}

	def dispatch String express(PrimitiveTypes pts) {
		switch (pts) {
			CarmaDouble: pts.express
			CarmaInteger: pts.express
			CarmaBoolean: pts.express
			Range: pts.express
		}
	}

	def dispatch String express(CarmaDouble pt) {
		var String toReturn = ""
		if (pt.negative != null)
			toReturn = toReturn + "-"
		toReturn = toReturn + pt.left + "." + pt.right
		if (pt.exponent != null)
			toReturn = toReturn + pt.exponent.express
		return toReturn
	}

	def dispatch String express(CarmaExponent exp) {
		var String negative = ""
		if (exp.negative != null)
			negative = "-"
		''' * «negative» Math.pow(«exp.base»,«exp.exponent»)'''
	}

	def dispatch String express(CarmaInteger pt) {
		if (pt.negative != null)
			return "-" + pt.value
		else
			return "" + pt.value
	}

	def dispatch String express(CarmaBoolean pt) {
		'''«pt.value»'''
	}

	def dispatch String express(AtomicVariable expression) {
		expression.value.express
	}

	def dispatch String express(VariableReference vr) {
		switch (vr) {
			VariableReferencePure: 		"attrib_"+vr.name.name
			VariableReferenceMy: 		"my_"+vr.name.name
			VariableReferenceReceiver: 	"receiver_"+vr.name.name
			VariableReferenceSender: 	"sender_"+vr.name.name
			VariableReferenceGlobal: 	"global_"+vr.name.name
			RecordReferencePure: 		"attrib_"+vr.name.name + "." + vr.feild.name
			RecordReferenceMy: 			"my_"+vr.name.name + "." + vr.feild.name
			RecordReferenceReceiver: 	"receiver_"+vr.name.name + "." + vr.feild.name
			RecordReferenceSender: 		"sender_"+vr.name.name + "." + vr.feild.name
			RecordReferenceGlobal: 		"global_"+vr.name.name + "." + vr.feild.name
		}
	}

	def dispatch String express(AtomicCalls expression) {
		expression.value.express
	}

	def dispatch String express(Calls calls) {
		switch (calls) {
			FunctionReferenceMan: calls.ref.express
			FunctionReferencePre: calls.ref.express
		}
	}

	def dispatch String express(FunctionCall functionCall) {
		'''«(functionCall.name as Name).name.toFirstLower»(«(functionCall.arguments as FunctionCallArguments).express»)'''
	}
	
	def dispatch String express(FunctionCallArguments arguments){
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
	
	def dispatch String express(FunctionArgument argument){
		argument.value.express
	}
	
	def dispatch String express(PreFunctionCall preFunctionCall) {
		switch (preFunctionCall) {
			PDFunction:			{'''pdf(«(preFunctionCall.arguments as PredFunctionCallArguments).express»)'''}
			UniformFunction:	{'''uniform(«(preFunctionCall.arguments as PredFunctionCallArguments).express»)'''}
			CeilingFunction:	{'''ceil(«(preFunctionCall.arguments as PredFunctionCallArguments).express»)'''}
			FloorFunction:		{'''floor(«(preFunctionCall.arguments as PredFunctionCallArguments).express»)'''}
			MaxFunction:		{'''max(«(preFunctionCall.arguments as PredFunctionCallArguments).express»)'''}
			MinFunction:		{'''min(«(preFunctionCall.arguments as PredFunctionCallArguments).express»)'''}	
			PowFunction:		{'''pow(«(preFunctionCall.arguments as PredFunctionCallArguments).express»)'''}
			AbsFunction:		{'''abs(«(preFunctionCall.arguments as PredFunctionCallArguments).express»)'''}	
		}
	}
	
	def dispatch String express(PredFunctionCallArguments arguments){
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
	
	def dispatch String express(PreArgument argument){
		argument.value.express
	}
	
	def dispatch String express(AtomicNow expression){
		'''now()'''
	}
	
	def dispatch String express(AtomicMeasure expression){
		'''«expression.value.expressMeasure(true)»'''
	}
	
	def dispatch String express(AtomicRecord expression){
		var instance = (expression.value as InstantiateRecord)
		'''new «(instance.type as Type).express» ( «(instance.arguments as RecordArguments).express» )'''
	}
	
	def dispatch String express(RecordArguments arguments){
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
	
	def dispatch String express(RecordArgument argument){
		argument.value.express
	}
	
	def String storeExpress(BaseType bt){
		if(bt.me.equals("int")){
			'''Double.class'''
		} else {
			'''«bt.me».class'''
		}
	}
	
	def dispatch String express(BaseType bt){
		if(bt.me.equals("int")){
			'''Double'''
		} else {
			'''«bt.me»'''
		}
	}
	
}