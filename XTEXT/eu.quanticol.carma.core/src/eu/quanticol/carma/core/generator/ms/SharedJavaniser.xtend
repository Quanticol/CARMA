package eu.quanticol.carma.core.generator.ms

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.AttribType
import eu.quanticol.carma.core.carma.BooleanExpression
import eu.quanticol.carma.core.carma.CarmaBoolean
import eu.quanticol.carma.core.carma.CarmaDouble
import eu.quanticol.carma.core.carma.CarmaInteger
import eu.quanticol.carma.core.carma.DoubleType
import eu.quanticol.carma.core.carma.Expressions
import eu.quanticol.carma.core.carma.IntgerType
import eu.quanticol.carma.core.carma.PrimitiveTypes
import eu.quanticol.carma.core.carma.Range
import eu.quanticol.carma.core.carma.RecordReferenceGlobal
import eu.quanticol.carma.core.carma.RecordReferenceMy
import eu.quanticol.carma.core.carma.RecordReferencePure
import eu.quanticol.carma.core.carma.RecordReferenceReceiver
import eu.quanticol.carma.core.carma.RecordReferenceSender
import eu.quanticol.carma.core.carma.RecordType
import eu.quanticol.carma.core.carma.SetComp
import eu.quanticol.carma.core.carma.Type
import eu.quanticol.carma.core.carma.VariableReference
import eu.quanticol.carma.core.carma.VariableReferenceGlobal
import eu.quanticol.carma.core.carma.VariableReferenceMy
import eu.quanticol.carma.core.carma.VariableReferencePure
import eu.quanticol.carma.core.carma.VariableReferenceReceiver
import eu.quanticol.carma.core.carma.VariableReferenceSender
import eu.quanticol.carma.core.typing.TypeProvider
import java.util.ArrayList

import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.core.typing.BaseType
import eu.quanticol.carma.core.carma.CarmaExponent

class SharedJavaniser {
	
	@Inject extension TypeProvider
	
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
	
	def String declareArguments(BooleanExpression be){
		var vrs = new ArrayList<VariableReference>(be.eAllOfType(VariableReference))
		vrs = vrs.clean
		var String toReturn = ""
		if(vrs.size > 0){
			toReturn = '''int «vrs.get(0).disarm(true)»;'''
			for(var i = 1; i < vrs.size; i++){
				toReturn = 
'''«toReturn»
int «vrs.get(i).disarm(true)»;'''
			}
		}
		var primitives = be.eAllOfType(PrimitiveTypes)
		if(primitives.size > 0){
			if(toReturn.length > 0)
				toReturn = 
'''«toReturn»
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
	
	def ArrayList<String> listArguments(BooleanExpression be){
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
	
	def String disarm(VariableReference vr, boolean outside) {
		switch (vr) {
			VariableReferencePure: 		"input_"	+vr.name.name
			VariableReferenceMy: 		"my_"		+vr.name.name
			VariableReferenceReceiver: 	"receiver_"	+vr.name.name
			VariableReferenceSender: 	"sender_"	+vr.name.name
			VariableReferenceGlobal: 	"global_"	+vr.name.name
			RecordReferencePure: 		if(outside){"input_"+vr.name.name+"."+vr.feild.name}else{"input_"+vr.name.name + "_" + vr.feild.name}
			RecordReferenceMy: 			if(outside){"my_"+vr.name.name+"."+vr.feild.name}else{"my_"+vr.name.name + "_" + vr.feild.name}
			RecordReferenceReceiver: 	if(outside){"receiver_"+vr.name.name+"."+vr.feild.name}else{"receiver_"+vr.name.name + "_" + vr.feild.name}
			RecordReferenceSender: 		if(outside){"sender_"+vr.name.name+"."+vr.feild.name}else{"sender_"+vr.name.name + "_" + vr.feild.name}
			RecordReferenceGlobal: 		if(outside){"global_"+vr.name.name+"."+vr.feild.name}else{"global_"+vr.name.name + "_" + vr.feild.name}
		}
	}
	
	def String disarm(PrimitiveTypes pts) {
		switch (pts) {
			CarmaDouble: 	"double_"+(Math.abs(pts.hashCode*pts.hashCode)+"").substring(0,4)
			CarmaInteger: 	"integer_"+(Math.abs(pts.hashCode*pts.hashCode)+"").substring(0,4)
			CarmaBoolean: 	"boolean_"+(Math.abs(pts.hashCode*pts.hashCode)+"").substring(0,4)
			Range: 			"integer_"+(Math.abs(pts.hashCode*pts.hashCode)+"").substring(0,4)
		}
	}
	
	def String declare(PrimitiveTypes pts) {
		switch (pts) {
			CarmaDouble: 	"double double_"+(Math.abs(pts.hashCode*pts.hashCode)+"").substring(0,4)+"="+pts.javanise
			CarmaInteger: 	"int integer_"+(Math.abs(pts.hashCode*pts.hashCode)+"").substring(0,4)+"="+pts.javanise
			CarmaBoolean: 	"boolean boolean_"+(Math.abs(pts.hashCode*pts.hashCode)+"").substring(0,4)+"="+pts.javanise
			Range: 			"int integer_"+(Math.abs(pts.hashCode*pts.hashCode)+"").substring(0,4)+"="+pts.javanise
		}
	}
	
	/**
	 * Get rid of all the "my." because these will be accessed INSIDE the measure function
	 */
	def ArrayList<VariableReference> clean(ArrayList<VariableReference> dirty){
		var ArrayList<VariableReference> toReturn = new ArrayList<VariableReference>()
		for(vr : dirty)
			switch (vr) {
				VariableReferencePure: 		toReturn.add(vr)
				VariableReferenceReceiver: 	toReturn.add(vr)
				VariableReferenceSender: 	toReturn.add(vr)
				VariableReferenceGlobal: 	toReturn.add(vr)
				RecordReferencePure: 		toReturn.add(vr)
				RecordReferenceReceiver: 	toReturn.add(vr)
				RecordReferenceSender: 		toReturn.add(vr)
				RecordReferenceGlobal: 		toReturn.add(vr)
			}
		return toReturn
	}
	
	def String declarePrimitiveTypes(Expressions expression){
		var pts = expression.eAllOfType(PrimitiveTypes)
		
		'''
		«FOR pt : pts»
		«pt.type.javanise» «pt.disarm» = «pt.javanise»;
		«ENDFOR»
		'''
	}
	
	def String javanise(BaseType bt){
		if(bt.me.equals("int")){
			'''int'''
		}else if(bt.me.equals("double")){
			'''double'''
		}else if(bt.me.equals("boolean")){
			'''boolean'''
		} else {
			'''«bt.me»'''
		}
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
}