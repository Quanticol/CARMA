package eu.quanticol.carma.core.generator.ms

import eu.quanticol.carma.core.carma.RecordReferenceGlobal
import eu.quanticol.carma.core.carma.RecordReferenceMy
import eu.quanticol.carma.core.carma.RecordReferencePure
import eu.quanticol.carma.core.carma.RecordReferenceReceiver
import eu.quanticol.carma.core.carma.RecordReferenceSender
import eu.quanticol.carma.core.carma.VariableReference
import eu.quanticol.carma.core.carma.VariableReferenceGlobal
import eu.quanticol.carma.core.carma.VariableReferenceMy
import eu.quanticol.carma.core.carma.VariableReferencePure
import eu.quanticol.carma.core.carma.VariableReferenceReceiver
import eu.quanticol.carma.core.carma.VariableReferenceSender
import eu.quanticol.carma.core.carma.CarmaInteger
import eu.quanticol.carma.core.carma.Range

class Javaniser {
	
	def String javanise(VariableReference vr){
		switch(vr){
			VariableReferencePure: 		vr.name.name
			VariableReferenceMy:		vr.name.name
			VariableReferenceReceiver:	vr.name.name
			VariableReferenceSender:	vr.name.name
			VariableReferenceGlobal:	vr.name.name
			RecordReferencePure:		vr.name.name+"_"+vr.feild.name
			RecordReferenceMy:			vr.name.name+"_"+vr.feild.name
			RecordReferenceReceiver:	vr.name.name+"_"+vr.feild.name
			RecordReferenceSender:		vr.name.name+"_"+vr.feild.name
			RecordReferenceGlobal:		vr.name.name+"_"+vr.feild.name
		}
	}
	
	def String javanise(CarmaInteger pt){
		if(pt.negative != null) "-" + pt.value
		return "this is a basic string"
	}
	
	def String javanise(Range pt){
		"NaN @ eu.quanticol.carma.core.generator.ms.Javaniser.javanise.Range"
	}
	
}