package eu.quanticol.carma.core.generator.ms.record

import eu.quanticol.carma.core.carma.AttribParameter
import eu.quanticol.carma.core.carma.AttribType
import eu.quanticol.carma.core.carma.CarmaInteger
import eu.quanticol.carma.core.carma.DoubleType
import eu.quanticol.carma.core.carma.FeildDeclaration
import eu.quanticol.carma.core.carma.IntgerType
import eu.quanticol.carma.core.carma.RecordReferenceGlobal
import eu.quanticol.carma.core.carma.RecordReferenceMy
import eu.quanticol.carma.core.carma.RecordReferencePure
import eu.quanticol.carma.core.carma.RecordReferenceReceiver
import eu.quanticol.carma.core.carma.RecordReferenceSender
import eu.quanticol.carma.core.carma.RecordType
import eu.quanticol.carma.core.carma.Type
import eu.quanticol.carma.core.carma.VariableReference
import eu.quanticol.carma.core.carma.VariableReferenceGlobal
import eu.quanticol.carma.core.carma.VariableReferenceMy
import eu.quanticol.carma.core.carma.VariableReferencePure
import eu.quanticol.carma.core.carma.VariableReferenceReceiver
import eu.quanticol.carma.core.carma.VariableReferenceSender
import java.util.ArrayList

import static extension org.eclipse.xtext.EcoreUtil2.*

class RecordJavaniser {
	
	def String javanise(Type type){
		switch(type){
			DoubleType: "double"
			IntgerType: "int"
			AttribType: "int"
			RecordType: type.name.name
		}
	}

	def String getParameter(AttribParameter parameter){
		'''«(parameter.type as AttribType).javanise» «parameter.name.name»'''
	}	
	
	def String getParameters(ArrayList<AttribParameter> parameters){
		var String toReturn = ""
		if(parameters.size > 0){
			toReturn = parameters.get(0).getParameter
			for(var i = 1; i < parameters.size; i++){
				toReturn = toReturn + ", " + parameters.get(i).getParameter
			}
		}
		return toReturn
	}
	
	def String declare(FeildDeclaration feildDeclaration){
		'''«feildDeclaration.type.javanise» «feildDeclaration.name.name»;'''
	}

	def String javanise(FeildDeclaration feildDeclaration){
		if(feildDeclaration.eAllOfType(VariableReference).size > 0)
			'''this.«feildDeclaration.name.name» = «(feildDeclaration.assign as VariableReference).javanise»;'''
		else
			'''this.«feildDeclaration.name.name» = «(feildDeclaration.assign as CarmaInteger).javanise»;'''
	}
	
	def String javanise(VariableReference variableReference){
		switch (variableReference) {
			VariableReferencePure: 		variableReference.name.name
			VariableReferenceMy: 		variableReference.name.name
			VariableReferenceReceiver: 	variableReference.name.name
			VariableReferenceSender: 	variableReference.name.name
			VariableReferenceGlobal: 	variableReference.name.name
			RecordReferencePure: 		variableReference.name.name + "." + variableReference.feild.name
			RecordReferenceMy: 			variableReference.name.name + "." + variableReference.feild.name
			RecordReferenceReceiver: 	variableReference.name.name + "." + variableReference.feild.name
			RecordReferenceSender: 		variableReference.name.name + "." + variableReference.feild.name
			RecordReferenceGlobal: 		variableReference.name.name + "." + variableReference.feild.name
		}
	}
	
	def String javanise(CarmaInteger carmaInteger){
		if (carmaInteger.negative != null)
			return "-" + carmaInteger.value
		else
			return "" + carmaInteger.value
	}

}