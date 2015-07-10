package eu.quanticol.carma.core.generator.ms.environment

import eu.quanticol.carma.core.carma.Action
import eu.quanticol.carma.core.carma.CarmaBoolean
import eu.quanticol.carma.core.carma.CarmaDouble
import eu.quanticol.carma.core.carma.CarmaExponent
import eu.quanticol.carma.core.carma.CarmaInteger
import eu.quanticol.carma.core.carma.PrimitiveTypes
import eu.quanticol.carma.core.carma.Range
import eu.quanticol.carma.core.typing.BaseType
import eu.quanticol.carma.core.carma.Probability

class EnvironmentJavaniser {
	
	
	def String javanise(Probability probability){
		(Math.abs(probability.hashCode*probability.hashCode)+"").substring(0,4)
	}
		
	def int actionName(Action action){
		var toReturn = 10 * 13
		
		for(var i = 0 ; i < action.name.name.length; i++){
			toReturn = toReturn + action.name.name.charAt(i) * 13
		}
		
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
}