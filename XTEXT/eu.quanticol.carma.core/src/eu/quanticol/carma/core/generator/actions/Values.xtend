package eu.quanticol.carma.core.generator.actions

import eu.quanticol.carma.core.carma.OutputActionArguments

class Values {
	
	
	def String getValues(OutputActionArguments valueBlock){
		'''
		@Override
		protected Object getValue(CarmaStore store) {
			«valueBlock»
		}
		'''
	}
}