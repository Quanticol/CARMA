package eu.quanticol.carma.core.generator.actions

class Values {
	
	
	def String getValues(String valueBlock){
		'''
		@Override
		protected Object getValue(CarmaStore store) {
			«valueBlock»
		}
		'''
	}
}