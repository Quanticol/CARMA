package eu.quanticol.carma.core.generator.actions

class Updates {
	
	def String getOutputUpdate(String updateBlock){
		'''
		@Override
		protected CarmaStoreUpdate getUpdate() {
			return new CarmaStoreUpdate() {
				
				@Override
				public void update(RandomGenerator r, CarmaStore store) {
					«updateBlock»
				}
			};
		}
		'''
	}
	
	def String getInputUpdate(String updateBlock){
		'''
		@Override
		protected CarmaStoreUpdate getUpdate(final Object value) {
			
			return new CarmaStoreUpdate() {
				@Override
				public void update(RandomGenerator r, CarmaStore store) {
					if (value instanceof int[]){
						«updateBlock»
					};
				};
			
			};
		};
		'''
	}
	
}