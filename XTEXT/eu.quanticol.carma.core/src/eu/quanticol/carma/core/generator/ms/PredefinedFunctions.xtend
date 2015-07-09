package eu.quanticol.carma.core.generator.ms

class PredefinedFunctions {
	
	def String getUniform(){
		'''
		public static int uniform(ArrayList<Object> input){
			RandomGenerator random = new DefaultRandomGenerator();
			return (int) input.get(random.nextInt(input.size()));
		}
		'''
	}
	
}