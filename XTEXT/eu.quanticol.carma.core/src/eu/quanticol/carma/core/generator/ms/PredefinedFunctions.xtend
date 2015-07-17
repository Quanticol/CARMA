package eu.quanticol.carma.core.generator.ms

class PredefinedFunctions {
	
	def String getUniform(){
		'''
		public Double uniform(ArrayList<Double> input){
			RandomGenerator random = new DefaultRandomGenerator();
			return (Double) input.get(random.nextInt(input.size()));
		}
		'''
	}
	
	def String getPow(){
		'''
		public Double pow(ArrayList<Double> input){
			return Math.pow(input.get(0),input.get(1));
		}
		'''
	}
	
	def String getAbs(){
		'''
		public Double abs(ArrayList<Double> input){
			return Math.abs(input.get(0));
		}
		'''
	}
	
}