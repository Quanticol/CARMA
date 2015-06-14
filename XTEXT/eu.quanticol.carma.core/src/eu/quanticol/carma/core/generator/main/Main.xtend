package eu.quanticol.carma.core.generator.main

import eu.quanticol.carma.core.generator.measures.MeasureManager
import java.util.ArrayList

class Main {
	
	def String getMain(String name, MeasureManager mm){
		'''
		/*MAIN*/
		public static void main( String[] argv ) {
			SimulationEnvironment<CarmaSystem> system = new SimulationEnvironment<CarmaSystem>(
				new «name»Factory()
			);
		
			int deadline = 50; 
			ArrayList<String> inputs = new ArrayList<String>();
			SamplingCollection<CarmaSystem> sc = new SamplingCollection<CarmaSystem>();
			
			«name.setupMeasures(mm)»
			
			system.setSampling(sc);
			system.simulate(100,50);
			for(int i = 0; i < sc.size(); i++){
				((StatisticSampling<CarmaSystem>) sc.get(i)).printName(System.out); if(inputs.size() > i){System.out.println(" with '" + inputs.get(i)+"' as arguments.");}
				((StatisticSampling<CarmaSystem>) sc.get(i)).printTimeSeries(System.out);
			}
		}
		'''
	}
	
	def String setupMeasures(String name, MeasureManager mm){
		'''
		«FOR key : mm.measures.keySet»
		«FOR list : mm.measures.get(key).produce»
		«getSingleDeclaration(name,key,convertArrayList(list))»
		«ENDFOR»
		«ENDFOR»
		'''
	}
	
	def String convertArrayList(ArrayList<String> list){
		var output = ""
		
		if(list.size > 0){
			output = list.get(0)
			for(var i = 1; i < list.size; i++){
				output = output + "," + list.get(i)
			}
		}
		
		return output;
	}
	
	
	def String getSingleDeclaration(String modelName, String measureName, String args){
		'''
		sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(deadline+1, 1.0, «modelName»Definition.getMeasure«measureName»(«args»)));
		inputs.add("«args»");
		'''
	}
}