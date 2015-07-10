package eu.quanticol.carma.core.generator.ms.main

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.BlockStyle
import eu.quanticol.carma.core.carma.BlockSystem
import eu.quanticol.carma.core.carma.Measure
import eu.quanticol.carma.core.carma.MeasureBlock
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.System
import eu.quanticol.carma.core.generator.ms.measure.MeasureJavaniser

class MainHandler {
	
	@Inject extension MeasureJavaniser
	
	def String getMain(Model model, System system){
		var measureBlock = (model.components as BlockStyle).measures
		'''
		public static void main( String[] argv ) {
			SimulationEnvironment<CarmaSystem> system = new SimulationEnvironment<CarmaSystem>(
				new «(system as BlockSystem).name.name»ModelFactory()
			);
		
			int deadline = 50; 
			ArrayList<String> inputs = new ArrayList<String>();
			SamplingCollection<CarmaSystem> sc = new SamplingCollection<CarmaSystem>();
			
			
			«measureBlock.printMeasures»
			
			system.setSampling(sc);
			system.simulate(100,50);
			for(int i = 0; i < sc.size(); i++){
				((StatisticSampling<CarmaSystem>) sc.get(i)).printName(System.out); if(inputs.size() > i){System.out.println(" with '" + inputs.get(i)+"' as arguments.");}
				((StatisticSampling<CarmaSystem>) sc.get(i)).printTimeSeries(System.out);
			}
		}
		'''
	}
	
	def String printMeasures(MeasureBlock measureBlock){
		
		'''
		«FOR measure : measureBlock.measures»
		sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(deadline+1, 1.0, getMeasure«measure.measure.javanise»(«measure.getArguments»)));
		inputs.add("0,0");
		«ENDFOR»
		'''
		
	}
	
	def String getArguments(Measure measure){
		
	}
	
	
}