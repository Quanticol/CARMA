package carma.producer;

import org.cmg.ml.sam.sim.SimulationFactory;

import eu.quanticol.carma.simulator.CarmaSystem;

public class ProducerFactory implements SimulationFactory<CarmaSystem> {

	public ProducerFactory(){
		
	}
	
	@Override
	public CarmaSystem getModel() {
		return new Producer();
	}

}
