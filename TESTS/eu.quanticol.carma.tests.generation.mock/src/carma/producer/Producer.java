package carma.producer;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.SimulationEnvironment;

import eu.quanticol.carma.simulator.CarmaComponent;
import eu.quanticol.carma.simulator.CarmaPredicate;
import eu.quanticol.carma.simulator.CarmaSequentialProcess;
import eu.quanticol.carma.simulator.CarmaStore;
import eu.quanticol.carma.simulator.CarmaSystem;

public class Producer extends CarmaSystem {
	
	//constructor
	public Producer(){
		super();
		
		/*ComponentName*/
		CarmaComponent producer = getProducer();
		
		/*add all components*/
		addComponent(producer);
	}
	
	/*For each Component*/
	/*ComponentName*/
	private CarmaComponent getProducer() {
		CarmaComponent c = new CarmaComponent();
		/*needs definition file name*/
		/*add all attributes to Store*/
		c.set( ProducerDefinition.PRODUCT_ATTRIBUTE, 0);
		/*add processes*/
		c.addAgent( new CarmaSequentialProcess(c, ProducerDefinition.ProducerProcess ) );
		return c;
	}
	
	/*ENVIRONMENT PROBABILITY*/
	@Override
	public double broadcastProbability(CarmaStore sender, CarmaStore receiver,
			int action) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public double unicastProbability(CarmaStore sender, CarmaStore receiver,
			int action) {
		// TODO Auto-generated method stub
		return 0;
	}
	
	/*ENVIRONMENT RATE*/
	@Override
	public double broadcastRate(CarmaStore sender, int action) {
		/*needs action names, and then environment rate values*/
		if (action == ProducerDefinition.PRODUCE) {
			return 1;
		}
		return 0;
	}

	@Override
	public double unicastRate(CarmaStore sender, int action) {
		// TODO Auto-generated method stub
		return 0;
	}
	
	/*ENVIRONMENT UPDATE*/
	@Override
	public void broadcastUpdate(RandomGenerator random, CarmaStore sender,
			int action, Object value ) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void unicastUpdate(RandomGenerator random, CarmaStore sender,CarmaStore receiver, int action , Object value ) {
		if ((CarmaPredicate.TRUE.satisfy(sender))&&(action == ProducerDefinition.PRODUCE)) {
			// CODE FOR GLOBAL STORE UPDATE
			global_store.set(
					ProducerDefinition.PRODUCT_ATTRIBUTE, 
					receiver.get( ProducerDefinition.PRODUCT_ATTRIBUTE , ProducerDefinition.PRODUCT_ATTRIBUTE_TYPE) +1);
			// CODE FOR INSTALLING NEW COMPONENTS (IF NEEDED)
		}
		// TODO Auto-generated method stub
	}
	
	public static void main( String[] argv ) {
		SimulationEnvironment<CarmaSystem> system = new SimulationEnvironment<CarmaSystem>(
				new ProducerFactory()
		);
		
		system.simulate(100,50);
	}

}
