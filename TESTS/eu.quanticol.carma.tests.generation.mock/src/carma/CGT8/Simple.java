package carma.CGT8;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.SimulationEnvironment;

import eu.quanticol.carma.simulator.*;
public class Simple extends CarmaSystem {
	
	//constructor
	public Simple(){
		addComponent(getProducer(1));
		addComponent(getProducer(2));
		addComponent(getProducer(3));
		addComponent(getProducer(4));
		addComponent(getProducer(5));
		addComponent(getProducer(6));
		addComponent(getConsumer(1));
		addComponent(getConsumer(2));
		addComponent(getConsumer(3));
		addComponent(getConsumer(4));
		addComponent(getConsumer(5));
		addComponent(getConsumer(6));
	}
	
	private CarmaComponent getProducer(int a) {
		CarmaComponent c4rm4 = new CarmaComponent();
		c4rm4.set( CGT8Definition.PRODUCT_ATTRIBUTE, a);
		c4rm4.set( CGT8Definition.POSITION_X_ATTRIBUTE, 0);
		c4rm4.set( CGT8Definition.POSITION_Y_ATTRIBUTE, 1);
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, CGT8Definition.ProducerProcess ) );
		return c4rm4;
	}
	private CarmaComponent getConsumer(int a) {
		CarmaComponent c4rm4 = new CarmaComponent();
		c4rm4.set( CGT8Definition.PRODUCT_ATTRIBUTE, a);
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, CGT8Definition.ConsumerProcess ) );
		return c4rm4;
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
		if (action == CGT8Definition.PRODUCE) {
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
	public void unicastUpdate(RandomGenerator random, CarmaStore sender,CarmaStore receiver, 
			int action, Object value ) {
		// TODO Auto-generated method stub
		
	}
	
	/*MAIN*/
	public static void main( String[] argv ) {
		SimulationEnvironment<CarmaSystem> system = new SimulationEnvironment<CarmaSystem>(
			new CGT8Factory()
		);

		system.simulate(100,50);
	}
	
}
