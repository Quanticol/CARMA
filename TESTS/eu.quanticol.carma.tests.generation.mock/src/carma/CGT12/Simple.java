package carma.CGT12;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.SimulationEnvironment;
import eu.quanticol.carma.simulator.*;
import org.cmg.ml.sam.sim.sampling.SamplingCollection;
import org.cmg.ml.sam.sim.sampling.StatisticSampling;
public class Simple extends CarmaSystem {
	
	//constructor
	public Simple(){
		addComponent(getProducer(1,1,1,1));
		addComponent(getConsumer(1,1,1,1));
		global_store.set(CGT12Definition.TRANSACTIONS_ATTRIBUTE,0);
	}
	
	//define components
	private CarmaComponent getProducer(int a,int b,int c,int d) {
		CarmaComponent c4rm4 = new CarmaComponent();
		c4rm4.set( CGT12Definition.PRODUCT_ATTRIBUTE, a);
		c4rm4.set( CGT12Definition.POSITION_X_ATTRIBUTE, c);
		c4rm4.set( CGT12Definition.POSITION_Y_ATTRIBUTE, d);
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT12Definition.ProducerProcess,
		CGT12Definition.ProducerProcess.getState("state_Produce" )));
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT12Definition.ProducerProcess,
		CGT12Definition.ProducerProcess.getState("state_Send" )));
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT12Definition.ProducerProcess,
		CGT12Definition.ProducerProcess.getState("state_Produce" )));
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT12Definition.ProducerProcess,
		CGT12Definition.ProducerProcess.getState("state_Send" )));
		return c4rm4;
	}
	private CarmaComponent getConsumer(int a,int b, int x, int y) {
		CarmaComponent c4rm4 = new CarmaComponent();
		c4rm4.set( CGT12Definition.PRODUCT_ATTRIBUTE, a);
		c4rm4.set( CGT12Definition.POSITION_X_ATTRIBUTE, x);
		c4rm4.set( CGT12Definition.POSITION_Y_ATTRIBUTE, y);
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT12Definition.ConsumerProcess,
		CGT12Definition.ConsumerProcess.getState("state_Consume" )));
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT12Definition.ConsumerProcess,
		CGT12Definition.ConsumerProcess.getState("state_Receive" )));
		return c4rm4;
	}
	
	//predicates
	/*ENVIRONMENT PROBABILITY PREDICATES*/
	//BROADCAST ENVIRONMENT PROBABILITY PREDICATES
	
	//UNICAST ENVIRONMENT PROBABILITY PREDICATES
	
	/*ENVIRONMENT RATE PREDICATES*/
	//BROADCAST ENVIRONMENT RATE PREDICATES
	
	//UNICAST ENVIRONMENT RATE PREDICATES
	
	/*ENVIRONMENT UPDATE PREDICATES*/
	//BROADCAST ENVIRONMENT UPDATE PREDICATES
	
	//UNICAST ENVIRONMENT UPDATE PREDICATES
	
	//evol rules
	@Override
	public double broadcastRate(CarmaStore sender, int action){
		if (action == CGT12Definition.PRODUCE
		&& CarmaPredicate.TRUE.satisfy(sender)
		) {
				return 0.5;
		}
		return 1.0;
	}
	
	@Override
	public double unicastRate(CarmaStore sender, int action){
		if (action == CGT12Definition.SEND
		&& CarmaPredicate.TRUE.satisfy(sender)
		) {
				return 1;
		}
		return 1.0;
	}
	
	@Override
	public double broadcastProbability(CarmaStore sender, CarmaStore receiver,int action){
		return 1.0;
	}
	
	@Override
	public double unicastProbability(CarmaStore sender, CarmaStore receiver,int action){
		if (action == CGT12Definition.SEND
		&& CarmaPredicate.TRUE.satisfy(sender)
		) {
				return 1;
		}
		return 1.0;
	}
	
	@Override
	public void broadcastUpdate(RandomGenerator r , CarmaStore sender, int action, Object value){
	}
	
	@Override
	public void unicastUpdate(RandomGenerator r , CarmaStore sender, CarmaStore receiver, int action, Object value){
		if (action == CGT12Definition.SEND
		&& CarmaPredicate.TRUE.satisfy(sender)
		) {
				//spawns
		}
	}
	
	//main
	/*MAIN*/
	public static void main( String[] argv ) {
		SimulationEnvironment<CarmaSystem> system = new SimulationEnvironment<CarmaSystem>(
			new CGT12Factory()
		);
	
		int deadline = 50;
		
		SamplingCollection<CarmaSystem> sc = new SamplingCollection<CarmaSystem>();
		
		sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(deadline+1, 1.0, CGT12Definition.getMeasureWaiting_Producer_Send(1,1)));
		sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(deadline+1, 1.0, CGT12Definition.getMeasureWaiting_Producer_Send(2,1)));
		sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(deadline+1, 1.0, CGT12Definition.getMeasureWaiting_Producer_Send(3,1)));
		sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(deadline+1, 1.0, CGT12Definition.getMeasureWaiting_Producer_Send(1,2)));
		sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(deadline+1, 1.0, CGT12Definition.getMeasureWaiting_Producer_Send(2,2)));
		sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(deadline+1, 1.0, CGT12Definition.getMeasureWaiting_Producer_Send(3,2)));
		sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(deadline+1, 1.0, CGT12Definition.getMeasureWaiting_Producer_Send(1,3)));
		sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(deadline+1, 1.0, CGT12Definition.getMeasureWaiting_Producer_Send(2,3)));
		sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(deadline+1, 1.0, CGT12Definition.getMeasureWaiting_Producer_Send(3,3)));
		
		system.setSampling(sc);
		system.simulate(100,50);
		for(int i = 0; i < sc.size(); i++){
			((StatisticSampling<CarmaSystem>) sc.get(i)).printTimeSeries(System.out);
		}
	}
	
}
