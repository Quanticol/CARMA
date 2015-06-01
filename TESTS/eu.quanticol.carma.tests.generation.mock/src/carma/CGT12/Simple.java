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
	private CarmaComponent getChild() {
		CarmaComponent c4rm4 = new CarmaComponent();
		c4rm4.set( CGT12Definition.ME_ATTRIBUTE, 0);
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT12Definition.ChildProcess,
		CGT12Definition.ChildProcess.getState("state_Nothing" )));
		return c4rm4;
	}
	
	//predicates
	
	
	
	
	
	
	//evol rules
	@Override
	public double broadcastRate(CarmaStore sender, int action){
		if (action == CGT12Definition.PRODUCE
		&& CarmaPredicate.TRUE.satisfy(sender)
		) {
				return 1;
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
				addComponent(getChild());
				boolean hasAttributes = true;
				int transactions = 0;
				if(global_store.get("transactions" , Integer.class) != null){
					transactions = global_store.get("transactions" , Integer.class); 
				} else { 
					hasAttributes = false;
				}
				if(hasAttributes){
					global_store.set("transactions",transactions + 1);
				}
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
		
		sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(deadline+1, 1.0, CGT12Definition.getMeasureWaiting__All(1)));
		
		system.setSampling(sc);
		system.simulate(100,50);
		for(int i = 0; i < sc.size(); i++){
			((StatisticSampling<CarmaSystem>) sc.get(i)).printTimeSeries(System.out);
		}
	}
	
}
