package carma.CGT15;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.SimulationEnvironment;
import eu.quanticol.carma.simulator.*;
import org.cmg.ml.sam.sim.sampling.Measure;
import org.cmg.ml.sam.sim.sampling.SamplingCollection;
import org.cmg.ml.sam.sim.sampling.StatisticSampling;
public class Simple extends CarmaSystem {
	
	//constructor
	public Simple(){
		addComponent(getProducer(1,1,1,1));
		addComponent(getConsumer(1,1,1,1));
		addComponent(getConsumer(1,1,2,1));
		addComponent(getConsumer(1,1,3,1));
		global_store.set(CGT15Definition.TRANSACTIONS_ATTRIBUTE,0);
	}
	
	//define components
	private CarmaComponent getProducer(int a,int b,int c,int d) {
		CarmaComponent c4rm4 = new CarmaComponent();
		c4rm4.set( CGT15Definition.PRODUCT_ATTRIBUTE, a);
		c4rm4.set( CGT15Definition.POSITION_X_ATTRIBUTE, c);
		c4rm4.set( CGT15Definition.POSITION_Y_ATTRIBUTE, d);
		c4rm4.set( CGT15Definition.TYPE_ATTRIBUTE, 1);
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT15Definition.ProducerProcess,
		CGT15Definition.ProducerProcess.getState("state_Produce" )));
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT15Definition.ProducerProcess,
		CGT15Definition.ProducerProcess.getState("state_Send" )));
		return c4rm4;
	}
	private CarmaComponent getConsumer(int a,int b, int x, int y) {
		CarmaComponent c4rm4 = new CarmaComponent();
		c4rm4.set( CGT15Definition.PRODUCT_ATTRIBUTE, a);
		c4rm4.set( CGT15Definition.POSITION_X_ATTRIBUTE, x);
		c4rm4.set( CGT15Definition.POSITION_Y_ATTRIBUTE, y);
		c4rm4.set( CGT15Definition.TYPE_ATTRIBUTE, 0);
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT15Definition.ConsumerProcess,
		CGT15Definition.ConsumerProcess.getState("state_Consume" )));
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT15Definition.ConsumerProcess,
		CGT15Definition.ConsumerProcess.getState("state_Receive" )));
		return c4rm4;
	}
	private CarmaComponent getChild( int x, int y) {
		CarmaComponent c4rm4 = new CarmaComponent();
		c4rm4.set( CGT15Definition.POSITION_X_ATTRIBUTE, x);
		c4rm4.set( CGT15Definition.POSITION_Y_ATTRIBUTE, y);
		c4rm4.set( CGT15Definition.TYPE_ATTRIBUTE, 2);
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT15Definition.ChildProcess,
		CGT15Definition.ChildProcess.getState("state_Nothing" )));
		return c4rm4;
	}
	
	//predicates
	
	
	
	
	
	
	//evol rules
	@Override
	public double broadcastRate(CarmaStore sender, int action){
		if (action == CGT15Definition.PRODUCE
		&& CarmaPredicate.TRUE.satisfy(sender)
		) {
				return 1;
		}
		if (action == CGT15Definition.NOTHING
		&& CarmaPredicate.TRUE.satisfy(sender)
		) {
				return 8;
		}
		return 1.0;
	}
	
	@Override
	public double unicastRate(CarmaStore sender, int action){
		if (action == CGT15Definition.SEND
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
		if (action == CGT15Definition.SEND
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
		if (action == CGT15Definition.SEND
		&& CarmaPredicate.TRUE.satisfy(sender)
		) {
				boolean hasAttributes = true;
				if(hasAttributes){
					
				addComponent(getChild(1,1,1,1));
				addComponent(getChild(2,1,1,1));
				addComponent(getChild(3,1,1,1));
				addComponent(getChild(1,2,1,1));
				addComponent(getChild(2,2,1,1));
				addComponent(getChild(3,2,1,1));
				addComponent(getChild(1,3,1,1));
				addComponent(getChild(2,3,1,1));
				addComponent(getChild(3,3,1,1));
				addComponent(getChild(1,1,2,1));
				addComponent(getChild(2,1,2,1));
				addComponent(getChild(3,1,2,1));
				addComponent(getChild(1,2,2,1));
				addComponent(getChild(2,2,2,1));
				addComponent(getChild(3,2,2,1));
				addComponent(getChild(1,3,2,1));
				addComponent(getChild(2,3,2,1));
				addComponent(getChild(3,3,2,1));
				addComponent(getChild(1,1,3,1));
				addComponent(getChild(2,1,3,1));
				addComponent(getChild(3,1,3,1));
				addComponent(getChild(1,2,3,1));
				addComponent(getChild(2,2,3,1));
				addComponent(getChild(3,2,3,1));
				addComponent(getChild(1,3,3,1));
				addComponent(getChild(2,3,3,1));
				addComponent(getChild(3,3,3,1));
				addComponent(getChild(1,1,1,2));
				addComponent(getChild(2,1,1,2));
				addComponent(getChild(3,1,1,2));
				addComponent(getChild(1,2,1,2));
				addComponent(getChild(2,2,1,2));
				addComponent(getChild(3,2,1,2));
				addComponent(getChild(1,3,1,2));
				addComponent(getChild(2,3,1,2));
				addComponent(getChild(3,3,1,2));
				addComponent(getChild(1,1,2,2));
				addComponent(getChild(2,1,2,2));
				addComponent(getChild(3,1,2,2));
				addComponent(getChild(1,2,2,2));
				addComponent(getChild(2,2,2,2));
				addComponent(getChild(3,2,2,2));
				addComponent(getChild(1,3,2,2));
				addComponent(getChild(2,3,2,2));
				addComponent(getChild(3,3,2,2));
				addComponent(getChild(1,1,3,2));
				addComponent(getChild(2,1,3,2));
				addComponent(getChild(3,1,3,2));
				addComponent(getChild(1,2,3,2));
				addComponent(getChild(2,2,3,2));
				addComponent(getChild(3,2,3,2));
				addComponent(getChild(1,3,3,2));
				addComponent(getChild(2,3,3,2));
				addComponent(getChild(3,3,3,2));
				addComponent(getChild(1,1,1,3));
				addComponent(getChild(2,1,1,3));
				addComponent(getChild(3,1,1,3));
				addComponent(getChild(1,2,1,3));
				addComponent(getChild(2,2,1,3));
				addComponent(getChild(3,2,1,3));
				addComponent(getChild(1,3,1,3));
				addComponent(getChild(2,3,1,3));
				addComponent(getChild(3,3,1,3));
				addComponent(getChild(1,1,2,3));
				addComponent(getChild(2,1,2,3));
				addComponent(getChild(3,1,2,3));
				addComponent(getChild(1,2,2,3));
				addComponent(getChild(2,2,2,3));
				addComponent(getChild(3,2,2,3));
				addComponent(getChild(1,3,2,3));
				addComponent(getChild(2,3,2,3));
				addComponent(getChild(3,3,2,3));
				addComponent(getChild(1,1,3,3));
				addComponent(getChild(2,1,3,3));
				addComponent(getChild(3,1,3,3));
				addComponent(getChild(1,2,3,3));
				addComponent(getChild(2,2,3,3));
				addComponent(getChild(3,2,3,3));
				addComponent(getChild(1,3,3,3));
				addComponent(getChild(2,3,3,3));
				addComponent(getChild(3,3,3,3));
				}
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
	
	//measures
	
	//main
	/*MAIN*/
	public static void main( String[] argv ) {
		SimulationEnvironment<CarmaSystem> system = new SimulationEnvironment<CarmaSystem>(
			new CGT15Factory()
		);
	
		int deadline = 50;
		
		SamplingCollection<CarmaSystem> sc = new SamplingCollection<CarmaSystem>();
		
		sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(deadline+1, 1.0, CGT15Definition.getMeasureWaiting__All(1)));
		
		system.setSampling(sc);
		system.simulate(100,50);
		for(int i = 0; i < sc.size(); i++){
			((StatisticSampling<CarmaSystem>) sc.get(i)).printTimeSeries(System.out);
		}
	}
	
}
