package carma.CGT10_EnvironmentUpdate_Unicast;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.SimulationEnvironment;
import eu.quanticol.carma.simulator.*;
public class Simple extends CarmaSystem {
	
	//constructor
	public Simple(){
		addComponent(getProducer(1,1,1,1));
		addComponent(getConsumer(1,1,1,1));
		global_store.set(CGT10_EnvironmentUpdate_UnicastDefinition.TRANSACTIONS,0)
		global_store.set(CGT10_EnvironmentUpdate_UnicastDefinition.EU_GLOBAL,1)
	}
	
	private CarmaComponent getProducer(int a,int b,int c,int d) {
		CarmaComponent c4rm4 = new CarmaComponent();
		c4rm4.set( CGT10_EnvironmentUpdate_UnicastDefinition.PRODUCT_ATTRIBUTE, a);
		c4rm4.set( CGT10_EnvironmentUpdate_UnicastDefinition.POSITION_X_ATTRIBUTE, c);
		c4rm4.set( CGT10_EnvironmentUpdate_UnicastDefinition.POSITION_Y_ATTRIBUTE, d);
		c4rm4.set( CGT10_EnvironmentUpdate_UnicastDefinition.EU_SENDER_ATTRIBUTE, 1);
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT10_EnvironmentUpdate_UnicastDefinition.ProducerProcess,
		CGT10_EnvironmentUpdate_UnicastDefinition.ProducerProcess.getState("state_Produce" ) );
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT10_EnvironmentUpdate_UnicastDefinition.ProducerProcess,
		CGT10_EnvironmentUpdate_UnicastDefinition.ProducerProcess.getState("state_Send" ) );
		return c4rm4;
	}
	private CarmaComponent getConsumer(int a,int b, int x, int y) {
		CarmaComponent c4rm4 = new CarmaComponent();
		c4rm4.set( CGT10_EnvironmentUpdate_UnicastDefinition.PRODUCT_ATTRIBUTE, a);
		c4rm4.set( CGT10_EnvironmentUpdate_UnicastDefinition.POSITION_X_ATTRIBUTE, x);
		c4rm4.set( CGT10_EnvironmentUpdate_UnicastDefinition.POSITION_Y_ATTRIBUTE, y);
		c4rm4.set( CGT10_EnvironmentUpdate_UnicastDefinition.EU_RECEIVER_ATTRIBUTE, 1);
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT10_EnvironmentUpdate_UnicastDefinition.ConsumerProcess,
		CGT10_EnvironmentUpdate_UnicastDefinition.ConsumerProcess.getState("state_Consume" ) );
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT10_EnvironmentUpdate_UnicastDefinition.ConsumerProcess,
		CGT10_EnvironmentUpdate_UnicastDefinition.ConsumerProcess.getState("state_Receive" ) );
		return c4rm4;
	}
	
	/*ENVIRONMENT PROBABILITY*/
	@Override
	public double broadcastProbability(CarmaStore sender, CarmaStore receiver,
	int action) {
		return 0;
	}
	
	@Override
	public double unicastProbability(CarmaStore sender, CarmaStore receiver,
	int action) {
		return 0;
	}
	
	/*ENVIRONMENT RATE*/
	@Override
	public double broadcastRate(CarmaStore sender, int action) {
		if (True
		 && action == CGT10_EnvironmentUpdate_UnicastDefinition.PRODUCE) {
				return 1;
		}
		return 0;
	}
	@Override
	public double unicastRate(CarmaStore sender, int action) {
		if (True
		 && action == CGT10_EnvironmentUpdate_UnicastDefinition.SEND) {
				return 1;
		}
	}
	
	/*ENVIRONMENT UPDATE PREDICATES*/
	//BROADCAST ENVIRONMENT UPDATE PREDICATES
	
	//UNICAST ENVIRONMENT UPDATE PREDICATES
	public static CarmaPredicate get_True_send_Predicate(){
		return new CarmaPredicate() {
	
			@Override
			public boolean satisfy(CarmaStore store) {
				//any global if required
				return //getting stores and create expressions
			}
			
		};
	}
	public static CarmaPredicate get_False_send_Predicate(){
		return new CarmaPredicate() {
	
			@Override
			public boolean satisfy(CarmaStore store) {
				//any global if required
				return //getting stores and create expressions
			}
			
		};
	}
	public static CarmaPredicate get_sender_eu_sender_EQUA_1_send_Predicate(){
		return new CarmaPredicate() {
	
			@Override
			public boolean satisfy(CarmaStore store) {
				//any global if required
				return //getting stores and create expressions
			}
			
		};
	}
	public static CarmaPredicate get_receiver_eu_receiver_EQUA_1_send_Predicate(){
		return new CarmaPredicate() {
	
			@Override
			public boolean satisfy(CarmaStore store) {
				//any global if required
				return //getting stores and create expressions
			}
			
		};
	}
	public static CarmaPredicate get_global_eu_global_EQUA_1_send_Predicate(){
		return new CarmaPredicate() {
	
			@Override
			public boolean satisfy(CarmaStore store) {
				//any global if required
				return //getting stores and create expressions
			}
			
		};
	}
	public static CarmaPredicate get_sender_eu_sender_EQUA_1_AND_receiver_eu_receiver_EQUA_1_send_Predicate(CarmaStore sender){
		return new CarmaPredicate() {
	
			@Override
			public boolean satisfy(CarmaStore store) {
				//any global if required
				return //getting stores and create expressions
			}
			
		};
	}
	public static CarmaPredicate get_sender_eu_sender_EQUA_1_AND_receiver_eu_receiver_EQUA_1_AND_global_eu_global_EQUA_1_send_Predicate(CarmaStore sender){
		return new CarmaPredicate() {
	
			@Override
			public boolean satisfy(CarmaStore store) {
				//any global if required
				return //getting stores and create expressions
			}
			
		};
	}
	
	/*ENVIRONMENT UPDATE*/
	@Override
	public void broadcastUpdate(RandomGenerator random, CarmaStore sender, 
	int action, Object value) {
	}
	
	@Override
	public void unicastUpdate(RandomGenerator random, CarmaStore sender, CarmaStore receiver,
	int action, Object value) {
		if ((CarmaPredicate.TRUE.satisfy(sender)
		 && action == CGT10_EnvironmentUpdate_UnicastDefinition.SEND) {
				//updates
		}
		if ((CarmaPredicate.FALSE.satisfy(sender)
		 && action == CGT10_EnvironmentUpdate_UnicastDefinition.SEND) {
				//updates
		}
		if (get_sender_eu_sender_EQUA_1_send_Predicate().satisfy(CarmaStore sender)
		 && action == CGT10_EnvironmentUpdate_UnicastDefinition.SEND) {
				//updates
		}
		if (get_receiver_eu_receiver_EQUA_1_send_Predicate().satisfy(CarmaStore receiver)
		 && action == CGT10_EnvironmentUpdate_UnicastDefinition.SEND) {
				//updates
		}
		if (get_global_eu_global_EQUA_1_send_Predicate().satisfy(CarmaStore sender)
		 && action == CGT10_EnvironmentUpdate_UnicastDefinition.SEND) {
				//updates
		}
		if (get_sender_eu_sender_EQUA_1_AND_receiver_eu_receiver_EQUA_1_send_Predicate().satisfy(CarmaStore receiver)
		 && action == CGT10_EnvironmentUpdate_UnicastDefinition.SEND) {
				//updates
		}
		if (get_sender_eu_sender_EQUA_1_AND_receiver_eu_receiver_EQUA_1_AND_global_eu_global_EQUA_1_send_Predicate().satisfy(CarmaStore receiver)
		 && action == CGT10_EnvironmentUpdate_UnicastDefinition.SEND) {
				//updates
		}
	}
	
	/*MAIN*/
	public static void main( String[] argv ) {
		SimulationEnvironment<CarmaSystem> system = new SimulationEnvironment<CarmaSystem>(
			new CGT10_EnvironmentUpdate_UnicastFactory()
		);
	
		system.simulate(100,50);
	}
	
}
