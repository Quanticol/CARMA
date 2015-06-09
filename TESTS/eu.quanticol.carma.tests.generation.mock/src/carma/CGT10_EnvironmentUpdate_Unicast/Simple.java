package carma.CGT10_EnvironmentUpdate_Unicast;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.SimulationEnvironment;
import eu.quanticol.carma.simulator.*;
public class Simple extends CarmaSystem {
	
	//constructor
	public Simple(){
		addComponent(getProducer(1,1,1,1));
		addComponent(getConsumer(1,1,1,1));
		global_store.set(CGT10_EnvironmentUpdate_UnicastDefinition.TRANSACTIONS_ATTRIBUTE,0);
		global_store.set( CGT10_EnvironmentUpdate_UnicastDefinition.TEST_X_ATTRIBUTE, 1);
		global_store.set( CGT10_EnvironmentUpdate_UnicastDefinition.TEST_Y_ATTRIBUTE, 1);
		global_store.set(CGT10_EnvironmentUpdate_UnicastDefinition.EU_GLOBAL_ATTRIBUTE,1);
	}
	
	private CarmaComponent getProducer(int a,int b,int c,int d) {
		CarmaComponent c4rm4 = new CarmaComponent();
		c4rm4.set( CGT10_EnvironmentUpdate_UnicastDefinition.PRODUCT_ATTRIBUTE, a);
		c4rm4.set( CGT10_EnvironmentUpdate_UnicastDefinition.POSITION_X_ATTRIBUTE, c);
		c4rm4.set( CGT10_EnvironmentUpdate_UnicastDefinition.POSITION_Y_ATTRIBUTE, d);
		c4rm4.set( CGT10_EnvironmentUpdate_UnicastDefinition.EU_SENDER_ATTRIBUTE, 1);
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT10_EnvironmentUpdate_UnicastDefinition.ProducerProcess,
		CGT10_EnvironmentUpdate_UnicastDefinition.ProducerProcess.getState("state_Produce" )));
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT10_EnvironmentUpdate_UnicastDefinition.ProducerProcess,
		CGT10_EnvironmentUpdate_UnicastDefinition.ProducerProcess.getState("state_Send" )));
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
		CGT10_EnvironmentUpdate_UnicastDefinition.ConsumerProcess.getState("state_Consume" )));
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT10_EnvironmentUpdate_UnicastDefinition.ConsumerProcess,
		CGT10_EnvironmentUpdate_UnicastDefinition.ConsumerProcess.getState("state_Receive" )));
		return c4rm4;
	}
	
	/*ENVIRONMENT PROBABILITY PREDICATES*/
	//BROADCAST ENVIRONMENT PROBABILITY PREDICATES
	public static CarmaPredicate get_sender_eu_sender_EQUA_1_produce_BROADCAST__BroadcastPredicateProb(){
		return new CarmaPredicate() {
	
			@Override
			public boolean satisfy(CarmaStore sender) {
				int eu_sender_s = sender.get("eu_sender" , Integer.class );
				return eu_sender_s == 1;
			}
			
		};
	}
	public static CarmaPredicate get_global_eu_global_EQUA_1_produce_BROADCAST__BroadcastPredicateProb(){
		return new CarmaPredicate() {
	
			@Override
			public boolean satisfy(CarmaStore store) {
				int eu_global = global_store.get("eu_global" , Integer.class );
				return eu_global == 1;
			}
			
		};
	}
	
	//UNICAST ENVIRONMENT PROBABILITY PREDICATES
	public static CarmaPredicate get_receiver_eu_receiver_EQUA_1_send_UnicastPredicateProb(){
		return new CarmaPredicate() {
	
			@Override
			public boolean satisfy(CarmaStore receiver) {
				int eu_receiver_r = receiver.get("eu_receiver" , Integer.class );
				return eu_receiver_r == 1;
			}
			
		};
	}
	public static CarmaPredicate get_sender_eu_sender_EQUA_1_AND_receiver_eu_receiver_EQUA_1_send_UnicastPredicateProb(final CarmaStore sender){
		return new CarmaPredicate() {
	
			@Override
			public boolean satisfy(CarmaStore receiver) {
				int eu_receiver_r = receiver.get("eu_receiver" , Integer.class );
				int eu_sender_s = sender.get("eu_sender" , Integer.class );
				return eu_sender_s == 1 && eu_receiver_r == 1;
			}
			
		};
	}
	public static CarmaPredicate get_sender_eu_sender_EQUA_1_AND_receiver_eu_receiver_EQUA_1_AND_global_eu_global_EQUA_1_send_UnicastPredicateProb(final CarmaStore sender){
		return new CarmaPredicate() {
	
			@Override
			public boolean satisfy(CarmaStore receiver) {
				int eu_global = global_store.get("eu_global" , Integer.class );
				int eu_receiver_r = receiver.get("eu_receiver" , Integer.class );
				int eu_sender_s = sender.get("eu_sender" , Integer.class );
				return eu_sender_s == 1 && eu_receiver_r == 1 && eu_global == 1;
			}
			
		};
	}
	
	/*ENVIRONMENT PROBABILITY*/
	@Override
	public double broadcastProbability(CarmaStore sender, CarmaStore receiver,
	int action) {
		if (action == CGT10_EnvironmentUpdate_UnicastDefinition.PRODUCE
		&& CarmaPredicate.TRUE.satisfy(sender)
		) {
				return 1;
		}
		if (action == CGT10_EnvironmentUpdate_UnicastDefinition.PRODUCE
		&& CarmaPredicate.FALSE.satisfy(sender)
		) {
				return 1;
		}
		if (action == CGT10_EnvironmentUpdate_UnicastDefinition.PRODUCE
		&& get_sender_eu_sender_EQUA_1_produce_BROADCAST__BroadcastPredicateProb().satisfy(sender)
		) {
				return 1;
		}
		if (action == CGT10_EnvironmentUpdate_UnicastDefinition.PRODUCE
		&& get_global_eu_global_EQUA_1_produce_BROADCAST__BroadcastPredicateProb().satisfy(sender)
		) {
				return 1;
		}
		return 0;
	}
	
	@Override
	public double unicastProbability(CarmaStore sender, CarmaStore receiver,
	int action) {
		if (action == CGT10_EnvironmentUpdate_UnicastDefinition.SEND
		&& CarmaPredicate.TRUE.satisfy(sender)
		) {
				return 1;
		}
		if (action == CGT10_EnvironmentUpdate_UnicastDefinition.SEND
		&& get_receiver_eu_receiver_EQUA_1_send_UnicastPredicateProb().satisfy(receiver)
		) {
				return 1;
		}
		if (action == CGT10_EnvironmentUpdate_UnicastDefinition.SEND
		&& get_sender_eu_sender_EQUA_1_AND_receiver_eu_receiver_EQUA_1_send_UnicastPredicateProb(sender).satisfy(receiver)
		) {
				return 1;
		}
		if (action == CGT10_EnvironmentUpdate_UnicastDefinition.SEND
		&& get_sender_eu_sender_EQUA_1_AND_receiver_eu_receiver_EQUA_1_AND_global_eu_global_EQUA_1_send_UnicastPredicateProb(sender).satisfy(receiver)
		) {
				return 1;
		}
		return 0;
	}
	
	/*ENVIRONMENT RATE PREDICATES*/
	//BROADCAST ENVIRONMENT RATE PREDICATES
	public static CarmaPredicate get_sender_eu_sender_EQUA_1_produce_BROADCAST__BroadcastPredicateRate(){
		return new CarmaPredicate() {
	
			@Override
			public boolean satisfy(CarmaStore sender) {
				int eu_sender_s = sender.get("eu_sender" , Integer.class );
				return eu_sender_s == 1;
			}
			
		};
	}
	public static CarmaPredicate get_global_eu_global_EQUA_1_produce_BROADCAST__BroadcastPredicateRate(){
		return new CarmaPredicate() {
	
			@Override
			public boolean satisfy(CarmaStore store) {
				int eu_global = global_store.get("eu_global" , Integer.class );
				return eu_global == 1;
			}
			
		};
	}
	
	//UNICAST ENVIRONMENT RATE PREDICATES
	public static CarmaPredicate get_sender_eu_sender_EQUA_1_send_UnicastPredicateRate(){
		return new CarmaPredicate() {
	
			@Override
			public boolean satisfy(CarmaStore sender) {
				int eu_sender_s = sender.get("eu_sender" , Integer.class );
				return eu_sender_s == 1;
			}
			
		};
	}
	
	/*ENVIRONMENT RATE*/
	@Override
	public double broadcastRate(CarmaStore sender, int action) {
		if (action == CGT10_EnvironmentUpdate_UnicastDefinition.PRODUCE
		&& CarmaPredicate.TRUE.satisfy(sender)
		) {
				return 1;
				
		}
		if (action == CGT10_EnvironmentUpdate_UnicastDefinition.PRODUCE
		&& CarmaPredicate.FALSE.satisfy(sender)
		) {
				return 1;
				
		}
		if (action == CGT10_EnvironmentUpdate_UnicastDefinition.PRODUCE
		&& get_sender_eu_sender_EQUA_1_produce_BROADCAST__BroadcastPredicateRate().satisfy(sender)
		) {
				return 1;
				
		}
		if (action == CGT10_EnvironmentUpdate_UnicastDefinition.PRODUCE
		&& get_global_eu_global_EQUA_1_produce_BROADCAST__BroadcastPredicateRate().satisfy(sender)
		) {
				return 1;
				
		}
		return 1;
	}
	@Override
	public double unicastRate(CarmaStore sender, int action) {
		if (action == CGT10_EnvironmentUpdate_UnicastDefinition.SEND
		&& CarmaPredicate.TRUE.satisfy(sender)
		) {
				return 1;
				
		}
		if (action == CGT10_EnvironmentUpdate_UnicastDefinition.SEND
		&& get_sender_eu_sender_EQUA_1_send_UnicastPredicateRate().satisfy(sender)
		) {
				return 1;
				
		}
		return 1;
	}
	
	/*ENVIRONMENT UPDATE PREDICATES*/
	//BROADCAST ENVIRONMENT UPDATE PREDICATES
	
	//UNICAST ENVIRONMENT UPDATE PREDICATES
	public static CarmaPredicate get_sender_eu_sender_EQUA_1_send_UnicastPredicateUpdate(){
		return new CarmaPredicate() {
	
			@Override
			public boolean satisfy(CarmaStore sender) {
				int eu_sender_s = sender.get("eu_sender" , Integer.class );
				return eu_sender_s == 1;
			}
			
		};
	}
	public static CarmaPredicate get_receiver_eu_receiver_EQUA_1_send_UnicastPredicateUpdate(){
		return new CarmaPredicate() {
	
			@Override
			public boolean satisfy(CarmaStore receiver) {
				int eu_receiver_r = receiver.get("eu_receiver" , Integer.class );
				return eu_receiver_r == 1;
			}
			
		};
	}
	public static CarmaPredicate get_global_eu_global_EQUA_1_send_UnicastPredicateUpdate(){
		return new CarmaPredicate() {
	
			@Override
			public boolean satisfy(CarmaStore store) {
				int eu_global = global_store.get("eu_global" , Integer.class );
				return eu_global == 1;
			}
			
		};
	}
	public static CarmaPredicate get_sender_eu_sender_EQUA_1_AND_receiver_eu_receiver_EQUA_1_send_UnicastPredicateUpdate(final CarmaStore sender){
		return new CarmaPredicate() {
	
			@Override
			public boolean satisfy(CarmaStore receiver) {
				int eu_receiver_r = receiver.get("eu_receiver" , Integer.class );
				int eu_sender_s = sender.get("eu_sender" , Integer.class );
				return eu_sender_s == 1 && eu_receiver_r == 1;
			}
			
		};
	}
	public static CarmaPredicate get_sender_eu_sender_EQUA_1_AND_receiver_eu_receiver_EQUA_1_AND_global_eu_global_EQUA_1_send_UnicastPredicateUpdate(final CarmaStore sender){
		return new CarmaPredicate() {
	
			@Override
			public boolean satisfy(CarmaStore receiver) {
				int eu_global = global_store.get("eu_global" , Integer.class );
				int eu_receiver_r = receiver.get("eu_receiver" , Integer.class );
				int eu_sender_s = sender.get("eu_sender" , Integer.class );
				return eu_sender_s == 1 && eu_receiver_r == 1 && eu_global == 1;
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
		if (action == CGT10_EnvironmentUpdate_UnicastDefinition.SEND 
		&& CarmaPredicate.TRUE.satisfy(sender)
		) {
		}
		if (action == CGT10_EnvironmentUpdate_UnicastDefinition.SEND 
		&& CarmaPredicate.FALSE.satisfy(sender)
		) {
		}
		if (action == CGT10_EnvironmentUpdate_UnicastDefinition.SEND 
		&& get_sender_eu_sender_EQUA_1_send_UnicastPredicateUpdate().satisfy(sender)
		) {
		}
		if (action == CGT10_EnvironmentUpdate_UnicastDefinition.SEND 
		&& get_receiver_eu_receiver_EQUA_1_send_UnicastPredicateUpdate().satisfy(receiver)
		) {
		}
		if (action == CGT10_EnvironmentUpdate_UnicastDefinition.SEND 
		&& get_global_eu_global_EQUA_1_send_UnicastPredicateUpdate().satisfy(sender)
		) {
		}
		if (action == CGT10_EnvironmentUpdate_UnicastDefinition.SEND 
		&& get_sender_eu_sender_EQUA_1_AND_receiver_eu_receiver_EQUA_1_send_UnicastPredicateUpdate(sender).satisfy(receiver)
		) {
		}
		if (action == CGT10_EnvironmentUpdate_UnicastDefinition.SEND 
		&& get_sender_eu_sender_EQUA_1_AND_receiver_eu_receiver_EQUA_1_AND_global_eu_global_EQUA_1_send_UnicastPredicateUpdate(sender).satisfy(receiver)
		) {
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
