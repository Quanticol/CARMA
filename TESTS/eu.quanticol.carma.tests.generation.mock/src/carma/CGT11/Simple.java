package carma.CGT11;

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
		global_store.set(CGT11Definition.TRANSACTIONS_ATTRIBUTE,0);
		global_store.set( CGT11Definition.TEST_X_ATTRIBUTE, 1);
		global_store.set( CGT11Definition.TEST_Y_ATTRIBUTE, 1);
		global_store.set(CGT11Definition.EU_GLOBAL_ATTRIBUTE,1);
	}
	
	//define components
	private CarmaComponent getProducer(int a,int b,int c,int d) {
		CarmaComponent c4rm4 = new CarmaComponent();
		c4rm4.set( CGT11Definition.PRODUCT_ATTRIBUTE, a);
		c4rm4.set( CGT11Definition.POSITION_X_ATTRIBUTE, c);
		c4rm4.set( CGT11Definition.POSITION_Y_ATTRIBUTE, d);
		c4rm4.set( CGT11Definition.EU_SENDER_ATTRIBUTE, 1);
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT11Definition.ProducerProcess,
		CGT11Definition.ProducerProcess.getState("state_Produce" )));
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT11Definition.ProducerProcess,
		CGT11Definition.ProducerProcess.getState("state_Send" )));
		return c4rm4;
	}
	private CarmaComponent getConsumer(int a,int b, int x, int y) {
		CarmaComponent c4rm4 = new CarmaComponent();
		c4rm4.set( CGT11Definition.PRODUCT_ATTRIBUTE, a);
		c4rm4.set( CGT11Definition.EU_RECEIVER_ATTRIBUTE, 1);
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT11Definition.ConsumerProcess,
		CGT11Definition.ConsumerProcess.getState("state_Consume" )));
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT11Definition.ConsumerProcess,
		CGT11Definition.ConsumerProcess.getState("state_Receive" )));
		return c4rm4;
	}
	
	//predicates
	/*ENVIRONMENT PROBABILITY PREDICATES*/
	//BROADCAST ENVIRONMENT PROBABILITY PREDICATES
	public static CarmaPredicate get_sender_eu_sender_EQUA_1_produce_BROADCAST__BroadcastPredicateProb(){
		return new CarmaPredicate() {
			@Override
			public boolean satisfy(CarmaStore sender) {
				boolean hasAttributes = true;
				int eu_sender_s = 0;
				if(sender.get("eu_sender" , Integer.class) != null){
					eu_sender_s = sender.get("eu_sender" , Integer.class); 
				} else { 
					hasAttributes = false;
				}
				if(hasAttributes)
					return eu_sender_s == 1;
				else
					return false;
			}
		};
	}
	public static CarmaPredicate get_global_eu_global_EQUA_1_produce_BROADCAST__BroadcastPredicateProb(){
		return new CarmaPredicate() {
			@Override
			public boolean satisfy(CarmaStore store) {
				boolean hasAttributes = true;
				int eu_global = 0;
				if(global_store.get("eu_global" , Integer.class) != null){
					eu_global = global_store.get("eu_global" , Integer.class); 
				} else { 
					hasAttributes = false;
				}
				if(hasAttributes)
					return eu_global == 1;
				else
					return false;
			}
		};
	}
	
	//UNICAST ENVIRONMENT PROBABILITY PREDICATES
	public static CarmaPredicate get_receiver_eu_receiver_EQUA_1_send_UnicastPredicateProb(){
		return new CarmaPredicate() {
			@Override
			public boolean satisfy(CarmaStore receiver) {
				boolean hasAttributes = true;
				int eu_receiver_r = 0;
				if(receiver.get("eu_receiver" , Integer.class) != null){
					eu_receiver_r = receiver.get("eu_receiver" , Integer.class); 
				} else { 
					hasAttributes = false;
				}
				if(hasAttributes)
					return eu_receiver_r == 1;
				else
					return false;
			}
		};
	}
	public static CarmaPredicate get_sender_eu_sender_EQUA_1_AND_receiver_eu_receiver_EQUA_1_send_UnicastPredicateProb(final CarmaStore sender){
		return new CarmaPredicate() {
			@Override
			public boolean satisfy(CarmaStore receiver) {
				boolean hasAttributes = true;
				int eu_sender_s = 0;
				if(sender.get("eu_sender" , Integer.class) != null){
					eu_sender_s = sender.get("eu_sender" , Integer.class); 
				} else { 
					hasAttributes = false;
				}
				int eu_receiver_r = 0;
				if(receiver.get("eu_receiver" , Integer.class) != null){
					eu_receiver_r = receiver.get("eu_receiver" , Integer.class); 
				} else { 
					hasAttributes = false;
				}
				if(hasAttributes)
					return eu_sender_s == 1 && eu_receiver_r == 1;
				else
					return false;
			}
		};
	}
	public static CarmaPredicate get_sender_eu_sender_EQUA_1_AND_receiver_eu_receiver_EQUA_1_AND_global_eu_global_EQUA_1_send_UnicastPredicateProb(final CarmaStore sender){
		return new CarmaPredicate() {
			@Override
			public boolean satisfy(CarmaStore receiver) {
				boolean hasAttributes = true;
				int eu_sender_s = 0;
				if(sender.get("eu_sender" , Integer.class) != null){
					eu_sender_s = sender.get("eu_sender" , Integer.class); 
				} else { 
					hasAttributes = false;
				}
				int eu_receiver_r = 0;
				if(receiver.get("eu_receiver" , Integer.class) != null){
					eu_receiver_r = receiver.get("eu_receiver" , Integer.class); 
				} else { 
					hasAttributes = false;
				}
				int eu_global = 0;
				if(global_store.get("eu_global" , Integer.class) != null){
					eu_global = global_store.get("eu_global" , Integer.class); 
				} else { 
					hasAttributes = false;
				}
				if(hasAttributes)
					return eu_sender_s == 1 && eu_receiver_r == 1 && eu_global == 1;
				else
					return false;
			}
		};
	}
	
	/*ENVIRONMENT RATE PREDICATES*/
	//BROADCAST ENVIRONMENT RATE PREDICATES
	public static CarmaPredicate get_sender_eu_sender_EQUA_1_produce_BROADCAST__BroadcastPredicateRate(){
		return new CarmaPredicate() {
			@Override
			public boolean satisfy(CarmaStore sender) {
				boolean hasAttributes = true;
				int eu_sender_s = 0;
				if(sender.get("eu_sender" , Integer.class) != null){
					eu_sender_s = sender.get("eu_sender" , Integer.class); 
				} else { 
					hasAttributes = false;
				}
				if(hasAttributes)
					return eu_sender_s == 1;
				else
					return false;
			}
		};
	}
	public static CarmaPredicate get_global_eu_global_EQUA_1_produce_BROADCAST__BroadcastPredicateRate(){
		return new CarmaPredicate() {
			@Override
			public boolean satisfy(CarmaStore store) {
				boolean hasAttributes = true;
				int eu_global = 0;
				if(global_store.get("eu_global" , Integer.class) != null){
					eu_global = global_store.get("eu_global" , Integer.class); 
				} else { 
					hasAttributes = false;
				}
				if(hasAttributes)
					return eu_global == 1;
				else
					return false;
			}
		};
	}
	
	//UNICAST ENVIRONMENT RATE PREDICATES
	public static CarmaPredicate get_sender_eu_sender_EQUA_1_AND_global_eu_global_EQUA_1_send_UnicastPredicateRate(){
		return new CarmaPredicate() {
			@Override
			public boolean satisfy(CarmaStore sender) {
				boolean hasAttributes = true;
				int eu_sender_s = 0;
				if(sender.get("eu_sender" , Integer.class) != null){
					eu_sender_s = sender.get("eu_sender" , Integer.class); 
				} else { 
					hasAttributes = false;
				}
				int eu_global = 0;
				if(global_store.get("eu_global" , Integer.class) != null){
					eu_global = global_store.get("eu_global" , Integer.class); 
				} else { 
					hasAttributes = false;
				}
				if(hasAttributes)
					return eu_sender_s == 1 && eu_global == 1;
				else
					return false;
			}
		};
	}
	
	/*ENVIRONMENT UPDATE PREDICATES*/
	//BROADCAST ENVIRONMENT UPDATE PREDICATES
	
	//UNICAST ENVIRONMENT UPDATE PREDICATES
	public static CarmaPredicate get_sender_eu_sender_EQUA_1_send_UnicastPredicateUpdate(){
		return new CarmaPredicate() {
			@Override
			public boolean satisfy(CarmaStore sender) {
				boolean hasAttributes = true;
				int eu_sender_s = 0;
				if(sender.get("eu_sender" , Integer.class) != null){
					eu_sender_s = sender.get("eu_sender" , Integer.class); 
				} else { 
					hasAttributes = false;
				}
				if(hasAttributes)
					return eu_sender_s == 1;
				else
					return false;
			}
		};
	}
	public static CarmaPredicate get_receiver_eu_receiver_EQUA_1_send_UnicastPredicateUpdate(){
		return new CarmaPredicate() {
			@Override
			public boolean satisfy(CarmaStore receiver) {
				boolean hasAttributes = true;
				int eu_receiver_r = 0;
				if(receiver.get("eu_receiver" , Integer.class) != null){
					eu_receiver_r = receiver.get("eu_receiver" , Integer.class); 
				} else { 
					hasAttributes = false;
				}
				if(hasAttributes)
					return eu_receiver_r == 1;
				else
					return false;
			}
		};
	}
	public static CarmaPredicate get_global_eu_global_EQUA_1_send_UnicastPredicateUpdate(){
		return new CarmaPredicate() {
			@Override
			public boolean satisfy(CarmaStore store) {
				boolean hasAttributes = true;
				int eu_global = 0;
				if(global_store.get("eu_global" , Integer.class) != null){
					eu_global = global_store.get("eu_global" , Integer.class); 
				} else { 
					hasAttributes = false;
				}
				if(hasAttributes)
					return eu_global == 1;
				else
					return false;
			}
		};
	}
	public static CarmaPredicate get_sender_eu_sender_EQUA_1_AND_receiver_eu_receiver_EQUA_1_send_UnicastPredicateUpdate(final CarmaStore sender){
		return new CarmaPredicate() {
			@Override
			public boolean satisfy(CarmaStore receiver) {
				boolean hasAttributes = true;
				int eu_sender_s = 0;
				if(sender.get("eu_sender" , Integer.class) != null){
					eu_sender_s = sender.get("eu_sender" , Integer.class); 
				} else { 
					hasAttributes = false;
				}
				int eu_receiver_r = 0;
				if(receiver.get("eu_receiver" , Integer.class) != null){
					eu_receiver_r = receiver.get("eu_receiver" , Integer.class); 
				} else { 
					hasAttributes = false;
				}
				if(hasAttributes)
					return eu_sender_s == 1 && eu_receiver_r == 1;
				else
					return false;
			}
		};
	}
	public static CarmaPredicate get_sender_eu_sender_EQUA_1_AND_receiver_eu_receiver_EQUA_1_AND_global_eu_global_EQUA_1_send_UnicastPredicateUpdate(final CarmaStore sender){
		return new CarmaPredicate() {
			@Override
			public boolean satisfy(CarmaStore receiver) {
				boolean hasAttributes = true;
				int eu_sender_s = 0;
				if(sender.get("eu_sender" , Integer.class) != null){
					eu_sender_s = sender.get("eu_sender" , Integer.class); 
				} else { 
					hasAttributes = false;
				}
				int eu_receiver_r = 0;
				if(receiver.get("eu_receiver" , Integer.class) != null){
					eu_receiver_r = receiver.get("eu_receiver" , Integer.class); 
				} else { 
					hasAttributes = false;
				}
				int eu_global = 0;
				if(global_store.get("eu_global" , Integer.class) != null){
					eu_global = global_store.get("eu_global" , Integer.class); 
				} else { 
					hasAttributes = false;
				}
				if(hasAttributes)
					return eu_sender_s == 1 && eu_receiver_r == 1 && eu_global == 1;
				else
					return false;
			}
		};
	}
	
	//evol rules
	@Override
	public double broadcastRate(CarmaStore sender, int action){
		if (action == CGT11Definition.PRODUCE
		&& CarmaPredicate.TRUE.satisfy(sender)
		) {
				return 1;
		}
		if (action == CGT11Definition.PRODUCE
		&& CarmaPredicate.FALSE.satisfy(sender)
		) {
				return 1;
		}
		if (action == CGT11Definition.PRODUCE
		&& get_sender_eu_sender_EQUA_1_produce_BROADCAST__BroadcastPredicateRate().satisfy(sender)
		) {
				return 0.5;
		}
		if (action == CGT11Definition.PRODUCE
		&& get_global_eu_global_EQUA_1_produce_BROADCAST__BroadcastPredicateRate().satisfy(sender)
		) {
				return 1;
		}
		return 1.0;
	}
	
	@Override
	public double unicastRate(CarmaStore sender, int action){
		if (action == CGT11Definition.SEND
		&& CarmaPredicate.TRUE.satisfy(sender)
		) {
				return 1;
		}
		if (action == CGT11Definition.SEND
		&& get_sender_eu_sender_EQUA_1_AND_global_eu_global_EQUA_1_send_UnicastPredicateRate().satisfy(sender)
		) {
				return 1;
		}
		return 1.0;
	}
	
	@Override
	public double broadcastProbability(CarmaStore sender, CarmaStore receiver,int action){
		if (action == CGT11Definition.PRODUCE
		&& CarmaPredicate.TRUE.satisfy(sender)
		) {
				return 1;
		}
		if (action == CGT11Definition.PRODUCE
		&& CarmaPredicate.FALSE.satisfy(sender)
		) {
				return 1;
		}
		if (action == CGT11Definition.PRODUCE
		&& get_sender_eu_sender_EQUA_1_produce_BROADCAST__BroadcastPredicateProb().satisfy(sender)
		) {
				return 1;
		}
		if (action == CGT11Definition.PRODUCE
		&& get_global_eu_global_EQUA_1_produce_BROADCAST__BroadcastPredicateProb().satisfy(sender)
		) {
				return 1;
		}
		return 1.0;
	}
	
	@Override
	public double unicastProbability(CarmaStore sender, CarmaStore receiver,int action){
		if (action == CGT11Definition.SEND
		&& CarmaPredicate.TRUE.satisfy(sender)
		) {
				return 1;
		}
		if (action == CGT11Definition.SEND
		&& get_receiver_eu_receiver_EQUA_1_send_UnicastPredicateProb().satisfy(receiver)
		) {
				return 1;
		}
		if (action == CGT11Definition.SEND
		&& get_sender_eu_sender_EQUA_1_AND_receiver_eu_receiver_EQUA_1_send_UnicastPredicateProb(sender).satisfy(receiver)
		) {
				return 1;
		}
		if (action == CGT11Definition.SEND
		&& get_sender_eu_sender_EQUA_1_AND_receiver_eu_receiver_EQUA_1_AND_global_eu_global_EQUA_1_send_UnicastPredicateProb(sender).satisfy(receiver)
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
		if (action == CGT11Definition.SEND
		&& CarmaPredicate.TRUE.satisfy(sender)
		) {
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
		if (action == CGT11Definition.SEND
		&& CarmaPredicate.FALSE.satisfy(sender)
		) {
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
		if (action == CGT11Definition.SEND
		&& get_sender_eu_sender_EQUA_1_send_UnicastPredicateUpdate().satisfy(sender)
		) {
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
		if (action == CGT11Definition.SEND
		&& get_receiver_eu_receiver_EQUA_1_send_UnicastPredicateUpdate().satisfy(receiver)
		) {
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
		if (action == CGT11Definition.SEND
		&& get_global_eu_global_EQUA_1_send_UnicastPredicateUpdate().satisfy(sender)
		) {
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
		if (action == CGT11Definition.SEND
		&& get_sender_eu_sender_EQUA_1_AND_receiver_eu_receiver_EQUA_1_send_UnicastPredicateUpdate(sender).satisfy(receiver)
		) {
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
		if (action == CGT11Definition.SEND
		&& get_sender_eu_sender_EQUA_1_AND_receiver_eu_receiver_EQUA_1_AND_global_eu_global_EQUA_1_send_UnicastPredicateUpdate(sender).satisfy(receiver)
		) {
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
			new CGT11Factory()
		);
	
		int deadline = 50;
		
		SamplingCollection<CarmaSystem> sc = new SamplingCollection<CarmaSystem>();
		
		sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(deadline+1, 1.0, CGT11Definition.getMeasureWaiting_Producer_Send(1,1)));
		sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(deadline+1, 1.0, CGT11Definition.getMeasureWaiting_Producer_Send(2,1)));
		sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(deadline+1, 1.0, CGT11Definition.getMeasureWaiting_Producer_Send(3,1)));
		sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(deadline+1, 1.0, CGT11Definition.getMeasureWaiting_Producer_Send(1,2)));
		sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(deadline+1, 1.0, CGT11Definition.getMeasureWaiting_Producer_Send(2,2)));
		sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(deadline+1, 1.0, CGT11Definition.getMeasureWaiting_Producer_Send(3,2)));
		sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(deadline+1, 1.0, CGT11Definition.getMeasureWaiting_Producer_Send(1,3)));
		sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(deadline+1, 1.0, CGT11Definition.getMeasureWaiting_Producer_Send(2,3)));
		sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(deadline+1, 1.0, CGT11Definition.getMeasureWaiting_Producer_Send(3,3)));
		
		system.setSampling(sc);
		system.simulate(100,50);
		for(int i = 0; i < sc.size(); i++){
			((StatisticSampling<CarmaSystem>) sc.get(i)).printTimeSeries(System.out);
		}
	}
	
}
