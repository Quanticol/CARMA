package carma.CGT11;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.SimulationEnvironment;
import org.cmg.ml.sam.sim.sampling.*;
import eu.quanticol.carma.simulator.*;

public class CGT11Definition {
	
	/*METHOD VARIABLES*/
	/*COMPONENT ATTRIBUTES*/
	public static final String PRODUCT_ATTRIBUTE = "product";
	public static final Class<Integer> PRODUCT_ATTRIBUTE_TYPE = Integer.class;
	public static final String POSITION_X_ATTRIBUTE = "position_x";
	public static final Class<Integer> POSITION_X_ATTRIBUTE_TYPE = Integer.class;
	public static final String EU_RECEIVER_ATTRIBUTE = "eu_receiver";
	public static final Class<Integer> EU_RECEIVER_ATTRIBUTE_TYPE = Integer.class;
	public static final String POSITION_Y_ATTRIBUTE = "position_y";
	public static final Class<Integer> POSITION_Y_ATTRIBUTE_TYPE = Integer.class;
	public static final String EU_SENDER_ATTRIBUTE = "eu_sender";
	public static final Class<Integer> EU_SENDER_ATTRIBUTE_TYPE = Integer.class;
	/*INPUT ARGUMENTS*/
	/*ENVIRONMENT ATTRIBUTES*/
	public static final String EU_GLOBAL_ATTRIBUTE = "eu_global";
	public static final Class<Integer> EU_GLOBAL_ATTRIBUTE_TYPE = Integer.class;
	public static final String TEST_Y_ATTRIBUTE = "test_y";
	public static final Class<Integer> TEST_Y_ATTRIBUTE_TYPE = Integer.class;
	public static final String TRANSACTIONS_ATTRIBUTE = "transactions";
	public static final Class<Integer> TRANSACTIONS_ATTRIBUTE_TYPE = Integer.class;
	public static final String TEST_X_ATTRIBUTE = "test_x";
	public static final Class<Integer> TEST_X_ATTRIBUTE_TYPE = Integer.class;
	/*ACTION*/
	public static final int PRODUCE = 0;
	public static final int SEND = 1;
	public static final int CONSUME = 2;
	/*RATES*/
	public static final double TRUE_SEND_RATE = 1;
	public static final double TRUE_PRODUCE_RATE = 1;
	public static final double FALSE_PRODUCE_RATE = 1;
	public static final double SENDER_EU_SENDER_EQUA_1_PRODUCE_RATE = 0.5;
	public static final double GLOBAL_EU_GLOBAL_EQUA_1_PRODUCE_RATE = 1;
	public static final double SENDER_EU_SENDER_EQUA_1_AND_GLOBAL_EU_GLOBAL_EQUA_1_SEND_RATE = 1;
	/*PROCESS*/
	public static final CarmaProcessAutomaton ProducerProcess = createProducerProcess();
	
	private static CarmaProcessAutomaton createProducerProcess() {
		
		CarmaProcessAutomaton toReturn = new CarmaProcessAutomaton("Producer");
		
		
		//create the states in the automata 
		CarmaProcessAutomaton.State state_Send = toReturn.newState("state_Send");
		CarmaProcessAutomaton.State state_Produce = toReturn.newState("state_Produce");
		
		CarmaOutput produce_Action = new CarmaOutput( PRODUCE, true ) {
			
			@Override
			protected CarmaPredicate getPredicate(CarmaStore outputStore) {
				return CarmaPredicate.FALSE;
			}
		
			@Override
			protected CarmaStoreUpdate getUpdate() {
				return new CarmaStoreUpdate() {
					
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
						boolean hasAttributes = true;
						int product = 0;
						if(store.get("product" , Integer.class) != null){
							product = store.get("product" , Integer.class); 
						} else { 
							hasAttributes = false;
						}
						if(hasAttributes){
							store.set("product",product + 1);
						}
					}
				};
			}
		
			@Override
			protected Object getValue(CarmaStore store) {
			return new Object();
			}
		};
		CarmaOutput send_Action = new CarmaOutput( SEND, false ) {
			
			@Override
			protected CarmaPredicate getPredicate(CarmaStore outputStore) {
				return new CarmaPredicate() {
					@Override
					public boolean satisfy(CarmaStore inputStore) {
						boolean hasAttributes = true;
						int eu_receiver_i = 0;
						if(inputStore.get("eu_receiver" , Integer.class) != null){
							eu_receiver_i = inputStore.get("eu_receiver" , Integer.class); 
						} else { 
							hasAttributes = false;
						}
						if(hasAttributes)
							return eu_receiver_i == 1;
						else
							return false;
					}
				};
			}
		
			@Override
			protected CarmaStoreUpdate getUpdate() {
				return new CarmaStoreUpdate() {
					
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
						boolean hasAttributes = true;
						int product = 0;
						if(store.get("product" , Integer.class) != null){
							product = store.get("product" , Integer.class); 
						} else { 
							hasAttributes = false;
						}
						if(hasAttributes){
							store.set("product",product - 1);
						}
					}
				};
			}
		
			@Override
			protected Object getValue(CarmaStore store) {
				int[] output = new int[1];
				output[0] = 1;
				return output;
			}
		};
		
		CarmaPredicate Send_Guard = new CarmaPredicate() {
			@Override
			public boolean satisfy(CarmaStore store) {
				boolean hasAttributes = true;
				int product = 0;
				if(store.get("product" , Integer.class) != null){
					product = store.get("product" , Integer.class); 
				} else { 
					hasAttributes = false;
				}
				if(hasAttributes)
					return product > 0;
				else
					return false;
			}
		};
		CarmaPredicate Produce_Guard = new CarmaPredicate() {
			@Override
			public boolean satisfy(CarmaStore store) {
				boolean hasAttributes = true;
				int product = 0;
				if(store.get("product" , Integer.class) != null){
					product = store.get("product" , Integer.class); 
				} else { 
					hasAttributes = false;
				}
				if(hasAttributes)
					return product > 0;
				else
					return false;
			}
		};
		
		//create the transitions between states
		toReturn.addTransition(state_Send,Send_Guard,send_Action,state_Send);
		toReturn.addTransition(state_Produce,Produce_Guard,produce_Action,state_Produce);
		
		return toReturn;
	}
	public static final CarmaProcessAutomaton ConsumerProcess = createConsumerProcess();
	
	private static CarmaProcessAutomaton createConsumerProcess() {
		
		CarmaProcessAutomaton toReturn = new CarmaProcessAutomaton("Consumer");
		
		
		//create the states in the automata 
		CarmaProcessAutomaton.State state_Receive = toReturn.newState("state_Receive");
		CarmaProcessAutomaton.State state_Consume = toReturn.newState("state_Consume");
		
		CarmaInput send_Action = new CarmaInput( SEND, false ) {
			
			@Override
			protected CarmaPredicate getPredicate(CarmaStore outputStore, Object value) {
				if (value instanceof int[]){
					return new CarmaPredicate() {
						@Override
						public boolean satisfy(CarmaStore inputStore) {
							boolean hasAttributes = true;
							int z_i = ((int[]) value)[0];
							int product_i = 0;
							if(inputStore.get("product" , Integer.class) != null){
								product_i = inputStore.get("product" , Integer.class); 
							} else { 
								hasAttributes = false;
							}
							if(hasAttributes)
								return product_i < 10 && z_i == 1;
							else
								return false;
						}
					};
				}
				return null;
			}
			
			@Override
			protected CarmaStoreUpdate getUpdate(final Object value) {
				
				return new CarmaStoreUpdate() {
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
						if (value instanceof int[]){
							boolean hasAttributes = true;
							int z = ((int[]) value)[0];
							int product = 0;
							if(store.get("product" , Integer.class) != null){
								product = store.get("product" , Integer.class); 
							} else { 
								hasAttributes = false;
							}
							if(hasAttributes){
								store.set("product",product - z);
							}
						};
					};
				
				};
			};
		};
		CarmaOutput consume_Action = new CarmaOutput( CONSUME, true ) {
			
			@Override
			protected CarmaPredicate getPredicate(CarmaStore outputStore) {
				return CarmaPredicate.FALSE;
			}
		
			@Override
			protected CarmaStoreUpdate getUpdate() {
				return new CarmaStoreUpdate() {
					
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
						boolean hasAttributes = true;
						int product = 0;
						if(store.get("product" , Integer.class) != null){
							product = store.get("product" , Integer.class); 
						} else { 
							hasAttributes = false;
						}
						if(hasAttributes){
							store.set("product",product - 1);
						}
					}
				};
			}
		
			@Override
			protected Object getValue(CarmaStore store) {
			return new Object();
			}
		};
		
		CarmaPredicate Consume_Guard = new CarmaPredicate() {
			@Override
			public boolean satisfy(CarmaStore store) {
				boolean hasAttributes = true;
				int product = 0;
				if(store.get("product" , Integer.class) != null){
					product = store.get("product" , Integer.class); 
				} else { 
					hasAttributes = false;
				}
				if(hasAttributes)
					return product > 0;
				else
					return false;
			}
		};
		CarmaPredicate Receive_Guard = new CarmaPredicate() {
			@Override
			public boolean satisfy(CarmaStore store) {
				boolean hasAttributes = true;
				int product = 0;
				if(store.get("product" , Integer.class) != null){
					product = store.get("product" , Integer.class); 
				} else { 
					hasAttributes = false;
				}
				if(hasAttributes)
					return product >= 0;
				else
					return false;
			}
		};
		
		//create the transitions between states
		toReturn.addTransition(state_Consume,Consume_Guard,consume_Action,state_Consume);
		toReturn.addTransition(state_Receive,Receive_Guard,send_Action,state_Receive);
		
		return toReturn;
	}
	/*MEASURES*/
	//predicate states get_MeasureName_State(ProcessName_ProcessName... || All)Predicate()
	public static CarmaProcessPredicate getMeasureWaiting_Producer_Send_State_Predicate(){
		return new CarmaProcessPredicate() {
			
			@Override
			public boolean eval(CarmaProcess p) {
				return ( 
				(((CarmaSequentialProcess) p).automaton() ==  ProducerProcess) && (
				(((CarmaSequentialProcess) p).automaton().getState("state_Send") != null ) ||
				(((CarmaSequentialProcess) p).getState() !=  null))
				) && 
				true;
			}
		};
	}
	//predicate for boolean expression get_MeasureName_BooleanExpression_Predicate()
	protected static CarmaPredicate getPredicateWaiting_Producer_Send(final int iMin, final int jMin) {
		return new CarmaPredicate() {
			@Override
			public boolean satisfy(CarmaStore store) {
				boolean hasAttributes = true;
				int eu_sender = 0;
				if(store.get("eu_sender" , Integer.class) != null){
					eu_sender = store.get("eu_sender" , Integer.class); 
				} else { 
					hasAttributes = false;
				}
				if(hasAttributes)
					return eu_sender == 1;
				else
					return false;
			}
		};
	}
	
	
	public static ComponentPredicate getMeasureWaiting_Producer_Send_BooleanExpression_Predicate(final int iMin, final int jMin){
		return new ComponentPredicate() {
			
			@Override
			public boolean eval(CarmaComponent c){
				return getPredicateWaiting_Producer_Send(iMin, jMin).satisfy(c.getStore()) && (c.isRunning(getMeasureWaiting_Producer_Send_State_Predicate()));
			}
		};
	}
	//getMethod
	public static Measure<CarmaSystem> getMeasureWaiting_Producer_Send(final int iMin, final int jMin){
		
		return new Measure<CarmaSystem>(){
		
			ComponentPredicate predicate = getMeasureWaiting_Producer_Send_BooleanExpression_Predicate(iMin, jMin);
		
			@Override
			public double measure(CarmaSystem t){
				//TODO
			
				return t.measure(predicate);
		
			};
		
			@Override
			public String getName() {
				return "Waiting_Producer_Send";
			}
		};
	}
}