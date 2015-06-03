package carma.CGT14eu;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.SimulationEnvironment;
import org.cmg.ml.sam.sim.sampling.*;
import eu.quanticol.carma.simulator.*;

public class CGT14euDefinition {
	
	/*METHOD VARIABLES*/
	/*COMPONENT ATTRIBUTES*/
	public static final String PRODUCT_ATTRIBUTE = "product";
	public static final Class<Integer> PRODUCT_ATTRIBUTE_TYPE = Integer.class;
	public static final String POSITION_X_ATTRIBUTE = "position_x";
	public static final Class<Integer> POSITION_X_ATTRIBUTE_TYPE = Integer.class;
	public static final String POSITION_Y_ATTRIBUTE = "position_y";
	public static final Class<Integer> POSITION_Y_ATTRIBUTE_TYPE = Integer.class;
	public static final String TYPE_ATTRIBUTE = "type";
	public static final Class<Integer> TYPE_ATTRIBUTE_TYPE = Integer.class;
	/*INPUT ARGUMENTS*/
	/*ENVIRONMENT ATTRIBUTES*/
	public static final String TRANSACTIONS_ATTRIBUTE = "transactions";
	public static final Class<Integer> TRANSACTIONS_ATTRIBUTE_TYPE = Integer.class;
	/*ACTION*/
	public static final int PRODUCE = 0;
	public static final int SEND = 1;
	public static final int CONSUME = 2;
	public static final int NOTHING = 3;
	/*RATES*/
	public static final double TRUE_SEND_RATE = 1;
	public static final double TRUE_PRODUCE_RATE = 1;
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
				return CarmaPredicate.TRUE;
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
						int type_i = 0;
						if(inputStore.get("type" , Integer.class) != null){
							type_i = inputStore.get("type" , Integer.class); 
						} else { 
							hasAttributes = false;
						}
						if(hasAttributes)
							return type_i == 0;
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
		
		
		//create the transitions between states
		toReturn.addTransition(state_Produce,produce_Action,state_Produce);
		toReturn.addTransition(state_Send,send_Action,state_Send);
		
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
				return CarmaPredicate.TRUE;
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
								store.set("product",product + z);
							}
						};
					};
				
				};
			};
		};
		CarmaOutput consume_Action = new CarmaOutput( CONSUME, true ) {
			
			@Override
			protected CarmaPredicate getPredicate(CarmaStore outputStore) {
				return CarmaPredicate.TRUE;
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
		
		//create the transitions between states
		toReturn.addTransition(state_Consume,Consume_Guard,consume_Action,state_Consume);
		toReturn.addTransition(state_Receive,send_Action,state_Receive);
		
		return toReturn;
	}
	public static final CarmaProcessAutomaton ChildProcess = createChildProcess();
	
	private static CarmaProcessAutomaton createChildProcess() {
		
		CarmaProcessAutomaton toReturn = new CarmaProcessAutomaton("Child");
		
		
		//create the states in the automata 
		CarmaProcessAutomaton.State state_Nothing = toReturn.newState("state_Nothing");
		
		CarmaOutput nothing_Action = new CarmaOutput( NOTHING, true ) {
			
			@Override
			protected CarmaPredicate getPredicate(CarmaStore outputStore) {
				return CarmaPredicate.TRUE;
			}
		
			@Override
			protected CarmaStoreUpdate getUpdate() {
				return new CarmaStoreUpdate() {
					
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
						boolean hasAttributes = true;
					}
				};
			}
		
			@Override
			protected Object getValue(CarmaStore store) {
			return new Object();
			}
		};
		
		
		//create the transitions between states
		toReturn.addTransition(state_Nothing,nothing_Action,state_Nothing);
		
		return toReturn;
	}
	/*MEASURES*/
	//predicate states get_MeasureName_State(ProcessName_ProcessName... || All)Predicate()
	public static CarmaProcessPredicate getMeasureWaiting__All_State_Predicate(){
		return new CarmaProcessPredicate() {
			
			@Override
			public boolean eval(CarmaProcess p) {
				return ( 
				(((CarmaSequentialProcess) p).automaton() ==  ChildProcess) && (
				(((CarmaSequentialProcess) p).automaton().getState("state_Nothing") != null ) ||
				(((CarmaSequentialProcess) p).getState() !=  null))
				)
				||( 
				(((CarmaSequentialProcess) p).automaton() ==  ProducerProcess) && (
				(((CarmaSequentialProcess) p).automaton().getState("state_Send") != null ) ||
				(((CarmaSequentialProcess) p).automaton().getState("state_Produce") != null ) ||
				(((CarmaSequentialProcess) p).getState() !=  null))
				)
				||( 
				(((CarmaSequentialProcess) p).automaton() ==  ConsumerProcess) && (
				(((CarmaSequentialProcess) p).automaton().getState("state_Receive") != null ) ||
				(((CarmaSequentialProcess) p).automaton().getState("state_Consume") != null ) ||
				(((CarmaSequentialProcess) p).getState() !=  null))
				)
				;
			}
		};
	}
	//predicate for boolean expression get_MeasureName_BooleanExpression_Predicate()
	protected static CarmaPredicate getPredicateWaiting__All(final int i) {
		return new CarmaPredicate() {
			@Override
			public boolean satisfy(CarmaStore store) {
				boolean hasAttributes = true;
				if(hasAttributes)
					return true;
				else
					return false;
			}
		};
	}
	
	
	public static ComponentPredicate getMeasureWaiting__All_BooleanExpression_Predicate(final int i){
		return new ComponentPredicate() {
			
			@Override
			public boolean eval(CarmaComponent c){
				return getPredicateWaiting__All(i).satisfy(c.getStore()) && (c.isRunning(getMeasureWaiting__All_State_Predicate()));
			}
		};
	}
	//getMethod
	public static Measure<CarmaSystem> getMeasureWaiting__All(final int i){
		
		return new Measure<CarmaSystem>(){
		
			ComponentPredicate predicate = getMeasureWaiting__All_BooleanExpression_Predicate(i);
		
			@Override
			public double measure(CarmaSystem t){
				//TODO
			
				return t.measure(predicate);
		
			};
		
			@Override
			public String getName() {
				return "Waiting__All";
			}
		};
	}
}
