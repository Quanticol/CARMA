package eu.quanticol.carma.core.tests

import org.junit.Test
import org.eclipse.xtext.junit4.XtextRunner
import org.junit.runner.RunWith
import org.eclipse.xtext.junit4.InjectWith
import com.google.inject.Inject
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper
import eu.quanticol.carma.core.carma.Model
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.generator.InMemoryFileSystemAccess
import org.eclipse.xtext.generator.IGenerator

import static org.junit.Assert.*
import org.eclipse.xtext.generator.IFileSystemAccess

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProviderCustom))
class CGT10 {
	@Inject extension CompilationTestHelper
	@Inject ParseHelper<Model> parseHelper 
	@Inject IGenerator underTest
	
	@Test
	def void testGenerationCodeProducer(){
		val model = parseHelper.parse('''
component Producer(enum a, enum b, enum c, Z){
    
    store{
        enum product := a;
        record position := {x := b, y := c};
    }

    behaviour{
        Produce = [my.product > 0] produce*{product := product + 1}.Produce + [my.product == 0] produceDouble*{product := product + 2}.Produce;
        Send = [my.product > 0] send<1>{product := product - 1}.Send;
    }

    init{
        Z;
    }
}

component Consumer(enum a, record t, Z){
    
    store{
        enum product := a;
        record position := t;
    }

    behaviour{
        Consume = [my.product > 0] consume*{product := product - 1}.Consume + [my.product > 2] consumeDouble*{product := product - 2}.Consume;
        Receive = [my.product > 0] send(z){product := product - z}.Receive;
    }

    init{
        Z;
    }
}

measures{
	measure Waiting[ enum i := 0..3, enum j := 0..3] = #{Consumer[*]  | position == {x := i, y:= j} };
}


system Simple{

    collective{
        new Producer(1..6,0..3,0..3,Produce|Send);
        new Consumer(1..6,{x := 0..3, y := 0..3},Consume|Receive);
    }

    environment{
    	
    	store{
    		enum transactions := 1;
    		record aPosition := { x := 2, y := 3 };
    	}
    	
        rate{
        	[True] produce* :=  1/(sender.transactions+1);
        	[True] send := 1;
        	[True] produceDouble* := 1;
        }
        
        update{
        	[True] send := transactions := transactions + 1;
        }
    }
}
		'''
		)
		val fsa = new InMemoryFileSystemAccess()
		underTest.doGenerate(model.eResource, fsa)
		println(fsa.files.get("DEFAULT_OUTPUTcarma/__synthetic0/__synthetic0Factory.java"))
		println(fsa.files)
		assertEquals(3,fsa.files.size)
		//DEFAULT_OUTPUT+"carma/__synthetic0/Simple.java"
		//DEFAULT_OUTPUTcarma/__synthetic0/__synthetic0Definition.java
		//DEFAULT_OUTPUTcarma/__synthetic0/__synthetic0Factory.java
		assertTrue(fsa.files.containsKey(IFileSystemAccess::DEFAULT_OUTPUT+"carma/__synthetic0/Simple.java"))
        assertEquals(
            '''package carma.__synthetic0;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.carma.*;
import org.cmg.ml.sam.sim.SimulationEnvironment;
public class Simple extends CarmaSystem {
	
	//constructor
	public Simple(){
		addComponent(getProducer(1,0,0));
		addComponent(getProducer(2,0,0));
		addComponent(getProducer(3,0,0));
		addComponent(getProducer(4,0,0));
		addComponent(getProducer(5,0,0));
		addComponent(getProducer(6,0,0));
		addComponent(getProducer(1,1,0));
		addComponent(getProducer(2,1,0));
		addComponent(getProducer(3,1,0));
		addComponent(getProducer(4,1,0));
		addComponent(getProducer(5,1,0));
		addComponent(getProducer(6,1,0));
		addComponent(getProducer(1,2,0));
		addComponent(getProducer(2,2,0));
		addComponent(getProducer(3,2,0));
		addComponent(getProducer(4,2,0));
		addComponent(getProducer(5,2,0));
		addComponent(getProducer(6,2,0));
		addComponent(getProducer(1,3,0));
		addComponent(getProducer(2,3,0));
		addComponent(getProducer(3,3,0));
		addComponent(getProducer(4,3,0));
		addComponent(getProducer(5,3,0));
		addComponent(getProducer(6,3,0));
		addComponent(getProducer(1,0,1));
		addComponent(getProducer(2,0,1));
		addComponent(getProducer(3,0,1));
		addComponent(getProducer(4,0,1));
		addComponent(getProducer(5,0,1));
		addComponent(getProducer(6,0,1));
		addComponent(getProducer(1,1,1));
		addComponent(getProducer(2,1,1));
		addComponent(getProducer(3,1,1));
		addComponent(getProducer(4,1,1));
		addComponent(getProducer(5,1,1));
		addComponent(getProducer(6,1,1));
		addComponent(getProducer(1,2,1));
		addComponent(getProducer(2,2,1));
		addComponent(getProducer(3,2,1));
		addComponent(getProducer(4,2,1));
		addComponent(getProducer(5,2,1));
		addComponent(getProducer(6,2,1));
		addComponent(getProducer(1,3,1));
		addComponent(getProducer(2,3,1));
		addComponent(getProducer(3,3,1));
		addComponent(getProducer(4,3,1));
		addComponent(getProducer(5,3,1));
		addComponent(getProducer(6,3,1));
		addComponent(getProducer(1,0,2));
		addComponent(getProducer(2,0,2));
		addComponent(getProducer(3,0,2));
		addComponent(getProducer(4,0,2));
		addComponent(getProducer(5,0,2));
		addComponent(getProducer(6,0,2));
		addComponent(getProducer(1,1,2));
		addComponent(getProducer(2,1,2));
		addComponent(getProducer(3,1,2));
		addComponent(getProducer(4,1,2));
		addComponent(getProducer(5,1,2));
		addComponent(getProducer(6,1,2));
		addComponent(getProducer(1,2,2));
		addComponent(getProducer(2,2,2));
		addComponent(getProducer(3,2,2));
		addComponent(getProducer(4,2,2));
		addComponent(getProducer(5,2,2));
		addComponent(getProducer(6,2,2));
		addComponent(getProducer(1,3,2));
		addComponent(getProducer(2,3,2));
		addComponent(getProducer(3,3,2));
		addComponent(getProducer(4,3,2));
		addComponent(getProducer(5,3,2));
		addComponent(getProducer(6,3,2));
		addComponent(getProducer(1,0,3));
		addComponent(getProducer(2,0,3));
		addComponent(getProducer(3,0,3));
		addComponent(getProducer(4,0,3));
		addComponent(getProducer(5,0,3));
		addComponent(getProducer(6,0,3));
		addComponent(getProducer(1,1,3));
		addComponent(getProducer(2,1,3));
		addComponent(getProducer(3,1,3));
		addComponent(getProducer(4,1,3));
		addComponent(getProducer(5,1,3));
		addComponent(getProducer(6,1,3));
		addComponent(getProducer(1,2,3));
		addComponent(getProducer(2,2,3));
		addComponent(getProducer(3,2,3));
		addComponent(getProducer(4,2,3));
		addComponent(getProducer(5,2,3));
		addComponent(getProducer(6,2,3));
		addComponent(getProducer(1,3,3));
		addComponent(getProducer(2,3,3));
		addComponent(getProducer(3,3,3));
		addComponent(getProducer(4,3,3));
		addComponent(getProducer(5,3,3));
		addComponent(getProducer(6,3,3));
		addComponent(getConsumer(1,0,0));
		addComponent(getConsumer(2,0,0));
		addComponent(getConsumer(3,0,0));
		addComponent(getConsumer(4,0,0));
		addComponent(getConsumer(5,0,0));
		addComponent(getConsumer(6,0,0));
		addComponent(getConsumer(1,1,0));
		addComponent(getConsumer(2,1,0));
		addComponent(getConsumer(3,1,0));
		addComponent(getConsumer(4,1,0));
		addComponent(getConsumer(5,1,0));
		addComponent(getConsumer(6,1,0));
		addComponent(getConsumer(1,2,0));
		addComponent(getConsumer(2,2,0));
		addComponent(getConsumer(3,2,0));
		addComponent(getConsumer(4,2,0));
		addComponent(getConsumer(5,2,0));
		addComponent(getConsumer(6,2,0));
		addComponent(getConsumer(1,3,0));
		addComponent(getConsumer(2,3,0));
		addComponent(getConsumer(3,3,0));
		addComponent(getConsumer(4,3,0));
		addComponent(getConsumer(5,3,0));
		addComponent(getConsumer(6,3,0));
		addComponent(getConsumer(1,0,1));
		addComponent(getConsumer(2,0,1));
		addComponent(getConsumer(3,0,1));
		addComponent(getConsumer(4,0,1));
		addComponent(getConsumer(5,0,1));
		addComponent(getConsumer(6,0,1));
		addComponent(getConsumer(1,1,1));
		addComponent(getConsumer(2,1,1));
		addComponent(getConsumer(3,1,1));
		addComponent(getConsumer(4,1,1));
		addComponent(getConsumer(5,1,1));
		addComponent(getConsumer(6,1,1));
		addComponent(getConsumer(1,2,1));
		addComponent(getConsumer(2,2,1));
		addComponent(getConsumer(3,2,1));
		addComponent(getConsumer(4,2,1));
		addComponent(getConsumer(5,2,1));
		addComponent(getConsumer(6,2,1));
		addComponent(getConsumer(1,3,1));
		addComponent(getConsumer(2,3,1));
		addComponent(getConsumer(3,3,1));
		addComponent(getConsumer(4,3,1));
		addComponent(getConsumer(5,3,1));
		addComponent(getConsumer(6,3,1));
		addComponent(getConsumer(1,0,2));
		addComponent(getConsumer(2,0,2));
		addComponent(getConsumer(3,0,2));
		addComponent(getConsumer(4,0,2));
		addComponent(getConsumer(5,0,2));
		addComponent(getConsumer(6,0,2));
		addComponent(getConsumer(1,1,2));
		addComponent(getConsumer(2,1,2));
		addComponent(getConsumer(3,1,2));
		addComponent(getConsumer(4,1,2));
		addComponent(getConsumer(5,1,2));
		addComponent(getConsumer(6,1,2));
		addComponent(getConsumer(1,2,2));
		addComponent(getConsumer(2,2,2));
		addComponent(getConsumer(3,2,2));
		addComponent(getConsumer(4,2,2));
		addComponent(getConsumer(5,2,2));
		addComponent(getConsumer(6,2,2));
		addComponent(getConsumer(1,3,2));
		addComponent(getConsumer(2,3,2));
		addComponent(getConsumer(3,3,2));
		addComponent(getConsumer(4,3,2));
		addComponent(getConsumer(5,3,2));
		addComponent(getConsumer(6,3,2));
		addComponent(getConsumer(1,0,3));
		addComponent(getConsumer(2,0,3));
		addComponent(getConsumer(3,0,3));
		addComponent(getConsumer(4,0,3));
		addComponent(getConsumer(5,0,3));
		addComponent(getConsumer(6,0,3));
		addComponent(getConsumer(1,1,3));
		addComponent(getConsumer(2,1,3));
		addComponent(getConsumer(3,1,3));
		addComponent(getConsumer(4,1,3));
		addComponent(getConsumer(5,1,3));
		addComponent(getConsumer(6,1,3));
		addComponent(getConsumer(1,2,3));
		addComponent(getConsumer(2,2,3));
		addComponent(getConsumer(3,2,3));
		addComponent(getConsumer(4,2,3));
		addComponent(getConsumer(5,2,3));
		addComponent(getConsumer(6,2,3));
		addComponent(getConsumer(1,3,3));
		addComponent(getConsumer(2,3,3));
		addComponent(getConsumer(3,3,3));
		addComponent(getConsumer(4,3,3));
		addComponent(getConsumer(5,3,3));
		addComponent(getConsumer(6,3,3));
		global_store.set(__synthetic0Definition.TRANSACTIONS,1)
		c4rm4.set( __synthetic0Definition.APOSITION_X_ATTRIBUTE, 2);
		c4rm4.set( __synthetic0Definition.APOSITION_Y_ATTRIBUTE, 3);
	}
	
	private CarmaComponent getProducer(int a,int b,int c) {
		CarmaComponent c4rm4 = new CarmaComponent();
		c4rm4.set( __synthetic0Definition.PRODUCT_ATTRIBUTE, a);
		c4rm4.set( __synthetic0Definition.POSITION_X_ATTRIBUTE, b);
		c4rm4.set( __synthetic0Definition.POSITION_Y_ATTRIBUTE, c);
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		__synthetic0Definition.ProducerProcess,
		__synthetic0Definition.ProducerProcess.getState("state_Produce" ) );
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		__synthetic0Definition.ProducerProcess,
		__synthetic0Definition.ProducerProcess.getState("state_Send" ) );
		return c4rm4;
	}
	private CarmaComponent getConsumer(int a, int x, int y) {
		CarmaComponent c4rm4 = new CarmaComponent();
		c4rm4.set( __synthetic0Definition.PRODUCT_ATTRIBUTE, a);
		c4rm4.set( __synthetic0Definition.POSITION_X_ATTRIBUTE, x);
		c4rm4.set( __synthetic0Definition.POSITION_Y_ATTRIBUTE, y);
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		__synthetic0Definition.ConsumerProcess,
		__synthetic0Definition.ConsumerProcess.getState("state_Consume" ) );
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		__synthetic0Definition.ConsumerProcess,
		__synthetic0Definition.ConsumerProcess.getState("state_Receive" ) );
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
		 && action == __synthetic0Definition.PRODUCE) {
				return 1 / eu.quanticol.carma.core.carma.impl.EnvironmentExpressionImpl@2c5d601e;
		}
		if (True
		 && action == __synthetic0Definition.PRODUCEDOUBLE) {
				return 1;
		}
		return 0;
	}
	@Override
	public double unicastRate(CarmaStore sender, int action) {
		if (True
		 && action == __synthetic0Definition.SEND) {
				return 1;
		}
	}
	
	/*ENVIRONMENT UPDATE*/
	@Override
	public void broadcastUpdate(RandomGenerator random, CarmaStore sender,
	int action) {
	}
	
	@Override
	public void unicastUpdate(RandomGenerator random, CarmaStore sender,
	int action) {
		if (True
		 && action == __synthetic0Definition.SEND) {
				//updates
		}
	}
	
	/*MAIN*/
	public static void main( String[] argv ) {
		SimulationEnvironment<CarmaSystem> system = new SimulationEnvironment<CarmaSystem>(
			new __synthetic0Factory()
		);
	
		system.simulate(100,50);
	}
	
}
'''.toString, fsa.files.get(IFileSystemAccess::DEFAULT_OUTPUT+"carma/__synthetic0/Simple.java").toString
        )
    assertTrue(fsa.files.containsKey(IFileSystemAccess::DEFAULT_OUTPUT+"carma/__synthetic0/__synthetic0Definition.java"))
    assertEquals('''package carma.__synthetic0;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.carma.*;
import org.cmg.ml.sam.sim.SimulationEnvironment;

public class __synthetic0Definition {
	
	/*METHOD VARIABLES*/
	/*COMPONENT ATTRIBUTES*/
	public static final String PRODUCT_ATTRIBUTE = "product";
	public static final Class<Integer> PRODUCT_ATTRIBUTE_TYPE = Integer.class;
	public static final String POSITION_X_ATTRIBUTE = "position_x";
	public static final Class<Integer> POSITION_X_ATTRIBUTE_TYPE = Integer.class;
	public static final String POSITION_Y_ATTRIBUTE = "position_y";
	public static final Class<Integer> POSITION_Y_ATTRIBUTE_TYPE = Integer.class;
	/*INPUT ARGUMENTS*/
	/*ENVIRONMENT ATTRIBUTES*/
	public static final String APOSITION_X_ATTRIBUTE = "aPosition_x";
	public static final Class<Integer> APOSITION_X_ATTRIBUTE_TYPE = Integer.class;
	public static final String TRANSACTIONS_ATTRIBUTE = "transactions";
	public static final Class<Integer> TRANSACTIONS_ATTRIBUTE_TYPE = Integer.class;
	public static final String APOSITION_Y_ATTRIBUTE = "aPosition_y";
	public static final Class<Integer> APOSITION_Y_ATTRIBUTE_TYPE = Integer.class;
	/*ACTION*/
	public static final int PRODUCE = 0;
	public static final int PRODUCEDOUBLE = 1;
	public static final int SEND = 2;
	public static final int CONSUME = 3;
	public static final int CONSUMEDOUBLE = 4;
	/*RATES*/
	public static final double PRODUCE_RATE = 1 / eu.quanticol.carma.core.carma.impl.EnvironmentExpressionImpl@2c5d601e;
	public static final double SEND_RATE = 1;
	public static final double PRODUCEDOUBLE_RATE = 1;
	/*PROCESS*/
	public static final CarmaProcessAutomaton ProducerProcess = createProducerProcess();
	
	private static CarmaProcessAutomaton createProducerProcess() {
		
		CarmaProcessAutomaton toReturn = new CarmaProcessAutomaton("Producer");
		
		
		//create the states in the automata 
		CarmaProcessAutomaton.State state_Send = toReturn.newState("state_Send");
		CarmaProcessAutomaton.State state_Produce = toReturn.newState("state_Produce");
		
		CarmaOutput produce_Action = new CarmaOutput( PRODUCE, true ) {
			
			@Override
			protected CarmaPredicate getPredicate(CarmaStore store) {
				return CarmaPredicate.FALSE;
			}
		
			@Override
			protected CarmaStoreUpdate getUpdate() {
				return new CarmaStoreUpdate() {
					
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
						int product = store.get("product" , Integer.class );
						store.set("product",product + 1);
					
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
			protected CarmaPredicate getPredicate(CarmaStore store) {
				return CarmaPredicate.FALSE;
			}
		
			@Override
			protected CarmaStoreUpdate getUpdate() {
				return new CarmaStoreUpdate() {
					
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
						int product = store.get("product" , Integer.class );
						store.set("product",product - 1);
					
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
		CarmaOutput produceDouble_Action = new CarmaOutput( PRODUCEDOUBLE, true ) {
			
			@Override
			protected CarmaPredicate getPredicate(CarmaStore store) {
				return CarmaPredicate.FALSE;
			}
		
			@Override
			protected CarmaStoreUpdate getUpdate() {
				return new CarmaStoreUpdate() {
					
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
						int product = store.get("product" , Integer.class );
						store.set("product",product + 2);
					
					}
				};
			}
		
			@Override
			protected Object getValue(CarmaStore store) {
				return new Object();
			}
		};
		
		CarmaPredicate Send_Guard = new CarmaPredicate() {
			@Override
			public boolean satisfy(CarmaStore store) {
				int product = store.get("product" , Integer.class );
				return product > 0;
			}
		};
		CarmaPredicate Produce_Guard = new CarmaPredicate() {
			@Override
			public boolean satisfy(CarmaStore store) {
				int product = store.get("product" , Integer.class );
				return product > 0;
			}
		};
		
		//create the transitions between states
		toReturn.addTransition(state_Send,Send_Guard,send_Action,state_Send);
		toReturn.addTransition(state_Produce,Produce_Guard,produceDouble_Action,state_Produce);
		toReturn.addTransition(state_Produce,Produce_Guard,produce_Action,state_Produce);
		
		return toReturn;
	}
	public static final CarmaProcessAutomaton ConsumerProcess = createConsumerProcess();
	
	private static CarmaProcessAutomaton createConsumerProcess() {
		
		CarmaProcessAutomaton toReturn = new CarmaProcessAutomaton("Consumer");
		
		
		//create the states in the automata 
		CarmaProcessAutomaton.State state_Receive = toReturn.newState("state_Receive");
		CarmaProcessAutomaton.State state_Consume = toReturn.newState("state_Consume");
		
		CarmaOutput consumeDouble_Action = new CarmaOutput( CONSUMEDOUBLE, true ) {
			
			@Override
			protected CarmaPredicate getPredicate(CarmaStore store) {
				return CarmaPredicate.FALSE;
			}
		
			@Override
			protected CarmaStoreUpdate getUpdate() {
				return new CarmaStoreUpdate() {
					
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
						int product = store.get("product" , Integer.class );
						store.set("product",product - 2);
					
					}
				};
			}
		
			@Override
			protected Object getValue(CarmaStore store) {
				return new Object();
			}
		};
		CarmaInput send_Action = new CarmaInput( SEND, false ) {
			
			@Override
			protected CarmaPredicate getPredicate(CarmaStore store, Object value) {
				return CarmaPredicate.TRUE;
			}
			
			@Override
			protected CarmaStoreUpdate getUpdate(final Object value) {
				
				return new CarmaStoreUpdate() {
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
						if (value instanceof int[]){
					
							int z = ((int[]) value)[0];
							int product = store.get("product" , Integer.class );
							store.set("product",product - z);
						
						};
					};
				
				};
			};
		};
		CarmaOutput consume_Action = new CarmaOutput( CONSUME, true ) {
			
			@Override
			protected CarmaPredicate getPredicate(CarmaStore store) {
				return CarmaPredicate.FALSE;
			}
		
			@Override
			protected CarmaStoreUpdate getUpdate() {
				return new CarmaStoreUpdate() {
					
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
						int product = store.get("product" , Integer.class );
						store.set("product",product - 1);
					
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
				int product = store.get("product" , Integer.class );
				return product > 2;
			}
		};
		CarmaPredicate Receive_Guard = new CarmaPredicate() {
			@Override
			public boolean satisfy(CarmaStore store) {
				int product = store.get("product" , Integer.class );
				return product > 0;
			}
		};
		
		//create the transitions between states
		toReturn.addTransition(state_Receive,Receive_Guard,send_Action,state_Receive);
		toReturn.addTransition(state_Consume,Consume_Guard,consume_Action,state_Consume);
		toReturn.addTransition(state_Consume,Consume_Guard,consumeDouble_Action,state_Consume);
		
		return toReturn;
	}
	/*MEASURES*/
}
'''.toString, fsa.files.get(IFileSystemAccess::DEFAULT_OUTPUT+"carma/__synthetic0/__synthetic0Definition.java").toString)
	assertTrue(fsa.files.containsKey(IFileSystemAccess::DEFAULT_OUTPUT+"carma/__synthetic0/__synthetic0Factory.java"))
    assertEquals('''package carma.__synthetic0;
import org.cmg.ml.sam.carma.CarmaSystem;
import org.cmg.ml.sam.sim.SimulationFactory;
public class __synthetic0Factory implements SimulationFactory<CarmaSystem> {

	public __synthetic0Factory() {
		
	}
	
	@Override
	public CarmaSystem getModel() {
		return new Simple();
	}

}
'''.toString, fsa.files.get(IFileSystemAccess::DEFAULT_OUTPUT+"carma/__synthetic0/__synthetic0Factory.java").toString)
	}
	


	
}