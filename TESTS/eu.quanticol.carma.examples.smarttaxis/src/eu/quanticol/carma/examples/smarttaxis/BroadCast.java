package eu.quanticol.carma.examples.smarttaxis;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.*;		
import eu.quanticol.carma.simulator.*;
import eu.quanticol.carma.simulator.space.Location;
import eu.quanticol.carma.simulator.space.Node;
import eu.quanticol.carma.simulator.space.SpaceModel;
import eu.quanticol.carma.simulator.space.Tuple;
import eu.quanticol.carma.simulator.space.Edge;
import java.util.LinkedList;
import java.util.HashSet;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.List;
import java.util.TreeSet;
import java.util.Collection;
import java.util.function.Function;
import java.util.function.Predicate;
import org.cmg.ml.sam.sim.sampling.*;


public class BroadCast extends CarmaModel {
	
	public BroadCast() {
		generateAgentBehaviour( );
	}
	

	
	public static class __RECORD__Message implements Cloneable {
		
		public Node __FIELD__locSender;
		
		public __RECORD__Message( Node __FIELD__locSender) {
			this.__FIELD__locSender = __FIELD__locSender;
		}
	
		public __RECORD__Message( __RECORD__Message record ) {
			this.__FIELD__locSender = record.__FIELD__locSender;
		}
		
		public String toString() {
			return "[ "+"locSender="+__FIELD__locSender+" ]";
		}
		
		public boolean equals( Object o ) {
			if (o instanceof __RECORD__Message) {
				__RECORD__Message other = (__RECORD__Message) o;
				return 
				this.__FIELD__locSender.equals( other.__FIELD__locSender )					
						;	
			}	
			return false;
		}
		
		public __RECORD__Message clone() {
			return new __RECORD__Message( this );
		}
	}

	public final int __CONST__N = 2;

	public LinkedList<Node> __FUN__createSendingList ( 
		HashSet<Node> __VARIABLE__next
	) {
		{
			//
			LinkedList<Node> __VARIABLE__toReturn =new LinkedList<Node>()
			;
			//
			//
			for( Node __VARIABLE__l:  __VARIABLE__next ) 
				{
					//
					__VARIABLE__toReturn = concatenate( __VARIABLE__toReturn , getList( __VARIABLE__l )  );
					//
				}
			//
			//
			return __VARIABLE__toReturn;
			//
		}
	}
	
	public SpaceModel get_SPACE_Grid(   Integer __VARIABLE__width,   Integer __VARIABLE__height  ) {
		SpaceModel sm = new SpaceModel();
		
		for( int __VARIABLE__i = 0 ; __VARIABLE__i < __VARIABLE__width ; __VARIABLE__i += 1 ) 
			{
				for( int __VARIABLE__j = 0 ; __VARIABLE__j < __VARIABLE__height ; __VARIABLE__j += 1 ) 
					{
						sm.addVertex( null , new Tuple( __VARIABLE__i,__VARIABLE__j ) );
					}
			}
		
		for( int __VARIABLE__i = 0 ; __VARIABLE__i < __VARIABLE__width ; __VARIABLE__i += 1 ) 
			{
				for( int __VARIABLE__j = 0 ; __VARIABLE__j < __VARIABLE__height ; __VARIABLE__j += 1 ) 
					{
						if (( __VARIABLE__i )<( ( __VARIABLE__width )-( 1 ) )) {
							{
								Node l1 = sm.getVertex( new Tuple( __VARIABLE__i,__VARIABLE__j) );
								Node l2 = sm.getVertex( new Tuple( ( __VARIABLE__i )+( 1 ),__VARIABLE__j) );
								if ((l1 != null)&&(l2 != null)) {
									HashMap<String,Object> data = new HashMap<>();
									sm.addEdge( l1 , data , l2);
									sm.addEdge( l2, data , l1 );
								}
							}		
						}
						if (( __VARIABLE__j )<( ( __VARIABLE__height )-( 1 ) )) {
							{
								Node l1 = sm.getVertex( new Tuple( __VARIABLE__i,__VARIABLE__j) );
								Node l2 = sm.getVertex( new Tuple( __VARIABLE__i,( __VARIABLE__j )+( 1 )) );
								if ((l1 != null)&&(l2 != null)) {
									HashMap<String,Object> data = new HashMap<>();
									sm.addEdge( l1 , data , l2);
									sm.addEdge( l2, data , l1 );
								}
							}		
						}
					}
			}
		
	
		return sm;
	}		
	
	
	
	
	/* START COMPONENT: Agent         */
	
	/* DEFINITIONS OF PROCESSES */
	public final CarmaProcessAutomaton _COMP_Agent = new CarmaProcessAutomaton("Agent");
	
	public final CarmaProcessAutomaton.State __STATE___Agent_IDLE = _COMP_Agent.newState("IDLE");		
	public final CarmaProcessAutomaton.State __STATE___Agent_ACTIVE = _COMP_Agent.newState("ACTIVE");		
	
	private void generateAgentBehaviour( ) {
		
		
		{
			CarmaAction action = new CarmaInput( 
				__ACT_NAME__forward , __ACT__forward , false  		
			) {
				
				@Override
				protected CarmaStoreUpdate getUpdate(CarmaSystem sys, final Object value, final double now) {
					
					LinkedList<Object> message = (LinkedList<Object>) value;
					return new CarmaStoreUpdate() {
						
						//@Override
						public void update(RandomGenerator r, CarmaStore store) {
							Node __MY__loc = store.get( "loc" , Node.class );
							Node __ATTR__loc = store.get( "loc" , Node.class );
							store.set( "pending", __FUN__createSendingList( 
										__MY__loc.getPoset()
									) );
						}
					};
								
				}	
				
				@Override
				protected CarmaPredicate getPredicate(CarmaSystem sys, CarmaStore myStore, Object value) {
							LinkedList<Object> message = (LinkedList<Object>) value;
							final Node __MY__loc = myStore.get( "loc" , Node.class );
							return new CarmaPredicate() {
			
								//@Override
								public boolean satisfy(double now,CarmaStore store) {
									try {
										Node __ATTR__loc = store.get( "loc" , Node.class );
										return ( __MY__loc.getPreset()
										.contains(  ( __ATTR__loc ) ));
									} catch (NullPointerException e) {
										return false;
									}
								}
								
							};
					
				}
							
			};		
			
			_COMP_Agent.addTransition( 
				__STATE___Agent_IDLE , 
				action , 
				__STATE___Agent_ACTIVE );			
		}
		{
			CarmaAction action = new CarmaOutput(
				__ACT_NAME__activate , __ACT__activate , true  		
			) {
				
				@Override
				protected Object getValue(CarmaSystem sys, CarmaStore store, final double now) {
					LinkedList<Object> toReturn = new LinkedList<Object>();
					final Node __MY__loc = store.get( "loc" , Node.class );
					final Node __ATTR__loc = store.get( "loc" , Node.class );
					return toReturn;
				}
				
				@Override
				protected CarmaStoreUpdate getUpdate(CarmaSystem sys,  final double now ) {
					return new CarmaStoreUpdate() {
						
						//@Override
						public void update(RandomGenerator r, CarmaStore store) {
							final Node __MY__loc = store.get( "loc" , Node.class );
							final Node __ATTR__loc = store.get( "loc" , Node.class );
							store.set( "pending", __FUN__createSendingList( 
										__MY__loc.getPoset()
									) );
						}
					};
				}
				
				@Override
				protected CarmaPredicate getPredicate(CarmaSystem sys, final CarmaStore myStore) {
					return CarmaPredicate.FALSE;
					
				}
			};		
			
			_COMP_Agent.addTransition( 
				__STATE___Agent_IDLE , 
				action , 
				__STATE___Agent_ACTIVE );			
		}
		{
			CarmaAction action = new CarmaInput( 
				__ACT_NAME__forward , __ACT__forward , false  		
			) {
				
				@Override
				protected CarmaStoreUpdate getUpdate(CarmaSystem sys, final Object value, final double now) {
					
					LinkedList<Object> message = (LinkedList<Object>) value;
					return new CarmaStoreUpdate() {
						
						//@Override
						public void update(RandomGenerator r, CarmaStore store) {
							Node __MY__loc = store.get( "loc" , Node.class );
							Node __ATTR__loc = store.get( "loc" , Node.class );
							store.set( "pending", __FUN__createSendingList( 
										__MY__loc.getPoset()
									) );
						}
					};
								
				}	
				
				@Override
				protected CarmaPredicate getPredicate(CarmaSystem sys, CarmaStore myStore, Object value) {
							LinkedList<Object> message = (LinkedList<Object>) value;
							final Node __MY__loc = myStore.get( "loc" , Node.class );
							return new CarmaPredicate() {
			
								//@Override
								public boolean satisfy(double now,CarmaStore store) {
									try {
										Node __ATTR__loc = store.get( "loc" , Node.class );
										return ( __MY__loc.getPreset()
										.contains(  ( __ATTR__loc ) ));
									} catch (NullPointerException e) {
										return false;
									}
								}
								
							};
					
				}
							
			};		
			
			_COMP_Agent.addTransition( 
				__STATE___Agent_ACTIVE , 
				action , 
				__STATE___Agent_ACTIVE );			
		}
		{
			CarmaPredicate _FOO_predicate0 = new CarmaPredicate() {
		
				//@Override
				public boolean satisfy(double now,CarmaStore store) {
					final Node __MY__loc = store.get( "loc" , Node.class );
					final Node __ATTR__loc = store.get( "loc" , Node.class );
					LinkedList<Node> __ATTR__pending = (LinkedList<Node>) store.get( "pending" );
					return carmaEquals( computeSize( __ATTR__pending ) , 0 );
				}
					
			};
			{
				CarmaAction action = new CarmaOutput(
					__ACT_NAME__done , __ACT__done , true  		
				) {
					
					@Override
					protected Object getValue(CarmaSystem sys, CarmaStore store, final double now) {
						LinkedList<Object> toReturn = new LinkedList<Object>();
						final Node __MY__loc = store.get( "loc" , Node.class );
						final Node __ATTR__loc = store.get( "loc" , Node.class );
						return toReturn;
					}
					
					@Override
					protected CarmaStoreUpdate getUpdate(CarmaSystem sys,  final double now ) {
						return new CarmaStoreUpdate() {
							
							//@Override
							public void update(RandomGenerator r, CarmaStore store) {
								final Node __MY__loc = store.get( "loc" , Node.class );
								final Node __ATTR__loc = store.get( "loc" , Node.class );
								store.set( "done", true );
							}
						};
					}
					
					@Override
					protected CarmaPredicate getPredicate(CarmaSystem sys, final CarmaStore myStore) {
						return CarmaPredicate.FALSE;
						
					}
				};		
				
				_COMP_Agent.addTransition( 
					__STATE___Agent_ACTIVE , 
					new CarmaPredicate.Conjunction(  _FOO_predicate0  ) , 
					action , 
					null );			
			}
		}
		{
			CarmaPredicate _FOO_predicate0 = new CarmaPredicate() {
		
				//@Override
				public boolean satisfy(double now,CarmaStore store) {
					final Node __MY__loc = store.get( "loc" , Node.class );
					final Node __ATTR__loc = store.get( "loc" , Node.class );
					LinkedList<Node> __ATTR__pending = (LinkedList<Node>) store.get( "pending" );
					return ( computeSize( __ATTR__pending ) )>( 0 );
				}
					
			};
			{
				CarmaAction action = new CarmaOutput(
					__ACT_NAME__forward , __ACT__forward , false  		
				) {
					
					@Override
					protected Object getValue(CarmaSystem sys, CarmaStore store, final double now) {
						LinkedList<Object> toReturn = new LinkedList<Object>();
						final Node __MY__loc = store.get( "loc" , Node.class );
						final Node __ATTR__loc = store.get( "loc" , Node.class );
						return toReturn;
					}
					
					@Override
					protected CarmaStoreUpdate getUpdate(CarmaSystem sys,  final double now ) {
						return new CarmaStoreUpdate() {
							
							//@Override
							public void update(RandomGenerator r, CarmaStore store) {
								final Node __MY__loc = store.get( "loc" , Node.class );
								final Node __ATTR__loc = store.get( "loc" , Node.class );
								LinkedList<Node> __MY__pending = (LinkedList<Node>) store.get( "pending" );
								store.set( "pending", tail( __MY__pending ) );
								__MY__pending = tail( __MY__pending );
							}
						};
					}
					
					@Override
					protected CarmaPredicate getPredicate(CarmaSystem sys, final CarmaStore myStore) {
						final Node __MY__loc = myStore.get( "loc" , Node.class );
						return new CarmaPredicate() {
				
							//@Override
							public boolean satisfy(double now,CarmaStore store) {
								try {
									Node __ATTR__loc = store.get( "loc" , Node.class );
									LinkedList<Node> __ATTR__pending = (LinkedList<Node>) store.get( "pending" );
									return carmaEquals( __ATTR__loc , head( __ATTR__pending ) );
								} catch (NullPointerException e) {
									return false;
								}
							}
							
						};
						
					}
				};		
				
				_COMP_Agent.addTransition( 
					__STATE___Agent_ACTIVE , 
					new CarmaPredicate.Conjunction(  _FOO_predicate0  ) , 
					action , 
					__STATE___Agent_ACTIVE );			
			}
		}
		
	}
	
	public CarmaComponent createComponentAgent( 
	) {
		CarmaComponent c = new CarmaComponent();
		c.setName( "Agent" );
		LinkedList<Node> __ATTR__pending;
		LinkedList<Node> __MY__pending;
		__ATTR__pending =  null;
		__MY__pending = __ATTR__pending;
		c.set( "pending" ,  __ATTR__pending );
		Boolean __ATTR__done;
		Boolean __MY__done;
		__ATTR__done =  false;
		__MY__done = __ATTR__done;
		c.set( "done" ,  __ATTR__done );
		c.addAgent( new CarmaSequentialProcess( c , _COMP_Agent , __STATE___Agent_IDLE ));
		return c;
	}	
	
	/* END COMPONENT: Agent */
		
	
	public static final int __ACT__forward = 0;	
	public static final String __ACT_NAME__forward = "forward";
	public static final int __ACT__activate = 1;	
	public static final String __ACT_NAME__activate = "activate";
	public static final int __ACT__done = 2;	
	public static final String __ACT_NAME__done = "done";
	
	
	public String[] getSystems() {
		return new String[] {
			"WithGrid"
		};	
	}
	
	public SimulationFactory<CarmaSystem> getFactory( String name ) {
		if ("WithGrid".equals( name )) {
			return getFactorySystemWithGrid();
		}
		return null;
	}
			
	
	public class __SYSTEM__WithGrid extends CarmaSystem {
		
		public __SYSTEM__WithGrid( ) {
			super(  get_SPACE_Grid ( 2,1) );
			Integer __ATTR__messages;
			Integer __GLOBAL__messages;
			__ATTR__messages =  0;
			__GLOBAL__messages = __ATTR__messages;
			setGLobalAttribute( "messages" , __ATTR__messages );
			Boolean __ATTR__activated;
			Boolean __GLOBAL__activated;
			__ATTR__activated =  false;
			__GLOBAL__activated = __ATTR__activated;
			setGLobalAttribute( "activated" , __ATTR__activated );
			CarmaSystem system = this;
			CarmaSystem sys = this;
			for( Node __VARIABLE__l:  CarmaSystem.getCurrentSpaceModel().getAll() )  {
				{
						CarmaComponent fooComponent = createComponentAgent(					
						);
						fooComponent.setLocation(__VARIABLE__l);
						system.addComponent( fooComponent );
				}
			}
		}
		
		@Override
		public double broadcastProbability( CarmaStore sender , CarmaStore receiver , int action ) {
			return 1.0;
		}
	
		@Override
		public double unicastProbability( CarmaStore sender , CarmaStore receiver , int action ) {
			return 1.0;
		}
		
		@Override
		public double broadcastRate(final CarmaStore sender, int action) {
			final CarmaSystem system = this;
			final CarmaSystem sys = this;
			final CarmaStore global = this.global;
			Boolean __GLOBAL__activated = (Boolean) global.get( "activated" );
			Node __SENDER__loc = sender.get( "loc" , Node.class );
			if ((action==__ACT__activate)
				) {
					//
					if (!( __GLOBAL__activated )) {
						//
						return 1.0;
						//
					}
					else {
						//
						return 0.0;
						//
					}
					//
				}
			{
				//
				return 1.0;
				//
			}
			
		}
		
		@Override
		public double unicastRate(final CarmaStore sender, int action) {
			final CarmaSystem system = this;
			final CarmaSystem sys = this;
			final CarmaStore global = this.global;
			Node __SENDER__loc = sender.get( "loc" , Node.class );
			{
				//
				return 1.0;
				//
			}
			
		}
		
		@Override
		public void broadcastUpdate( 
			final RandomGenerator random , 
			final CarmaStore sender , 
			final int action , 
			final Object value ) {
			final CarmaSystem system = this;
			final CarmaSystem sys = this;
			final CarmaStore global = this.global;
			final CarmaStore store = this.global;
			Node __SENDER__loc = sender.get( "loc" , Node.class );
			if (action==__ACT__activate) {
				store.set( "activated", true );
				return ;				
			}
		}
		
		@Override
		public void unicastUpdate( 
			final RandomGenerator random , 
			final CarmaStore sender , 
			final CarmaStore receiver, 
			int action , 
			final Object value ) {
			final CarmaSystem system = this;
			final CarmaSystem sys = this;
			final CarmaStore global = this.global;
			final CarmaStore store = this.global;
			Integer __GLOBAL__messages = (Integer) global.get( "messages" );
			Node __SENDER__loc = sender.get( "loc" , Node.class );
			if (action==__ACT__forward) {
				store.set( "messages", ( __GLOBAL__messages )+( 1 ) );
				return ;				
			}
		}		
	}
	
	
	public SimulationFactory<CarmaSystem> getFactorySystemWithGrid() {
		return new SimulationFactory<CarmaSystem>() {
	
			//@Override
			public CarmaSystem getModel() {
				CarmaSystem sys = new __SYSTEM__WithGrid();
				CarmaSystem.setCurrentSpaceModel( sys.getSpaceModel() );
				return sys;
			}
		
			//@Override
			public Measure<CarmaSystem> getMeasure(String name) {
				// TODO Auto-generated method stub
				//FIXME!!!!
				return null;
			}
		
		};
		
	}
	
		
		public String[] getMeasures() {
			TreeSet<String> sortedSet = new TreeSet<String>( );
			sortedSet.add( "active" );
			sortedSet.add( "idle" );
			sortedSet.add( "done" );
			sortedSet.add( "messages" );
			return sortedSet.toArray( new String[ sortedSet.size() ] );
		}
		
		public Measure<CarmaSystem> getMeasure( String name , Map<String,Object> parameters ) {
			if ("active".equals( name ) ) {
				return getMeasureactive( parameters );
			}
			if ("idle".equals( name ) ) {
				return getMeasureidle( parameters );
			}
			if ("done".equals( name ) ) {
				return getMeasuredone( parameters );
			}
			if ("messages".equals( name ) ) {
				return getMeasuremessages( parameters );
			}
			return null;
		}
	
		public String[] getMeasureParameters( String name ) {
			if ("active".equals( name ) ) {
				return new String[] { };
			}
			if ("idle".equals( name ) ) {
				return new String[] { };
			}
			if ("done".equals( name ) ) {
				return new String[] { };
			}
			if ("messages".equals( name ) ) {
				return new String[] { };
			}
			return new String[] {};
		}
		
		public Map<String,Class<?>> getParametersType( String name ) {
			if ("active".equals( name ) ) {
				HashMap<String,Class<?>> toReturn = new HashMap<>();
				return toReturn;
			}
			if ("idle".equals( name ) ) {
				HashMap<String,Class<?>> toReturn = new HashMap<>();
				return toReturn;
			}
			if ("done".equals( name ) ) {
				HashMap<String,Class<?>> toReturn = new HashMap<>();
				return toReturn;
			}
			if ("messages".equals( name ) ) {
				HashMap<String,Class<?>> toReturn = new HashMap<>();
				return toReturn;
			}
			return new HashMap<>();
		}
		
	
		private double __MEASURE__active( CarmaSystem system ) {
			final CarmaStore global = system.getGlobalStore();
			final double now = system.now();
			final CarmaSystem sys = system;
			return system.measure( 
				new BasicComponentPredicate(
					new CarmaPredicate() {
						
						//Here we assume that the following "final" references are available (if needed):
						//- global: reference to the global store;
						//- sender: reference to the store of sender;
						//- receiver: reference to the store of the receiver;				
						//@Override
						public boolean satisfy(double now,CarmaStore store) {
							Node __MY__loc = store.get( "loc" , Node.class );
							try{
								Boolean result = true;
								return (result==null?false:result);
							} catch (NullPointerException e) {
								return false;
							}
						}
					
						
					}
					, new CarmaProcessPredicate() {
				
						//@Override
						public boolean eval(CarmaProcess p) {
							if (p instanceof CarmaSequentialProcess) {
								CarmaSequentialProcess csp = (CarmaSequentialProcess) p;
								try{
									return csp.getName().equals("Agent")&&csp.getState().getName().equals("ACTIVE");
								} catch (NullPointerException e) {
									return false;
								}
							}
							return false;
						}
									
					}
					)
			)
			;
		}
		
		
		private Measure<CarmaSystem> getMeasureactive( 
			Map<String,Object> parameters
		) {
			
		
			return new Measure<CarmaSystem>() {
			
				//@Override
				public double measure(final CarmaSystem system) {
					return __MEASURE__active( system );
				}
		
				//@Override
				public String getName() {
					return "active";
				}
			
			};
			
		}
		
		private double __MEASURE__idle( CarmaSystem system ) {
			final CarmaStore global = system.getGlobalStore();
			final double now = system.now();
			final CarmaSystem sys = system;
			return system.measure( 
				new BasicComponentPredicate(
					new CarmaPredicate() {
						
						//Here we assume that the following "final" references are available (if needed):
						//- global: reference to the global store;
						//- sender: reference to the store of sender;
						//- receiver: reference to the store of the receiver;				
						//@Override
						public boolean satisfy(double now,CarmaStore store) {
							Node __MY__loc = store.get( "loc" , Node.class );
							try{
								Boolean result = true;
								return (result==null?false:result);
							} catch (NullPointerException e) {
								return false;
							}
						}
					
						
					}
					, new CarmaProcessPredicate() {
				
						//@Override
						public boolean eval(CarmaProcess p) {
							if (p instanceof CarmaSequentialProcess) {
								CarmaSequentialProcess csp = (CarmaSequentialProcess) p;
								try{
									return csp.getName().equals("Agent")&&csp.getState().getName().equals("IDLE");
								} catch (NullPointerException e) {
									return false;
								}
							}
							return false;
						}
									
					}
					)
			)
			;
		}
		
		
		private Measure<CarmaSystem> getMeasureidle( 
			Map<String,Object> parameters
		) {
			
		
			return new Measure<CarmaSystem>() {
			
				//@Override
				public double measure(final CarmaSystem system) {
					return __MEASURE__idle( system );
				}
		
				//@Override
				public String getName() {
					return "idle";
				}
			
			};
			
		}
		
		private double __MEASURE__done( CarmaSystem system ) {
			final CarmaStore global = system.getGlobalStore();
			final double now = system.now();
			final CarmaSystem sys = system;
			return system.measure( 
				new BasicComponentPredicate(
					new CarmaPredicate() {
						
						//Here we assume that the following "final" references are available (if needed):
						//- global: reference to the global store;
						//- sender: reference to the store of sender;
						//- receiver: reference to the store of the receiver;				
						//@Override
						public boolean satisfy(double now,CarmaStore store) {
							Boolean __MY__done = (Boolean) store.get( "done" );
							Node __MY__loc = store.get( "loc" , Node.class );
							try{
								Boolean result = carmaEquals( __MY__done , true );
								return (result==null?false:result);
							} catch (NullPointerException e) {
								return false;
							}
						}
					
						
					}
				)
			)
			;
		}
		
		
		private Measure<CarmaSystem> getMeasuredone( 
			Map<String,Object> parameters
		) {
			
		
			return new Measure<CarmaSystem>() {
			
				//@Override
				public double measure(final CarmaSystem system) {
					return __MEASURE__done( system );
				}
		
				//@Override
				public String getName() {
					return "done";
				}
			
			};
			
		}
		
		private double __MEASURE__messages( CarmaSystem system ) {
			final CarmaStore global = system.getGlobalStore();
			final double now = system.now();
			final CarmaSystem sys = system;
			Integer __GLOBAL__messages = (Integer) global.get( "messages" );
			return __GLOBAL__messages;
		}
		
		
		private Measure<CarmaSystem> getMeasuremessages( 
			Map<String,Object> parameters
		) {
			
		
			return new Measure<CarmaSystem>() {
			
				//@Override
				public double measure(final CarmaSystem system) {
					return __MEASURE__messages( system );
				}
		
				//@Override
				public String getName() {
					return "messages";
				}
			
			};
			
		}
		
		public static void main( String[] argv ) {
			
			BroadCast bc = new BroadCast();
			
			SimulationEnvironment<CarmaSystem> system = new SimulationEnvironment<CarmaSystem>(
					bc.getFactory("WithGrid")
			);
			int deadline = 20;
			system.simulate(200,deadline);
//			minBikes.printTimeSeries(System.out);
//			maxBikes.printTimeSeries(System.out);
//			averageBikes.printTimeSeries(System.out);
		}		
	
}
