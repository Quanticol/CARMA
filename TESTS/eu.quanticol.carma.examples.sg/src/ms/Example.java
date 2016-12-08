package ms;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.*;		
import eu.quanticol.carma.simulator.*;
import eu.quanticol.carma.simulator.space.Node;
import eu.quanticol.carma.simulator.space.SpaceModel;
import eu.quanticol.carma.simulator.space.Tuple;
import java.util.LinkedList;
import java.util.Map;
import java.util.HashSet;
import java.util.HashMap;
import java.util.TreeSet;
import java.util.Collection;
import java.util.function.Function;
import java.util.function.Predicate;
import org.cmg.ml.sam.sim.sampling.*;


public class Example extends CarmaModel {
	
	public Example() {
		generateUserBehaviour( );
		generateTaxiBehaviour( );
		generateArrivalBehaviour( );
		setUpMeasures();
	}
	


	public static final int __CONST__SIZE = 3;
	public static final int __CONST__K = 5;
	public static final int __CONST__MAX_USER = (int) ( ( ( 1.5 )*( ( __CONST__SIZE )*( __CONST__SIZE ) ) )*( __CONST__K ) );
	public static final double __CONST__R_T = 5.0;
	public static final double __CONST__R_C = 2.0;
	public static final double __CONST__R_A = 10.0;
	public static final double __CONST__R_STEP = 2.0;
	public static final double __CONST__P_LOST = 0.125;
	public static final int __CONST__SWITCH_TIME = 100;

	public static Double __FUN__Mrate ( 
		Node __VARIABLE__l1,Node __VARIABLE__l2
	) {
		{
			//
			Double __VARIABLE__t =(double) ( ( Math.abs( ( _FEATURE_loc_x_grid(  (int) __VARIABLE__l1.get(0), (int) __VARIABLE__l1.get(1)) )-( _FEATURE_loc_x_grid(  (int) __VARIABLE__l2.get(0), (int) __VARIABLE__l2.get(1)) ) ) )+( Math.abs( ( _FEATURE_loc_y_grid(  (int) __VARIABLE__l1.get(0), (int) __VARIABLE__l1.get(1)) )-( _FEATURE_loc_y_grid(  (int) __VARIABLE__l2.get(0), (int) __VARIABLE__l2.get(1)) ) ) ) );
			//
			//
			Double __VARIABLE__r =0.0;
			//
			//
			if (( __VARIABLE__t )>( 1.0 )) {
				//
				__VARIABLE__r =( __CONST__R_STEP )/( __VARIABLE__t );
				//
			}
			else {
				//
				__VARIABLE__r =__CONST__R_STEP;
				//
			}
			//
			//
			return __VARIABLE__r;
			//
		}
	}
	public static Double __FUN__Arate ( 
		Double __VARIABLE__time,Node __VARIABLE__l1
	) {
		{
			//
			Double __VARIABLE__r =0.0;
			//
			//
			if (__VARIABLE__l1.isInArea( "center" )) {
				//
				if (( __VARIABLE__time )<( __CONST__SWITCH_TIME )) return ( 0.5 )*( __CONST__R_A );
				else {
					//
					return ( 1.5 )*( __CONST__R_A );
					//
				}
				//
			}
			else {
				//
				if (( __VARIABLE__time )<( __CONST__SWITCH_TIME )) return __CONST__R_A;
				else {
					//
					return ( 0.4 )*( __CONST__R_A );
					//
				}
				//
			}
			//
		}
	}
	public Double __FUN__Takeprob ( 
		Integer __VARIABLE__taxisAtLoc
	) {
		{
			//
			Double __VARIABLE__x_ =0.0;
			//
			//
			if (carmaEquals( __VARIABLE__taxisAtLoc , 0 )) {
				//
				__VARIABLE__x_ =0.0;
				//
			}
			else {
				//
				__VARIABLE__x_ =1.0;
				//
			}
			//
			//
			return __VARIABLE__x_;
			//
		}
	}
	
	public SpaceModel get_SPACE_grid(   Integer __VARIABLE__width,   Integer __VARIABLE__height  ) {
		SpaceModel sm = new SpaceModel();
		
		for (Object v0: generateIntervalList( 0 , __VARIABLE__width ) ) {
			for (Object v1: generateIntervalList( 0 , __VARIABLE__height ) ) {
				sm.addVertex( v0,v1 );
			}			
		}			
		
		for( Node l: sm.getAll() ) {
			{
				int __VARIABLE__x  = (int) l.get( 0 );
				int __VARIABLE__y  = (int) l.get( 1 );
					if (( __VARIABLE__x )<( ( __VARIABLE__width )-( 1 ) )) {
						sm.addEdge( l , new HashMap<>()	, sm.getVertex( new Tuple( ( __VARIABLE__x )+( 1 ), __VARIABLE__y  ) ) );
						sm.addEdge( sm.getVertex( new Tuple( ( __VARIABLE__x )+( 1 ), __VARIABLE__y  ) ) , new HashMap<>()	, l );
					}
			}
			{
				int __VARIABLE__x  = (int) l.get( 0 );
				int __VARIABLE__y  = (int) l.get( 1 );
					if (( __VARIABLE__y )<( ( __VARIABLE__height )-( 1 ) )) {
						sm.addEdge( l , new HashMap<>()	, sm.getVertex( new Tuple( ( __VARIABLE__x )+( 1 ), __VARIABLE__y  ) ) );
						sm.addEdge( sm.getVertex( new Tuple( ( __VARIABLE__x )+( 1 ), __VARIABLE__y  ) ) , new HashMap<>()	, l );
					}
			}
		}
	
		{
			HashSet<Node> newLabel = new HashSet<>();
			for (Node l: sm.getAll()) {
			{
				int __VARIABLE__x  = (int) l.get( 0 );
				int __VARIABLE__y  = (int) l.get( 1 );
				if (( ( ( carmaEquals( __VARIABLE__x , 0 ) )||( carmaEquals( __VARIABLE__y , 0 ) ) )||( carmaEquals( __VARIABLE__x , ( __VARIABLE__width )-( 1 ) ) ) )||( carmaEquals( __VARIABLE__y , ( __VARIABLE__height )-( 1 ) ) )) {
						newLabel.add( l );
				}
			}
			}
			sm.setArea( "border" , newLabel );
		}
		{
			HashSet<Node> newLabel = new HashSet<>();
			for (Node l: sm.getAll()) {
			{
				int __VARIABLE__x  = (int) l.get( 0 );
				int __VARIABLE__y  = (int) l.get( 1 );
				if (( carmaEquals( __VARIABLE__x , 1 ) )&&( carmaEquals( __VARIABLE__y , 1 ) )) {
						newLabel.add( l );
				}
			}
			}
			sm.setArea( "center" , newLabel );
		}
		return sm;
	}		
	
	public static Integer _FEATURE_loc_x_grid (
		int __VARIABLE__x,
		int __VARIABLE__y
	) {
		{
			//
			return __VARIABLE__x;
			//
		}
	}
	public static Integer _FEATURE_loc_y_grid (
		int __VARIABLE__x,
		int __VARIABLE__y
	) {
		{
			//
			return __VARIABLE__y;
			//
		}
	}
	
	
	
	/* START COMPONENT: User         */
	
	/* DEFINITIONS OF PROCESSES */
	public final CarmaProcessAutomaton _COMP_User = new CarmaProcessAutomaton("User");
	
	public final CarmaProcessAutomaton.State __STATE___User_Wait = _COMP_User.newState("Wait");		
	
	private void generateUserBehaviour( ) {
		
		
		{
			CarmaAction action = new CarmaOutput(
				__ACT__call , false  		
			) {
				
				@Override
				protected Object getValue(CarmaSystem sys, CarmaStore store, final double now) {
					LinkedList<Object> toReturn = new LinkedList<Object>();
					final Node __MY__loc = store.get( "loc" , Node.class );					
					toReturn.add( __MY__loc );
					return toReturn;
				}
				
				@Override
				protected CarmaStoreUpdate getUpdate(CarmaSystem sys,  final double now ) {
					return new CarmaStoreUpdate() {					
						//@Override
						public void update(RandomGenerator r, CarmaStore store) {
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
								return true;
							} catch (NullPointerException e) {
								return false;
							}
						}
						
					};
					
				}
			};		
			
			_COMP_User.addTransition( 
				__STATE___User_Wait , 
				action , 
				__STATE___User_Wait );			
		}
		{
			CarmaAction action = new CarmaOutput(
				__ACT__take , false  		
			) {
				
				@Override
				protected Object getValue(CarmaSystem sys, CarmaStore store, final double now) {
					LinkedList<Object> toReturn = new LinkedList<Object>();
					final Node __MY__loc = store.get( "loc" , Node.class );					
					final Node __MY__dest = store.get( "dest" , Node.class );
					toReturn.add( __MY__dest );
					return toReturn;
				}
				
				@Override
				protected CarmaStoreUpdate getUpdate(CarmaSystem sys,  final double now ) {
					return new CarmaStoreUpdate() {					
						//@Override
						public void update(RandomGenerator r, CarmaStore store) {
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
								return carmaEquals( __MY__loc , __MY__loc );
							} catch (NullPointerException e) {
								return false;
							}
						}
						
					};
					
				}
			};		
			
			_COMP_User.addTransition( 
				__STATE___User_Wait , 
				action , 
				null ,
				true);			
		}
		
	}
	
	public CarmaComponent createComponentUser( 
		Node __VARIABLE__userDestination  
	) {
		CarmaComponent c = new CarmaComponent();
		c.set( "dest" ,  __VARIABLE__userDestination );
		c.addAgent( new CarmaSequentialProcess( c , _COMP_User , __STATE___User_Wait ));
		return c;
	}	
	
	/* END COMPONENT: User */
		
	
	/* START COMPONENT: Taxi         */
	
	/* DEFINITIONS OF PROCESSES */
	public final CarmaProcessAutomaton _COMP_Taxi = new CarmaProcessAutomaton("Taxi");
	
	public final CarmaProcessAutomaton.State __STATE___Taxi_F = _COMP_Taxi.newState("F");		
	public final CarmaProcessAutomaton.State __STATE___Taxi_G = _COMP_Taxi.newState("G");		
	
	private void generateTaxiBehaviour( ) {
		
		
		{
			CarmaPredicate _FOO_predicate0 = new CarmaPredicate() {
		
				//@Override
				public boolean satisfy(double now,CarmaStore store) {
					final Node __MY__loc = store.get( "loc" , Node.class );					
					final Boolean __ATTR__free = store.get( "free" , Boolean.class );
					return __ATTR__free;
				}
					
			};
			{
				CarmaAction action = new CarmaInput( 
					__ACT__take , false  		
				) {
					
					@Override
					protected CarmaStoreUpdate getUpdate(CarmaSystem sys, final Object value, final double now) {
						
						LinkedList<Object> message = (LinkedList<Object>) value;
						final Node __VARIABLE__x = (Node) message.get(0);
						return new CarmaStoreUpdate() {
							
							//@Override
							public void update(RandomGenerator r, CarmaStore store) {
								final Node __MY__loc = store.get( "loc" , Node.class );					
								store.set( "dest", __VARIABLE__x );
								store.set( "free", false );
							}
						};
									
					}	
					
					@Override
					protected CarmaPredicate getPredicate(CarmaSystem sys, CarmaStore myStore, Object value) {
								LinkedList<Object> message = (LinkedList<Object>) value;
								final Node __VARIABLE__x = (Node) message.get(0);
								final Node __MY__loc = myStore.get( "loc" , Node.class );					
								return new CarmaPredicate() {
				
									//@Override
									public boolean satisfy(double now,CarmaStore store) {
										try {
											return true;
										} catch (NullPointerException e) {
											return false;
										}
									}
									
								};
						
					}
								
				};		
				
				_COMP_Taxi.addTransition( 
					__STATE___Taxi_F , 
					new CarmaPredicate.Conjunction(  _FOO_predicate0  ) , 
					action , 
					__STATE___Taxi_G );			
			}
		}
		{
			CarmaAction action = new CarmaInput( 
				__ACT__call , false  		
			) {
				
				@Override
				protected CarmaStoreUpdate getUpdate(CarmaSystem sys, final Object value, final double now) {
					
					LinkedList<Object> message = (LinkedList<Object>) value;
					final Node __VARIABLE__pos = (Node) message.get(0);
					return new CarmaStoreUpdate() {
						
						//@Override
						public void update(RandomGenerator r, CarmaStore store) {
							final Node __MY__loc = store.get( "loc" , Node.class );					
							System.out.println("HERE!!!");
							store.set( "dest", __VARIABLE__pos );
						}
					};
								
				}	
				
				@Override
				protected CarmaPredicate getPredicate(CarmaSystem sys, CarmaStore myStore, Object value) {
							LinkedList<Object> message = (LinkedList<Object>) value;
							final Node __VARIABLE__pos = (Node) message.get(0);
							final Node __MY__loc = myStore.get( "loc" , Node.class );					
							return new CarmaPredicate() {
			
								//@Override
								public boolean satisfy(double now,CarmaStore store) {
									try {
										return !( carmaEquals( __MY__loc , __VARIABLE__pos ) );
									} catch (NullPointerException e) {
										return false;
									}
								}
								
							};
					
				}
							
			};		
			
			_COMP_Taxi.addTransition( 
				__STATE___Taxi_F , 
				action , 
				__STATE___Taxi_G );			
		}
		{
			CarmaAction action = new CarmaOutput(
				__ACT__move , true  		
			) {
				
				@Override
				protected Object getValue(CarmaSystem sys, CarmaStore store, final double now) {
					LinkedList<Object> toReturn = new LinkedList<Object>();
					final Node __MY__loc = store.get( "loc" , Node.class );					
					return toReturn;
				}
				
				@Override
				protected CarmaStoreUpdate getUpdate(CarmaSystem sys,  final double now ) {
					return new CarmaStoreUpdate() {
						
						//@Override
						public void update(RandomGenerator r, CarmaStore store) {
							final Node __MY__loc = store.get( "loc" , Node.class );					
							final Node __ATTR__dest = store.get( "dest" , Node.class );
							store.set( "", __ATTR__dest );
							store.set( "dest", null );
							store.set( "free", true );
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
								return false;
							} catch (NullPointerException e) {
								return false;
							}
						}
						
					};
					
				}
			};		
			
			_COMP_Taxi.addTransition( 
				__STATE___Taxi_G , 
				action , 
				__STATE___Taxi_F );			
		}
		
	}
	
	public CarmaComponent createComponentTaxi( 
	) {
		CarmaComponent c = new CarmaComponent();
		c.set( "dest" ,  null );
		c.set( "free" ,  true );
		c.addAgent( new CarmaSequentialProcess( c , _COMP_Taxi , __STATE___Taxi_F ));
		return c;
	}	
	
	/* END COMPONENT: Taxi */
		
	
	/* START COMPONENT: Arrival         */
	
	/* DEFINITIONS OF PROCESSES */
	public final CarmaProcessAutomaton _COMP_Arrival = new CarmaProcessAutomaton("Arrival");
	
	public final CarmaProcessAutomaton.State __STATE___Arrival_A = _COMP_Arrival.newState("A");		
	
	private void generateArrivalBehaviour( ) {
		
		
		{
			CarmaAction action = new CarmaOutput(
				__ACT__arrival , true  		
			) {
				
				@Override
				protected Object getValue(CarmaSystem sys, CarmaStore store, final double now) {
					LinkedList<Object> toReturn = new LinkedList<Object>();
					final Node __MY__loc = store.get( "loc" , Node.class );					
					return toReturn;
				}
				
				@Override
				protected CarmaStoreUpdate getUpdate(CarmaSystem sys,  final double now ) {
					return new CarmaStoreUpdate() {					
						//@Override
						public void update(RandomGenerator r, CarmaStore store) {
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
								return false;
							} catch (NullPointerException e) {
								return false;
							}
						}
						
					};
					
				}
			};		
			
			_COMP_Arrival.addTransition( 
				__STATE___Arrival_A , 
				action , 
				__STATE___Arrival_A );			
		}
		
	}
	
	public CarmaComponent createComponentArrival( 
	) {
		CarmaComponent c = new CarmaComponent();
		c.addAgent( new CarmaSequentialProcess( c , _COMP_Arrival , __STATE___Arrival_A ));
		return c;
	}	
	
	/* END COMPONENT: Arrival */
		
	
	public static final int __ACT__call = 0;	
	public static final int __ACT__take = 1;	
	public static final int __ACT__move = 2;	
	public static final int __ACT__arrival = 3;	
	
	
	public String[] getSystems() {
		return new String[] {
			"Scenario1",
			"Scenario2"
		};	
	}
	
	public SimulationFactory<CarmaSystem> getFactory( String name ) {
		if ("Scenario1".equals( name )) {
			return getFactorySystemScenario1();
		}
		if ("Scenario2".equals( name )) {
			return getFactorySystemScenario2();
		}
		return null;
	}
			
	
	public class __SYSTEM__Scenario1 extends CarmaSystem {
		
		public __SYSTEM__Scenario1( ) {
			super(  get_SPACE_grid ( __CONST__SIZE,__CONST__SIZE) );
			setGLobalAttribute( "active_users" , 0 );
			CarmaSystem system = this;
			CarmaSystem sys = this;
			for( Node __VARIABLE__l:  sys.getSpaceModel().getAll() )  {
				{
					for (int pop = 0; pop < __CONST__K ; pop++ ) {
						CarmaComponent fooComponent = createComponentTaxi(					
						);
						fooComponent.setLocation(__VARIABLE__l);
						system.addComponent( fooComponent );
					}
				}
				{
						CarmaComponent fooComponent = createComponentArrival(					
						);
						fooComponent.setLocation(__VARIABLE__l);
						system.addComponent( fooComponent );
				}
			}
		}
		
		@Override
		public double broadcastProbability(
			final CarmaStore sender, 
			final CarmaStore receiver,
			int action) {
			final CarmaSystem system = this;
			final CarmaSystem sys = this;
			final CarmaStore global = this.global;
			final Node __SENDER__loc = sender.get( "loc" , Node.class );
			final Node __RECEIVER__loc = receiver.get( "loc" , Node.class );
			if ((action==__ACT__call)
				) {
					//
					return ( 1 )-( __CONST__P_LOST );
					//
				}
			{
				//
				return 1.0;
				//
			}
		}
	
		@Override
		public double unicastProbability(
			final CarmaStore sender, 
			final CarmaStore receiver,
			int action) {
			final CarmaSystem system = this;
			final CarmaSystem sys = this;
			final CarmaStore global = this.global;
			final Node __SENDER__loc = sender.get( "loc" , Node.class );
			final Node __RECEIVER__loc = receiver.get( "loc" , Node.class );
			if ((action==__ACT__take)
				) {
					//
					return __FUN__Takeprob( 
								Integer.valueOf( system.measure( 
									new BasicComponentPredicate(
										new CarmaPredicate() {
											
											//Here we assume that the following "final" references are available (if needed):
											//- global: reference to the global store;
											//- sender: reference to the store of sender;
											//- receiver: reference to the store of the receiver;				
											//@Override
											public boolean satisfy(double now,CarmaStore store) {
												final Node __MY__loc = store.get( "loc" , Node.class );
												try{
													Boolean result = carmaEquals( __MY__loc , __SENDER__loc );
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
														return csp.getName().equals("Taxi")&&csp.getState().getName().equals("F");
													} catch (NullPointerException e) {
														return false;
													}
												}
												return false;
											}
														
										}
										)
								)
								 )
							);
					//
				}
			{
				//
				return 1.0;
				//
			}
		}
		
		@Override
		public double broadcastRate(final CarmaStore sender, int action) {
			final CarmaSystem system = this;
			final CarmaSystem sys = this;
			final CarmaStore global = this.global;
			final Node __SENDER__dest = sender.get( "dest" , Node.class );
			final Node __SENDER__loc = sender.get( "loc" , Node.class );
			if ((action==__ACT__call)
				) {
					//
					return __CONST__R_C;
					//
				}
			if ((action==__ACT__move)
				) {
					//
					return __FUN__Mrate( 
								__SENDER__loc,
								__SENDER__dest
							);
					//
				}
			if ((action==__ACT__arrival)
				) {
					//
					return __CONST__R_A;
					//
				}
			{
				//
				return 0.0;
				//
			}
			
		}
		
		@Override
		public double unicastRate(final CarmaStore sender, int action) {
			final CarmaSystem system = this;
			final CarmaSystem sys = this;
			final CarmaStore global = this.global;
			final Node __SENDER__loc = sender.get( "loc" , Node.class );
			if ((action==__ACT__take)
				) {
					//
					return __CONST__R_T;
					//
				}
			{
				//
				return 0.0;
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
			final Integer __ATTR__active_users = global.get( "active_users" , Integer.class );
			final Node __SENDER__loc = sender.get( "loc" , Node.class );
			if (action==__ACT__arrival) {
				if ( ( __ATTR__active_users )<( __CONST__MAX_USER ) ) {
					if ( __SENDER__loc.isInArea( "center" ) ) {
						{
								CarmaComponent fooComponent = createComponentUser(					
									RandomGeneratorRegistry.uniformSelect( removeAll( sys.getSpaceModel().getAll() , getSet( sys.getSpaceModel().getVertex( new Tuple(1,1) ) )  ) )
								);
								fooComponent.setLocation(__SENDER__loc);
								system.addComponent( fooComponent );
						}
					} else {
						{
								CarmaComponent fooComponent = createComponentUser(					
									sys.getSpaceModel().getVertex( new Tuple(1,1) )
								);
								fooComponent.setLocation(__SENDER__loc);
								system.addComponent( fooComponent );
						}
					}
					store.set( "active_users", ( __ATTR__active_users )+( 1 ) );
				} else {
				}
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
			final Integer __ATTR__active_users = global.get( "active_users" , Integer.class );
			final Node __SENDER__loc = sender.get( "loc" , Node.class );
			if (action==__ACT__take) {
				store.set( "active_users", ( __ATTR__active_users )-( 1 ) );
				return ;				
			}
		}		
	}
	
	
	public SimulationFactory<CarmaSystem> getFactorySystemScenario1() {
		return new SimulationFactory<CarmaSystem>() {
	
			//@Override
			public CarmaSystem getModel() {
				return new __SYSTEM__Scenario1();
			}
		
			//@Override
			public Measure<CarmaSystem> getMeasure(String name) {
				// TODO Auto-generated method stub
				//FIXME!!!!
				return null;
			}
		
		};
		
	}
	public class __SYSTEM__Scenario2 extends CarmaSystem {
		
		public __SYSTEM__Scenario2( ) {
			super(  get_SPACE_grid ( __CONST__SIZE,__CONST__SIZE) );
			setGLobalAttribute( "active_users" , 0 );
			CarmaSystem system = this;
			CarmaSystem sys = this;
			for( Node __VARIABLE__l:  sys.getSpaceModel().getAll() )  {
				{
					for (int pop = 0; pop < __CONST__K ; pop++ ) {
						CarmaComponent fooComponent = createComponentTaxi(					
						);
						fooComponent.setLocation(__VARIABLE__l);
						system.addComponent( fooComponent );
					}
				}
				{
						CarmaComponent fooComponent = createComponentArrival(					
						);
						fooComponent.setLocation(__VARIABLE__l);
						system.addComponent( fooComponent );
				}
			}
		}
		
		@Override
		public double broadcastProbability(
			final CarmaStore sender, 
			final CarmaStore receiver,
			int action) {
			final CarmaSystem system = this;
			final CarmaSystem sys = this;
			final CarmaStore global = this.global;
			final Node __SENDER__loc = sender.get( "loc" , Node.class );
			final Node __RECEIVER__loc = receiver.get( "loc" , Node.class );
			if ((action==__ACT__call)
				) {
					//
					return ( 1 )-( __CONST__P_LOST );
					//
				}
			{
				//
				return 1.0;
				//
			}
		}
	
		@Override
		public double unicastProbability(
			final CarmaStore sender, 
			final CarmaStore receiver,
			int action) {
			final CarmaSystem system = this;
			final CarmaSystem sys = this;
			final CarmaStore global = this.global;
			final Node __SENDER__loc = sender.get( "loc" , Node.class );
			final Node __RECEIVER__loc = receiver.get( "loc" , Node.class );
			if ((action==__ACT__take)
				) {
					//
					return __FUN__Takeprob( 
								Integer.valueOf( system.measure( 
									new BasicComponentPredicate(
										new CarmaPredicate() {
											
											//Here we assume that the following "final" references are available (if needed):
											//- global: reference to the global store;
											//- sender: reference to the store of sender;
											//- receiver: reference to the store of the receiver;				
											//@Override
											public boolean satisfy(double now,CarmaStore store) {
												final Node __MY__loc = store.get( "loc" , Node.class );
												try{
													Boolean result = carmaEquals( __MY__loc , __SENDER__loc );
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
														return csp.getName().equals("Taxi")&&csp.getState().getName().equals("F");
													} catch (NullPointerException e) {
														return false;
													}
												}
												return false;
											}
														
										}
										)
								)
								 )
							);
					//
				}
			{
				//
				return 1.0;
				//
			}
		}
		
		@Override
		public double broadcastRate(final CarmaStore sender, int action) {
			final CarmaSystem system = this;
			final CarmaSystem sys = this;
			final CarmaStore global = this.global;
			final Node __SENDER__dest = sender.get( "dest" , Node.class );
			final Node __SENDER__loc = sender.get( "loc" , Node.class );
			if ((action==__ACT__call)
				) {
					//
					return __CONST__R_C;
					//
				}
			if ((action==__ACT__move)
				) {
					//
					return __FUN__Mrate( 
								__SENDER__loc,
								__SENDER__dest
							);
					//
				}
			if ((action==__ACT__arrival)
				) {
					//
					return __FUN__Arate( 
								Double.valueOf( now ),
								__SENDER__loc
							);
					//
				}
			{
				//
				return 0.0;
				//
			}
			
		}
		
		@Override
		public double unicastRate(final CarmaStore sender, int action) {
			final CarmaSystem system = this;
			final CarmaSystem sys = this;
			final CarmaStore global = this.global;
			final Node __SENDER__loc = sender.get( "loc" , Node.class );
			if ((action==__ACT__take)
				) {
					//
					return __CONST__R_T;
					//
				}
			{
				//
				return 0.0;
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
			final Integer __ATTR__active_users = global.get( "active_users" , Integer.class );
			final Node __SENDER__loc = sender.get( "loc" , Node.class );
			if (action==__ACT__arrival) {
				if ( ( __ATTR__active_users )<( __CONST__MAX_USER ) ) {
					if ( __SENDER__loc.isInArea( "center" ) ) {
						{
								CarmaComponent fooComponent = createComponentUser(					
									RandomGeneratorRegistry.uniformSelect( removeAll( sys.getSpaceModel().getAll() , getSet( sys.getSpaceModel().getVertex( new Tuple(1,1) ) )  ) )
								);
								fooComponent.setLocation(__SENDER__loc);
								system.addComponent( fooComponent );
						}
					} else {
						{
								CarmaComponent fooComponent = createComponentUser(					
									sys.getSpaceModel().getVertex( new Tuple(1,1) )
								);
								fooComponent.setLocation(__SENDER__loc);
								system.addComponent( fooComponent );
						}
					}
					store.set( "active_users", ( __ATTR__active_users )+( 1 ) );
				} else {
				}
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
			final Integer __ATTR__active_users = global.get( "active_users" , Integer.class );
			final Node __SENDER__loc = sender.get( "loc" , Node.class );
			if (action==__ACT__take) {
				store.set( "active_users", ( __ATTR__active_users )-( 1 ) );
				return ;				
			}
		}		
	}
	
	
	public SimulationFactory<CarmaSystem> getFactorySystemScenario2() {
		return new SimulationFactory<CarmaSystem>() {
	
			//@Override
			public CarmaSystem getModel() {
				return new __SYSTEM__Scenario2();
			}
		
			//@Override
			public Measure<CarmaSystem> getMeasure(String name) {
				// TODO Auto-generated method stub
				//FIXME!!!!
				return null;
			}
		
		};
		
	}
	
	
	private HashMap<String,Measure<CarmaSystem>> measures;
			
	public String[] getMeasures() {
		TreeSet<String> sortedSet = new TreeSet<String>( measures.keySet() );
		return sortedSet.toArray( new String[ sortedSet.size() ] );
	}
	
	public Measure<CarmaSystem> getMeasure( String name ) {
		return measures.get( name );
	}
	
	protected void setUpMeasures() {
		measures = new HashMap<String,Measure<CarmaSystem>>();
		buildMeasureWaitingUser( );
		buildMeasureFreeTaxi( );
		buildMeasureTaxi( );
		buildMeasureTravelling( );
		buildMeasureAllTaxi( );
		buildMeasureAllUsers( );
	}	
	
	
	private void buildMeasureWaitingUser( ) {
		for( 
			int v0 = 0 ; 
			v0 <= ( __CONST__SIZE )-( 1 ) ; 
			v0++
		) {
			for( 
				int v1 = 0 ; 
				v1 <= ( __CONST__SIZE )-( 1 ) ; 
				v1++
			) {
				measures.put( 
								"WaitingUser["+v0+v1+"]" ,
								getMeasureWaitingUser( v0,v1)
							);
			}
		}
	}
	
	
	private Measure<CarmaSystem> getMeasureWaitingUser( 
		final Integer __VARIABLE__i,final Integer __VARIABLE__j
	) {
	
		return new Measure<CarmaSystem>() {
		
			//@Override
			public double measure(final CarmaSystem system) {
				final CarmaStore global = system.getGlobalStore();
				final double now = system.now();
				return system.measure( 
					new BasicComponentPredicate(
						new CarmaPredicate() {
							
							//Here we assume that the following "final" references are available (if needed):
							//- global: reference to the global store;
							//- sender: reference to the store of sender;
							//- receiver: reference to the store of the receiver;				
							//@Override
							public boolean satisfy(double now,CarmaStore store) {
								final Node __MY__loc = store.get( "loc" , Node.class );
								try{
									Boolean result = ( carmaEquals( _FEATURE_loc_x_grid(  (int) __MY__loc.get(0), (int) __MY__loc.get(1)) , __VARIABLE__i ) )&&( carmaEquals( _FEATURE_loc_y_grid(  (int) __MY__loc.get(0), (int) __MY__loc.get(1)) , __VARIABLE__j ) );
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
										return csp.getName().equals("User")&&csp.getState().getName().equals("Wait");
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
	
			//@Override
			public String getName() {
				return "WaitingUser"+"_"+__VARIABLE__i+"_"+__VARIABLE__j;
			}
		
		};
		
	}
	
	
	private void buildMeasureFreeTaxi( ) {
		for( 
			int v0 = 0 ; 
			v0 <= ( __CONST__SIZE )-( 1 ) ; 
			v0++
		) {
			for( 
				int v1 = 0 ; 
				v1 <= ( __CONST__SIZE )-( 1 ) ; 
				v1++
			) {
				measures.put( 
								"FreeTaxi["+v0+v1+"]" ,
								getMeasureFreeTaxi( v0,v1)
							);
			}
		}
	}
	
	
	private Measure<CarmaSystem> getMeasureFreeTaxi( 
		final Integer __VARIABLE__i,final Integer __VARIABLE__j
	) {
	
		return new Measure<CarmaSystem>() {
		
			//@Override
			public double measure(final CarmaSystem system) {
				final CarmaStore global = system.getGlobalStore();
				final double now = system.now();
				return system.measure( 
					new BasicComponentPredicate(
						new CarmaPredicate() {
							
							//Here we assume that the following "final" references are available (if needed):
							//- global: reference to the global store;
							//- sender: reference to the store of sender;
							//- receiver: reference to the store of the receiver;				
							//@Override
							public boolean satisfy(double now,CarmaStore store) {
								final Node __MY__loc = store.get( "loc" , Node.class );
								try{
									Boolean result = ( carmaEquals( _FEATURE_loc_x_grid(  (int) __MY__loc.get(0), (int) __MY__loc.get(1)) , __VARIABLE__i ) )&&( carmaEquals( _FEATURE_loc_y_grid(  (int) __MY__loc.get(0), (int) __MY__loc.get(1)) , __VARIABLE__j ) );
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
										return csp.getName().equals("Taxi")&&csp.getState().getName().equals("F");
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
	
			//@Override
			public String getName() {
				return "FreeTaxi"+"_"+__VARIABLE__i+"_"+__VARIABLE__j;
			}
		
		};
		
	}
	
	
	private void buildMeasureTaxi( ) {
		for( 
			int v0 = 0 ; 
			v0 <= ( __CONST__SIZE )-( 1 ) ; 
			v0++
		) {
			for( 
				int v1 = 0 ; 
				v1 <= ( __CONST__SIZE )-( 1 ) ; 
				v1++
			) {
				measures.put( 
								"Taxi["+v0+v1+"]" ,
								getMeasureTaxi( v0,v1)
							);
			}
		}
	}
	
	
	private Measure<CarmaSystem> getMeasureTaxi( 
		final Integer __VARIABLE__i,final Integer __VARIABLE__j
	) {
	
		return new Measure<CarmaSystem>() {
		
			//@Override
			public double measure(final CarmaSystem system) {
				final CarmaStore global = system.getGlobalStore();
				final double now = system.now();
				return system.measure( 
					new BasicComponentPredicate(
						new CarmaPredicate() {
							
							//Here we assume that the following "final" references are available (if needed):
							//- global: reference to the global store;
							//- sender: reference to the store of sender;
							//- receiver: reference to the store of the receiver;				
							//@Override
							public boolean satisfy(double now,CarmaStore store) {
								final Node __MY__loc = store.get( "loc" , Node.class );
								try{
									Boolean result = ( carmaEquals( _FEATURE_loc_x_grid(  (int) __MY__loc.get(0), (int) __MY__loc.get(1)) , __VARIABLE__i ) )&&( carmaEquals( _FEATURE_loc_y_grid(  (int) __MY__loc.get(0), (int) __MY__loc.get(1)) , __VARIABLE__j ) );
									return (result==null?false:result);
								} catch (NullPointerException e) {
									return false;
								}
							}
						
							
						}
						 , 
						new CarmaProcessPredicate() {
					
							//@Override
							public boolean eval(CarmaProcess p) {
								if (p instanceof CarmaSequentialProcess) {
									CarmaSequentialProcess csp = (CarmaSequentialProcess) p;
									try{
										return csp.getName().equals("Taxi");
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
	
			//@Override
			public String getName() {
				return "Taxi"+"_"+__VARIABLE__i+"_"+__VARIABLE__j;
			}
		
		};
		
	}
	
	
	private void buildMeasureTravelling( ) {
		measures.put( 
						"Travelling" ,
						getMeasureTravelling( )
					);
	}
	
	
	private Measure<CarmaSystem> getMeasureTravelling( 
	) {
	
		return new Measure<CarmaSystem>() {
		
			//@Override
			public double measure(final CarmaSystem system) {
				final CarmaStore global = system.getGlobalStore();
				final double now = system.now();
				return (double) ( system.measure( 
					new BasicComponentPredicate(
						new CarmaPredicate() {
							
							//Here we assume that the following "final" references are available (if needed):
							//- global: reference to the global store;
							//- sender: reference to the store of sender;
							//- receiver: reference to the store of the receiver;				
							//@Override
							public boolean satisfy(double now,CarmaStore store) {
								final Boolean __MY__free = store.get( "free" , Boolean.class );
								final Node __MY__loc = store.get( "loc" , Node.class );
								try{
									Boolean result = __MY__free;
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
										return csp.getName().equals("Taxi")&&csp.getState().getName().equals("G");
									} catch (NullPointerException e) {
										return false;
									}
								}
								return false;
							}
										
						}
						)
				)
				 );
			}
	
			//@Override
			public String getName() {
				return "Travelling";
			}
		
		};
		
	}
	
	
	private void buildMeasureAllTaxi( ) {
		measures.put( 
						"AllTaxi" ,
						getMeasureAllTaxi( )
					);
	}
	
	
	private Measure<CarmaSystem> getMeasureAllTaxi( 
	) {
	
		return new Measure<CarmaSystem>() {
		
			//@Override
			public double measure(final CarmaSystem system) {
				final CarmaStore global = system.getGlobalStore();
				final double now = system.now();
				return system.measure( 
					new BasicComponentPredicate(
						new CarmaPredicate() {
							
							//Here we assume that the following "final" references are available (if needed):
							//- global: reference to the global store;
							//- sender: reference to the store of sender;
							//- receiver: reference to the store of the receiver;				
							//@Override
							public boolean satisfy(double now,CarmaStore store) {
								final Node __MY__loc = store.get( "loc" , Node.class );
								try{
									Boolean result = true;
									return (result==null?false:result);
								} catch (NullPointerException e) {
									return false;
								}
							}
						
							
						}
						 , 
						new CarmaProcessPredicate() {
					
							//@Override
							public boolean eval(CarmaProcess p) {
								if (p instanceof CarmaSequentialProcess) {
									CarmaSequentialProcess csp = (CarmaSequentialProcess) p;
									try{
										return csp.getName().equals("Taxi");
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
	
			//@Override
			public String getName() {
				return "AllTaxi";
			}
		
		};
		
	}
	
	
	private void buildMeasureAllUsers( ) {
		measures.put( 
						"AllUsers" ,
						getMeasureAllUsers( )
					);
	}
	
	
	private Measure<CarmaSystem> getMeasureAllUsers( 
	) {
	
		return new Measure<CarmaSystem>() {
		
			//@Override
			public double measure(final CarmaSystem system) {
				final CarmaStore global = system.getGlobalStore();
				final double now = system.now();
				return system.measure( 
					new BasicComponentPredicate(
						new CarmaPredicate() {
							
							//Here we assume that the following "final" references are available (if needed):
							//- global: reference to the global store;
							//- sender: reference to the store of sender;
							//- receiver: reference to the store of the receiver;				
							//@Override
							public boolean satisfy(double now,CarmaStore store) {
								final Node __MY__loc = store.get( "loc" , Node.class );
								try{
									Boolean result = true;
									return (result==null?false:result);
								} catch (NullPointerException e) {
									return false;
								}
							}
						
							
						}
						 , 
						new CarmaProcessPredicate() {
					
							//@Override
							public boolean eval(CarmaProcess p) {
								if (p instanceof CarmaSequentialProcess) {
									CarmaSequentialProcess csp = (CarmaSequentialProcess) p;
									try{
										return csp.getName().equals("User");
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
	
			//@Override
			public String getName() {
				return "AllUsers";
			}
		
		};
		
	}

	@Override
	public String[] getMeasureParameters(String name) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Map<String, Class<?>> getParametersType(String name) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Measure<CarmaSystem> getMeasure(String name, Map<String, Object> parameters) {
		// TODO Auto-generated method stub
		return null;
	}
	
	
	
}
