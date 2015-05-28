/**
 * 
 */
package eu.quanticol.carma.examples.smarttaxis;

import java.util.Arrays;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.sampling.Measure;

import eu.quanticol.carma.simulator.CarmaComponent;
import eu.quanticol.carma.simulator.CarmaInput;
import eu.quanticol.carma.simulator.CarmaOutput;
import eu.quanticol.carma.simulator.CarmaPredicate;
import eu.quanticol.carma.simulator.CarmaProcess;
import eu.quanticol.carma.simulator.CarmaProcessAutomaton;
import eu.quanticol.carma.simulator.CarmaProcessPredicate;
import eu.quanticol.carma.simulator.CarmaSequentialProcess;
import eu.quanticol.carma.simulator.CarmaStore;
import eu.quanticol.carma.simulator.CarmaStoreUpdate;
import eu.quanticol.carma.simulator.CarmaSystem;
import eu.quanticol.carma.simulator.ComponentPredicate;

/**
 * @author loreti
 *
 */
public class SmartTaxisDefinitions {
	
	/*
	 * 0 -> 1 , 3
	 * 1 -> 0 , 2 , 4
	 * 2 -> 1 , 5
	 * 3 -> 0 , 4 , 6
	 * 3 -> 
	 * 
	 */
	
	public static class InfoClass {
	
		private boolean[] info;
		private int elements;
		
		public InfoClass( int size ) {
			this.info = new boolean[size];
			this.elements = 0;
		}
		
		public InfoClass(boolean[] info) {
			this.info = info;
		}

		public boolean get( int i ) {
			return this.info[i];
		}
		
		public InfoClass merge( InfoClass ic ) {
			InfoClass newInfo = new InfoClass(info.length);
			for( int i=0 ; i<info.length ; i++ ) {
				if (this.info[i]||ic.info[i]) {
					newInfo.set(i);					
				}
			}
			return newInfo;
		}

		public void age(int i) {
			if (elements == 0 ) {
				return ;
			}
			int count = i;
			for( int j = 0 ; j<info.length ; j++ ) {
				if (info[j]) {
					if (count==0) {
						info[j]=false;
					} else {
						count--;
					}
				}
			}
			this.elements--;
		}

		public void set(int i) {
			if (!this.info[i]) {
				this.info[i] = true;				
				this.elements++;
			}
		}

		public double getMovingProbability(int loc) {
			double stay = 0.0;
			double move = 0.0;
			if (info[loc]) {
				stay = 0.9;
			} else {
				stay = 0.1;
			}
			double count = 0.0;
			for( int i=0 ; i<info.length ; i++ ) {
				if (info[i]&&(i!=loc)) {
					count += 1.0;
				}
			}
			if (count > 0) {
				move = count/(info.length-1);
			} else {
				move = 0.01;
			}
			double toReturn = (move/(stay+move));
//			System.out.println(toReturn);
			return toReturn;
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			return Arrays.toString(info) ;
		}

		public int elements() {
			return elements;
		}
		
	}
	
	public static final int GRID_WIDTH = 3;
	public static final int GRID_HEIGHT = 3;
	
	public static final int NUMBER_OF_LOCATIONS = GRID_HEIGHT*GRID_WIDTH;
	
	
	/* ACTIVITIES */
	public static final int CALL = 0;
	public static final int TAKE = 1;
	public static final int CHANGE = 2;
	public static final int MOVE = 3;
	public static final int AGE = 5;
	public static final int EXCH = 6;
	public static final int ARRIVE = 7;
	
	/* ATTRIBUTES */
	public static final String LOC_ATTRIBUTE = "loc";
	public static final Class<Integer> LOC_ATTRIBUTE_TYPE = Integer.class;
	public static final String DEST_ATTRIBUTE = "dest";
	public static final Class<Integer> DEST_ATTRIBUTE_TYPE = Integer.class;
	public static final String OCCUPIED_ATTRIBUTE = "occupied";
	public static final Class<Boolean> OCCUPIED_ATTRIBUTE_TYPE = Boolean.class;
	public static final String INFO_ATTRIBUTE = "info";
	public static final Class<InfoClass> INFO_ATTRIBUTE_TYPE = InfoClass.class;

	public static final CarmaProcessAutomaton UserProcess = createUserProcess();
	public static final CarmaProcessAutomaton TaxiProcess = createTaxiProcess();
	public static final CarmaProcessAutomaton InfoProcess = createInfoProcess();
	public static final CarmaProcessAutomaton ArrivalProcess = createArrivalProcess();
	//Time units: 1 minute;
	public static final double CALL_RATE = 0.25; //A user asks for a taxi every 10 t.u.
	public static final double EXCH_RATE = 0.0; 
	public static final double TAKE_RATE = 1.0; //When available a taxi needs 1 t.u. to take a user.
	public static final double P_LOST = 0.75; //A taxi ignores a call with probability 0.25
	public static final double STEP_RATE = 2.0; //Average time needed to move from one zone to an adiacent one.
	public static final double CHANGE_RATE = 5.0; //Each taxi waits in a zone 30 t.u. before changing the zone.
	public static final double AGE_RATE = 0.5; //Info are updated  
	public static final double ARRIVAL_RATE = 10.0; //A users needs a taxi every 10 minutes.
	
	public static final CarmaProcessPredicate WAITING_USER_PROCESS = new CarmaProcessPredicate() {
		
		@Override
		public boolean eval(CarmaProcess p) {
			return (p instanceof CarmaSequentialProcess)&&
					(((CarmaSequentialProcess) p).automaton() ==  UserProcess)&&
					(((CarmaSequentialProcess) p).getState() !=  null);
		}
		
	};
	
	public static final double LIMIT = 400;
	public static final int TAXIS = 5;
	
	public static ComponentPredicate getWaitingUserPredicate( int loc ) {
		return new ComponentPredicate() {
		
			@Override
			public boolean eval(CarmaComponent c) {
				return (c.get(LOC_ATTRIBUTE, LOC_ATTRIBUTE_TYPE).equals( loc ))&&(c.isRunning(WAITING_USER_PROCESS));
			}
	
		};
	}

	public static ComponentPredicate getAvailableTaxiPredicate( int loc ) {
		return new ComponentPredicate() {
		
			@Override
			public boolean eval(CarmaComponent c) {
				return (c.get(LOC_ATTRIBUTE, LOC_ATTRIBUTE_TYPE).equals( loc ))&&
						(c.get(OCCUPIED_ATTRIBUTE, OCCUPIED_ATTRIBUTE_TYPE) != null)&&
						(c.get(OCCUPIED_ATTRIBUTE, OCCUPIED_ATTRIBUTE_TYPE).equals(false))&&
						(c.get(DEST_ATTRIBUTE, DEST_ATTRIBUTE_TYPE).equals(-1));
			}
	
		};
	}

	public static ComponentPredicate getAvailableTaxiPredicate( ) {
		return new ComponentPredicate() {
		
			@Override
			public boolean eval(CarmaComponent c) {
				return (c.get(OCCUPIED_ATTRIBUTE, OCCUPIED_ATTRIBUTE_TYPE) != null)&&
						(c.get(OCCUPIED_ATTRIBUTE, OCCUPIED_ATTRIBUTE_TYPE).equals(false))&&
						(c.get(DEST_ATTRIBUTE, DEST_ATTRIBUTE_TYPE).equals(-1));
			}
	
		};
	}
	

	public static ComponentPredicate getMovingTaxiPredicate( int loc ) {
		return new ComponentPredicate() {
		
			@Override
			public boolean eval(CarmaComponent c) {
				return (c.get(LOC_ATTRIBUTE, LOC_ATTRIBUTE_TYPE).equals( loc ))&&
						(c.get(OCCUPIED_ATTRIBUTE, OCCUPIED_ATTRIBUTE_TYPE) != null)&&
						(c.get(OCCUPIED_ATTRIBUTE, OCCUPIED_ATTRIBUTE_TYPE).equals(false))&&
						(!c.get(DEST_ATTRIBUTE, DEST_ATTRIBUTE_TYPE).equals(-1));
			}
	
		};
	}

	public static ComponentPredicate getMovingTaxiPredicate( ) {
		return new ComponentPredicate() {
		
			@Override
			public boolean eval(CarmaComponent c) {
				return (c.get(OCCUPIED_ATTRIBUTE, OCCUPIED_ATTRIBUTE_TYPE) != null)&&
						(c.get(OCCUPIED_ATTRIBUTE, OCCUPIED_ATTRIBUTE_TYPE).equals(false))&&
						(!c.get(DEST_ATTRIBUTE, DEST_ATTRIBUTE_TYPE).equals(-1));
			}
	
		};
	}

	
	private static CarmaProcessAutomaton createUserProcess() {
		CarmaProcessAutomaton toReturn = new CarmaProcessAutomaton("User");

		CarmaProcessAutomaton.State stateW = toReturn.newState("W");
		
//		CaspaOutput callAction = new CaspaOutput( CALL , true ) {
		CarmaOutput callAction = new CarmaOutput( CALL , false ) {
			
			@Override
			protected Object getValue(CarmaStore store) {
				return store.get(LOC_ATTRIBUTE, LOC_ATTRIBUTE_TYPE);
			}
			
			@Override
			protected CarmaStoreUpdate getUpdate() {				
				return new CarmaStoreUpdate() {
					
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
						
					}
				};
			}
			
			@Override
			protected CarmaPredicate getPredicate(final CarmaStore userStore) {
				return CarmaPredicate.TRUE;
//				return new CaspaPredicate() {
//
//					@Override
//					public boolean satisfy(CaspaStore store) {
//						return userStore.get(LOC_ATTRIBUTE, LOC_ATTRIBUTE_TYPE).equals(
//							store.get(LOC_ATTRIBUTE, LOC_ATTRIBUTE_TYPE)
//						);
//					}
//					
//				};
			}
		};
		
		CarmaOutput takeAction = new CarmaOutput( TAKE , false ) {
			
			@Override
			protected Object getValue(CarmaStore store) {
				return store.get(DEST_ATTRIBUTE, DEST_ATTRIBUTE_TYPE);
			}
			
			@Override
			protected CarmaStoreUpdate getUpdate() {				
				return new CarmaStoreUpdate() {
					
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
						
					}
				};
			}
			
			@Override
			protected CarmaPredicate getPredicate(final CarmaStore userStore) {
				return new CarmaPredicate() {

					@Override
					public boolean satisfy(CarmaStore store) {
						return userStore.get(LOC_ATTRIBUTE, LOC_ATTRIBUTE_TYPE).equals(
							store.get(LOC_ATTRIBUTE, LOC_ATTRIBUTE_TYPE)
						);
					}
					
				};
			}
		};
		
		
		toReturn.addTransition(stateW, callAction, stateW);
		toReturn.addTransition(stateW, takeAction, null);
		
		return toReturn;
	}


	private static CarmaProcessAutomaton createArrivalProcess() {
		CarmaProcessAutomaton toReturn = new CarmaProcessAutomaton("Arrival");
		
		CarmaProcessAutomaton.State stateA = toReturn.newState("A");
		
		CarmaOutput arrive = new CarmaOutput(ARRIVE , true) {
			
			@Override
			protected Object getValue(CarmaStore store) {
				return new Object();
			}
			
			@Override
			protected CarmaStoreUpdate getUpdate() {
				return new CarmaStoreUpdate() {
					
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
					}
				};
			}
			
			@Override
			protected CarmaPredicate getPredicate(CarmaStore store) {
				return CarmaPredicate.FALSE;
			}
		};
		
		toReturn.addTransition(stateA, arrive, stateA);
		
		return toReturn;
	}


	public static CarmaProcessAutomaton createInfoProcess() {
		CarmaProcessAutomaton toReturn = new CarmaProcessAutomaton("Info");
		
		CarmaProcessAutomaton.State stateI = toReturn.newState("I");

		CarmaInput callAction = new CarmaInput(CALL,true) {
			
			@Override
			protected CarmaStoreUpdate getUpdate(final Object value) {
				if (value instanceof Integer) {
					return new CarmaStoreUpdate() {
						
						@Override
						public void update(RandomGenerator r, CarmaStore store) {
							InfoClass ic = store.get(INFO_ATTRIBUTE, INFO_ATTRIBUTE_TYPE);
//							ic.set( store.get(LOC_ATTRIBUTE, LOC_ATTRIBUTE_TYPE));
							ic.set( (Integer) value );
						}
						
					};
				}
				
				return null;
			}
			
			@Override
			protected CarmaPredicate getPredicate(CarmaStore store, Object value) {
				return CarmaPredicate.TRUE;
			}
		};
		
		CarmaOutput ageAction = new CarmaOutput( AGE , true ) {
			
			@Override
			protected Object getValue(CarmaStore store) {
				return new Object();
			}
			
			@Override
			protected CarmaStoreUpdate getUpdate() {
				return new CarmaStoreUpdate() {
					
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
						InfoClass ic = store.get(INFO_ATTRIBUTE, INFO_ATTRIBUTE_TYPE);
						ic.age( r.nextInt( NUMBER_OF_LOCATIONS ) );
					}
				};
			}
			
			@Override
			protected CarmaPredicate getPredicate(CarmaStore store) {
				return CarmaPredicate.FALSE;
			}
		};
		
		CarmaOutput exchOutAction = new CarmaOutput( EXCH , true ) {
			
			@Override
			protected Object getValue(CarmaStore store) {
				return store.get(INFO_ATTRIBUTE, INFO_ATTRIBUTE_TYPE);
			}
			
			@Override
			protected CarmaStoreUpdate getUpdate() {
				return new CarmaStoreUpdate() {
					
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
					}
				};
			}
			
			@Override
			protected CarmaPredicate getPredicate(final CarmaStore taxiStore) {
				return new CarmaPredicate() {

					@Override
					public boolean satisfy(CarmaStore store) {
						return taxiStore.get(LOC_ATTRIBUTE, LOC_ATTRIBUTE_TYPE).equals(
								store.get(LOC_ATTRIBUTE, LOC_ATTRIBUTE_TYPE)
							);
					}
					
				};
			}
		};
		
		CarmaInput exchInAction = new CarmaInput( EXCH , true ) {
			
			@Override
			protected CarmaStoreUpdate getUpdate(final Object value) {
					if (value instanceof InfoClass) {
						return new CarmaStoreUpdate() {
							
							@Override
							public void update(RandomGenerator r, CarmaStore store) {
								store.set(INFO_ATTRIBUTE, store.get(INFO_ATTRIBUTE, INFO_ATTRIBUTE_TYPE).merge((InfoClass) value));
							}
						};
					}
					return new CarmaStoreUpdate() {
						
						@Override
						public void update(RandomGenerator r, CarmaStore store) {
							
						}
					};
			}
			
			@Override
			protected CarmaPredicate getPredicate(CarmaStore store, Object value) {
				return CarmaPredicate.TRUE;
			}
		};
		
		toReturn.addTransition(stateI, callAction, stateI);
		toReturn.addTransition(stateI, ageAction, stateI);
//		toReturn.addTransition(stateI, exchOutAction, stateI);
//		toReturn.addTransition(stateI, exchInAction, stateI);
		
		return toReturn;
	}


	private static CarmaProcessAutomaton createTaxiProcess() {
		CarmaProcessAutomaton toReturn = new CarmaProcessAutomaton("Taxi");

		CarmaProcessAutomaton.State stateF = toReturn.newState("F");
		CarmaProcessAutomaton.State stateG = toReturn.newState("G");
		
		CarmaInput takeAction = new CarmaInput( TAKE , false ) {

			@Override
			protected CarmaPredicate getPredicate(final CarmaStore taxiStore, Object value) {
				return CarmaPredicate.TRUE;
			}

			@Override
			protected CarmaStoreUpdate getUpdate(final Object value) {
				return new CarmaStoreUpdate() {
					
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
						store.set(DEST_ATTRIBUTE, value);
						store.set(OCCUPIED_ATTRIBUTE, true);
					}
					
				};
			}
			
		};
		
//		CaspaInput callAction = new CaspaInput(CALL,true) {
		CarmaInput callAction = new CarmaInput(CALL,false) {
			
			@Override
			protected CarmaStoreUpdate getUpdate(final Object value) {
				if (value instanceof Integer) {
					return new CarmaStoreUpdate() {
						
						@Override
						public void update(RandomGenerator r, CarmaStore store) {
							store.set(DEST_ATTRIBUTE, value);
						}
						
					};
				}
				
				return null;
			}
			
			@Override
			protected CarmaPredicate getPredicate(CarmaStore taxiStore, Object value) {
				return new CarmaPredicate() {

					@Override
					public boolean satisfy(CarmaStore store) {
						return !taxiStore.get(LOC_ATTRIBUTE, LOC_ATTRIBUTE_TYPE).equals(
								store.get(LOC_ATTRIBUTE, LOC_ATTRIBUTE_TYPE)
								);
					}
					
				};
			}
		};


//		CaspaOutput changeAction = new CaspaOutput( CHANGE , true ) {
//			
//			@Override
//			protected Object getValue(CaspaStore store) {
//				return new Object();
//			}
//			
//			@Override
//			protected CaspaStoreUpdate getUpdate() {
//				return new CaspaStoreUpdate() {
//					
//					@Override
//					public void update(RandomGenerator r, CaspaStore store) {
//						int[] nearLoc = SmartTaxisDefinitions.near( store.get(LOC_ATTRIBUTE, LOC_ATTRIBUTE_TYPE));
//						int newLoc = nearLoc[r.nextInt(nearLoc.length)];
//						store.set(DEST_ATTRIBUTE, newLoc);
//					}
//					
//				};
//			}
//			
//			@Override
//			protected CaspaPredicate getPredicate(CaspaStore store) {
//				return CaspaPredicate.FALSE;
//			}
//		};
		
		CarmaOutput moveAction = new CarmaOutput( MOVE , true ) {
			
			@Override
			protected Object getValue(CarmaStore store) {
				return new Object();
			}
			
			@Override
			protected CarmaStoreUpdate getUpdate() {
				return new CarmaStoreUpdate() {
					
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
						store.set(LOC_ATTRIBUTE, store.get(DEST_ATTRIBUTE, DEST_ATTRIBUTE_TYPE));
						store.set(DEST_ATTRIBUTE, -1);
						store.set(INFO_ATTRIBUTE, new InfoClass(NUMBER_OF_LOCATIONS));
						store.set(OCCUPIED_ATTRIBUTE, false);
					}
					
				};
			}
			
			@Override
			protected CarmaPredicate getPredicate(CarmaStore store) {
				return CarmaPredicate.FALSE;
			}
		};
		
		toReturn.addTransition(stateF, takeAction, stateG);
		toReturn.addTransition(stateF, callAction, stateG);
//		toReturn.addTransition(stateF, changeAction, stateG);
		toReturn.addTransition(stateG, moveAction, stateF);		
		
		return toReturn;
	}


	public static int[] near(int loc) {
		int y = loc/3;
		int x = loc%3;
		if ((x == 0)&&(y==0)) {
			return new int[] { getLocId(x,y+1) , getLocId(x+1,y) };
		}
		if ((x == 0)&&(y==(GRID_HEIGHT-1))) {
			return new int[] { getLocId(x,y-1) , getLocId(x+1,y) };
		}
		if ((x == (GRID_WIDTH-1))&&(y==0)) {
			return new int[] { getLocId(x,y+1) , getLocId(x-1,y) };
		}
		if ((x == (GRID_WIDTH-1))&&(y==(GRID_HEIGHT-1))) {
			return new int[] { getLocId(x,y-1) , getLocId(x-1,y) };
		}
		if ((x == 0)&&(y>0)&&(y<GRID_HEIGHT-1)) {
			return new int[] {
					getLocId(x, y-1) ,
					getLocId(x+1,y),
					getLocId(x,y+1)
			};
		}
		if ((x == (GRID_WIDTH-1))&&(y>0)&&(y<GRID_HEIGHT-1)) {
			return new int[] {
					getLocId(x, y-1) ,
					getLocId(x-1,y),
					getLocId(x,y+1)
			};
		}
		if ((x>0)&&(x < (GRID_WIDTH-1))&&(y==0)) {
			return new int[] {
					getLocId(x+1, y) ,
					getLocId(x,y+1),
					getLocId(x-1,y)
			};
		}
		if ((x>0)&&(x < (GRID_WIDTH-1))&&(y==GRID_HEIGHT-1)) {
			return new int[] {
					getLocId(x+1, y) ,
					getLocId(x,y-1),
					getLocId(x-1,y)
			};
		}
		return new int[] {
				getLocId(x+1, y) ,				
				getLocId(x-1, y) ,
				getLocId(x, y-1) ,
				getLocId(x, y+1) 
		};
	}


	public static int getLocId(int x, int y) {
		return x*GRID_WIDTH+y;
	}
	
	public static Measure<CarmaSystem> getMeasureOfWaitingUsers( int loc ) {
		
		return new Measure<CarmaSystem>() {
			
				ComponentPredicate predicate = SmartTaxisDefinitions.getWaitingUserPredicate(loc);

				@Override
				public double measure(CarmaSystem t) {
					return t.measure(predicate);
				}

				@Override
				public String getName() {
					return "WAITING_AT_"+loc;
				}
				
		};
		
	}
	
	public static Measure<CarmaSystem> getMeasureOfAvailabelTaxis( int loc ) {
		
		return new Measure<CarmaSystem>() {
			
				ComponentPredicate predicate = SmartTaxisDefinitions.getAvailableTaxiPredicate(loc);

				@Override
				public double measure(CarmaSystem t) {
					double mes = t.measure(predicate);
//					System.out.println(mes+" "+(TAXIS*NUMBER_OF_LOCATIONS)+" "+(mes/(TAXIS*NUMBER_OF_LOCATIONS)));
					return mes/(TAXIS*NUMBER_OF_LOCATIONS);
				}

				@Override
				public String getName() {
					return "FREE_AT_"+loc;
				}
				
		};
		
	}

	public static Measure<CarmaSystem> getMeasureOfAvailabelTaxis( ) {
		
		return new Measure<CarmaSystem>() {
			
				ComponentPredicate predicate = SmartTaxisDefinitions.getAvailableTaxiPredicate();

				@Override
				public double measure(CarmaSystem t) {
					return t.measure(predicate)/(TAXIS*NUMBER_OF_LOCATIONS);
				}

				@Override
				public String getName() {
					return "TOTAL_FREE";
				}
				
		};
		
	}

	public static Measure<CarmaSystem> getMeasureOfMovingTaxis( int loc ) {
		
		return new Measure<CarmaSystem>() {
			
				ComponentPredicate predicate = SmartTaxisDefinitions.getMovingTaxiPredicate(loc);

				@Override
				public double measure(CarmaSystem t) {
					return t.measure(predicate);
				}

				@Override
				public String getName() {
					return "MOVING_AT_"+loc;
				}
				
		};
		
	}

	public static Measure<CarmaSystem> getMeasureOfMovingTaxis( ) {
		
		return new Measure<CarmaSystem>() {
			
				ComponentPredicate predicate = SmartTaxisDefinitions.getMovingTaxiPredicate();

				@Override
				public double measure(CarmaSystem t) {
					double mes = t.measure(predicate);
//					System.out.println(mes+" "+(TAXIS*NUMBER_OF_LOCATIONS)+" "+(mes/(TAXIS*NUMBER_OF_LOCATIONS)));
					return mes/(TAXIS*NUMBER_OF_LOCATIONS);
				}

				@Override
				public String getName() {
					return "TOTAL_MOVING";
				}
				
		};
		
	}
	public static double steps(Integer from, Integer to) {
		int y1 = from/3;
		int x1 = from%3;
		int y2 = to/3;
		int x2 = to%3;
		int steps = Math.abs(x1-x2)+Math.abs(y1-y2);
		if (steps == 0) {
			return 1.0;
		}
		return steps;
	}

	public static double arrivalRate(double d, int loc) {
//		if (d<=LIMIT/2) {
//			if (loc==getLocId(1, 1)) {
//				return 0.25*ARRIVAL_RATE/NUMBER_OF_LOCATIONS;
//			} else {
//				return 0.75*ARRIVAL_RATE/NUMBER_OF_LOCATIONS;
//			}
//		} else {
//			if (loc==getLocId(1, 1)) {
//				return 0.75*ARRIVAL_RATE/NUMBER_OF_LOCATIONS;
//			} else {
//				return 0.25*ARRIVAL_RATE/NUMBER_OF_LOCATIONS;
//			}
//		}
		return ARRIVAL_RATE/((double) NUMBER_OF_LOCATIONS);		
	}

	public static int getDestination(double now, int loc, RandomGenerator r) {
		if (loc == getLocId(1, 1)) {
			return r.nextInt(NUMBER_OF_LOCATIONS);
		} else {
			return getLocId(1, 1);
		}
//		return r.nextInt(NUMBER_OF_LOCATIONS);
	}

}
