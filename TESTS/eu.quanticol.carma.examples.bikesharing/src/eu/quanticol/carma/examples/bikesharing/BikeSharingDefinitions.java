/**
 * 
 */
package eu.quanticol.carma.examples.bikesharing;

import org.apache.commons.math3.random.RandomGenerator;

import eu.quanticol.carma.simulator.CarmaInput;
import eu.quanticol.carma.simulator.CarmaOutput;
import eu.quanticol.carma.simulator.CarmaPredicate;
import eu.quanticol.carma.simulator.CarmaProcessAutomaton;
import eu.quanticol.carma.simulator.CarmaStore;
import eu.quanticol.carma.simulator.CarmaStoreUpdate;
import eu.quanticol.carma.simulator.CarmaSystem;

/**
 * @author loreti
 *
 */
public class BikeSharingDefinitions {

	public static final int TAKE_BIKE = 0;
	public static final int RETURN_BIKE = 1;
	public static final int MOVE = 2;
	public static final int STOP = 3;
	public static final int RESTART = 4;

	public static final CarmaProcessAutomaton UserProcess = createUserProcess();

	public static final CarmaProcessAutomaton ParkingProcess = createParkingProcess();

	private static CarmaProcessAutomaton createUserProcess() {
		CarmaProcessAutomaton toReturn = new CarmaProcessAutomaton("User");

		CarmaProcessAutomaton.State pedestrian = toReturn.newState("Pedestrian");
		CarmaProcessAutomaton.State waitingBike = toReturn.newState("WaitingBike");
		CarmaProcessAutomaton.State biker = toReturn.newState("Biker");
		CarmaProcessAutomaton.State waitingSlot = toReturn.newState("WaitingSlot");
		
		CarmaOutput moveAction = new CarmaOutput( BikeSharing.MOVE , true ) {
			
			@Override
			protected Object getValue(CarmaSystem sys, CarmaStore store, double now) {
				return null;
			}
			
			@Override
			protected CarmaStoreUpdate getUpdate(CarmaSystem sys,  double now) {				
				return new CarmaStoreUpdate() {
					
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
						int next = r.nextInt(BikeSharing.ZONES);
						store.set("zone", next );
					}
				};
			}
			
			@Override
			protected CarmaPredicate getPredicate(CarmaSystem sys, CarmaStore store) {
				return CarmaPredicate.FALSE;
			}
		};
		
		CarmaOutput stopAction = new CarmaOutput( BikeSharing.STOP , true ) {
			
			@Override
			protected Object getValue(CarmaSystem sys, CarmaStore store, double now) {
				return null;
			}
			
			@Override
			protected CarmaStoreUpdate getUpdate(CarmaSystem sys,double now) {				
				return null;
			}
			
			@Override
			protected CarmaPredicate getPredicate(CarmaSystem sys, CarmaStore store) {
				return CarmaPredicate.FALSE;
			}
		};

		CarmaOutput restartAction = new CarmaOutput( BikeSharing.RESTART , true ) {
			
			@Override
			protected Object getValue(CarmaSystem sys, CarmaStore store, double now) {
				return null;
			}
			
			@Override
			protected CarmaStoreUpdate getUpdate(CarmaSystem sys, double now) {				
				return null;
			}
			
			@Override
			protected CarmaPredicate getPredicate(CarmaSystem sys, CarmaStore store) {
				return CarmaPredicate.FALSE;
			}
		};

		CarmaInput takeBike = new CarmaInput( BikeSharing.TAKE_BIKE , false ) {

			@Override
			protected CarmaPredicate getPredicate(CarmaSystem sys, CarmaStore store, Object value) {
				return CarmaPredicate.TRUE;
			}

			@Override
			protected CarmaStoreUpdate getUpdate(CarmaSystem sys, Object value, double now) {
				return new CarmaStoreUpdate() {
					
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
						store.set( "status ", BikeSharing.BIKER );
					}
					
				};
			}
			
		};

		CarmaInput returnBike = new CarmaInput( BikeSharing.RETURN_BIKE , false ) {

			@Override
			protected CarmaPredicate getPredicate(CarmaSystem sys, CarmaStore store, Object value) {
				return CarmaPredicate.TRUE;
			}

			@Override
			protected CarmaStoreUpdate getUpdate(CarmaSystem sys, Object value, double now) {
				return new CarmaStoreUpdate() {
					
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
						store.set( "status ", BikeSharing.PEDESTRIAN );
					}
					
				};
			}
			
		};

		
		toReturn.addTransition(biker, moveAction, biker);
		toReturn.addTransition(biker, stopAction, waitingSlot);
		toReturn.addTransition(pedestrian, restartAction, waitingBike);
		toReturn.addTransition(waitingBike, takeBike, biker);
		toReturn.addTransition(waitingSlot, returnBike, pedestrian);
		
		return toReturn;
	}

	private static CarmaProcessAutomaton createParkingProcess() {
		CarmaProcessAutomaton toReturn = new CarmaProcessAutomaton("User");
		
		CarmaProcessAutomaton.State state = toReturn.newState("S");
		
		CarmaOutput takeBike = new CarmaOutput( BikeSharing.TAKE_BIKE , false ) {
			
			@Override
			protected Object getValue(CarmaSystem sys, CarmaStore store, double now) {
				return null;
			}
			
			@Override
			protected CarmaStoreUpdate getUpdate(CarmaSystem sys, double now) {
				return new CarmaStoreUpdate() {
					
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
						int bikes = store.get("bikes" , Integer.class );
						int slots = store.get("slots" , Integer.class );
						store.set( "bikes" , bikes-1);
						store.set( "slots" , slots+1);
					}
					
				};
			}
			
			@Override
			protected CarmaPredicate getPredicate(CarmaSystem sys, CarmaStore store) {
				return new CarmaPredicate.HasValue<Integer>("zone", Integer.class, store.get("zone",Integer.class));
			}
		};

		CarmaOutput returnBike = new CarmaOutput( BikeSharing.RETURN_BIKE , false ) {
			
			@Override
			protected Object getValue(CarmaSystem sys, CarmaStore store, double now) {
				return null;
			}
			
			@Override
			protected CarmaStoreUpdate getUpdate(CarmaSystem sys, double now) {
				return new CarmaStoreUpdate() {
					
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
						int bikes = store.get("bikes" , Integer.class );
						int slots = store.get("slots" , Integer.class );
						store.set( "bikes" , bikes+1);
						store.set( "slots" , slots-1);
					}
					
				};
			}
			
			@Override
			protected CarmaPredicate getPredicate(CarmaSystem sys, CarmaStore store) {
				return new CarmaPredicate.HasValue<Integer>("zone", Integer.class, store.get("zone",Integer.class));
			}
		};
		
		CarmaPredicate returnBikeGuard = new CarmaPredicate() {

			@Override
			public boolean satisfy(double now,CarmaStore store) {
				int slots = store.get("slots", Integer.class);
				return slots > 0;
			}
			
		};

		CarmaPredicate takeBikeGuard = new CarmaPredicate() {

			@Override
			public boolean satisfy(double now,CarmaStore store) {
				int bikes = store.get("bikes", Integer.class);
				return bikes > 0;
			}
			
		};

		toReturn.addTransition(state,takeBikeGuard,takeBike,state);
		toReturn.addTransition(state,returnBikeGuard,returnBike,state);
		
		return toReturn;
	}
	
	
}
