/**
 * 
 */
package eu.quanticol.carma.examples.gc;

import org.apache.commons.math3.random.RandomGenerator;

import eu.quanticol.carma.simulator.CarmaAction;
import eu.quanticol.carma.simulator.CarmaComponent;
import eu.quanticol.carma.simulator.CarmaInput;
import eu.quanticol.carma.simulator.CarmaOutput;
import eu.quanticol.carma.simulator.CarmaPredicate;
import eu.quanticol.carma.simulator.CarmaProcess;
import eu.quanticol.carma.simulator.CarmaProcessAutomaton;
import eu.quanticol.carma.simulator.CarmaSequentialProcess;
import eu.quanticol.carma.simulator.CarmaStore;
import eu.quanticol.carma.simulator.CarmaStoreUpdate;

/**
 * @author loreti
 *
 */
public class GroupiesCelebritiesDefinitions {

	public final static int KIND_A = 0;
	public final static int KIND_B = 1;
	public final static int ACTION_ID = 0;
	public final static String KIND_ATTRIBUTE = "kind";
	
	public final static CarmaProcessAutomaton GroupyAutomaton = createGroupyAutomaton();
	public final static CarmaProcessAutomaton CelebrityAutomaton = createCelebrityAutomaton();
	
	public static CarmaProcess getGroupyProcess( CarmaComponent component ) {
		return new CarmaSequentialProcess(component, GroupyAutomaton);
	}

	public static CarmaProcess getCelebrityProcess( CarmaComponent component ) {
		return new CarmaSequentialProcess(component, CelebrityAutomaton);
	}

	private static CarmaProcessAutomaton createGroupyAutomaton() {
		CarmaProcessAutomaton automaton = new CarmaProcessAutomaton("Groupy");
		
		CarmaProcessAutomaton.State state = automaton.newState("G");
		
		CarmaAction spreadAction = new CarmaOutput( ACTION_ID , true ) {
			
			@Override
			protected Object getValue(CarmaStore store, double now) {
				return store.get("kind", Integer.class);
			}
			
			@Override
			protected CarmaStoreUpdate getUpdate(double now) {
				return null;
			}
			
			@Override
			protected CarmaPredicate getPredicate(CarmaStore store) {
				return CarmaPredicate.TRUE;
			}
		};
		
		CarmaInput inputAction = new CarmaInput( ACTION_ID , true ) {

			@Override
			protected CarmaPredicate getPredicate(CarmaStore store, Object value) {
				return CarmaPredicate.TRUE;
			}

			@Override
			protected CarmaStoreUpdate getUpdate(final Object value, double now) {
				return new CarmaStoreUpdate() {
					
					@Override
					public void update(RandomGenerator r, CarmaStore store ) {
						store.set(KIND_ATTRIBUTE, value);
					}
					
				};
			}
			
		};
		
		
		automaton.addTransition(state, spreadAction, state);
		automaton.addTransition(state, inputAction, state);
		
		
		return automaton;
	}
	
	private static CarmaProcessAutomaton createCelebrityAutomaton() {
		CarmaProcessAutomaton automaton = new CarmaProcessAutomaton("Celebrity");
		
		CarmaProcessAutomaton.State state = automaton.newState("C");
		
		CarmaAction spreadAction = new CarmaOutput( ACTION_ID , true ) {
			
			@Override
			protected Object getValue(CarmaStore store, double now) {
				return store.get("kind", Integer.class);
			}
			
			@Override
			protected CarmaStoreUpdate getUpdate(double now) {
				return null;
			}
			
			@Override
			protected CarmaPredicate getPredicate(CarmaStore store) {
				return CarmaPredicate.TRUE;
			}
		};
		
		CarmaInput inputAction = new CarmaInput( ACTION_ID , true ) {

			@Override
			protected CarmaPredicate getPredicate(CarmaStore localStore, Object value) {
				final int kind = localStore.get(KIND_ATTRIBUTE, Integer.class).intValue();
				return new CarmaPredicate() {

					@Override
					public boolean satisfy(double now,CarmaStore store) {
						return kind == store.get(KIND_ATTRIBUTE, Integer.class).intValue();
					}
					
				};
			}

			@Override
			protected CarmaStoreUpdate getUpdate(final Object value, double now) {
				return new CarmaStoreUpdate() {
					
					@Override
					public void update(RandomGenerator r, CarmaStore store ) {
						int value = store.get(KIND_ATTRIBUTE, Integer.class);
						store.set(KIND_ATTRIBUTE, ((value+1)%2) );
					}
					
				};
			}
			
		};
		
		
		automaton.addTransition(state, spreadAction, state);
		automaton.addTransition(state, inputAction, state);
		
		
		return automaton;
	}
}
