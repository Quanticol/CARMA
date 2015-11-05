/**
 * 
 */
package eu.quanticol.SIRS;

import org.apache.commons.math3.random.RandomGenerator;

import eu.quanticol.carma.simulator.CarmaInput;
import eu.quanticol.carma.simulator.CarmaOutput;
import eu.quanticol.carma.simulator.CarmaPredicate;
import eu.quanticol.carma.simulator.CarmaProcessAutomaton;
import eu.quanticol.carma.simulator.CarmaStore;
import eu.quanticol.carma.simulator.CarmaStoreUpdate;

/**
 * @author loreti
 *
 */
public class SIRSDefinitions {

	//actions
	public static final int CONTACT = 0;
	public static final int RECOVERY = 1;
	public static final int SUSCEPTIBLE = 2;
	public static final int MOVE = 3;

	public static final CarmaProcessAutomaton SIRProcess = createSIRProcess();

	public static final CarmaProcessAutomaton MoveProcess = createMoveProcess();

	private static CarmaProcessAutomaton createSIRProcess() {
		CarmaProcessAutomaton toReturn = new CarmaProcessAutomaton("SIR");

		CarmaProcessAutomaton.State susceptible = toReturn.newState("Susceptible");
		CarmaProcessAutomaton.State infective = toReturn.newState("Infective");
		CarmaProcessAutomaton.State recovered = toReturn.newState("Recovered");
		
		CarmaInput contactInAction = new CarmaInput( SIRS.CONTACT , true ) {

			@Override
			protected CarmaPredicate getPredicate(CarmaStore store, Object value, double now) {
				// TODO Auto-generated method stub
				return null;
			}

			@Override
			protected CarmaStoreUpdate getUpdate(Object value, double now) {
				// TODO Auto-generated method stub
				return null;
			}
			
		};
		
		CarmaOutput contactOutAction = new CarmaOutput( SIRS.CONTACT , true ) {

			@Override
			protected CarmaPredicate getPredicate(CarmaStore store, double now) {
				// TODO Auto-generated method stub
				return null;
			}

			@Override
			protected CarmaStoreUpdate getUpdate(double now) {
				// TODO Auto-generated method stub
				return null;
			}

			@Override
			protected Object getValue(CarmaStore store, double now) {
				// TODO Auto-generated method stub
				return null;
			}


			
		};


		CarmaOutput recoveryAction = new CarmaOutput( SIRS.RECOVERY , true ) {

			@Override
			protected CarmaPredicate getPredicate(CarmaStore store, double now) {
				// TODO Auto-generated method stub
				return null;
			}

			@Override
			protected CarmaStoreUpdate getUpdate(double now) {
				// TODO Auto-generated method stub
				return null;
			}

			@Override
			protected Object getValue(CarmaStore store, double now) {
				// TODO Auto-generated method stub
				return null;
			}
			
		};
		
		CarmaOutput susceptibleAction = new CarmaOutput( SIRS.SUSCEPTIBLE , true ) {

			@Override
			protected CarmaPredicate getPredicate(CarmaStore store, double now) {
				// TODO Auto-generated method stub
				return null;
			}

			@Override
			protected CarmaStoreUpdate getUpdate(double now) {
				// TODO Auto-generated method stub
				return null;
			}

			@Override
			protected Object getValue(CarmaStore store, double now) {
				// TODO Auto-generated method stub
				return null;
			}
			
		};

		
		toReturn.addTransition(susceptible, contactInAction, infective);
		toReturn.addTransition(infective, contactOutAction, infective);
		toReturn.addTransition(infective, recoveryAction, recovered);
		toReturn.addTransition(recovered, susceptibleAction, susceptible);
		
		return toReturn;
	}

	private static CarmaProcessAutomaton createMoveProcess() {
		CarmaProcessAutomaton toReturn = new CarmaProcessAutomaton("A");
		
		CarmaProcessAutomaton.State state = toReturn.newState("A");
		
		CarmaOutput move = new CarmaOutput( SIRS.MOVE , true ) {

			@Override
			protected CarmaPredicate getPredicate(CarmaStore store, double now) {
				// TODO Auto-generated method stub
				return null;
			}

			@Override
			protected CarmaStoreUpdate getUpdate(double now) {
				// TODO Auto-generated method stub
				return null;
			}

			@Override
			protected Object getValue(CarmaStore store, double now) {
				// TODO Auto-generated method stub
				return null;
			}
			

		};

		toReturn.addTransition(state,move,state);
		
		return toReturn;
	}
	
	
}
