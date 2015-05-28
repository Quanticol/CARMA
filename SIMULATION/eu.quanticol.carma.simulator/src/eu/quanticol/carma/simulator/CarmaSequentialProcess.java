/**
 * 
 */
package eu.quanticol.carma.simulator;

import java.util.LinkedList;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.Activity;
import org.cmg.ml.sam.sim.util.ComposedWeightedStructure;
import org.cmg.ml.sam.sim.util.WeightedStructure;

import eu.quanticol.carma.simulator.CarmaProcessAutomaton.State;
import eu.quanticol.carma.simulator.CarmaProcessAutomaton.Transition;

/**
 * @author loreti
 *
 */
public class CarmaSequentialProcess extends CarmaProcess {

	private State currenstate;
	private CarmaProcessAutomaton automaton;
	private boolean kill = false;

	public CarmaSequentialProcess(CarmaComponent component, CarmaProcessAutomaton automaton) {
		this(component,automaton,automaton.getInitialState());
	}

	public CarmaSequentialProcess(CarmaComponent component, CarmaProcessAutomaton automaton, String initialState ) {
		this(component,automaton,automaton.getState(initialState));
	}

	public CarmaSequentialProcess(CarmaComponent component,
			CarmaProcessAutomaton automaton, State initialState) {
		super( component , automaton.getName() );
		this.automaton = automaton;
		this.currenstate = initialState;
	}

	public void setKill() {
		this.kill = true;
	}
	
	public boolean isKill() {
		return this.kill;
	}

	@Override
	public WeightedStructure<Activity> getActivities( CarmaSystem caspaSystem ) {
		WeightedStructure<Activity> toReturn = new ComposedWeightedStructure<Activity>();
		if (currenstate == null) {
			return toReturn;
		}
		LinkedList<CarmaProcessAutomaton.Transition> transitions = currenstate.getTransitions();
		CarmaStore store = getComponent().store;
		for (CarmaProcessAutomaton.Transition transition : transitions) {
			CarmaPredicate guard = transition.getGuard();
			CarmaAction action = transition.getAction();
			final CarmaProcessAutomaton.State next = transition.getNextState();
			if (guard.satisfy(store)) {
				toReturn = toReturn.add( 
					action.getActivity(
						caspaSystem, 
						getComponent(), 
						new Activity() {				
							@Override
							public boolean execute(RandomGenerator r) {
								return setState(next);
							}
						}
					)
				); 
			}
		}
		return toReturn;
	}

	protected boolean setState(State next) {
		this.currenstate = next;
		return true;
	}

	@Override
	public WeightedStructure<Activity> doReceiveBroadcast(CarmaSystem system,
			CarmaStore sender, int action, Object value) {
		return doReceiveInput(system,sender,action,value,true);
	}

	@Override
	public WeightedStructure<Activity> doReceiveUnicast(CarmaSystem system,
			CarmaStore sender, int action, Object value) {
		return doReceiveInput(system,sender,action,value,false);
	}

	private WeightedStructure<Activity> doReceiveInput(CarmaSystem system,
			CarmaStore sender, int action, Object value, boolean broadcast) {
		WeightedStructure<Activity> toReturn = new ComposedWeightedStructure<Activity>();
		if (currenstate == null) {
			return toReturn;
		}
		LinkedList<CarmaProcessAutomaton.Transition> transitions = currenstate.getTransitions();
		for (CarmaProcessAutomaton.Transition transition : transitions) {
			final CarmaProcessAutomaton.State next = transition.getNextState();
			toReturn = toReturn.add(
				transition.getAction().receive(
						system, 
						getComponent(), 
						sender, 
						action, 
						value, 
						broadcast, 
						new Activity() {				
							@Override
							public boolean execute(RandomGenerator r) {
								return setState(next);
							}
						})
			);
		}
		return toReturn;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		if (currenstate==null) {
			return this.getName()+"[nil]";
		}
		return currenstate.toString();
	}

	public State getState() {
		return currenstate;
	}

	public CarmaProcessAutomaton automaton() {
		return automaton;
	}


}
