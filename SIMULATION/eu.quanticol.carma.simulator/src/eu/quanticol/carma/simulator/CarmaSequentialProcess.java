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

	private State currentstate;
	private CarmaProcessAutomaton automaton;
	private boolean kill = false;

	public CarmaSequentialProcess(CarmaComponent component, CarmaProcessAutomaton automaton) {
		this(component,automaton,automaton.getInitialState());
	}

	public CarmaSequentialProcess(CarmaComponent component, CarmaProcessAutomaton automaton, String initialState ) {
		this(component,automaton,automaton.getState(initialState));
	}

	public CarmaSequentialProcess(CarmaProcessAutomaton automaton, State initialState) {
		super( null , automaton.getName() );
		this.automaton = automaton;
		this.currentstate = initialState;
	}

	public CarmaSequentialProcess(CarmaComponent component,
			CarmaProcessAutomaton automaton, State initialState) {
		super( component , automaton.getName() );
		this.automaton = automaton;
		this.currentstate = initialState;
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
		if (currentstate == null) {
			return toReturn;
		}
		LinkedList<CarmaProcessAutomaton.Transition> transitions = currentstate.getTransitions();
		CarmaStore store = getComponent().store;
		for (CarmaProcessAutomaton.Transition transition : transitions) {
			CarmaPredicate guard = transition.getGuard();
			CarmaAction action = transition.getAction();
			final CarmaProcessAutomaton.State next = transition.getNextState();
			if (guard.satisfy(caspaSystem.now() , store)) {
				toReturn = toReturn.add( 
					action.getActivity(
						caspaSystem, 
						getComponent(), 
						new Activity() {	
							
							
							@Override
							public boolean execute(RandomGenerator r) {
								if (transition.isKill()) {
									doKill();
								} else {
									setState(next);
								}
								caspaSystem.removeKilled();
								return true;
							}

							@Override
							public String getName() {
								return "";
							}
						}
					)
				); 
			}
		}
		return toReturn;
	}

	protected boolean doKill() {
		return this.component.kill();
	}

	protected boolean setState(State next) {
		this.currentstate = next;
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
		if (currentstate == null) {
			return toReturn;
		}
		LinkedList<CarmaProcessAutomaton.Transition> transitions = currentstate.getTransitions();
		CarmaStore store = getComponent().store;
		for (CarmaProcessAutomaton.Transition transition : transitions) {
			final CarmaProcessAutomaton.State next = transition.getNextState();
			if (transition.getGuard().satisfy(system.now(), store)) {
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
									if (transition.isKill()) {
										doKill();
									} else {
										setState(next);
									}
									return true;
								}

								@Override
								public String getName() {
									return "";
								}
							})
				);
			}
		}
		return toReturn;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		if (currentstate==null) {
			return this.getName()+"[nil]";
		}
		return currentstate.toString();
	}

	public State getState() {
		return currentstate;
	}

	public CarmaProcessAutomaton automaton() {
		return automaton;
	}

	/* (non-Javadoc)
	 * @see eu.quanticol.carma.simulator.CarmaProcess#getName()
	 */
	@Override
	public String getName() {
		return (currentstate==null?"nil":currentstate.getName());
	}
	
	


}
