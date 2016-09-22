/**
 * 
 */
package eu.quanticol.carma.simulator;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;

/**
 * @author loreti
 *
 */
public class CarmaProcessAutomaton {
	
	private String processName;
	private int stateCounter;
	private HashMap<String,State> states;
	private State init;
	
	public CarmaProcessAutomaton(String processName) {
		this.stateCounter = 0;
		this.states = new HashMap<String,State>();
		this.processName = processName;
	}
	
	
	public State newState( String name ) {
		if (states.containsKey(name)) {
			throw new IllegalArgumentException("Duplicated state name!");
		}
		State s = new State (stateCounter++ , name);
		states.put(name,s);
		if (init == null) {
			this.init = s;
		}
		return s;
	}

	public void addTransition( State s1 , CarmaAction action , State s2 ) {
		s1.addTransition(action, s2,false);
	}

	public void addTransition( State s1 , CarmaAction action , State s2 , boolean isKill ) {
		s1.addTransition(action, s2,isKill);
	}

	public void addTransition( State s1 , CarmaPredicate predicate , CarmaAction action , State s2 , boolean isKill ) {
		s1.addTransition(predicate , action, s2,isKill);
	}
	
	public void addTransition( State s1 , CarmaPredicate predicate , CarmaAction action , State s2 ) {
		s1.addTransition(predicate , action, s2,false);
	}

	public class Transition {
		
		private CarmaAction action;
		private State nextState;
		private CarmaPredicate guard;
		private boolean isKill;
		
		public Transition(CarmaAction action , State nextState ) {
			this(CarmaPredicate.TRUE,action,nextState,false);
		}
		
		public Transition(CarmaAction action , State nextState , boolean isKill ) {
			this(CarmaPredicate.TRUE,action,nextState,isKill);
		}

		public Transition(CarmaPredicate guard , CarmaAction action , State nextState ) {
			this(guard,action,nextState,false);
		}
		
		public Transition(CarmaPredicate guard , CarmaAction action , State nextState , boolean isKill ) {
			this.guard = guard;
			this.action = action;
			this.nextState = nextState;
			this.isKill = isKill;
		}
		
		/**
		 * @return the action
		 */
		public CarmaAction getAction() {
			return action;
		}
		/**
		 * @return the next
		 */
		public State getNextState() {
			return nextState;
		}
		public CarmaPredicate getGuard() {
			return guard;
		}
		
		public boolean isKill() {
			return isKill;
		}
	}
	
	
	public class State {
		
		private int id;
		
		private String name;
		
		private LinkedList<Transition> transitions = new LinkedList<Transition>();

		public State(int id, String name) {
			this.id = id;
			this.name = name;
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			return id;
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		@Override
		public boolean equals(Object obj) {
			if (obj instanceof State) {
				return this.id == ((State) obj).id;
			}
			return false;
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			return processName+"["+name+"]";
		}
		
		private void addTransition( CarmaAction act , State next , boolean isKill ) {
			transitions.add(new Transition(act,next,isKill));
		}
		
		public void addTransition(CarmaPredicate guard, CarmaAction act,
				State next, boolean isKill) {
			transitions.add(new Transition(guard,act,next,isKill));
		}


		
		public LinkedList<Transition> getTransitions() {
			return transitions;
		}

		public String getName() {
			return name;
		}
	}


	public State getInitialState() {
		return init;
	}


	public String getName() {
		return processName;
	}


	public State getState(String name) {
		return states.get(name);
	}


}
