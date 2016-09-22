/**
 * 
 */
package eu.quanticol.carma.simulator;

import java.util.LinkedList;

/**
 * @author loreti
 *
 */
public class BasicComponentPredicate implements ComponentPredicate {

	private final CarmaProcessPredicate[] states;
	private final CarmaPredicate guard;
	
	public BasicComponentPredicate( CarmaPredicate guard , CarmaProcessPredicate ... states  ) {
		this.states = states;
		this.guard = guard;
	}
	
	@Override
	public boolean eval(double now,CarmaComponent c) {
		try {
			if (this.guard.satisfy(now,c.store)) {
				boolean[] foo = new boolean[c.processes.size()];
				for( int i=0 ; i<states.length ; i++ ) {
					boolean flag = false;
					for( int j=0 ; (j<c.processes.size())&&!flag ; j++ ) {
						if ((!foo[j])&&
							states[i].eval( c.processes.get(j) )) {
								foo[j] = true;
								flag = true;
						}
					}
					if (!flag) {
						return false;
					}
				}
				return true;
			}
			return false;
		} catch (NullPointerException e) {
			return false;
		}
	}

}
