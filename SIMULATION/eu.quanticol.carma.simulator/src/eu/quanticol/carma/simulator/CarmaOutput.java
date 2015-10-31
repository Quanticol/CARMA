/**
 * 
 */
package eu.quanticol.carma.simulator;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.Activity;
import org.cmg.ml.sam.sim.SequenceOfActivities;
import org.cmg.ml.sam.sim.util.WeightedElement;
import org.cmg.ml.sam.sim.util.WeightedStructure;

/**
 * @author loreti
 *
 */
public abstract class CarmaOutput implements CarmaAction {
	
	@Override
	public String toString() {
		return action+(broadcast?"*":"");
	}

	private int action;
	private boolean broadcast;

	public CarmaOutput(int action, boolean broadcast ) {
		super();
		this.action = action;
		this.broadcast = broadcast;
	}

	/**
	 * @return the action
	 */
	public int getAction() {
		return action;
	}

	/**
	 * @return the broadcast
	 */
	public boolean isBroadcast() {
		return broadcast;
	}

	/**
	 * @return the predicate
	 */
	protected abstract CarmaPredicate getPredicate( CarmaStore store , double now );

	/**
	 * @return the update
	 */
	protected abstract CarmaStoreUpdate getUpdate( double now );

	protected double getRate(CarmaSystem caspaSystem, CarmaComponent caspaComponent) {
		if (broadcast) {
			return caspaSystem.broadcastRate(caspaComponent.store, action);
		} else {
			return caspaSystem.unicastRate(caspaComponent.store, action);
		}
	}

	@Override
	public WeightedStructure<Activity> getActivity(final CarmaSystem caspaSystem,
			final CarmaComponent caspaComponent,
			Activity continuation ) {
		Activity actionActivity = new Activity() {
			
			@Override
			public boolean execute(RandomGenerator r) {
				if (broadcast) {
					Object value = getValue( caspaComponent.store , caspaSystem.now );
					caspaSystem.broadcastOutput(r, caspaComponent, action, getPredicate(caspaComponent.store,caspaSystem.now ), value );
					CarmaStoreUpdate update = getUpdate(caspaSystem.now );
					if (update != null) {
						update.update( r , caspaComponent.store );
					}
					caspaSystem.broadcastUpdate(r,caspaComponent.store, action, value );
					return true;
				} else {
					if (caspaSystem.unicastOutput(r, caspaComponent, action, getPredicate(caspaComponent.store,caspaSystem.now ), getValue( caspaComponent.store,caspaSystem.now ))) {
						getUpdate(caspaSystem.now ).update( r , caspaComponent.store );
						//N.B. The update of a unicast is triggered inside input action!!!!
						return true;
					}
					return false;
				}
			}
			
		};
		if (continuation != null) {
			actionActivity = new SequenceOfActivities( actionActivity , continuation );
		}
		return new WeightedElement<Activity>(getRate(caspaSystem, caspaComponent), actionActivity);
	}

	protected abstract Object getValue(CarmaStore store, double now);

	@Override
	public WeightedStructure<Activity> receive(CarmaSystem caspaSystem,
			CarmaComponent caspaComponent, CarmaStore sender, int action,
			Object value, boolean broadcast, Activity continuation) {
		return null;
	}


	
}
