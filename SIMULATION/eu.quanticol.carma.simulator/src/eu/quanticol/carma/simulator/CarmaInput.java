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
public abstract class CarmaInput implements CarmaAction {
	
	private int action;
	private boolean broadcast;

	public CarmaInput(int action, boolean broadcast) {
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
	protected abstract CarmaPredicate getPredicate( CarmaStore store , Object value );

	/**
	 * @return the update
	 */
	protected abstract CarmaStoreUpdate getUpdate( Object value , double now );

	protected double getReceivingProbability(CarmaSystem caspaSystem , CarmaStore receiver , CarmaStore sender , Object value ) {
		if (broadcast) {
			return caspaSystem.broadcastProbability(sender, receiver, action);
		} else {
			return caspaSystem.unicastProbability(sender, receiver, action);
		}
	}

	@Override
	public WeightedStructure<Activity> getActivity(final CarmaSystem caspaSystem,
			CarmaComponent caspaComponent,
			Activity continuation ) {
		return null;
	}

	@Override
	public WeightedStructure<Activity> receive( 
			final CarmaSystem caspaSystem , 
			final CarmaComponent caspaComponent ,
			final CarmaStore sender , 
			final int	action ,
			final Object value , 
			boolean broadcast , 
			Activity continuation ) {
		if ((this.broadcast == broadcast) 
			&&(this.action == action) 
			&&(this.getPredicate(caspaComponent.store, value).satisfy(caspaSystem.now(),sender)) 
		) {
			Activity actionActivity = new Activity() {
				
				@Override
				public boolean execute(RandomGenerator r) {
					CarmaStoreUpdate update = getUpdate(value, caspaSystem.now);
					if (update != null) {
						update.update( r , caspaComponent.store );
					}
					if (!CarmaInput.this.broadcast) {
						caspaSystem.unicastUpdate(r,sender, caspaComponent.store , action , value );
					}
					return true;
				}
				
			};
			if (continuation != null) {
				actionActivity = new SequenceOfActivities( actionActivity , continuation );
			}
			return new WeightedElement<Activity>(
					getReceivingProbability(caspaSystem, caspaComponent.store, sender, value), 
					actionActivity
			);
		}
		return null;
	}

	
}
