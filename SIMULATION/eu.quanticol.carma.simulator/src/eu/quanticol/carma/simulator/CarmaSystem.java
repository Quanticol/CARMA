/**
 * 
 */
package eu.quanticol.carma.simulator;

import java.util.LinkedList;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.Activity;
import org.cmg.ml.sam.sim.ModelI;
import org.cmg.ml.sam.sim.util.ComposedWeightedStructure;
import org.cmg.ml.sam.sim.util.WeightedStructure;

/**
 * @author loreti
 *
 */
public abstract class CarmaSystem implements ModelI {

	protected LinkedList<CarmaComponent> collective;
	
	protected static CarmaStore global_store;
	
	protected double now;

	public CarmaSystem() {
		this.global_store = new CarmaStore();
		this.collective = new LinkedList<CarmaComponent>();
		this.now = 0.0;
	}
	
	public void broadcastOutput( RandomGenerator r , CarmaComponent sender,
			int action, CarmaPredicate predicate, Object value) {
		for (CarmaComponent c : collective) {
			if (c!=sender) {
				c.inputBroadcast(r , this , sender , action , predicate , value );
			}
		}
	}
	
	public boolean unicastOutput( RandomGenerator r , CarmaComponent sender, int action,
			CarmaPredicate predicate, Object value) {
		WeightedStructure<Activity> activeInputs = new ComposedWeightedStructure<Activity>();

		for (CarmaComponent caspaComponent : collective) {
			if (sender != caspaComponent) {
				activeInputs = activeInputs.add( caspaComponent.inputUnicast(this,sender,action,predicate,value) );				
			}
			
		}
		
		if (activeInputs.getTotalWeight()>0.0) {
			double foo = r.nextDouble();
			foo = foo*activeInputs.getTotalWeight();
			activeInputs.select(foo).getElement().execute(r);
			return true;
		}
		return false;
	}
	

	public abstract double broadcastProbability( CarmaStore sender , CarmaStore receiver , int action );
	
	public abstract double unicastProbability( CarmaStore sender , CarmaStore receiver , int action );

	public abstract double broadcastRate( CarmaStore sender , int action );

	public abstract double unicastRate( CarmaStore sender , int action );
	
	public abstract void broadcastUpdate( RandomGenerator random , CarmaStore sender , int action , Object value );

	public abstract void unicastUpdate( RandomGenerator random , CarmaStore sender , CarmaStore receiver, int action , Object value );
	
	public int count( CarmaPredicate p ) {
		int counter = 0;
		
		for (CarmaComponent caspaComponent : collective) {
			if (p.satisfy(caspaComponent.store)) {
				counter++;
			}
		}
		
		return counter;
	}

	public void addComponent( CarmaComponent c ) {
		this.collective.add(c);
	}
	
	public LinkedList<CarmaComponent> getCollective() {
		return collective;
	}

	/* (non-Javadoc)
	 * @see org.cmg.ml.sam.sim.ModelI#getActions()
	 */
	@Override
	public WeightedStructure<Activity> getActivities() {
		WeightedStructure<Activity> toReturn = new ComposedWeightedStructure<Activity>();
		for (CarmaComponent caspaComponent : collective) {
			toReturn.add( caspaComponent.getActivities( this ) );
		}
		return toReturn;
	}

	/* (non-Javadoc)
	 * @see org.cmg.ml.sam.sim.ModelI#timeStep(double)
	 */
	@Override
	public void timeStep(double dt) {
		this.now += dt;
	}
	
	public double now() {
		return now;
	}

	public int measure( ComponentPredicate p ) {
		int count = 0;
		for (CarmaComponent c : collective) {
			if (p.eval(c)) {
				count++;
			}
		}
		return count;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return global_store.toString()+"|-"+this.collective.toString();
	}

	public Iterable<CarmaComponent> components() {
		return collective;
	}
	
}
