/**
 * 
 */
package eu.quanticol.carma.simulator;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.Activity;
import org.cmg.ml.sam.sim.ModelI;
import org.cmg.ml.sam.sim.sampling.Measure;
import org.cmg.ml.sam.sim.util.ComposedWeightedStructure;
import org.cmg.ml.sam.sim.util.WeightedStructure;

import eu.quanticol.carma.simulator.space.SpaceModel;

/**
 * @author loreti
 *
 */
public abstract class CarmaSystem implements ModelI {
	
	
	private static SpaceModel currentSpaceModel = new SpaceModel();
	
	public static void setCurrentSpaceModel( SpaceModel model ) {
		if (model != null) {
			currentSpaceModel = model;
		} else {
			currentSpaceModel = new SpaceModel();
		}
	}
	
	public static SpaceModel getCurrentSpaceModel() {
		return currentSpaceModel;
	}
	
	
	public static final String LOC_ATTRIBUTE_NAME = "loc";
	
	protected LinkedList<CarmaComponent> collective;
	
	protected CarmaStore global;
	
	protected double now;
	
	protected final SpaceModel space;

	
	public CarmaSystem( ) {
		this( new SpaceModel() );
	}
	
	public CarmaSystem( SpaceModel space ) {
		setCurrentSpaceModel(space);
		this.global = new CarmaStore();
		this.collective = new LinkedList<CarmaComponent>();
		this.now = 0.0;
		this.space = space;
	}
	
	public SpaceModel getSpaceModel() {
		return this.space;
	}
	
	public void broadcastOutput( RandomGenerator r , CarmaComponent sender,
			int action, CarmaPredicate predicate, Object value) {
		for (CarmaComponent c : collective) {
			if (c!=sender) {
				c.inputBroadcast(r , this , sender , action , predicate , value );				
			}
		}
	}
	
	public void removeKilled() {
		LinkedList<CarmaComponent> newCollective = new LinkedList<>();
		for (CarmaComponent carmaComponent : collective) {
			if (!carmaComponent.isKilled()) {
				newCollective.add(carmaComponent);
			}
		}
		this.collective = newCollective;
	}
	

	public void setGLobalAttribute( String attribute , Object value ) {
		global.set(attribute, value);
	}
	
	public <T> T getGlobalAttribute( String attribute, Class<T> clazz ) {
		return global.get(attribute, clazz);
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
			try {
				if (p.satisfy(now,caspaComponent.store)) {
					counter++;
				}
			} catch (NullPointerException e) {				
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
	public WeightedStructure<Activity> getActivities( RandomGenerator r ) {
		WeightedStructure<Activity> toReturn = new ComposedWeightedStructure<Activity>();
		for (CarmaComponent caspaComponent : collective) {
			toReturn = toReturn.add( caspaComponent.getActivities( this ) );
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
			try {
			if (p.eval(now,c)) {
				count++;
			}
			} catch (NullPointerException e) {
				
			}
		}
		return count;
	}
	
	public double min( Measure<CarmaStore> m , CarmaPredicate guard ) {
		double value = Double.POSITIVE_INFINITY;
		for (CarmaComponent carmaComponent : collective) {
			try {
				if (guard.satisfy(now,carmaComponent.store)) {
					double v = m.measure(carmaComponent.store);
					if (v<value) {
						value = v;
					}
				}
			} catch (NullPointerException e) {				
			}
		}		
		return value;
	}

	public double max( Measure<CarmaStore> m , CarmaPredicate guard ) {
		double value = Double.NEGATIVE_INFINITY;
		for (CarmaComponent carmaComponent : collective) {
			try {
				if (guard.satisfy(now,carmaComponent.store)) {
					double v = m.measure(carmaComponent.store);
					if (v>value) {
						value = v;
					}
				}
			} catch (NullPointerException e ){				
			}
		}
		return value;
	}

	public double average( Measure<CarmaStore> m , CarmaPredicate guard ) {
		double value = 0.0;
		int count = 0;
		for (CarmaComponent carmaComponent : collective) {
			try {
				if (guard.satisfy(now,carmaComponent.store)) {
					value += m.measure(carmaComponent.store);
					count++;
				}
			} catch (NullPointerException e) {
			}
		}
		return value/count;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return global.toString()+"|-"+this.collective.toString();
	}

	public Iterable<CarmaComponent> components() {
		return collective;
	}
	
	public CarmaStore getGlobalStore() {
		return global;
	}
	
	public static <T> T uniform( RandomGenerator rg , T ... args ) {
		int idx = rg.nextInt(args.length);
		return args[idx];
	}
	
	public LinkedList<Map<String,Object>> retrieve( CarmaPredicate guard , String[] attributes  ) {
		LinkedList<Map<String,Object>> toReturn = new LinkedList<>();
		for (CarmaComponent carmaComponent : collective) {
			if (guard.satisfy(now,carmaComponent.store)) {
				HashMap<String,Object> data = new HashMap<>();
				for( int i=0 ; i<attributes.length ; i++ ) {
					Object foo = carmaComponent.get(attributes[i], Object.class );
					if (foo != null) {
						data.put( attributes[i] , foo );
					}
				}
				toReturn.add(data);
			}
		}		
		return toReturn;
	}
}
