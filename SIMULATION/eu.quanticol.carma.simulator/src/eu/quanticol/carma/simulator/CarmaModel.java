/**
 * 
 */
package eu.quanticol.carma.simulator;

import java.util.HashMap;
import java.util.Map;

import org.cmg.ml.sam.sim.SimulationFactory;
import org.cmg.ml.sam.sim.sampling.Measure;

import eu.quanticol.carma.simulator.space.SpaceModel;


/**
 * @author loreti
 *
 */
public abstract class CarmaModel {
	
	public abstract String[] getSystems();
	
	public abstract SimulationFactory<CarmaSystem> getFactory( String name );
	
	public abstract String[] getMeasures();
	
	public abstract String[] getMeasureParameters( String name );
	
	public abstract Map<String,Class<?>> getParametersType( String name );
	
	public abstract Measure<CarmaSystem> getMeasure( String name , Map<String,Object> parameters );
	
	public Measure<CarmaSystem> getMeasure( String name ) {
		return getMeasure(name, new HashMap<String, Object>() );
	}

	public boolean checkParameterType( String measure , Map<String,Object> parameters ) {
		String[] names = getMeasureParameters(measure);
		if (names == null) {
			return false;
		}
		if (parameters.size() != names.length) {
			return false;
		}
		Map<String,Class<?>> types = getParametersType(measure);
		for (String n : names) {
			Class<?> t = types.get(n);
			Object v = parameters.get(n);
			if ((t==null)||(v==null)||(!t.isInstance(v))) {
				return false;
			}
		}
		return true;
	}
	
}
