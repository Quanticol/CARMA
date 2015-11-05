/**
 * 
 */
package eu.quanticol.carma.simulator;

import org.cmg.ml.sam.sim.SimulationFactory;
import org.cmg.ml.sam.sim.sampling.Measure;


/**
 * @author loreti
 *
 */
public interface CarmaModel {
	
	public String[] getSystems();
	
	public SimulationFactory<CarmaSystem> getFactory( String name );
	
	public String[] getMeasures();
	
	public Measure<CarmaSystem> getMeasure( String name );

}
