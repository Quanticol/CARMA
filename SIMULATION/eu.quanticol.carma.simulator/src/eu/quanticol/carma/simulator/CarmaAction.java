/**
 * 
 */
package eu.quanticol.carma.simulator;

import org.cmg.ml.sam.sim.Activity;
import org.cmg.ml.sam.sim.util.WeightedStructure;

/**
 * @author loreti
 *
 */
public interface CarmaAction {
	
	public WeightedStructure<Activity> getActivity( 
			CarmaSystem caspaSystem , 
			CarmaComponent caspaComponent , 
			Activity continuation 
		);

	public WeightedStructure<Activity> receive( 
			CarmaSystem caspaSystem , 
			CarmaComponent caspaComponent ,
			CarmaStore sender , 
			int action ,
			Object value , 
			boolean broadcast , 
			Activity continuation 
		);
	
}
