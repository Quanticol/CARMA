/**
 * 
 */
package eu.quanticol.ms.tests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.pm.BroadcastRule;
import org.cmg.ml.sam.sim.pm.PopulationRule;
import org.cmg.ml.sam.sim.pm.PopulationState;
import org.cmg.ml.sam.sim.pm.PopulationTransition;
import org.cmg.ml.sam.sim.pm.Update;

/**
 * @author loreti
 *
 */
public class TestUtils {

	public static double[] averageDrift(int iterations,RandomGenerator rg, PopulationRule r , PopulationState state) {
		double[] drift = new double[state.size()];
		for( int i=0 ; i<iterations; i++) {
			int[] rdrift = run(rg,r,state);
			for( int k=0 ;k<rdrift.length ; k++ ) {
				drift[k] += rdrift[k]; 
			}
		}
		for( int k=0 ;k<drift.length ; k++ ) {
			drift[k] = drift[k]/iterations; 
		}
		return drift;
	}

	public static int[] run( RandomGenerator rg, PopulationRule r , PopulationState state ) {
		int[] drift = new int[state.size()];
		PopulationTransition pt = r.apply(rg, state  );
		Update u = pt.apply(rg);
		if (u != null) {
			for( int i=0 ;i<drift.length ; i++ ) {
				drift[i] = u.get(i);
			}
		}
		return drift;
	}

	public static void checkAvverage(int iterations, RandomGenerator rg, PopulationRule r, PopulationState state, double[] expected , double error ) {
		double[] ad = TestUtils.averageDrift(iterations, rg, r, state);
		for( int i=0 ; i<ad.length ; i++ ) {
			assertEquals("Specie "+i, expected[i], ad[i],error);
		}
	}

	public static void check(Update u, int[] drift) {
		assertNotNull(u);
		for(int i=0; i<drift.length; i++) {
			assertEquals(drift[i],u.get(i));
		}
	}

}
