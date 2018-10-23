/**
 * 
 */
package eu.quanticol.ms.tests;

import static org.junit.Assert.*;

import org.cmg.ml.sam.sim.RandomGeneratorRegistry;
import org.cmg.ml.sam.sim.pm.PopulationState;
import org.cmg.ml.sam.sim.pm.PopulationTransition;
import org.cmg.ml.sam.sim.pm.UnicastRule;
import org.junit.Test;

/**
 * @author loreti
 *
 */
public class TestUnicastRule {
	
	
	public static final UnicastRule getRule( double sendingRate , double w1, double w2 ) {
		return new UnicastRule(
				"r1", 
				(rg -> sendingRate), 
				0, 
				(rg -> 1),
				new UnicastRule.UnicastReceiver(2, (s->w1), (rg->3)),
				new UnicastRule.UnicastReceiver(4, (s->w2), (rg->5))
				);		
	}
	
	@Test			
	public void testRuleFail( ) {
		RandomGeneratorRegistry rgi = RandomGeneratorRegistry.getInstance();		
		UnicastRule r1 = getRule(1.0, 1, 1);
		PopulationTransition tra = r1.apply(rgi.get(), new PopulationState( new int[] {1,0,0,0,0,0} ));
		assertNull(tra);
	}
			
	@Test			
	public void testRuleSucc1( ) {
		RandomGeneratorRegistry rgi = RandomGeneratorRegistry.getInstance();		
		UnicastRule r1 = getRule(1.0, 1, 1);
		PopulationTransition tra = r1.apply(rgi.get(), new PopulationState( new int[] {1,0,1,0,0,0} ));
		assertNotNull(tra);
		assertEquals(1.0,tra.getRate(),0.0);
		TestUtils.check(tra.apply(rgi.get()), new int[] { -1 , +1 , -1, +1 , 0, 0 });
	}

	@Test			
	public void testRuleSucc2( ) {
		RandomGeneratorRegistry rgi = RandomGeneratorRegistry.getInstance();		
		UnicastRule r1 = getRule(1.0, 1, 1);
		PopulationTransition tra = r1.apply(rgi.get(), new PopulationState( new int[] {1,0,0,0,1,0} ));
		assertNotNull(tra);
		assertEquals(1.0,tra.getRate(),0.0);
		TestUtils.check(tra.apply(rgi.get()), new int[] { -1 , +1 , 0, 0 , -1, 1 });
	}

	@Test			
	public void testRuleSucc3( ) {
		RandomGeneratorRegistry rgi = RandomGeneratorRegistry.getInstance();		
		UnicastRule r1 = getRule(1.0, 1, 1);
		PopulationTransition tra = r1.apply(rgi.get(), new PopulationState( new int[] {1,0,1,0,1,0} ));
		assertNotNull(tra);
		assertEquals(1.0,tra.getRate(),0.0);
	}

	@Test			
	public void testRuleSucc4( ) {
		RandomGeneratorRegistry rgi = RandomGeneratorRegistry.getInstance();		
		UnicastRule r1 = getRule(1.0, 1, 1);
		PopulationTransition tra = r1.apply(rgi.get(), new PopulationState( new int[] {5,0,1,0,1,0} ));
		assertNotNull(tra);
		assertEquals(5.0,tra.getRate(),0.0);
	}

	@Test			
	public void testRuleAverage( ) {
		RandomGeneratorRegistry rgi = RandomGeneratorRegistry.getInstance();		
		UnicastRule r1 = getRule(1.0, 1, 1);
		TestUtils.checkAvverage(100000, rgi.get(), r1, new PopulationState( new int[] {1,0,1,0,1,0} ), new double[] { -1 , +1 , -0.5, 0.5 , -0.5, 0.5 }, 0.1);
	}

	@Test			
	public void testRuleAverage2( ) {
		double rate = 1.0;
		double w1 = 1.0;
		double w2 = 1.0;
		int size1 = 10;
		int size2 = 1;
		RandomGeneratorRegistry rgi = RandomGeneratorRegistry.getInstance();		
		UnicastRule r1 = getRule(rate, w1, w2);
		double d1 = (size1*w1)/(size1*w1+size2*w2);
		double d2 = (size2*w1)/(size1*w1+size2*w2);
		TestUtils.checkAvverage(100000, rgi.get(), r1, new PopulationState( new int[] {1,0,size1,0,size2,0} ), new double[] { -1 , +1 , -d1, +d1 , -d2 , d2 }, 0.1);
	}
}
