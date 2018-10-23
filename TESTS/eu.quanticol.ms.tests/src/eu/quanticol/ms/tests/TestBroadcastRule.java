/**
 * 
 */
package eu.quanticol.ms.tests;

import static org.junit.Assert.*;

import java.util.function.BiFunction;
import java.util.function.Function;

import org.cmg.ml.sam.sim.RandomGeneratorRegistry;
import org.cmg.ml.sam.sim.pm.BroadcastRule;
import org.cmg.ml.sam.sim.pm.PopulationState;
import org.cmg.ml.sam.sim.pm.PopulationTransition;
import org.cmg.ml.sam.sim.pm.Update;
import org.junit.Test;

/**
 * @author loreti
 *
 */
public class TestBroadcastRule {
	
	public static int ITERATIONS = 100000;
	
	public static double ERROR = 0.1;

	private static final BroadcastRule RULE_1 = new BroadcastRule(
			"test", 
			(s -> 1.0), 
			0, 
			(rg -> 1)
		);

	private static final BroadcastRule RULE_2 = new BroadcastRule(
			"test", 
			(s -> 1.0), 
			0, 
			(rg -> 1),
			new BroadcastRule.BroadcastReceiver(1, (s -> 1.0), (rg -> 0))
		);

	private static final Function<Double,BroadcastRule> F_RULE_3 = p -> new BroadcastRule(
			"test", 
			(s -> 1.0), 
			0, 
			(rg -> 0),
			new BroadcastRule.BroadcastReceiver(1, (s -> p), (rg -> 2))
		);

	private static final BiFunction<Double,Double,BroadcastRule> F_RULE_4 = (p1, p2) -> new BroadcastRule(
			"test", 
			(s -> 1.0), 
			0, 
			(rg -> 0),
			new BroadcastRule.BroadcastReceiver(1, (s -> p1), (rg -> (rg.nextDouble()<p2?2:3)))
		);

	
	@Test
	public void testRULE1_Fail() {
		RandomGeneratorRegistry rgi = RandomGeneratorRegistry.getInstance();		
		PopulationTransition pt = RULE_1.apply(rgi.get(), new PopulationState( new int[] { 0 , 0 } )  );
		assertNull( pt );
	}
	
	@Test
	public void testRULE1_Success() {
		RandomGeneratorRegistry rgi = RandomGeneratorRegistry.getInstance();		
		PopulationTransition pt = RULE_1.apply(rgi.get(), new PopulationState( new int[] { 1 , 0 } )  );
		assertNotNull( pt );
		assertEquals(1.0, pt.getRate(), 0.0);
		Update u = pt.apply(rgi.get());
		assertNotNull( u );
		assertEquals(-1,u.get(0));
		assertEquals(1,u.get(1));		
	}

	@Test
	public void testRULE1_Rate() {
		RandomGeneratorRegistry rgi = RandomGeneratorRegistry.getInstance();		
		PopulationTransition pt = RULE_1.apply(rgi.get(), new PopulationState( new int[] { 5 , 0 } )  );
		assertNotNull( pt );
		assertEquals(5.0, pt.getRate(), 0.0);
		Update u = pt.apply(rgi.get());
		TestUtils.check(u,new int[] {-1,1});
	}

	@Test
	public void testRULE2_Success() {
		RandomGeneratorRegistry rgi = RandomGeneratorRegistry.getInstance();		
		PopulationTransition pt = RULE_2.apply(rgi.get(), new PopulationState( new int[] { 1 , 0 } )  );
		assertNotNull( pt );
		assertEquals(1.0, pt.getRate(), 0.0);
		Update u = pt.apply(rgi.get());
		TestUtils.check(u,new int[] {-1,1});
	}

	@Test
	public void testRULE2_Success2() {
		RandomGeneratorRegistry rgi = RandomGeneratorRegistry.getInstance();		
		PopulationTransition pt = RULE_2.apply(rgi.get(), new PopulationState( new int[] { 1 , 1 } )  );
		assertNotNull( pt );
		assertEquals(1.0, pt.getRate(), 0.0);
		Update u = pt.apply(rgi.get());
		assertNotNull( u );
		assertTrue(u.getUpdate().isEmpty());
	}

	@Test
	public void testRULE2_Success3() {
		RandomGeneratorRegistry rgi = RandomGeneratorRegistry.getInstance();		
		PopulationTransition pt = RULE_2.apply(rgi.get(), new PopulationState( new int[] { 1 , 2 } )  );
		assertNotNull( pt );
		assertEquals(1.0, pt.getRate(), 0.0);
		Update u = pt.apply(rgi.get());
		TestUtils.check(u,new int[] {1,-1});
	}
	
	
	@Test
	public void testRULE3_NoMove() {
		BroadcastRule br = F_RULE_3.apply(0.0);
		RandomGeneratorRegistry rgi = RandomGeneratorRegistry.getInstance();		
		PopulationTransition pt = br.apply(rgi.get(), new PopulationState( new int[] { 1 , 100 , 0 } )  );
		assertNotNull( pt );
		assertEquals(1.0, pt.getRate(), 0.0);
		Update u = pt.apply(rgi.get());
		assertNotNull( u );
		assertTrue(u.getUpdate().isEmpty());
	}

	@Test
	public void testRULE3_AllMove() {
		BroadcastRule br = F_RULE_3.apply(1.0);
		RandomGeneratorRegistry rgi = RandomGeneratorRegistry.getInstance();		
		PopulationTransition pt = br.apply(rgi.get(), new PopulationState( new int[] { 1 , 100 , 0 } )  );
		assertNotNull( pt );
		assertEquals(1.0, pt.getRate(), 0.0);
		Update u = pt.apply(rgi.get());
		assertNotNull( u );
		TestUtils.check(u,new int[] {0,-100,100});
	}

	@Test
	public void testRULE3_Average() {
		RandomGeneratorRegistry rgi = RandomGeneratorRegistry.getInstance();		
		BroadcastRule br = F_RULE_3.apply(0.5);
		TestUtils.checkAvverage(ITERATIONS, rgi.get(), br, new PopulationState( new int[] { 1 , 1000 , 0 } ), new double[] { 0.0 , -500.0 , +500.0 }, ERROR);
	}

	@Test
	public void testRULE3_Average2() {
		RandomGeneratorRegistry rgi = RandomGeneratorRegistry.getInstance();		
		BroadcastRule br = F_RULE_3.apply(0.25);
		TestUtils.checkAvverage(ITERATIONS, rgi.get(), br, new PopulationState( new int[] { 1 , 1000 , 0 } ), new double[] { 0.0 , -250.0 , +250.0 }, ERROR);
	}

	@Test
	public void testRULE4_Average() {
		RandomGeneratorRegistry rgi = RandomGeneratorRegistry.getInstance();		
		BroadcastRule br = F_RULE_4.apply(1.0,1.0);
		TestUtils.checkAvverage(1000, rgi.get(), br, new PopulationState( new int[] { 1 , 1000 , 0 , 0} ), new double[] { 0.0 , -1000.0 , +1000.0 , 0 }, ERROR);
	}

	@Test
	public void testRULE4_Average2() {
		RandomGeneratorRegistry rgi = RandomGeneratorRegistry.getInstance();		
		BroadcastRule br = F_RULE_4.apply(1.0,0.0);
		TestUtils.checkAvverage(1000, rgi.get(), br, new PopulationState( new int[] { 1 , 1000 , 0 , 0} ), new double[] { 0.0 , -1000.0 , 0 , +1000.0 }, ERROR);
	}

	@Test
	public void testRULE4_Average3() {
		RandomGeneratorRegistry rgi = RandomGeneratorRegistry.getInstance();		
		BroadcastRule br = F_RULE_4.apply(1.0,0.5);
		TestUtils.checkAvverage(ITERATIONS, rgi.get(), br, new PopulationState( new int[] { 1 , 1000 , 0 , 0} ), new double[] { 0.0 , -1000.0 , +500.0 , +500.0 }, ERROR);
	}

	@Test
	public void testRULE4_Average4() {
		double p1 = 0.1;
		double p2 = 0.1;
		int size = 1000;
		RandomGeneratorRegistry rgi = RandomGeneratorRegistry.getInstance();		
		BroadcastRule br = F_RULE_4.apply(p1,p2);
		TestUtils.checkAvverage(ITERATIONS, rgi.get(), br, new PopulationState( new int[] { 1 , size , 0 , 0} ), new double[] { 0.0 , -size*p1 , size*p1*p2 , size*p1*(1-p2) }, ERROR);
	}

	@Test
	public void testRULE4_Average5() {
		int size = 1000;
		double p1 = 5.0/size;
		double p2 = 0.5;
		RandomGeneratorRegistry rgi = RandomGeneratorRegistry.getInstance();		
		BroadcastRule br = F_RULE_4.apply(p1,p2);
		TestUtils.checkAvverage(ITERATIONS, rgi.get(), br, new PopulationState( new int[] { 1 , size , 0 , 0} ), new double[] { 0.0 , -size*p1 , size*p1*p2 , size*p1*(1-p2) }, ERROR);
	}
}
