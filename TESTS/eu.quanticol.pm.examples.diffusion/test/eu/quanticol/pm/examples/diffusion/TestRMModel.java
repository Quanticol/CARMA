package eu.quanticol.pm.examples.diffusion;

import static org.junit.jupiter.api.Assertions.*;

import java.io.FileNotFoundException;

import org.cmg.ml.sam.sim.Activity;
import org.cmg.ml.sam.sim.DefaultRandomGenerator;
import org.cmg.ml.sam.sim.pm.PopulationModel;
import org.cmg.ml.sam.sim.tests.pm.RBFactory;
import org.cmg.ml.sam.sim.tests.pm.RBModel;
import org.cmg.ml.sam.sim.util.WeightedElement;
import org.cmg.ml.sam.sim.util.WeightedStructure;
import org.junit.jupiter.api.Test;

class TestRMModel {

	@Test
	void test() throws FileNotFoundException {
		RBFactory rbm = new RBFactory(new int[] {7, 32, 60, 1} , 10, 1.0, 1.0, 0.5, 0.5);
		PopulationModel pm = rbm.getModel();
		WeightedStructure<Activity> act = pm.getActivities(new DefaultRandomGenerator());
		
//		RBModel.run("test", 1, 10.0, 100, );
	}

}
