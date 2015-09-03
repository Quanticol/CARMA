package eu.quanticol.carma.ui.views;

import java.util.ArrayList;

import org.cmg.ml.sam.sim.sampling.SimulationTimeSeries;
import org.cmg.ml.sam.sim.sampling.StatisticSampling;

import eu.quanticol.carma.simulator.CarmaSystem;
import eu.quanticol.carma.ui.laboratory.ExperimentJob;

public class ExperimentJobProvider {
	
	private ArrayList<ExperimentJob> ejbs;
	private SimulationTimeSeries sts;
	
	public ExperimentJobProvider(ExperimentJob experimentJob){
		
		ejbs = new ArrayList<ExperimentJob>();
		
		ejbs.add(experimentJob);
	}
	
	public ArrayList<ExperimentJob> getResults(){
		return ejbs;
	}
	
	public String toCSVString(){
		
		String toReturn = "";
		return toReturn;
	}

}
