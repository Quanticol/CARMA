/**
 * 
 */
package eu.quanticol.carma.core.ui.views;

import org.cmg.ml.sam.sim.sampling.SimulationTimeSeries;
import org.eclipse.nebula.visualization.xygraph.dataprovider.AbstractDataProvider;
import org.eclipse.nebula.visualization.xygraph.dataprovider.ISample;
import org.eclipse.nebula.visualization.xygraph.dataprovider.Sample;
import org.eclipse.nebula.visualization.xygraph.linearscale.Range;

/**
 * @author loreti
 *
 */
public class SimulationTrace extends AbstractDataProvider {

	private SimulationTimeSeries serie;

	public SimulationTrace(SimulationTimeSeries serie) {
		super(false);
		this.serie = serie;
	}

	@Override
	public int getSize() {
		return serie.getSize();
	}
	
	public String getName() {
		return serie.getName();
	}

	@Override
	public ISample getSample(int index) {
//		double interval = serie.getStandardDeviation(index)/Math.sqrt( replications );
		return new Sample(serie.getTime(index), 
				serie.getMean(index),
				serie.getConfidenceInterval(index),
				serie.getConfidenceInterval(index),
				0,
				0
				);
	}

//	@Override
	protected void innerUpdate() {
		// TODO Auto-generated method stub
		
	}

	public double confidenceInterval( int index ) {
		return serie.getConfidenceInterval( index );
	}
	
//	@Override
	protected void updateDataRange(boolean positiveOnly) {
		this.xDataMinMax = new Range(0.0, serie.getTime(serie.getSize()-1));
//		double minY = Double.MAX_VALUE;
		double minY = 0;
		double maxY = Double.MIN_VALUE;
		for( int i=0 ; i<serie.getSize() ; i++ ) {
			double confidence = confidenceInterval(i);
			double value = serie.getMean(i)+confidence;
			if (value < minY) {
				minY = value;
			}
			if (value > maxY) {
				maxY = value;
			}
		}
		this.yDataMinMax = new Range(minY, maxY);
//		this.yDataMinMax = new Range(0, maxY);
	}


}
