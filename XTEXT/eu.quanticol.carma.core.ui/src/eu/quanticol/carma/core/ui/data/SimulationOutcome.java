/**
 * 
 */
package eu.quanticol.carma.core.ui.data;

import java.util.List;

import org.cmg.ml.sam.sim.sampling.SimulationTimeSeries;

/**
 * @author loreti
 *
 */
public class SimulationOutcome {

	private String startingTime;
	private double totalTime;
	private double averageTime;
	
	private List<SimulationTimeSeries> collectedData;
	
	public SimulationOutcome( String startingTime , double totalTime , double averageTime , List<SimulationTimeSeries> collectedData ) {
		this.startingTime = startingTime;
		this.totalTime = totalTime;
		this.averageTime = averageTime;
		this.collectedData = collectedData;
	}

	/**
	 * @return the startingTime
	 */
	public String getStartingTime() {
		return startingTime;
	}

	/**
	 * @return the totalTime
	 */
	public double getTotalTime() {
		return totalTime;
	}

	/**
	 * @return the averageTime
	 */
	public double getAverageTime() {
		return averageTime;
	}
	
	/**
	 * @return the collected data.
	 */
	public List<SimulationTimeSeries> getCollectedData() {
		return collectedData;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		long temp;
		temp = Double.doubleToLongBits(averageTime);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		result = prime * result + ((startingTime == null) ? 0 : startingTime.hashCode());
		temp = Double.doubleToLongBits(totalTime);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		return result;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		SimulationOutcome other = (SimulationOutcome) obj;
		if (Double.doubleToLongBits(averageTime) != Double.doubleToLongBits(other.averageTime))
			return false;
		if (startingTime == null) {
			if (other.startingTime != null)
				return false;
		} else if (!startingTime.equals(other.startingTime))
			return false;
		if (Double.doubleToLongBits(totalTime) != Double.doubleToLongBits(other.totalTime))
			return false;
		return true;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return startingTime;// + "(" + (totalTime/1000) + "]";
	}
	
	
}
