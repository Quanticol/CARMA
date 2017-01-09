/**
 * 
 */
package eu.quanticol.carma.simulator2;

import java.util.LinkedList;
import java.util.function.Function;

/**
 * @author loreti
 *
 */
public class OutputActionData {
	
	private final int componentId;
	private final int agentIndex;
	private final int agentId;
	
	private final Function<Store,LinkedList<OutputStepData>> steps;

	/**
	 * @param componentId
	 * @param agentIndex
	 * @param agentId
	 * @param steps
	 */
	public OutputActionData(int componentId, int agentIndex, int agentId,
			Function<Store, LinkedList<OutputStepData>> steps) {
		super();
		this.componentId = componentId;
		this.agentIndex = agentIndex;
		this.agentId = agentId;
		this.steps = steps;
	}

	/**
	 * @return the componentId
	 */
	public int getComponentId() {
		return componentId;
	}

	/**
	 * @return the agentIndex
	 */
	public int getAgentIndex() {
		return agentIndex;
	}

	/**
	 * @return the agentId
	 */
	public int getAgentId() {
		return agentId;
	}

	/**
	 * @return the steps
	 */
	public Function<Store, LinkedList<OutputStepData>> getSteps() {
		return steps;
	}

	
	
}
