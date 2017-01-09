/**
 * 
 */
package eu.quanticol.carma.simulator2;

import java.util.function.BiPredicate;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * @author loreti
 *
 */
public class InputStepData {

	private final BiPredicate<Store,Object> inputPredicate;
	
	private final Function<Store, Store> storeUpdate;
	
	private final int nextProcessId;
	
	/**
	 * @param outputPredicate
	 * @param outputValue
	 * @param storeUpdate
	 */
	public InputStepData(BiPredicate<Store,Object> inputPredicate, Function<Store, Store> storeUpdate, int nextProcessId) {
		super();
		this.inputPredicate = inputPredicate;
		this.storeUpdate = storeUpdate;
		this.nextProcessId = nextProcessId;
	}

	/**
	 * @return the outputPredicate
	 */
	public BiPredicate<Store, Object> getOutputPredicate() {
		return inputPredicate;
	}

	/**
	 * @return the storeUpdate
	 */
	public Function<Store, Store> getStoreUpdate() {
		return storeUpdate;
	}

	/**
	 * @return the nextProcessId
	 */
	public int getNextProcessId() {
		return nextProcessId;
	}

	
}
