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
public abstract class CarmaProcess {
	
	protected CarmaComponent component;
	protected String name;

	public CarmaProcess( String name ) {
		this(null,name);
	}
	
	public CarmaProcess(CarmaComponent component , String name ) {
		this.name = name;
		this.component = component;
	}
	
	protected void setComponent( CarmaComponent component ) {
		this.component = component;
	}
	
	protected CarmaComponent getComponent( ) {
		return this.component;
	}
	
	protected CarmaStore getStore() {
		return component.store;
	}
	
	protected <T> T get( String attribute , Class<T> clazz ) {
		return component.get(attribute, clazz);
	}
	
	protected void set( String attribute , Object value ) {
		this.component.set( attribute , value );
	}
	
	public String getName( ) {
		return name;
	}
	
	public abstract WeightedStructure<Activity> doReceiveBroadcast( CarmaSystem system , CarmaStore sender ,  int action, Object value );

	public abstract WeightedStructure<Activity> doReceiveUnicast( CarmaSystem system , CarmaStore sender ,  int action, Object value );

	public abstract WeightedStructure<Activity> getActivities(CarmaSystem caspaSystem);
	
	protected boolean doKill() {
		return this.component.kill();
	}

}
