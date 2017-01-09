/**
 * 
 */
package org.cmg.ml.sam.sim.pm;

/**
 * @author loreti
 *
 */
public class Drift {

	private int pre;
	
	private int post;
	
	public Drift( ) {
		this( 0 , 0 );
	}
	
	public Drift( int pre, int post) {
		this.pre = pre;
		this.post = post;
	}
	
	public void addToPreset( int x ) {
		this.pre += x;
	}
	
	public void addToPoset( int x ) {
		this.post += x;
	}
	
	public int getPreset() {
		return pre;
	}
	
	public int getPoset() {
		return post;
	}
}
