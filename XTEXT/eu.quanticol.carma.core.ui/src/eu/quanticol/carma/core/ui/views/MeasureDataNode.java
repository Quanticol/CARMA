/**
 * 
 */
package eu.quanticol.carma.core.ui.views;

import eu.quanticol.carma.core.ui.data.MeasureData;

/**
 * @author loreti
 *
 */
public class MeasureDataNode {

	private MeasureNode parent;
	private MeasureData md;

	public MeasureDataNode(MeasureNode parent, MeasureData md) {
		this.parent = parent;
		this.md = md;
	}

	/**
	 * @return the parent
	 */
	public MeasureNode getParent() {
		return parent;
	}

	/**
	 * @return the md
	 */
	public MeasureData getMd() {
		return md;
	}

}
