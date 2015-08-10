/**
 * 
 */
package eu.quanticol.carma.core.ui;

import eu.quanticol.carma.simulator.CarmaModel;

/**
 * @author loreti
 *
 */
public class Util {

	public CarmaModel loadModel( String packageName , String className ) 
			  	throws ClassNotFoundException, InstantiationException, IllegalAccessException {

		ClassLoader classLoader = CarmaModel.class.getClassLoader();
        Class<?> clazz = classLoader.loadClass(packageName+"."+className);
        
        Object o = clazz.newInstance();
        
        if (o instanceof CarmaModel) {
        	return (CarmaModel) o;
        } else {
        	return null;//FIXME: An exception could be thrown here!!!
        }
	}
	
}
