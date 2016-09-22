/**
 * 
 */
package eu.quanticol.carma.core.tests;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.util.JavaVersion;
import org.eclipse.xtext.xbase.compiler.GeneratorConfig;
import org.eclipse.xtext.xbase.compiler.IGeneratorConfigProvider;

/**
 * @author loreti
 *
 */
@SuppressWarnings("restriction")
public class MyGeneratorConfigProvider implements IGeneratorConfigProvider{
	
		@Override
		public GeneratorConfig get(EObject context) {
			GeneratorConfig gc = new GeneratorConfig();
			gc.setJavaSourceVersion(JavaVersion.JAVA8);
			return gc;
		}
    	  

}
