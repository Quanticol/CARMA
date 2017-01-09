/**
 * 
 */
package eu.quanticol.carma.core;

import java.util.LinkedList;

import javax.tools.DiagnosticCollector;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.resource.XtextResourceSet;

import com.google.inject.Injector;

import eu.quanticol.carma.core.carma.Model;
import eu.quanticol.carma.core.generator.ms.MSCompiler;
import eu.quanticol.carma.core.generator.ms.model.ModelHandler;
import eu.quanticol.carma.core.utils.LabelUtil;
import eu.quanticol.carma.simulator.CarmaModel;
import eu.quanticol.carma.simulator.CarmaSystem;


/**
 * @author loreti
 *
 */
public class ModelLoader {

	private XtextResourceSet resourceSet;
	private CharSequenceCompiler<CarmaModel> compiler;
	//private Utilities utilities;
	private ModelHandler generator;

	public ModelLoader() {
		Injector injector = new CARMAStandaloneSetup().createInjectorAndDoEMFRegistration();
		this.resourceSet = injector.getInstance(XtextResourceSet.class);
		this.resourceSet.addLoadOption(XtextResource.OPTION_RESOLVE_ALL, Boolean.TRUE);		
		this.generator = new ModelHandler();//injector.getInstance(CARMAGenerator.class);
//		this.utilities = injector.getInstance(Utilities.class);
		this.compiler = new CharSequenceCompiler<>(CarmaSystem.class.getClassLoader(), new LinkedList<>());
    }
	
	public CarmaModel load( String fileName ) {
		System.out.println("Loading: "+fileName);
		URI uri = URI.createURI(fileName);
		Resource resource = resourceSet.getResource(uri, true);
		EObject object = resource.getContents().get(0);
		if (object instanceof Model) {
			Model model = (Model) object;
			String packageName = MSCompiler.PACK;//utilities.getPackageName(uri); 
			String className = new LabelUtil().name(model);
			String fullClassName = packageName+"."+className;
			CharSequence code = generator.modelToJava(model, className, packageName);//generator.generateJavaCode(model, packageName, className);
			final DiagnosticCollector<JavaFileObject> errs =
		            new DiagnosticCollector<JavaFileObject>();
			try {
				Class<CarmaModel> compiledScript = compiler.compile(fullClassName, code, errs, new Class<?>[] { CarmaSystem.class } );
				return compiledScript.newInstance();
			} catch (ClassCastException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (CharSequenceCompilerException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IllegalAccessException e) {
				e.printStackTrace();
			} catch (InstantiationException e) {
				e.printStackTrace();
			}
		}

		
	    return null;

	}
	
	class JavaSourceFromString extends SimpleJavaFileObject {
		  final String code;

		  JavaSourceFromString(String name, String code) {
		    super(java.net.URI.create("string:///" + name.replace('.','/') + Kind.SOURCE.extension),Kind.SOURCE);
		    this.code = code;
		  }

		  @Override
		  public CharSequence getCharContent(boolean ignoreEncodingErrors) {
		    return code;
		  }
	}

}
