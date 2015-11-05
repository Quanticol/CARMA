package eu.quanticol.carma.core.ui

import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.generator.ms.MSCompiler
import eu.quanticol.carma.core.utils.LabelUtil
import eu.quanticol.carma.simulator.CarmaModel
import java.net.MalformedURLException
import java.net.URLClassLoader
import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.Path
import org.eclipse.emf.common.util.EList
import org.eclipse.emf.ecore.EObject
import org.eclipse.jdt.core.JavaCore
import org.eclipse.jdt.launching.JavaRuntime
import org.eclipse.ui.PlatformUI
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.ui.editor.XtextEditor
import org.eclipse.xtext.util.concurrent.IUnitOfWork

class CarmaUiUtil {

	LabelUtil labelUtil = new LabelUtil()

	def loadModel( Model m ) {

		var packageName = MSCompiler::PACK
		var className = labelUtil.name( m )

		var classLoader = typeof(CarmaModel).classLoader
		var clazz = classLoader.loadClass(packageName+"."+className)
		
		var o = clazz.newInstance
		
		if (o instanceof CarmaModel) {
			o
		} else {
			null
		}
		
	}
	
	def loadModel( XtextEditor editor ) {
		var CarmaModel result = null
		var content = editor.document.readOnly(
					new IUnitOfWork<EList<EObject>, XtextResource>(){

                        def override EList<EObject> exec(XtextResource state) throws Exception {
                                if (state.getErrors().size()>0) {
                                        return null;
                                }
                                return state.getContents();
                        }
                        
                }
		)
		if (( content != null)&&(content.size > 0)) {
			var model = content.get(0)
			if (model instanceof Model) {
				var project = editor.resource
				if (project instanceof IFile) {
					var packageName = MSCompiler::PACK
					var className = labelUtil.name( model )
			
					var classLoader = typeof(CarmaModel).classLoader
					var clazz = classLoader.loadClassFromProject(
						packageName+"."+className ,	
						project.project
					)
							
					var o = clazz.newInstance
					
					if (o instanceof CarmaModel) {
						result = o
					} 
				}
			}
		}		
		result
	}
	
	/*

		XtextEditor editor = getXtextEditor(event);
		if (editor == null) {
			return null;
		}
		EObject model = getModel(editor);
		if (model == null) {
			return null;
		}
		IFile file = (IFile) editor.getResource();
		String extension = file.getFileExtension();
		LinkedList<IModelCheckerExtensionPoint> languageSimulators = loadModelCheckers(extension);
		if (languageSimulators.size() == 0) {
			return null;
		}
		ModelCheckingDialog dialog = new ModelCheckingDialog(file,model,languageSimulators , HandlerUtil.getActiveShell(event),  SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL);
		dialog.open();		
		return null;

	 */
	
	 def Class<?> loadClassFromProject( ClassLoader parentClassLoader , String className , IProject project ) throws CoreException, 
MalformedURLException, ClassNotFoundException {
		var classPathEntries = JavaRuntime.computeDefaultRuntimeClassPath(JavaCore.create(project));
		var entry = classPathEntries.get(0)
		var path = new Path(entry);
		var url = path.toFile().toURI().toURL();
		var URL = newArrayList( url )
		var cl = new URLClassLoader( URL , parentClassLoader);
		cl.loadClass(className)
	}
	
	
	def getActiveModels() {
		var references = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getEditorReferences()
		var xtextEditorList = references.map[it.getPart(false)].filter(typeof(XtextEditor))
		newHashMap( xtextEditorList.map[it.resource -> it.loadModel].filter[ it.value != null ] )
	}
	
	
}