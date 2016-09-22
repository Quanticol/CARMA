package eu.quanticol.carma.core.ui

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.generator.ms.MSCompiler
import eu.quanticol.carma.core.ui.nature.CarmaNature
import eu.quanticol.carma.core.utils.LabelUtil
import eu.quanticol.carma.simulator.CarmaModel
import java.net.MalformedURLException
import java.net.URLClassLoader
import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.Path
import org.eclipse.emf.common.util.EList
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.EObject
import org.eclipse.jdt.core.JavaCore
import org.eclipse.jdt.launching.JavaRuntime
import org.eclipse.ui.PlatformUI
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.ui.editor.XtextEditor
import org.eclipse.xtext.util.concurrent.IUnitOfWork
import org.eclipse.xtext.ui.resource.IResourceSetProvider
import org.eclipse.xtext.resource.IResourceDescriptions
import eu.quanticol.carma.core.ui.views.models.ProjectSimulationSuite
import java.io.BufferedReader
import eu.quanticol.carma.core.ui.data.SimulationExperiment
import java.util.LinkedList
import eu.quanticol.carma.core.ui.data.MeasureData
import java.io.PrintWriter
import eu.quanticol.carma.core.ui.views.models.SimulationSuiteElement
import eu.quanticol.carma.core.ui.data.SimulationExperiment
import java.io.ByteArrayInputStream
import java.io.StringWriter
import java.io.InputStreamReader
import eu.quanticol.carma.core.ui.views.models.WorkspaceSimulationSuite
import org.eclipse.core.resources.IResourceChangeListener
import org.eclipse.core.resources.IResourceChangeEvent

class CarmaUiUtil {
	
	@Inject
	def CarmaUiUtil() {
		
	}

	@Inject 
	IResourceSetProvider resourceSetProvider; 

	@Inject
    IResourceDescriptions resourceDescriptions;

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
		content.loadModel( editor.resource.project )
	}
	
	def loadModel( EList<EObject> content , IProject project ) {
		var CarmaModel result = null
			try {
			if (( content != null)&&(content.size > 0)) {
				var model = content.get(0)
				if (model instanceof Model) {
	//				if (project instanceof IFile) {
						var packageName = MSCompiler::PACK
						var className = labelUtil.name( model )
				
						var classLoader = typeof(CarmaModel).classLoader
						var clazz = classLoader.loadClassFromProject(
							packageName+"."+className ,	
							project.project
						)
						
						var o = clazz ?. newInstance
						
						if (o instanceof CarmaModel) {
							result = o
						} 
					}
	//			}
			} 				
		} catch (RuntimeException e) {				
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
	
	 def Class<?> loadClassFromProject( ClassLoader parentClassLoader , String className , IProject project ) {
		try {
			var classPathEntries = JavaRuntime.computeDefaultRuntimeClassPath(JavaCore.create(project));
			var entry = classPathEntries.get(0)
			var path = new Path(entry);
			var url = path.toFile().toURI().toURL();
			var URL = newArrayList( url )
			var cl = new URLClassLoader( URL , parentClassLoader);
			cl.loadClass(className)
		} catch (Exception e) {
			return null;
		}
	
	}
	
	
	def getActiveModels() {
		var references = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getEditorReferences()
		var xtextEditorList = references.map[it.getPart(false)].filter(typeof(XtextEditor))
		newHashMap( xtextEditorList.map[it.resource -> it.loadModel].filter[ it.value != null ] )
	}

	def getActiveModels( IProject project ) {
		var references = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getEditorReferences()
		var xtextEditorList = references.map[it.getPart(false)].filter(typeof(XtextEditor))
		newHashMap( xtextEditorList.filter[it.resource.project.name==project.name].map[it.resource -> it.loadModel].filter[ it.value != null ] )
	}
	
	def getCarmaProjects() {
		ResourcesPlugin.workspace.root.projects.filter[ it.isOpen ].filter[it.hasNature(CarmaNature::ID)]
	}
	
	def getProjectModels( IProject project ) {
		//		carmaProjects.forEach[it.accept(new CarmaProjectVisitor())]		
		var visitor = new CarmaProjectVisitor();
		project.accept(visitor);
		newHashMap(visitor.carmaModels.map[it -> project.loadCarmaModel(it) ].filter[ it.value != null ])		
//		project.activeModels
	}
	
	def loadCarmaModel( IProject p , IResource r ) {
		var rSet = resourceSetProvider.get(p)
		if (r.exists) {
			rSet.getResource(
	                URI.createPlatformPluginURI(r.getFullPath().toOSString(), false), true) ?. contents ?. loadModel(p);
		} else {
			null
		}
		//Resource.Factory.Registry.INSTANCE.createResource(URI.createURI(r.fullPath.toOSString)) ?. contents ?. loadModel(p);
	}
	
	def getResourceSetProvider() {
		resourceSetProvider
	}
	
	def saveProjectSimulationSuite( ProjectSimulationSuite suite ) {
		var suiteFile = suite.project.getFile("experiments");
		var sWriter = new StringWriter();
		var w = new PrintWriter( sWriter );
		for (SimulationSuiteElement sse : suite.getSimulationSuiteElements()) {
			sse.simulationExperiment.writeSimuluationExperiment(w)
		}
		if (suiteFile.exists) {
			suiteFile.setContents( new ByteArrayInputStream(sWriter.toString().getBytes()) , true , false , null );
		} else {
			suiteFile.create( new ByteArrayInputStream(sWriter.toString().getBytes()) , true , null);
		}
		suiteFile.setDerived(true, null);
	}
	
	def loadProjectSimulationSuite( IProject project ) {
		var file = project.getFile("experiments")
		if (file.exists) {
			try {
				var content = file.contents	
				new ProjectSimulationSuite( project , project.readSimulationExperimentFrom( new BufferedReader( new InputStreamReader( content ) ) ) ) 
			} catch (Exception e) {
				//TODO: Add here log info!
				new ProjectSimulationSuite( project , new LinkedList<SimulationExperiment>() );		
			}
		} else {
			new ProjectSimulationSuite( project , new LinkedList<SimulationExperiment>() );		
		}
	}
		
	def writeSimuluationExperiment( SimulationExperiment e , PrintWriter writer ) {
			writer.println(e.name);
			writer.println(e.resource.projectRelativePath);
			writer.println(e.system);
			writer.println(e.replications);
			writer.println(e.simulationTime);
			writer.println(e.samplings);
			for (MeasureData m : e.measures) {
				MeasureData.writeTo(writer,m);
			}
			writer.println();		
	}
	
	def readSimulationExperimentFrom( IProject project , BufferedReader reader ) {
		var toReturn = new LinkedList<SimulationExperiment>();
		
		var name = reader.readLine(); 
		while ((name != null)&&(!name.isEmpty())) {
			var sourceModel = reader.readLine();
			var system = reader.readLine();
			var replications = Integer.parseInt(reader.readLine());
			var simulationTime = Double.parseDouble(reader.readLine());
			var samplings = Integer.parseInt(reader.readLine());
			var measures = new LinkedList<MeasureData>();
			var measure = MeasureData.parseMeasure(reader);
			while (measure != null) {
				measures.add(measure);
				measure = MeasureData.parseMeasure(reader);
			}
			var resource = project.getFile(sourceModel)
			if (resource.exists) {
				toReturn.add(new SimulationExperiment(name, resource, project.loadCarmaModel(resource), system, replications, simulationTime, samplings, measures));
			} else {
				toReturn.add(new SimulationExperiment(name, resource, null, system, replications, simulationTime, samplings, measures));
			}
			name = reader.readLine();
		}
		
		toReturn
	}
	
	def getWorkspaceSimulationSuite() {
		new WorkspaceSimulationSuite( newLinkedList( getCarmaProjects().map[it.loadProjectSimulationSuite] ) )
		
	}
	
	def saveWorkspaceSimulationSuite( WorkspaceSimulationSuite suite ) {
		suite.projectSuites.forEach[ it.saveProjectSimulationSuite ]
	}
	
	def registerChangeListener( IResourceChangeListener listener ) {
		ResourcesPlugin.workspace.addResourceChangeListener( listener );
		
	}
	
}