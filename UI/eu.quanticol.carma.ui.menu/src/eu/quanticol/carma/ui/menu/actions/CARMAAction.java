//package eu.quanticol.carma.ui.menu.actions;
//import java.lang.reflect.InvocationTargetException;
//import java.lang.reflect.Method;
//import java.net.MalformedURLException;
//import java.net.URL;
//import java.net.URLClassLoader;
//
//import org.eclipse.core.resources.IProject;
//import org.eclipse.core.resources.IWorkspace;
//import org.eclipse.core.resources.IWorkspaceRoot;
//import org.eclipse.core.resources.IncrementalProjectBuilder;
//import org.eclipse.core.resources.ResourcesPlugin;
//import org.eclipse.core.runtime.CoreException;
//import org.eclipse.core.runtime.IPath;
//import org.eclipse.core.runtime.IProgressMonitor;
//import org.eclipse.core.runtime.NullProgressMonitor;
//import org.eclipse.core.runtime.Path;
//import org.eclipse.jdt.core.JavaCore;
//import org.eclipse.jdt.launching.JavaRuntime;
//import org.eclipse.jface.action.IAction;
//import org.eclipse.jface.viewers.ISelection;
//import org.eclipse.ui.IWorkbenchWindow;
//import org.eclipse.ui.IWorkbenchWindowActionDelegate;
//import org.eclipse.jface.dialogs.MessageDialog;

///**
// * Our sample action implements workbench action delegate.
// * The action proxy will be created by the workbench and
// * shown in the UI. When the user tries to use the action,
// * this delegate will be created and execution will be 
// * delegated to it.
// * @see IWorkbenchWindowActionDelegate
// */
//public class CARMAAction implements IWorkbenchWindowActionDelegate {
//	private IWorkbenchWindow window;
//	/**
//	 * The constructor.
//	 */
//	public CARMAAction() {
//	}
//
//	/**
//	 * The action has been activated. The argument of the
//	 * method represents the 'real' action sitting
//	 * in the workbench UI.
//	 * @see IWorkbenchWindowActionDelegate#run
//	 */
//	public void run(IAction action) {
//		
//		IProject myProject = ResourcesPlugin.getWorkspace().getRoot().getProject("outputFolder");
//		ClassLoader parentClassLoader = myProject.getClass().getClassLoader();
//		try {
//			String[] classPathEntries = JavaRuntime.computeDefaultRuntimeClassPath(JavaCore.create(myProject));
//			 String entry = classPathEntries[0];
//			 IPath path = new Path(entry);
//			 URL url = path.toFile().toURI().toURL();
//			 URLClassLoader classLoader = new URLClassLoader(new URL[] { url } , parentClassLoader);
//			 
//			 Class<?> myClass = classLoader.loadClass("carma.CGT15.Simple");
//			 Object system = myClass.newInstance();
//			 Method myMethod = myClass.getMethod("main",new Class[]{});
//			 
//			 myClass = classLoader.loadClass("carma.CGT15.CGT15Definition");
//			 Object definition = myClass.newInstance();
//			 myClass = classLoader.loadClass("carma.CGT15.CGT15Factory");
//			 Object factory = myClass.newInstance();
//			 
//			 myMethod.invoke(system,new Object[]{});
//			 
//		} catch (CoreException | MalformedURLException | ClassNotFoundException | InstantiationException | IllegalAccessException | NoSuchMethodException | SecurityException | IllegalArgumentException | InvocationTargetException e) {
//			e.printStackTrace();
//		}
		
		
//		IProject myProject = null;
//		IProgressMonitor myProgressMonitor = new NullProgressMonitor();
//		
//		IWorkspace workspace = ResourcesPlugin.getWorkspace();
//		IWorkspaceRoot root = workspace.getRoot();
//		myProject = root.getProject("outputFolder");
//		
//		
//		try {
//			myProject.build(IncrementalProjectBuilder.INCREMENTAL_BUILD, myProgressMonitor);
//		} catch (CoreException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
		
//		try {
//			 ClassLoader myClassLoader = ClassLoader.getSystemClassLoader();
//			 
//			 String classNameToBeLoaded = "carma.CGT15.Simple";
//			 
//			 Class<?> myClass = myClassLoader.loadClass(classNameToBeLoaded);
//			 
//			 Object whatInstance = myClass.newInstance();
//			 
//			 String methodParameter = "a quick brown fox";
//			 
//			 Method myMethod = myClass.getMethod("demoMethod",new Class[] { String.class });
//			 
//			 String returnValue = (String) myMethod.invoke(whatInstance,
//	                    new Object[] { methodParameter });
//			 
//			 System.out.println("The value returned from the method is:"
//	                    + returnValue);
//			 
//		}catch (Exception e) {
//            e.printStackTrace();
//        } 
		
//		MessageDialog.openInformation(
//			window.getShell(),
//			"Ui",
//			"Hello, Eclipse world");
//	}
//
//	/**
//	 * Selection in the workbench has been changed. We 
//	 * can change the state of the 'real' action here
//	 * if we want, but this can only happen after 
//	 * the delegate has been created.
//	 * @see IWorkbenchWindowActionDelegate#selectionChanged
//	 */
//	public void selectionChanged(IAction action, ISelection selection) {
//	}
//
//	/**
//	 * We can use this method to dispose of any system
//	 * resources we previously allocated.
//	 * @see IWorkbenchWindowActionDelegate#dispose
//	 */
//	public void dispose() {
//	}
//
//	/**
//	 * We will cache window object in order to
//	 * be able to provide parent shell for the message dialog.
//	 * @see IWorkbenchWindowActionDelegate#init
//	 */
//	public void init(IWorkbenchWindow window) {
//		this.window = window;
//	}
//}