/**
 * 
 */
package eu.quanticol.carma.ui.util;

import java.util.LinkedList;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.PlatformUI;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.XtextEditor;
import org.eclipse.xtext.util.concurrent.IUnitOfWork;

import eu.quanticol.carma.core.carma.Model;

/**
 * @author loreti
 *
 */
public class CarmaUiUtils {
	
	public static LinkedList<Model> getActiveModels( ) {
		IEditorReference[] references = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getEditorReferences();
		
		
		LinkedList<Model> models = new LinkedList<>();
		for (IEditorReference iEditorReference : references) {
			if (iEditorReference.getEditor(false) instanceof XtextEditor) {
                
                EList<EObject> values = ((XtextEditor) iEditorReference.getEditor(false)).getDocument().readOnly(new IUnitOfWork<EList<EObject>, XtextResource>(){

                        @Override
                        public EList<EObject> exec(XtextResource state) throws Exception {
                                if (state.getErrors().size()>0) {
                                        return null;
                                }
                                return state.getContents();
                        }
                        
                });             
                if ((values != null)&&(values.size() > 0)) {
                        if (values.get(0) instanceof Model) {
                        	models.add((Model) values.get(0));
                        }
                }
           }
		}
		return models;
		
		
	}
	
	

}
