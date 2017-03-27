/**
 * 
 */
package eu.quanticol.carma.core.ui;

import java.net.URL;

import org.apache.log4j.Logger;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

import eu.quanticol.carma.core.ui.internal.CARMAActivator;

/**
 * @author loreti
 *
 */
public class ExtendedCARMAActivator extends CARMAActivator {
	
	// The plug-in ID
	public static final String PLUGIN_ID = "eu.quanticol.carma.core.ui"; //$NON-NLS-1$
	
	private static final Logger logger = Logger.getLogger(ExtendedCARMAActivator.class);
	
	private static ExtendedCARMAActivator INSTANCE;

	public static final String IMG_ADD_EXERIMENT_ID = "eu.quanticol.carma.ui.images.add_experiment"; //$NON-NLS-1$
	public static final String IMG_ADD_EXERIMENT_PATH  = "icons/add_obj.gif"; //$NON-NLS-1$

	public static final String IMG_COPY_EXERIMENT_ID = "eu.quanticol.carma.ui.images.copy_experiment"; //$NON-NLS-1$
	public static final String IMG_COPY_EXERIMENT_PATH  = "icons/copy.gif"; //$NON-NLS-1$
	
	public static final String IMG_DELETE_EXERIMENT_ID = "eu.quanticol.carma.ui.images.delete_experiment"; //$NON-NLS-1$
	public static final String IMG_DELETE_EXERIMENT_PATH  = "icons/delete_obj.gif"; //$NON-NLS-1$
	
	public static final String IMG_EDIT_EXERIMENT_ID = "eu.quanticol.carma.ui.images.edit_experiment"; //$NON-NLS-1$
	public static final String IMG_EDIT_EXERIMENT_PATH  = "icons/write_obj.gif"; //$NON-NLS-1$

	public static final String IMG_EXPORT_EXERIMENT_ID = "eu.quanticol.carma.ui.images.export_experiment"; //$NON-NLS-1$
	public static final String IMG_EXPORT_EXERIMENT_PATH  = "icons/datasheet.gif"; //$NON-NLS-1$

	public static final String IMG_PLOT_EXERIMENT_ID = "eu.quanticol.carma.ui.images.plot_experiment"; //$NON-NLS-1$
	public static final String IMG_PLOT_EXERIMENT_PATH  = "icons/chart_line.gif"; //$NON-NLS-1$

	public static final String IMG_RUN_EXERIMENT_ID = "eu.quanticol.carma.ui.images.run_experiment"; //$NON-NLS-1$
	public static final String IMG_RUN_EXERIMENT_PATH  = "icons/lrun_obj.gif"; //$NON-NLS-1$

	public static final String IMG_SAVE_SUITE_ID = "eu.quanticol.carma.ui.images.save_suite"; //$NON-NLS-1$
	public static final String IMG_SAVE_SUITE_PATH  = "icons/save_edit.gif"; //$NON-NLS-1$

	public static final String IMG_SAVEALL_SUITE_ID = "eu.quanticol.carma.ui.images.saveall_suite"; //$NON-NLS-1$
	public static final String IMG_SAVEALL_SUITE_PATH  = "icons/saveall_edit.gif"; //$NON-NLS-1$

	public static final String IMG_PRJ_SUITE_ID = "eu.quanticol.carma.ui.images.prj_suite"; //$NON-NLS-1$
	public static final String IMG_PRJ_SUITE_PATH  = "icons/prj_obj.gif"; //$NON-NLS-1$

	public static final String IMG_EXPERIMENT_ELEMENT_ID = "eu.quanticol.carma.ui.images.experiment"; //$NON-NLS-1$
	public static final String IMG_EXPERIMENT_ELEMENT_PATH  = "icons/composite_obj.gif"; //$NON-NLS-1$

	public static final String IMG_EXPERIMENT_ELEMENT_FAIL_ID = "eu.quanticol.carma.ui.images.experiment_error"; //$NON-NLS-1$
	public static final String IMG_EXPERIMENT_ELEMENT_FAIL_PATH  = "icons/signed_no.gif"; //$NON-NLS-1$

	public static final String IMG_SHOW_EXPERIMENT_DATA_ID = "eu.quanticol.carma.ui.images.show_experiment_data"; //$NON-NLS-1$
	public static final String IMG_SHOW_EXPERIMENT_DATA_PATH  = "icons/watchlist_view.gif"; //$NON-NLS-1$

	public static final String IMG_REFRESH_ID = "eu.quanticol.carma.ui.images.refresh"; //$NON-NLS-1$
	public static final String IMG_REFRESH_PATH  = "icons/refresh.gif"; //$NON-NLS-1$
	
	private String[] IMAGE_IDS = new String[] {
		IMG_ADD_EXERIMENT_ID ,
		IMG_COPY_EXERIMENT_ID ,
		IMG_DELETE_EXERIMENT_ID	,
		IMG_EDIT_EXERIMENT_ID ,
		IMG_EXPORT_EXERIMENT_ID ,
		IMG_PLOT_EXERIMENT_ID ,
		IMG_RUN_EXERIMENT_ID ,
		IMG_SAVE_SUITE_ID ,
		IMG_SAVEALL_SUITE_ID ,
		IMG_PRJ_SUITE_ID ,
		IMG_EXPERIMENT_ELEMENT_ID ,
		IMG_EXPERIMENT_ELEMENT_FAIL_ID ,
		IMG_SHOW_EXPERIMENT_DATA_ID,
		IMG_REFRESH_ID
	};
	
	private String[] IMAGE_PATHS = new String[] {
		IMG_ADD_EXERIMENT_PATH ,
		IMG_COPY_EXERIMENT_PATH ,
		IMG_DELETE_EXERIMENT_PATH ,
		IMG_EDIT_EXERIMENT_PATH ,
		IMG_EXPORT_EXERIMENT_PATH ,
		IMG_PLOT_EXERIMENT_PATH ,
		IMG_RUN_EXERIMENT_PATH ,
		IMG_SAVE_SUITE_PATH ,
		IMG_SAVEALL_SUITE_PATH ,
		IMG_PRJ_SUITE_PATH ,
		IMG_EXPERIMENT_ELEMENT_PATH ,
		IMG_EXPERIMENT_ELEMENT_FAIL_PATH ,
		IMG_SHOW_EXPERIMENT_DATA_PATH,
		IMG_REFRESH_PATH
	};

	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		INSTANCE = this;
	}
	
	@Override
	public void stop(BundleContext context) throws Exception {
		INSTANCE = null;
		super.stop(context);
	}
	
	public static ExtendedCARMAActivator getInstance() {
		return INSTANCE;
	}

	@Override
	protected void initializeImageRegistry(ImageRegistry reg) {
        Bundle bundle = Platform.getBundle(PLUGIN_ID);
        
        for (int i=0 ; i<IMAGE_IDS.length ; i++) {
            IPath path = new Path(IMAGE_PATHS[i]);
            URL url = FileLocator.find(bundle, path, null);
            ImageDescriptor desc = ImageDescriptor.createFromURL(url);
            reg.put(IMAGE_IDS[i], desc);
        }
             
    }
	
	

}
