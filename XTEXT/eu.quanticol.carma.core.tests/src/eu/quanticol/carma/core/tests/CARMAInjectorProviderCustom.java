package eu.quanticol.carma.core.tests;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.util.JavaVersion;
import org.eclipse.xtext.xbase.compiler.GeneratorConfig;
import org.eclipse.xtext.xbase.compiler.IGeneratorConfigProvider;

import com.google.inject.Binder;
import com.google.inject.Guice;
import com.google.inject.Injector;

import eu.quanticol.carma.core.CARMAInjectorProvider;
import eu.quanticol.carma.core.CARMARuntimeModule;
import eu.quanticol.carma.core.CARMAStandaloneSetup;

public class CARMAInjectorProviderCustom extends CARMAInjectorProvider {

//    @Override
//    protected Injector internalCreateInjector() {
//        return new CARMAStandaloneSetup() {
//            @Override
//            public Injector createInjector() {
//                return Guice.createInjector(new CARMARuntimeModule() {
//                    // this is required only by the CompilationTestHelper since
//                    // Xtext 2.7
//                    @SuppressWarnings("unused")
//                    public Class<? extends org.eclipse.xtend.lib.macro.file.MutableFileSystemSupport> bindMutableFileSystemSupport() {
//                        return org.eclipse.xtext.xbase.file.JavaIOFileSystemSupport.class;
//                    }
//
//                    // this is required only by the CompilationTestHelper since
//                    // Xtext 2.7
//                    @SuppressWarnings("unused")
//                    public Class<? extends com.google.inject.Provider<org.eclipse.xtext.xbase.file.WorkspaceConfig>> provideWorkspaceConfig() {
//                        return org.eclipse.xtext.xbase.file.RuntimeWorkspaceConfigProvider.class;
//                    }
//                });
//            }
//        }.createInjectorAndDoEMFRegistration();
//    }
	
	@Override
	protected CARMARuntimeModule createRuntimeModule() {
		// make it work also with Maven/Tycho and OSGI
		// see https://bugs.eclipse.org/bugs/show_bug.cgi?id=493672
		return new CARMARuntimeModule() {
			@Override
			public ClassLoader bindClassLoaderToInstance() {
				return CARMAInjectorProvider.class
						.getClassLoader();
			}
			
        @SuppressWarnings("restriction")
		@Override
          public void configure(Binder binder) {
        	  super.configure(binder);
        	  binder.bind(IGeneratorConfigProvider.class).to(MyGeneratorConfigProvider.class);
          }
		};
	}
	
}