package eu.quanticol.carma.core.tests;

import com.google.inject.Guice;
import com.google.inject.Injector;

import eu.quanticol.carma.core.CARMAInjectorProvider;
import eu.quanticol.carma.core.CARMARuntimeModule;
import eu.quanticol.carma.core.CARMAStandaloneSetup;

public class CARMAInjectorProviderCustom extends CARMAInjectorProvider {

    @Override
    protected Injector internalCreateInjector() {
        return new CARMAStandaloneSetup() {
            @Override
            public Injector createInjector() {
                return Guice.createInjector(new CARMARuntimeModule() {
                    // this is required only by the CompilationTestHelper since
                    // Xtext 2.7
                    @SuppressWarnings("unused")
                    public Class<? extends org.eclipse.xtend.lib.macro.file.MutableFileSystemSupport> bindMutableFileSystemSupport() {
                        return org.eclipse.xtext.xbase.file.JavaIOFileSystemSupport.class;
                    }

                    // this is required only by the CompilationTestHelper since
                    // Xtext 2.7
                    @SuppressWarnings("unused")
                    public Class<? extends com.google.inject.Provider<org.eclipse.xtext.xbase.file.WorkspaceConfig>> provideWorkspaceConfig() {
                        return org.eclipse.xtext.xbase.file.RuntimeWorkspaceConfigProvider.class;
                    }
                });
            }
        }.createInjectorAndDoEMFRegistration();
    }
}