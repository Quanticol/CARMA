package eu.quanticol.carma.core.generator;

import static com.google.common.collect.Sets.newHashSet;

import java.util.Set;

import org.eclipse.xtext.generator.IFileSystemAccess;
import org.eclipse.xtext.generator.IOutputConfigurationProvider;
import org.eclipse.xtext.generator.OutputConfiguration;

public class MyOutputConfigurationProvider implements
    IOutputConfigurationProvider {

	/**
	 * @return a set of {@link OutputConfiguration} available for the generator
	 */
	public Set<OutputConfiguration> getOutputConfigurations() {
	    OutputConfiguration defaultOutput = new OutputConfiguration(IFileSystemAccess.DEFAULT_OUTPUT);
	    defaultOutput.setDescription("Output Folder");
	    defaultOutput.setOutputDirectory("./src-gen");
	    defaultOutput.setOverrideExistingResources(true);
	    defaultOutput.setCreateOutputDirectory(true);
	    defaultOutput.setCleanUpDerivedResources(true);
	    defaultOutput.setSetDerivedProperty(true);
	    return newHashSet(defaultOutput);
	}
}