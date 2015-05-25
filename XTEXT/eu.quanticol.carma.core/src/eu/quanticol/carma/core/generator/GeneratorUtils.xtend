package eu.quanticol.carma.core.generator

import eu.quanticol.carma.core.carma.RecordDeclaration
import eu.quanticol.carma.core.carma.RecordReferenceMy
import eu.quanticol.carma.core.carma.RecordReferencePure
import eu.quanticol.carma.core.carma.RecordReferenceReciever
import eu.quanticol.carma.core.carma.RecordReferenceSender
import eu.quanticol.carma.core.carma.RecordReferenceThis
import eu.quanticol.carma.core.carma.VariableDeclarationEnum
import eu.quanticol.carma.core.carma.VariableDeclarationRecord
import eu.quanticol.carma.core.carma.VariableReference
import eu.quanticol.carma.core.carma.VariableReferenceMy
import eu.quanticol.carma.core.carma.VariableReferencePure
import eu.quanticol.carma.core.carma.VariableReferenceReciever
import eu.quanticol.carma.core.carma.VariableReferenceSender
import eu.quanticol.carma.core.carma.VariableReferenceThis
import eu.quanticol.carma.core.typing.TypeProvider
import com.google.inject.Inject
import eu.quanticol.carma.core.utils.LabelUtil
import eu.quanticol.carma.core.utils.Util

import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.core.carma.VariableDeclaration

class GeneratorUtils {
	
	@Inject extension TypeProvider
	@Inject extension LabelUtil
	@Inject extension Util
	
	def String declareVariable(VariableReference vr){
		switch(vr){
			VariableReferencePure		: {vr.getStore}
			VariableReferenceMy			: {vr.getStore}
			VariableReferenceThis		: {vr.getStore}
			VariableReferenceReciever	: {vr.getReceiver}
			VariableReferenceSender		: {vr.getSender}
			RecordReferencePure			: {vr.getStore}
			RecordReferenceMy			: {vr.getStore}
			RecordReferenceThis			: {vr.getStore}
			RecordReferenceReciever		: {vr.getReceiver}
			RecordReferenceSender		: {vr.getSender}
		}
	}
	
	
	def String getStore(VariableReference vr){
		'''
		«var vd = vr.getVariableDeclaration»
		«switch(vd){
		VariableDeclarationEnum:	'''«vd.convertPrimitiveType» «vd.name.label» = store.get("«vd.name.label»" , «vd.convertType».class );'''
		VariableDeclarationRecord:	{
						var rds = vd.eAllOfType(RecordDeclaration)
						'''
						«FOR rd : rds»
						«vd.convertPrimitiveType» «vd.name.label»_«rd.name.label» = store.get("«vd.name.label»_«rd.name.label»" , «vd.convertType».class );
						«ENDFOR»
						'''
					}
				}»
		'''
	}
	
	def String getSender(VariableReference vr){
		'''
		«var vd = vr.getVariableDeclaration»
		«switch(vd){
		VariableDeclarationEnum:	'''«vd.convertPrimitiveType» «vd.name.label» = sender.get("«vd.name.label»" , «vd.convertType».class );'''
		VariableDeclarationRecord:	{
						var rds = vd.eAllOfType(RecordDeclaration)
						'''
						«FOR rd : rds»
						«vd.convertPrimitiveType» «vd.name.label»_«rd.name.label» = sender.get("«vd.name.label»_«rd.name.label»" , «vd.convertType».class );
						«ENDFOR»
						'''
					}
				}»
		'''
	}
	
	def String getReceiver(VariableReference vr){
		'''
		«var vd = vr.getVariableDeclaration»
		«switch(vd){
		VariableDeclarationEnum:	'''«vd.convertPrimitiveType» «vd.name.label» = receiver.get("«vd.name.label»" , «vd.convertType».class );'''
		VariableDeclarationRecord:	{
						var rds = vd.eAllOfType(RecordDeclaration)
						'''
						«FOR rd : rds»
						«vd.convertPrimitiveType» «vd.name.label»_«rd.name.label» = receiver.get("«vd.name.label»_«rd.name.label»" , «vd.convertType».class );
						«ENDFOR»
						'''
					}
				}»
		'''
	}
	
	def String getStore(VariableDeclaration vd){
		var output = ""
		switch(vd){
			VariableDeclarationEnum: {
				output = '''«vd.convertPrimitiveType» «vd.name.label» = store.get("«vd.name.label»" , «vd.convertType».class );'''+"\n"
					}
			VariableDeclarationRecord: {
				var rds = vd.eAllOfType(RecordDeclaration)
				for(rd : rds){
						output = output + '''«vd.convertPrimitiveType» «vd.name.label»_«rd.name.label» = store.get("«vd.name.label»_«rd.name.label»" , «vd.convertType».class );'''+"\n"
				}
			}
		}
		return output
	}
	
	def String setStore(VariableDeclaration vd, String expression){
		var output = ""
		switch(vd){
			VariableDeclarationEnum: {
						output = '''store.set("«vd.name.label»",«expression»);'''
					}
					VariableDeclarationRecord: {
						var rds = vd.eAllOfType(RecordDeclaration)
						for(rd : rds){
							output = output + '''store.set("«vd.name.label»_«rd.name.label»",«expression»);'''
						}
					}
				}
		return output
	}
}