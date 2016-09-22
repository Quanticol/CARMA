package eu.quanticol.carma.core.generator.ms.environment

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.ProbabilityBlock
import eu.quanticol.carma.core.generator.ms.expression.ExpressionHandler
import eu.quanticol.carma.core.utils.Util
import eu.quanticol.carma.core.carma.Probability
import eu.quanticol.carma.core.carma.Expression
import eu.quanticol.carma.core.carma.RateBlock
import eu.quanticol.carma.core.carma.Rate
import eu.quanticol.carma.core.carma.UpdateBlock
import eu.quanticol.carma.core.carma.EnvironmentUpdate
import eu.quanticol.carma.core.generator.ms.collective.CollectiveHandler
import eu.quanticol.carma.core.generator.ms.attribute.AttributeHandler
import eu.quanticol.carma.core.utils.ReferenceContext
import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.core.carma.UpdateCommand
import eu.quanticol.carma.core.carma.UpdateAssignment
import eu.quanticol.carma.core.carma.UpdateCollectionAdd
import eu.quanticol.carma.core.generator.ms.function.FunctionHandler
import eu.quanticol.carma.core.carma.FunctionCommand
import eu.quanticol.carma.core.carma.AttributeDeclaration
import eu.quanticol.carma.core.generator.ms.system.SystemHandler
import eu.quanticol.carma.core.carma.WeightBlock
import eu.quanticol.carma.core.carma.Weight
import eu.quanticol.carma.core.typing.CarmaType

class EnvironmentHandler {
	
	@Inject extension ExpressionHandler
	@Inject extension Util
	@Inject extension SystemHandler
	@Inject extension AttributeHandler
	@Inject extension FunctionHandler
	
	def handleProbabilityBlock( ProbabilityBlock block ) {
		'''
		@Override
		public double broadcastProbability(
			final CarmaStore sender, 
			final CarmaStore receiver,
			int action) {
			final CarmaSystem system = this;
			final CarmaSystem sys = this;
			final CarmaStore global = this.global;
			«block.probabilities.probabilityBody(block.value)»
		}
		'''
	}
	
	def handleWeightBlock( WeightBlock block ) {
		'''
		@Override
		public double unicastProbability(
			final CarmaStore sender, 
			final CarmaStore receiver,
			int action) {
			final CarmaSystem system = this;
			final CarmaSystem sys = this;
			final CarmaStore global = this.global;
			«block.weights.weightBody(block.value)»
		}
		'''
	}	
	
	def handleRateBlock( RateBlock block ) {
		'''
		@Override
		public double broadcastRate(final CarmaStore sender, int action) {
			final CarmaSystem system = this;
			final CarmaSystem sys = this;
			final CarmaStore global = this.global;
			«block.rates.filter[ it.stub.isIsBroadcast ].rateBody( block.value )»
		}

		@Override
		public double unicastRate(final CarmaStore sender, int action) {
			final CarmaSystem system = this;
			final CarmaSystem sys = this;
			final CarmaStore global = this.global;
			«block.rates.filter[ !it.stub.isIsBroadcast ].rateBody( block.value )»
		}
		'''		
	}
	
	def handleUpdateBlock( UpdateBlock block ) {
		'''
		@Override
		public void broadcastUpdate( 
			final RandomGenerator random , 
			final CarmaStore sender , 
			final int action , 
			final Object value ) {
			final CarmaSystem system = this;
			final CarmaSystem sys = this;
			final CarmaStore global = this.global;
			final CarmaStore store = this.global;
			«block.updates.filter[ it.stub.isBroadcast ].updateBody»	
		}

		@Override
		public void unicastUpdate( 
			final RandomGenerator random , 
			final CarmaStore sender , 
			final CarmaStore receiver, 
			int action , 
			final Object value ) {
			final CarmaSystem system = this;
			final CarmaSystem sys = this;
			final CarmaStore global = this.global;
			final CarmaStore store = this.global;
			«block.updates.filter[ !it.stub.isBroadcast ].updateBody»	
		}		
		'''
	}
	
	/*
	def probabilityBody( Iterable<Probability> probabilities , Expression defaultValue ) {
		'''
		«FOR a:(probabilities.map[it.expression]+probabilities.filter[it.guard != null].map[it.guard.booleanExpression]).globalAttributes»
		«a.attributeTemporaryVariableDeclaration(ReferenceContext::GLOBAL,"global")»
		«ENDFOR»
		«FOR a:(probabilities.map[it.expression]+probabilities.filter[it.guard != null].map[it.guard.booleanExpression]).senderAttributes»
		«a.attributeTemporaryVariableDeclaration(ReferenceContext::SENDER,"sender")»
		«ENDFOR»
		«FOR a:(probabilities.map[it.expression]+probabilities.filter[it.guard != null].map[it.guard.booleanExpression]).receiverAttributes»
		«a.attributeTemporaryVariableDeclaration(ReferenceContext::RECEIVER,"receiver")»
		«ENDFOR»
		«FOR p:probabilities»
		if ((action==«p.stub.activity.name.actionName»)
			«IF p.guard != null»
			&&
			(«p.guard.booleanExpression.expressionToJava»)
			«ENDIF»			
			) {
				return «p.expression.expressionToJava»;				
		}
		«ENDFOR»
		«IF defaultValue != null»
		return «defaultValue.expressionToJava»;
		«ELSE»
		return 1.0;
		«ENDIF»			
		'''
	}
	 */
	
	def probabilityBody( Iterable<Probability> probabilities , FunctionCommand defaultValue ) {
		'''
		«FOR a:probabilities.map[it.expression].map[it.attributesInFunctionCommand( ReferenceContext::GLOBAL )].flatten.removeDuplicates »
		«a.attributeTemporaryVariableDeclaration(ReferenceContext::GLOBAL,"global")»
		«ENDFOR»
		«FOR a:probabilities.map[it.expression].map[it.attributesInFunctionCommand( ReferenceContext::SENDER )].flatten.removeDuplicates »
		«a.attributeTemporaryVariableDeclaration(ReferenceContext::SENDER,"sender")»
		«ENDFOR»
		«ReferenceContext::SENDER.locTemporaryVariableDeclaration("sender")»
		«FOR a:probabilities.map[it.expression].map[it.attributesInFunctionCommand( ReferenceContext::RECEIVER )].flatten.removeDuplicates »
		«a.attributeTemporaryVariableDeclaration(ReferenceContext::RECEIVER,"receiver")»
		«ENDFOR»
		«ReferenceContext::RECEIVER.locTemporaryVariableDeclaration("receiver")»
		«FOR p:probabilities»
		if ((action==«p.activity.name.actionName»)
			) «p.expression.functionBodyToJava»
		«ENDFOR»
		«IF defaultValue != null»
		«defaultValue.functionBodyToJava»
		«ELSE»
		return 1.0;
		«ENDIF»			
		'''
	}

	def weightBody( Iterable<Weight> weights, FunctionCommand defaultValue ) {
		'''
		«FOR a:weights.map[it.expression].map[it.attributesInFunctionCommand( ReferenceContext::GLOBAL )].flatten.removeDuplicates »
		«a.attributeTemporaryVariableDeclaration(ReferenceContext::GLOBAL,"global")»
		«ENDFOR»
		«FOR a:weights.map[it.expression].map[it.attributesInFunctionCommand( ReferenceContext::SENDER )].flatten.removeDuplicates »
		«a.attributeTemporaryVariableDeclaration(ReferenceContext::SENDER,"sender")»
		«ENDFOR»
		«ReferenceContext::SENDER.locTemporaryVariableDeclaration("sender")»
		«FOR a:weights.map[it.expression].map[it.attributesInFunctionCommand( ReferenceContext::RECEIVER )].flatten.removeDuplicates »
		«a.attributeTemporaryVariableDeclaration(ReferenceContext::RECEIVER,"receiver")»
		«ENDFOR»
		«ReferenceContext::RECEIVER.locTemporaryVariableDeclaration("receiver")»
		«FOR p:weights»
		if ((action==«p.activity.name.actionName»)
			) «p.expression.functionBodyToJava»
		«ENDFOR»
		«IF defaultValue != null»
		«defaultValue.functionBodyToJava»
		«ELSE»
		return 1.0;
		«ENDIF»			
		'''
	}
	
	def Iterable<AttributeDeclaration> removeDuplicates(Iterable<AttributeDeclaration> attributes) {
		val result = newHashSet()
		attributes.forEach[ a |
			if (!result.contains(result)) {
				result.add( a )
			}
		]
		result
	}

	def rateBody( Iterable<Rate> rates , FunctionCommand defaultValue ) {
		'''
		«FOR a:rates.map[it.expression].map[it.attributesInFunctionCommand( ReferenceContext::GLOBAL )].flatten.removeDuplicates »
		«a.attributeTemporaryVariableDeclaration(ReferenceContext::GLOBAL,"global")»
		«ENDFOR»
		«FOR a:rates.map[it.expression].map[it.attributesInFunctionCommand( ReferenceContext::SENDER )].flatten.removeDuplicates »
		«a.attributeTemporaryVariableDeclaration(ReferenceContext::SENDER,"sender")»
		«ENDFOR»
		«ReferenceContext::SENDER.locTemporaryVariableDeclaration("sender")»
		«FOR p:rates»
		if ((action==«p.stub.activity.name.actionName»)
			) «p.expression.functionBodyToJava»
		«ENDFOR»
		«IF defaultValue != null»
		«defaultValue.functionBodyToJava»
		«ELSE»
		return 1.0;
		«ENDIF»			

		'''
	}
	
	def updateBody( Iterable<EnvironmentUpdate> updates ) {
		'''
		«FOR a:updates.map[it.getAllContentsOfType(typeof(Expression))].flatten.referencedAttibutes»
		«a.attributeTemporaryVariableDeclaration(ReferenceContext::NONE,"global")»
		«ENDFOR»
		«FOR a:updates.map[it.getAllContentsOfType(typeof(Expression))].flatten.globalAttributes»
		«a.attributeTemporaryVariableDeclaration(ReferenceContext::GLOBAL,"global")»
		«ENDFOR»
		«FOR a:updates.map[it.getAllContentsOfType(typeof(Expression))].flatten.senderAttributes»
		«a.attributeTemporaryVariableDeclaration(ReferenceContext::SENDER,"sender")»
		«ENDFOR»
		«ReferenceContext::SENDER.locTemporaryVariableDeclaration("sender")»
		«FOR u:updates»
		if (action==«u.stub.activity.name.actionName») {
			«u.command.generateCollective»
			return ;				
		}
		«ENDFOR»
		'''
	}
	
	def getUpdateEnvironmentCommandCode(UpdateCommand command) {
		switch (command) {
			UpdateAssignment: '''global.set( "«command.reference.attributeName»" , «command.expression.expressionToJava» );'''
			UpdateCollectionAdd: '''«command.reference.attributeName.attributeName(ReferenceContext::MY)».add(«command.arg.expressionToJava»)'''
		}
	}
	
	
}