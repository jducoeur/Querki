package querki.system

import com.amazonaws.client.builder.AwsClientBuilder
import com.amazonaws.services.secretsmanager.AWSSecretsManagerClientBuilder
import com.amazonaws.services.secretsmanager.model.GetSecretValueRequest
import com.typesafe.config.ConfigFactory
import play.api.Configuration
import querki.util.QLog

object SecretsLoader {

  /**
   * Given the configuration of the system, load the secrets and merge them into that configuration.
   *
   * The secrets file is stored in AWS Secrets Manager, and is in HOCON format itself.
   *
   * If there is no name specified for the secrets file (which should only be true for the scenario tests),
   * this is a no-op.
   */
  def addToConfiguration(
    configWithEnv: Configuration,
    querkiEnv: QuerkiEnv
  ): Configuration = {
    configWithEnv.getOptional[String]("querki.aws.secretsName").map { secretName =>
      val secretsEndpoint = configWithEnv.getOptional[String]("querki.aws.secretsEndpoint")
      val region = configWithEnv.get[String]("querki.aws.region")
      // The name of the HOCON file containing the secrets, from Secrets Manager:
      val secretsManagerBase =
        AWSSecretsManagerClientBuilder
          .standard()
      val secretsManager =
        secretsEndpoint.map { endpoint =>
          // If there is a configured endpoint, we're in local development, and need to point to that:
          secretsManagerBase.withEndpointConfiguration(new AwsClientBuilder.EndpointConfiguration(
            endpoint,
            region
          ))
        }.getOrElse {
          secretsManagerBase.withRegion(region)
        }
          .build()
      val secretRequest = new GetSecretValueRequest()
      // Yes, horrible and mutable, but it's a Java API.
      // TODO: see if there is a decent Scala wrapper for the AWS API
      secretRequest.setSecretId(secretName)
      val secretResult = secretsManager.getSecretValue(secretRequest)
      val hoconStr = secretResult.getSecretString()
      val secretsConfig = ConfigFactory.parseString(hoconStr)
      val secretsConfiguration = Configuration(secretsConfig)

      val fullConfig = configWithEnv ++ secretsConfiguration

      fullConfig
    }.getOrElse {
      // There is no secretsName. This should only be true in the scenario-test case:
      if (querkiEnv == QuerkiEnv.Scenario)
        configWithEnv
      else
        throw new Exception(s"No secretsName found, and the environment is not Scenario!")
    }
  }
}
