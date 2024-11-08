package fr.qln.sortspotifyscala.models.errors

object DomainErrors:

  trait DomainError extends Throwable
  
  trait AuthenticationError extends DomainError
  case object InvalidReturnedState extends AuthenticationError
  case object AuthenticationValuesNotFound extends AuthenticationError
  case object AuthenticationValuesAlreadyExists extends AuthenticationError
  
  
  trait ConfigError extends DomainError
   
  trait SessionError extends DomainError
  case object SessionNotFoundInStore extends SessionError
  case object SessionExpired extends SessionError
  case object SessionAlreadyExists extends SessionError
  case object SessionNotFoundInRequest extends SessionError
  case object TokensNotDefined extends SessionError
  case object SessionNotAuthenticated extends SessionError
  
  
  trait SortError extends DomainError
  case object FilterTargetsShouldHaveOneOfEachTargets extends SortError
  case object FilterTokenDoesntExist extends SortError
  case object SortConfigBodyHasWrongFormat extends SortError