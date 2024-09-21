package fr.qln.sortspotifyscala.models


object DomainErrors:

  trait DomainError extends Throwable
  
  trait AuthenticationError extends DomainError
  case object InvalidReturnedState extends AuthenticationError
  
  trait ConfigServiceError extends DomainError
   
