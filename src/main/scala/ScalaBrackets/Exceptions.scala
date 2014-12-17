package ScalaBrackets

/**
 * Created by Matthew on 11/24/2014.
 */
class SeedLimitExceededError extends RuntimeException("cannot seed more players than seed order list.")
class GeneratorException(reason: String) extends RuntimeException(reason)
class BracketException(reason: String) extends RuntimeException(reason)