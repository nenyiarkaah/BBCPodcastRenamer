package domains

import scala.util.matching.Regex

case class RenamePattern(pattern: Regex, replacement: String)
