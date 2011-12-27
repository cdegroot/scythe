package gen.util

/**
 * Helper class to make sure we emit readable indentation.
 */
case class Indenter(sb: StringBuilder = new StringBuilder, level: Int = 0) {
  def more = Indenter(sb, level + 2)
  def less = Indenter(sb, level - 2)
  def append(s: Any): Indenter = { sb.append(s); this }
  def indent(s: Any): Indenter = { sb.append(" " * level).append(s); this }
  def newline: Indenter = { sb.append("\n"); this }

  def apply(f: (Indenter => Unit)) =  { f(this); this }

  override def toString = sb.toString
}

