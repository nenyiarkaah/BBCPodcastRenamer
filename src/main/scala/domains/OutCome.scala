package domains

object OutCome extends Enumeration {
  type OutCome = Value
  val TagsRead, TagsRenamed, RootDestinationMapped, DestinationMapped, FileRenamed, ReTagged, UnTagged, MoveSuccessful, MoveUnSuccessful = Value
}
