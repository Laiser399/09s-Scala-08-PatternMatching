package org.mai.stackoverflow

object Main extends App {

  val loader = new DataLoader {
    override def basePath: String = "stackoverflow"
  }

  val entities = loader.loadData()

  val commentsFromEntities = Logic.getComments(entities)
  commentsFromEntities take(10) foreach println

  val (users, posts, comments, votes, badges, tags) = Logic.splitEntities(entities)

  val reachPosts = Logic.enrichPosts(posts, users, tags);
  println
  reachPosts take(10) foreach println

  val reachComments = Logic.enrichComments(comments, posts, users)
  println
  reachComments take(10) foreach println

  val userLinks = Logic.findAllUserLinks(users)
  println
  userLinks
    .take(10)
    .foreach(x => println(s"${x._1.id} -> ${x._2.toList}"))

  val topUsersByBadge = Logic.findTopUsersByBadge(users, badges, "Student", 100)
  println
  topUsersByBadge
    .take(10)
    .foreach(u => println(s"Id=${u.id}, Name=${u.displayName}, Rep=${u.reputation}"))
}
