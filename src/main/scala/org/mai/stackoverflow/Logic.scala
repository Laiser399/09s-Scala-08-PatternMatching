package org.mai.stackoverflow

object Logic {

  //obtain all comments from entities
  def getComments(entities: Seq[Entity]): Seq[Comment] = {
    entities
      .collect {
        case e: Comment => e
      }
  }

  // 49 ms
  //split entities by type
  def splitEntities(entities: Seq[Entity]): (Seq[User], Seq[Post], Seq[Comment], Seq[Vote], Seq[Badge], Seq[Tag]) = {
    val grouped = entities.groupBy(_.getClass)

    val users = grouped.getOrElse(classOf[User], Seq()).map(_.asInstanceOf[User])
    val posts = grouped.getOrElse(classOf[Post], Seq()).map(_.asInstanceOf[Post])
    val comments = grouped.getOrElse(classOf[Comment], Seq()).map(_.asInstanceOf[Comment])
    val votes = grouped.getOrElse(classOf[Vote], Seq()).map(_.asInstanceOf[Vote])
    val badges = grouped.getOrElse(classOf[Badge], Seq()).map(_.asInstanceOf[Badge])
    val tags = grouped.getOrElse(classOf[Tag], Seq()).map(_.asInstanceOf[Tag])

    (users, posts, comments, votes, badges, tags)
  }

  // 4729 ms
  def splitEntities2(entities: Seq[Entity]): (Seq[User], Seq[Post], Seq[Comment], Seq[Vote], Seq[Badge], Seq[Tag]) = {
    val initAcc = Tuple6[Seq[User], Seq[Post], Seq[Comment], Seq[Vote], Seq[Badge], Seq[Tag]](
      Seq[User](), Seq[Post](), Seq[Comment](), Seq[Vote](), Seq[Badge](), Seq[Tag]())

    entities
      .foldLeft(initAcc)((acc, entity) => {
        entity match {
          case u: User => acc.copy(_1 = acc._1 :+ u)
          case p: Post => acc.copy(_2 = acc._2 :+ p)
          case c: Comment => acc.copy(_3 = acc._3 :+ c)
          case v: Vote => acc.copy(_4 = acc._4 :+ v)
          case b: Badge => acc.copy(_5 = acc._5 :+ b)
          case t: Tag => acc.copy(_6 = acc._6 :+ t)
        }
      })
  }

  //populate fields owner, lastEditor, tags with particular users from Seq[Post] and tags from Seq[Tag]
  def enrichPosts(posts: Seq[Post], users: Seq[User], tags: Seq[Tag]): Seq[EnrichedPost] = {
    val usersById = users.map(u => u.id -> u).toMap
    val tagsByName = tags.map(t => t.tagName -> t).toMap

    posts.collect {
      case post if
        usersById.contains(post.ownerUserId)
          && post.lastEditorUserId.forall(usersById.contains)
          && post.tags.forall(tagsByName.contains) =>
        EnrichedPost(post, usersById(post.ownerUserId), post.lastEditorUserId.map(usersById), post.tags.map(tagsByName))
    }
  }

  //populate fields post and owner with particular post from Seq[Post] and user from Seq[User]
  def enrichComments(comments: Seq[Comment], posts: Seq[Post], users: Seq[User]): Seq[EnrichedComment] = {
    val postsById = posts.map(p => p.id -> p).toMap
    val usersById = users.map(u => u.id -> u).toMap

    comments.collect {
      case comment if
        postsById.contains(comment.postId)
          && usersById.contains(comment.userId) =>
        EnrichedComment(comment, postsById(comment.postId), usersById(comment.userId))
    }
  }

  //find all links (like http://example.com/examplePage) in aboutMe field
  def findAllUserLinks(users: Seq[User]): Seq[(User, Seq[String])] = {
    Seq[(User, Seq[String])]()
  }

  //find all users with the reputation bigger then reputationLImit with particular badge
  def findTopUsersByBadge(users: Seq[User], basges: Seq[Badge], badgeName: String, reputationLimit: Int): Seq[User] = {
    Seq()
  }
}

case class EnrichedPost(
                         post: Post,
                         owner: User,
                         lastEditor: Option[User],
                         tags: Seq[Tag]
                       ) {
  override lazy val toString: String =
    s"EnrichedPost(PostId=${post.id}, OwnerName=${owner.displayName}, " +
      s"LastEditorName=${lastEditor.map(_.displayName)}, Tags=${tags.map(_.id)})"
}

case class EnrichedComment(
                            comment: Comment,
                            post: Post,
                            owner: User
                          ) {
  override lazy val toString: String = s"EnrichedComment(CommentId=${comment.id}, " +
    s"PostTitle=${post.title}, OwnerName=${owner.displayName})"
}
