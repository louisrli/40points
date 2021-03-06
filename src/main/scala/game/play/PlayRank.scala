package com.louis.fortypoints.game.play

import com.louis.fortypoints.card._

sealed trait PlayRank
case class Single(card: Card) extends PlayRank
case object InvalidPlay extends PlayRank  // Defer error handling to clients
// TODO(louisli) Food for thought: what if we wanted to support arbitrary single, pair, etc. etc.?
