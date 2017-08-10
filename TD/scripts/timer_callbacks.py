def onSegmentExit(timerOp, segment, interrupt):
  timerOp.par.play = 0
  mod('server').enableVotes(0)
  maxVote = op("maxVote")[0].eval()
  op("voteResults").appendRow([maxVote])
  op("voteValues").appendRow([op(me.fetch('votesList', '--')[1:])[segment - 1, int(maxVote)]])
  op("resetVotes").par.value0.pulse(1, frames=2)
  return

