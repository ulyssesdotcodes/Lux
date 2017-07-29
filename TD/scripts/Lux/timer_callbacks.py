def onSegmentExit(timerOp, segment, interrupt):
  timerOp.par.play = 0
  mod('server').enableVotes(0)
  return

