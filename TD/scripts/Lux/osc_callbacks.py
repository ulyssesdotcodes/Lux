# me - this DAT
# 
# dat - the DAT that received a message
# rowIndex - the row number the message was placed into
# message - an ascii representation of the data
#           Unprintable characters and unicode characters will
#           not be preserved. Use the 'bytes' parameter to get
#           the raw bytes that were sent.
# bytes - a byte array of the message.
# timeStamp - the arrival time component the OSC message
# address - the address component of the OSC message
# args - a list of values contained within the OSC message
# peer - a Peer object describing the originating message
#   peer.close()    #close the connection
#   peer.owner  #the operator to whom the peer belongs
#   peer.address    #network address associated with the peer
#   peer.port       #network port associated with the peer
#

import scripts

def receiveOSC(dat, rowIndex, message, bytes, timeStamp, address, args, peer):
  addr = "/project1/lambda" + address
  if args[0] == "create":
    clazz = scripts.getClass(args[1], 'none')
    if clazz == "none":
      print("Couldn't find " + args[1])
      return

    if op(addr) != None:
      op(addr).destroy()

    name = addr[(addr.rfind('/') + 1):]
    par = addr[:(addr.rfind('/'))]
    op(par).create(clazz[0], name)

    # TODO: Figure out a clean way to not special case these
    if clazz[1] == 'out' and clazz[2] == 'SOP':
      op(addr).render = True
      op(addr).display = True

    if clazz[1] == 'geo':
      op(addr + "/torus1").destroy()

  elif args[0] == "connect" and op(addr):
    op("/project1/lambda" + args[2]).outputConnectors[0].connect(op(addr).inputConnectors[args[1]])

  elif args[0] == "parameter" and op(addr):
    pars = op(addr).pars(args[1])
    if len(pars) > 0:
      if isfloat(args[2]):
        pars[0].val = args[2]
      else:
        pars[0].expr = args[2]

  elif args[0] == "command" and op(addr):
    if args[1] == "pulse":
      pars = op(addr).pars(args[2])
      if len(pars) > 0:
        if isfloat(args[3]):
          pars[0].pulse(float(args[3]), frames=float(args[4]))
        else:
          pars[0].pulse(args[3])
    elif args[1] == "store":
      op(addr).store(args[2], args[3])

  elif args[0] == "text" and op(addr):
    op(addr).text = args[1]

  return

def isfloat(value):
  try:
    float(value)
    return True
  except ValueError:
    return False
