package special.ivy

import scala.annotation.Annotation

@ivy class clause extends Annotation

abstract class LockWithPublicKey(publicKey: PublicKey, value: Value) extends Contract {
  @clause def spend(sig: Signature) {
    verify { checkSig(publicKey, sig) }
    unlock(value)
  }
}

abstract class LockWithMultisig(
    pubKey1: PublicKey,
    pubKey2: PublicKey,
    pubKey3: PublicKey,
    value: Value
) extends Contract {
  @clause def spend (sig1: Signature, sig2: Signature) {
    verify { checkMultiSig(List(pubKey1, pubKey2, pubKey3), List(sig1 , sig2)) }
    unlock (value)
  }
}

abstract class LockWithPublicKeyHash(pubKeyHash: Sha256[PublicKey], value: Value) extends Contract {
  @clause def spend(pubKey: PublicKey, sig: Signature) {
    verify { sha256(pubKey) == pubKeyHash }
    verify { checkSig(pubKey, sig) }
    unlock (value)
  }
}

abstract class RevealPreimage(hash: Sha256[Bytes], value: Value) extends Contract {
  @clause def reveal(string: Bytes) {
    verify { sha256(string) == hash }
    unlock (value)
  }
}

abstract class RevealCollision(value: Value) extends Contract {
  @clause def reveal(string1: Bytes, string2: Bytes) {
    verify ( string1 != string2 )
    verify ( sha1(string1) == sha1(string2) )
    unlock ( value )
  }
}

abstract class RevealFixedPoint(value: Value) extends Contract {
  @clause def reveal(hash: Bytes) {
    verify ( bytes(sha256(hash)) == hash )
    unlock ( value )
  }
}

abstract class LockUntil(publicKey: PublicKey, time: Time, value: Value) extends Contract {
  @clause def spend(sig: Signature) {
    verify ( checkSig(publicKey, sig) )
    verify ( after(time) )
    unlock ( value )
  }
}

abstract class LockDelay(publicKey: PublicKey, delay: Duration, value: Value) extends Contract {
  @clause def spend(sig: Signature) {
    verify ( checkSig(publicKey, sig) )
    verify ( older(delay) )
    unlock ( value )
  }
}

abstract class TransferWithTimeout(
    sender: PublicKey,
    recipient: PublicKey,
    timeout: Time,
    value: Value
) extends Contract {
  @clause def transfer(senderSig: Signature, recipientSig: Signature) {
    verify(checkSig(sender, senderSig))
    verify(checkSig(recipient, recipientSig))
    unlock(value)
  }

  @clause def timeout(senderSig: Signature) {
    verify(checkSig(sender, senderSig))
    verify(after(timeout))
    unlock(value)
  }
}

abstract class EscrowWithDelay(
    sender: PublicKey,
    recipient: PublicKey,
    escrow: PublicKey,
    delay: Duration,
    value: Value
) extends Contract {
  @clause def transfer(sig1: Signature, sig2: Signature) {
    verify(checkMultiSig(List(sender, recipient, escrow), List(sig1, sig2)))
    unlock(value)
  }

  @clause def timeout(sig: Signature) {
    verify(checkSig(sender, sig))
    verify(older(delay))
    unlock(value)
  }
}

abstract class VaultSpend(
    hotKey: PublicKey,
    coldKey: PublicKey,
    delay: Duration,
    value: Value
) extends Contract {
  @clause def cancel(sig: Signature) {
    verify(checkSig(coldKey, sig))
    unlock(value)
  }

  @clause def complete(sig: Signature) {
    verify(older(delay))
    verify(checkSig(hotKey, sig))
    unlock(value)
  }
}
