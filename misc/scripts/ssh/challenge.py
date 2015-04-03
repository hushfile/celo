#!/usr/bin/env python
# vim: set sw=4 sts=4 et tw=80 :
# -*- coding: utf-8 -*-

import binascii
import sys

import pysodium as s

if __name__ == '__main__':
    if len(sys.argv) < 2:
        sys.exit(1)

    if sys.argv[1] == 'gen':
        (public, secret) = s.crypto_sign_keypair()
        print "Secret: %s" % binascii.hexlify(secret)
        print "Public: %s" % binascii.hexlify(public)

        sys.exit(0)

    if sys.argv[1] == 'sign':
        if len(sys.argv) < 4:
            sys.exit(1)

        secret = sys.argv[2]
        token  = sys.argv[3]

        print "Signing using '%s': '%s'" % (secret, token)
        print "Token: %s" % binascii.hexlify(s.crypto_sign(token, binascii.unhexlify(secret)))

        sys.exit(0)
