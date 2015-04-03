SSH Challenge Auth for Celo
===========================

1. Install `pysodium` with:

    $ pip install pysodium

2. Create an ed25519 key pair:

        $ ./challenge.py gen
        Secret: ***secret key***
        Public: ***public key***

3. SSH into Celo to receive the challenge. Your public key is your username and
   will grant you access to the public key's storage area.

   See `config/sys.config` for timeout and default passphrase values.

        $ ssh -p 10022 ***public key***@127.0.0.1
        ***public key***@127.0.0.1's password: ***default challenge passphrase***
        Token expires in 60 seconds
        Token: ***challenge token***
        Connection to 127.0.0.1 closed.

4. Generate a signed challenge token using your ed25519 secret key and the token.

        ./challenge.py sign ***secret key*** ***challenge token***
        Signing using '***secret key***': '***challenge token***'
        Token: ***signed challenge token***

6. Use the signed challenge token to authenticate to Celo's sftp storage
   front-end.
