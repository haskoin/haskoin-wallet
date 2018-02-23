# Haskoin Wallet

Haskoin Wallet (hw) is a lightweight Bitcoin and Bitcoin Cash command line
wallet. It can be used to manage cold storage funds in an online/offline
environment. An online computer can query balances and prepare transactions that
can be signed securely by an offline (air gapped) computer.

Haskoin Wallet doesn't store blockchain information locally and relies on
information provided by a blockchain indexer. This allows Haskoin Wallet to be
very fast and responsive but requires trust in the indexer. In sensitive
environments, it is recommended to run Haskoin Wallet behind your own trusted
indexer.

## Commands Documentation

```console
hw command args [--options]
```

### mnemonic

```console
hw mnemonic [--dice] [--entropy=20]
```

- **Offline Command** (Should be run on an offline computer)
- Follow up commands: [createacc], [signtx]

Generate a 12 word human readable english [mnemonic] using your systems entropy
(typically /dev/random). You have to write down your mnemonic on paper and keep
it in a safe place. If you loose your mnemonic you will loose all the funds in
your wallet. You will be asked for your mnemonic while creating new accounts
with createacc and signing transactions with the signtx command. Haskoin Wallet
does not store the mnemonic or any private keys generated from it on disk.

#### mnemonic options

```console
-d --dice=True (default False)
```
Complement the system entropy by providing your own entropy using 6-sided dice.
You will be prompted for an exact amount of dice throws depending on how much
entropy is required. If you want to benefit from this feature fully, you must
throw real dice and not computer simulated dice. The result has to be entered as
a sequence of numbers from 1 to 6. The entropy used to generate the [mnemonic]
will be the bitwise xor of the system and dice entropy.
    
```console
-e 20 --entropy=20 (default 16)
```
Specify the amount of entropy that you want to use to generate a [mnemonic]. By
default, 16 bytes are used. You can increase this amount to 20, 24, 28 or 32. If
used together with the --dice command, you will be prompted for additional dice
throws to match the entropy.

### createacc

```console
hw createacc [--deriv=1] [--network=bitcoincash]
```

- **Offline Command** (Should be run on an offline computer)
- Follow up commands: [importacc]

Haskoin Wallet uses the [BIP44] hierarchical deterministic key generation
scheme. The following derivation path is used for addresses:

```
m/44'/coin_type'/account'/change/address_index
```

The [createacc] command will generate an extended [BIP32] public key at the
following derivation level:

```
M/44'/coin_type'/account'
```

As all the derivations below this level are non-hardened, this public key can be
used as a read-only wallet key on an online computer. When running this command,
you will be prompted for your [mnemonic] and passphrase. The output of this
command will be a file in your home directory containing the extended public key
(account key). This file can be imported in an online computer for watching
transactions and balances without having the ability to spend any funds. You can
use the same mnemonic with different passphrases to generate different account
keys. To recover your funds successfully, it is important that you use the same
combination of mnemonic, passphrase and account index (the `--deriv` option).

#### createacc options

```console
-d --deriv=1 (default 0)
```
Choose a different account in the [BIP44] derivation scheme. By default, the account
`0` will be used. You could, for example, create a spending and a savings account 
with the same [mnemonic] and passphrase using accounts `0` and `1`. 

```console
-n --network=[bitcoin,testnet3,bitcoincash,cashtest] (default 0)
```
Specify the network. The following are supported:

- `bitcoin` Production Bitcoin network
- `testnet3` Testnet3 Bitcoin network
- `bitcoincash` Production Bitcoin Cash network
- `cashtest` Testnet Bitcoin Cash network

The network will affect the constants being used to derive addresses and keys.
It will also affect how transactions are signed so they are valid in their
respective networks.
    
[mnemonic]: #mnemonic
[createacc]: #createacc 
[importacc]: #importacc
[signtx]: #signtx
[BIP32]: https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki
[BIP44]: https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki
