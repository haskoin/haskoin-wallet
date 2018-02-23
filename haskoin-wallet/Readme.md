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
- Follow up commands: [createacc](#createacc), [signtx](#signtx)

Generate a 12 word human readable english mnemonic using your systems entropy
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
a sequence of numbers from 1 to 6. The entropy used to generate the mnemonic
will be the bitwise xor of the system and dice entropy.
    
```console
-e 20 --entropy=20 (default 16)
```
Specify the amount of entropy that you want to use to generate a mnemonic. By
default, 16 bytes are used. You can increase this amount to 20, 24, 28 or 32. If
used together with the --dice command, you will be prompted for additional dice
throws to match the entropy.
