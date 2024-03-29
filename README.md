# Haskoin Wallet

Haskoin Wallet (`hw`) is a lightweight [BIP44] command line wallet for bitcoin
and bitcoin-cash. It can be used to manage cold storage funds in an
online/offline environment. It requires a haskoin-store server for querying
addresses and transactions. It is suitable for small-ish wallets that are
managed by hand although work is being done to improve performance.

## Dependencies

```console
apt install libsecp256k1-dev
```

## Build

```console
stack build && stack test
stack install
```

## Usage

```console
hw --help
hw COMMAND --help
```

## Verify release binaries

### Get the GPG key

```console
gpg --recv-keys 5E98C15CC51B3E9EF0D40EA61690774F742FEAC4
gpg --list-keys --fingerprint 5E98C15CC51B3E9EF0D40EA61690774F742FEAC4
```

Make sure the key fingerprint matches this string:

```console
5E98 C15C C51B 3E9E F0D4  0EA6 1690 774F 742F EAC4
```

### Verify the signature file and hashes

```console
gpg --verify SHA256SUMS.asc
sha256sum --check SHA256SUMS
```

## Critical Bugs

* Versions prior to 0.8.0 do not derive the correct accounts when using more
  than one mnemonic (not the --split option, but different wallets). For
  example, creating an account with mnemonic 1 yields account /44'/0'/0'. If you
  then create an account using mnemonic 2, it yields the account /44'/0'/1'. As
  it is a new mnemonic and thus a new wallet, it should yield account
  /44'/0'/0'. Your funds are still secure and not lost but you should write down
  the account derivation that you are using for reference if you need to recover
  it. This bug is fixed in version 0.8.0.

* Version 0.7.0 does not derive the correct account when using a --split
  mnemonic. Your funds are still secure and not lost but you should use the
  0.7.0 binary to move your funds to a new wallet created with the latest
  release.

[BIP32]: https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki
[BIP32]: https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki
[BIP44]: https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki
