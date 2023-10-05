# Haskoin Wallet

Haskoin Wallet (`hw`) is a lightweight BIP44 command line wallet for bitcoin and bitcoin-cash. It can be used to manage cold storage funds in an online/offline environment. It requires a haskoin-store server for querying addresses and transactions. It is currently not suitable for large wallets as the local database is a JSON file.

## Build

```console
stack build && stack test
stack install
```

## Usage

```console
hw --help
```

## Verify binaries

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

[BIP32]: https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki
[BIP32]: https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki
[BIP44]: https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki
