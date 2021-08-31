# Command line examples

* Retrieve the password and save it to clipboard`clortho www.example.com`
* Retrieve the password, but get the name from clipboard `clortho`

## Actions

Actions are specified with options

### Password retrival 
* No option: just retrieve the password
* `--old` retrieve the previous password, equivalent to `--back=1`
* `--back=n`retrieve an old password

### Cleaning
* `--vacuum` clean the old passowrds of the specified name (to be specified on the CLI)
* `--scurdammoce-o-passato` clean *all* the old passwords, ask confirmation interactively

### Create/renewe
* `--create`create a new entry in the database
* `--renew` create a new password for an existing entry
* `--password=foo` provide the password; if it is not specified it is generated randomly
* When the password is generated
  * With `--len` the number of characters of the password can be specified
  * With `--nbit` or `--nbits` the number of bits of entropy can be specified. `--len` and `--nbits` are mutually exclusive
  * With `--charset` we can specify which characters must/may be used in creating the password
  * The above options are not compatible with `--password`
* The latest change can be reverted with `--roll-back` or `--undo` (they are equivalent).
* `--delete` remove completely the entry from the DB. Confirmation.

## Matching Options

* Name matching is case insensitive by default, it can be made case sensitive with `--case-sensitive`. 
* Name matching is done in a "smart" way if both arguments match the URL syntax, it is done in "basic" way otherwise. The strict matching can be forced with `--not-smart` or `--strict`
* If specified at creation time, matching options are associated with the entry; at retrieval time they apply only to the query

## In/Out options

* To be used in password retrieval
  * `--in` or `-i` get the name from stdin
  * `--out` or `-o` print the password to stdout
  * `--inout`, `--in-out`, `--filter` or `-f` is equivalent to `-i` and `-o` (act like a filter)


