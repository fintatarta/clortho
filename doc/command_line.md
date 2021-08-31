# Command line examples

* Retrieve the password and save it to clipboard`clortho www.example.com`
* Retrieve the password, but get the name from clipboard `clortho`

## Actions

Actions are specified with options

### Password retrival 
* No option: just retrieve the password
* `--old` retrieve the password, but not the current one
* `--back=n`retrieve an old password

### Cleaning
* `--vacuum` clean the old passowrds of the specified name
* `--scurdammoce-o-passato` clean *all* the old passwords

### Create/renewe
* `--create`create a new entry in the database
* `--renew` create a new password for an existing entry
* `--password=foo` provide the password; if it is not specified it is generated randomly
* When the password is generated
  * With `--len` the number of characters of the password can be specified
  * With `--nbit` or `--nbits` the number of bits of entropy can be specified. `--len` and `--nbits` are mutually exclusive
  * With `--charset` we can specify which characters must/may be used in creating the password
  * The above options are not compatible with `--password`

