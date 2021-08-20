# Ideas in random order about what clortho should du

## Model

* Clortho is just a table that maps _names_ to _secrets_.  Usually a name is a website, but it can be also a label
* In a typical usage `clortho` is called by giving on the command line a name. If the name is in the database, it returns the password.  The password can be "returned" in the clipboard or printed to the standard output
* If no name is given on the command line, `clortho` gets the name from the clipboard
* If the name is not in the database, `clortho` exit with an error. It does not create a new entry by default because in this way one can check if a name exists.
* The matching between an entry name and the CL name is done as follows
  * If both names match the URL syntax `scheme://host:port/path?query-string#fragment-id` then
    * The names are parsed in the different components `scheme`, `host`, `port`, `path`, `query` and `fragment` 
    * Two names does not match if there is a component defined in both names that assumes different values
    * If two names match their "matching strength" is equal to the number of defined components that matches
    * The entry that "matches more" the CL is selected
  * If at least one name is not an URL, then the match is just a string comparison
  * Matching is done in a case-insensitive way, unless a specific option is used
  * If --strict is specified, URLs are matched as strings

* A name remembers all the past passwords.  This is useful because sometimes it can happen that a password is renewed by mistake and overwriting the old can be quite annoying. (Yes, it happened to me)
* An old password can be recovered with `--old` (for the previous one) or `--back=n` (for the `n`-th one, `--back=1` is equivalent to `--old`)
* The latest change can be reverted with `--roll-back` or `--undo` (they are equivalent).
* With `--vacuum` all the previous passwords of a specific name are forgotten
* With `--scurdammoce-o-passato` (_let's forget the past_ in Neapolitan :wink:) all the previous passwords of *all names* are forgotten

* A new entry can be created with `--create`
* To an existing entry we can add a new password with `--renew`
* The password can be specified directly with `--password=foobar`; if `--password=-` is used, the password is read from standard input
* If no `--password` is given, the password will be randomly generated
* When the password is generated
  * With `--len` the number of characters of the password can be specified
  * With `--nbit` or `--nbits` the number of bits of entropy can be specified. `--len` and `--nbits` are mutually exclusive
  * With `--charset` we can specify which characters must/may be used in creating the password

## Charset spec

The model behind the specification of the charset is the following

* The set of printable ascii characters (32-127) is partitioned into _sets_
* Every set has two limits: the minimum (`min`) and the maximum (`max`) number of characters that must be used
* `min` is >= 0, `max` can be infinity; it is always `min <= max`
* If `max = 0` the characters in the set cannot be used in the password (as unbelivable as it can be, there are few sites that have a set of _prohibited_ characters, why making the password weaker by design?)
* The charset is represented with a string like this

```
/A-Z/a-z/0-9/-$%+*/!?!/
```
This string specifies that the password must have at least one uppercase letter, one lowercase letter, one digit and one character in the sset `-$%+*`; question mark and esclamation cannot be used. Note that the string above is **not** equivalent to 
```
/A-Za-z0-9/-$%+*/!?!/
```
since in this case we require that at least one alphanumeric character is present, while in the previous case we were asking for one uppercase, one lowercase and one digit.

The syntax of the spec string is as follows
```
  spec          = '/' (set-spec '/')+
  set-spec      = option-list? char-or-range*
  char-or-range = char ('-' char)
  char          = "any character but '/'" | '//'
  option-list   = '::' option (',' option) '::'
  option        = name ('=' value-char*)?
  name          = [a-zA-Z-]+
  value-char    = "any character but ','"
```
In other words, the spec string is a sequence of sets of characters expressed in a way similar to the `[...]` construction in regexp (we use `/` instead of `[...]` since the latter has a special meaning in most shells and would always require quoting. The `option-list` is not used now, but we defined it, just to have a placeholder in the syntax. In the future maybe it will be used to set `min` and `max` esplicitely (it is not clear if this is useful)

Every `set-spec` in the spec is interpreted as follows

* If `set-spec` has more than one character and it begins with `!`, the set is marked as _prohibited_ (`max=min=0`), the `!` removed and processing continues. If `set-spec = "!"`, then the set is a mandatory set that contains only `!`
* If `set-spec` has more than one character and it begins with `^`, the specified set is _complemented_ (e.g., ),  the `^` removed and processing continues. If `set-spec = "^"`, then the set  contains only `^`. For example
  * `/^A-Za-z/` is the set of all non-alphabetic characters
  * if `/!0-9/` is used, the password can contain only digits
  * with `/!^/` the password cannot have a `^`
* At this point the set is the union of all the chars in the `set-spec`. If `//` is used does not close the set, but it is equivalent to `/`.  For example `////` is the set with only `/`, `/^///` is everything but `/`

After all the `set-spec` in the spec string have been parsed
* All the sets that are marked with `!` are merged into a single _prohibited_ set
* All the other specified sets are the _mandatory_ set
* The set of the characters not specified in the string are the _optional_ set
* Any character in common between a mandatory set and the prohibited set is removed from the mandatory set; the same holds for the optional and the prohibited set. For example, with the spec `/A-Za-z/!wW/` the password must contain only alphabetic characters, but not `w` nor `W` (a funny constraint, I agree)
* To the prohibited set `min=max=0` is assigned
* To each mandatory set `min=1, max=inf` is assigned
* To the optional set `min=0, max=inf` is assigned

