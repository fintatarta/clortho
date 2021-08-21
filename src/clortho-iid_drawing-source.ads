with System;

--
--  This package is a low-level package that generates a random bit string
--  in a cryptographically strong way.  It can use OS resources; for example,
--  in recent Linux it calls C function getrandom().
--
--  Because of its dependency on the available OS resources, the body
--  the body will differ, depending on the OS  Note that the specs are
--  OS independent.
--
private package Clortho.IID_Drawing.Source is
   type Word is mod System.Max_Binary_Modulus;

   function Random_Word return Word;
end Clortho.IID_Drawing.Source;
