with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

package Clortho.Db is
   pragma SPARK_Mode;

   type Password_Db is private;

   type Cursor is new Natural;

   type Abstract_Db_User is interface;

   procedure Callback (User : in out Abstract_Db_User;
                       Db   : in out Password_Db)
   is abstract
     with
       Pre'Class => Is_Valid (Db),
         Post'Class => Is_Valid (Db);

   function Is_Valid (Db : Password_Db) return Boolean;

   function Error_Message (Db : Password_Db) return String
     with
       Pre => not Is_Valid (Db);

   function Empty_Db return Password_Db
     with
       Post =>
         Is_Valid (Empty_Db'Result)
         and then First (Empty_Db'Result) > Last (Empty_Db'Result);

   function First (Db : Password_Db) return Cursor
     with
       Pre => Is_Valid (Db);

   function Last (Db : Password_Db) return Cursor
     with
       Pre => Is_Valid (Db);

   function Name (Db    : Password_Db;
                  Index : Cursor)
                  return String
     with
       Pre =>
         Is_Valid (Db)
         and then Index >= First (Db)
         and then Index <= Last (Db);

   function Load (Filename : String;
                  DB_Key   : String)
                  return Password_Db;

   procedure Save (Filename : String;
                   DB_Key   : String;
                   Db       : Password_Db)
     with
       Pre => Is_Valid (Db);

   procedure Use_DB (Filename      : String;
                     DB_Key        : String;
                     User          : in out Abstract_Db_User'Class;
                     Success       :    out Boolean;
                     Error_Message : out Unbounded_String);

   function Contains (Db   : Password_Db;
                      Name : String)
                      return Boolean
     with
       Pre => Is_Valid (Db);

   function N_Versions (Db   : Password_Db;
                        Name : String)
                        return Positive
     with
       Pre => Is_Valid (Db) and then Contains (Db, Name);

   function Get_Password (Db      : Password_Db;
                          Name    : String;
                          Version : Positive := 1)
                          return String
     with
       Pre =>
         Is_Valid (Db)
         and then Contains (Db, Name)
         and then Version <= N_Versions (Db, Name);

   procedure Create_Entry (Db       : in out Password_Db;
                           Name     : String;
                           Password : String)
     with
       Pre =>
         Is_Valid (Db)
         and then not Contains (Db, Name),
         Post =>
           Is_Valid (Db)
           and then Contains (Db, Name)
           and then N_Versions (Db, Name) = 1
           and then Get_Password (Db, Name) = Password;

   procedure Push_Password (Db       : in out Password_Db;
                            Name     : String;
                            Password : String)
     with
       Pre =>
         Is_Valid (Db)
         and then Contains (Db, Name),
         Post =>
           Is_Valid (Db)
           and then Contains (Db, Name)
           and then N_Versions (Db, Name) - 1 = N_Versions (Db'Old, Name)
           and then Get_Password (Db, Name) = Password;

   procedure Pop_Password (Db   : in out Password_Db;
                           Name : String)
     with
       Pre =>
         Is_Valid (Db)
         and then Contains (Db, Name)
         and then N_Versions (Db, Name) > 1,
         Post =>
           Is_Valid (Db)
           and then Contains (Db, Name)
           and then N_Versions (Db, Name) = N_Versions (Db'Old, Name) - 1;

   procedure Vacuum_Entry (Db   : in out Password_Db;
                           Name : String)
     with
       Pre =>
         Is_Valid (Db)
         and then Contains (Db, Name),
         Post =>
           Is_Valid (Db)
           and then Contains (Db, Name)
           and then N_Versions (Db, Name) = 1;

private
   type Password_Db is null record;
end Clortho.Db;
