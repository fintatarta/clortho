with Clortho.Command_Line;
with Clortho.Exit_Statuses;
with Clortho.Db;

package Clortho.Command_Dispatchers is
   pragma SPARK_Mode;

   type Dispatcher_Type is
     new Db.Abstract_Db_User
   with
     private;

   function New_Dispatcher (Config : Command_Line.Parsed_CLI)
                            return Dispatcher_Type
     with
       Pre => Command_Line.Is_Ok (Config);

   overriding procedure Callback (User    : in out Dispatcher_Type;
                                  Db      : in out Clortho.Db.Password_Db;
                                  Status  : out Exit_Statuses.Exit_Status);

private
   type Dispatcher_Type is new Db.Abstract_Db_User
     with
      record
         Config : Command_Line.Parsed_CLI;
      end record;
end Clortho.Command_Dispatchers;
