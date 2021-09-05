package Clortho.DB_Keys is
   type DB_Key_Type is new String;

   function Get return DB_Key_Type;

   function Is_Agent_Call return Boolean;

   procedure Work_As_Agent;
end Clortho.DB_Keys;
