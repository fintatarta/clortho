with Clortho.DB_Keys;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Clortho.Agent_Protocol_Data is
   type Agent_Action is (Get_Key, Bye);

   type Agent_Command is private;

   function Action (Item : Agent_Command) return Agent_Action;

   function Gimme_Key return Agent_Command;

   function Bye return Agent_Command;

   type Reply_Status is (OK, Data);
   type Agent_Reply is private;

   function Status (Reply : Agent_Reply) return Reply_Status;

   function OK return Agent_Reply;

   function Key_Data (Key : DB_Keys.DB_Key_Type) return Agent_Reply;

   function Key (Reply : Agent_Reply) return DB_Keys.DB_Key_Type
     with
       Pre => Status (Reply) = Data;

private

   type Agent_Command is
      record
         Action : Agent_Action;
      end record;

   function Gimme_Key return Agent_Command
   is (Agent_Command'(Action => Get_Key));

   function Bye return Agent_Command
   is (Agent_Command'(Action => Bye));

   type Agent_Reply is
      record
         Status : Reply_Status;
         Data   : Unbounded_String;
      end record;

   function Action (Item : Agent_Command) return Agent_Action
   is (Item.Action);

   function OK return Agent_Reply
   is (Agent_Reply'(Status => OK,
                    Data   => Null_Unbounded_String));

   function Key_Data (Key : DB_Keys.DB_Key_Type) return Agent_Reply
   is (Agent_Reply'(Status => Data,
                    Data   => To_Unbounded_String (String (Key))));

end Clortho.Agent_Protocol_Data;
