with Clortho.DB_Keys;

package Clortho.Agent_Protocol_Data is
   type Agent_Action is (Get_Key, Bye);

   type Agent_Command (<>) is private;

   function Action (Item : Agent_Command) return Agent_Action;

   function Gimme_Key return Agent_Command;

   function Bye return Agent_Command;

   type Reply_Status is (OK, Data);
   type Agent_Reply (<>) is private;

   function Status (Reply : Agent_Reply) return Reply_Status;

   function OK return Agent_Reply;

   function Key_Data (Key : DB_Keys.DB_Key_Type) return Agent_Reply;

private

   type Agent_Command is
      record
         Action : Agent_Action;
      end record;

   function Gimme_Key return Agent_Command
   is (Agent_Command'(Action => Get_Key));

   function Bye return Agent_Command
   is (Agent_Command'(Action => Bye));

   type Agent_Reply (Name   : Reply_Status;
                     Length : Natural)
   is
      record
         case Name is
            when OK =>
               null;

            when Data =>
               D : String (1 .. Length);
         end case;
      end record;

   function Action (Item : Agent_Command) return Agent_Action
   is (Item.Action);

   function OK return Agent_Reply
   is (Agent_Reply'(Name   => OK,
                    Length => 0));

   function Key_Data (Key : DB_Keys.DB_Key_Type) return Agent_Reply
   is (Agent_Reply'(Name   => Data,
                    Length => Key'Length,
                    D      => String (Key)));

end Clortho.Agent_Protocol_Data;
