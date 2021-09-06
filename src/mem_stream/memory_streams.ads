
with Ada.Streams;              use Ada.Streams;
with Ada.Containers.Indefinite_Holders;

package Memory_Streams is
   pragma SPARK_Mode;
   pragma Elaborate_Body;

   type Memory_Stream (<>) is
     new Ada.Streams.Root_Stream_Type
   with
     private;

   function Create (Data : Stream_Element_Array) return Memory_Stream
     with
       Post =>
         Free_Space (Create'Result) = 0
         and Capacity (Create'Result) = Data'Length;

   function Create  (Size : Stream_Element_Count) return Memory_Stream
     with
       Post =>
         Free_Space (Create'Result) = Size
         and Capacity (Create'Result) = Size;

   function Resize (Stream : Memory_Stream;
                    Size   : Stream_Element_Count)
                    return Memory_Stream;

   function Free_Space (Stream : Memory_Stream) return Stream_Element_Count;

   function Capacity (Stream : Memory_Stream) return Stream_Element_Offset;

   procedure Read (Stream : in out Memory_Stream;
                   Item   : out Stream_Element_Array;
                   Last   : out Stream_Element_Offset);

   procedure Write (Stream : in out Memory_Stream;
                    Item   : in Stream_Element_Array);

   function Dump (Stream : Memory_Stream) return Stream_Element_Array;

   Capacity_Error : exception;
private
   package H is
     new Ada.Containers.Indefinite_Holders (Stream_Element_Array);

   type Memory_Stream (Size : Stream_Element_Count) is
     new Ada.Streams.Root_Stream_Type
   with
      record
         --  Buffer : H.Holder;
         Buffer        : Stream_Element_Array (1 .. Size);
         Next_To_Read  : Stream_Element_Offset;
         Next_To_Write : Stream_Element_Offset;
      end record;
end Memory_Streams;
