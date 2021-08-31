
--
--  This package contains only a procedure that prints a line to the
--  standard error.  Why? Because SPARK does not allow for the use of
--  Standard_Error (why?), so I isolate this here in order to keep the
--  remaining code SPARK compliant.
--

package Clortho.Logging is
   procedure Print_To_Stderr (Msg : String);
end Clortho.Logging;
