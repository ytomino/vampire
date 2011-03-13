-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Calendar;
with Ada.Text_IO;
with Ada.Directories;
with Ada.IO_Exceptions;
with Tabula.Debug;
procedure Tabula.Unlock (
	Lock_Name : in not null Static_String_Access;
	Debug_Log_File_Name : in not null Static_String_Access)
is
	Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
	Count : Natural := 0;
begin
	Debug.Hook (Debug_Log_File_Name, Now);
	Deleting: loop
		begin
			Ada.Directories.Delete_Tree (Lock_Name.all);
			delay 0.1;
			Count := Count + 1;
		exception
			when Ada.IO_Exceptions.Name_Error => exit Deleting;
		end;
	end loop Deleting;
	declare
		use Ada.Text_IO;
		Message : constant String := Natural'Image (Count) & " OK.";
	begin
		Ada.Debug.Put (Message);
		Put ("content-type: text/plain"); New_Line;
		New_Line;
		Put (Message); New_Line;
	end;
end Tabula.Unlock;
