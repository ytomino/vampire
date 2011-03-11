-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Calendar;
with Ada.Text_IO;
with Ada.Directories;
with Ada.IO_Exceptions;
with Tabula.Configurations;
with Tabula.Debug;
procedure Tabula.Unlock is
	Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
	Count : Natural := 0;
begin
	Debug.Hook (Configurations.Debug_Log_File_Name'Access, Now);
	Deleting: loop
		begin
			Ada.Directories.Delete_Tree (Tabula.Configurations.Lock_Name);
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
