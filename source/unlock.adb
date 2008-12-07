-- The Village of Vampire by YT, このソースコードはNYSLです
pragma Ada_05;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories;
with Ada.IO_Exceptions;
with Tabula.Configurations;
procedure Unlock is
	Count : Natural := 0;
begin
	Deleting: loop
		begin
			Ada.Directories.Delete_File(Tabula.Configurations.Lock_Name);
			delay 0.1;
			Count := Count + 1;
		exception
			when Ada.IO_Exceptions.Name_Error =>
				exit Deleting;
		end;
	end loop Deleting;
	Put("content-type: text/plain"); New_Line;
	New_Line;
	Put(Natural'Image(Count)); Put(" OK."); New_Line;
end Unlock;
